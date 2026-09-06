;;; bili-api.el --- Bilibili API transport and WBI signing  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 0WD0
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; HTTPS-only read transport, response validation, Effect cancellation
;; capabilities, and WBI signing.  This is the only module that sends cookies.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'url-http)
(require 'url-parse)
(require 'url-util)
(require 'appkit-core)
(require 'appkit-effect)
(require 'bili-auth)
(require 'bili-core)

(defvar url-http-response-status)
(defvar url-http-end-of-headers)
(defconst bili-api--web-root "https://api.bilibili.com"
  "Trusted root for Bilibili web APIs.")

(defconst bili-api--live-root "https://api.live.bilibili.com"
  "Trusted root for Bilibili live APIs.")

(defconst bili-api-user-agent
  "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 bili.el/0.1.0"
  "User-Agent sent only to trusted Bilibili API and media origins.")

(defconst bili-api--response-limit (* 4 1024 1024)
  "Maximum bytes accepted in one Bilibili response body.")

(defconst bili-api--wbi-mixin-table
  [46 47 18 2 53 8 23 32 15 50 10 31 58 3 45 35
      27 43 5 49 33 9 42 19 29 28 14 39 12 38 41 13
      37 48 7 16 24 55 40 61 26 17 0 1 60 51 30 4
      22 25 54 21 56 59 6 63 57 62 11 36 20 34 44 52]
  "Permutation used to derive Bilibili's WBI mixin key.")

(defconst bili-api--effect-owner (make-symbol "bili-api-effect-owner")
  "Sentinel selecting Effect-owned transport without an AppKit handle.")

(defvar bili-api--wbi-key nil
  "Transport-local cached WBI signing key.")

(defvar bili-api--wbi-expires-at nil
  "Expiry time for `bili-api--wbi-key'.")

(cl-defstruct (bili-api-request
               (:constructor bili-api--request-create))
  "One Appkit-owned, possibly multi-step Bilibili read request."
  owner
  callback
  errback
  buffer
  handle
  credential
  settled-p)

(defun bili-api--trusted-url-p (value)
  "Return non-nil when VALUE is a trusted Bilibili API URL."
  (when (stringp value)
    (condition-case nil
        (let ((url (url-generic-parse-url value)))
          (and (string-equal (url-type url) "https")
               (member (downcase (or (url-host url) ""))
                       '("api.bilibili.com" "api.live.bilibili.com"))
               (or (null (url-port url)) (= (url-port url) 443))
               (null (url-user url))
               (null (url-password url))))
      (error nil))))

(defun bili-api--parameter-text (value)
  "Return protocol query text for VALUE."
  (cond
   ((stringp value) (substring-no-properties value))
   ((integerp value) (number-to-string value))
   ((eq value t) "true")
   ((null value) "")
   (t (error "Invalid Bilibili query parameter: %S" value))))

(defun bili-api--parameter-name (key)
  "Return protocol query name for KEY."
  (let ((name (if (symbolp key) (symbol-name key) key)))
    (unless (and (stringp name)
                 (string-match-p "\\`[A-Za-z0-9_]+\\'" name))
      (error "Invalid Bilibili query name: %S" key))
    name))

(defun bili-api--query-string (parameters)
  "Encode query PARAMETERS without mutating them."
  (mapconcat
   (lambda (pair)
     (format "%s=%s"
             (url-hexify-string (bili-api--parameter-name (car pair)))
             (url-hexify-string (bili-api--parameter-text (cdr pair)))))
   parameters "&"))

(defun bili-api--endpoint-url (root endpoint parameters)
  "Return trusted ROOT and ENDPOINT with encoded PARAMETERS."
  (unless (and (stringp endpoint)
               (string-prefix-p "/" endpoint)
               (not (string-match-p "[?#]" endpoint)))
    (error "Invalid Bilibili API endpoint: %S" endpoint))
  (concat root endpoint
          (when parameters
            (concat "?" (bili-api--query-string parameters)))))

(defun bili-api-wbi-mixin-key (image-url sub-url)
  "Derive a WBI mixin key from IMAGE-URL and SUB-URL."
  (cl-labels
      ((file-key
         (url)
         (unless (stringp url)
           (error "Bilibili WBI image URL is invalid"))
         (let* ((path (url-filename (url-generic-parse-url url)))
                (name (file-name-base (or (file-name-nondirectory path) ""))))
           (unless (= (length name) 32)
             (error "Bilibili WBI image key is invalid"))
           name)))
    (let* ((source (concat (file-key image-url) (file-key sub-url)))
           (mixed
            (mapconcat
             (lambda (index) (string (aref source index)))
             bili-api--wbi-mixin-table "")))
      (substring mixed 0 32))))

(defun bili-api-wbi-sign (parameters mixin-key timestamp)
  "Return sorted signed WBI PARAMETERS for MIXIN-KEY and TIMESTAMP."
  (unless (and (stringp mixin-key) (= (length mixin-key) 32))
    (error "Bilibili WBI mixin key is invalid"))
  (unless (and (integerp timestamp) (> timestamp 0))
    (error "Bilibili WBI timestamp is invalid"))
  (let ((normalized
         (cl-loop for (key . value) in parameters
                  unless (equal (bili-api--parameter-name key) "w_rid")
                  collect
                  (cons (bili-api--parameter-name key)
                        (replace-regexp-in-string
                         "[!'()*]" "" (bili-api--parameter-text value))))))
    (setq normalized
          (cons (cons "wts" (number-to-string timestamp))
                (cl-remove-if (lambda (pair) (equal (car pair) "wts"))
                              normalized)))
    (setq normalized
          (sort normalized (lambda (left right)
                             (string< (car left) (car right)))))
    (append normalized
            (list
             (cons "w_rid"
                   (md5 (concat (bili-api--query-string normalized)
                                mixin-key)))))))

(defun bili-api--safe-error-message (error-data credential)
  "Return ERROR-DATA text with values from CREDENTIAL redacted."
  (let ((message (error-message-string error-data)))
    (when (bili-auth-credential-p credential)
      (dolist (secret (list (bili-auth-credential-sessdata credential)
                            (bili-auth-credential-csrf credential)))
        (when (and (stringp secret) (not (string-empty-p secret)))
          (setq message (string-replace secret "[REDACTED]" message)))))
    message))

(defun bili-api--discard-buffer (buffer)
  "Stop and kill retrieval BUFFER without delivering a callback."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when-let* ((process (get-buffer-process buffer)))
        (set-process-sentinel process nil)
        (when (process-live-p process)
          (delete-process process)))
      (let ((kill-buffer-query-functions nil))
        (kill-buffer buffer)))))

(defun bili-api--retire (request)
  "Retire REQUEST's Appkit handle and transport buffer."
  (when-let* ((handle (bili-api-request-handle request)))
    (appkit-retire-handle handle))
  (setf (bili-api-request-handle request) nil)
  (bili-api--discard-buffer (bili-api-request-buffer request))
  (setf (bili-api-request-buffer request) nil))

(defun bili-api--cancel-owned (request)
  "Cancel Appkit-owned Bilibili REQUEST without user callbacks."
  (unless (bili-api-request-settled-p request)
    (setf (bili-api-request-settled-p request) t)
    (bili-api--retire request)))

(defun bili-api-cancel (request)
  "Cancel opaque in-flight Bilibili REQUEST exactly once."
  (unless (bili-api-request-p request)
    (error "Invalid Bilibili request"))
  (if-let* ((handle (bili-api-request-handle request))
            ((appkit-handle-alive-p handle)))
      (appkit-cancel-handle handle)
    (bili-api--cancel-owned request))
  t)

(defun bili-api--fail (request message)
  "Settle REQUEST through its error callback with MESSAGE."
  (unless (bili-api-request-settled-p request)
    (setf (bili-api-request-settled-p request) t)
    (let ((errback (bili-api-request-errback request)))
      (bili-api--retire request)
      (funcall errback message))))

(defun bili-api--succeed (request value)
  "Settle REQUEST through its success callback with VALUE."
  (unless (bili-api-request-settled-p request)
    (setf (bili-api-request-settled-p request) t)
    (let ((callback (bili-api-request-callback request)))
      (bili-api--retire request)
      (funcall callback value))))

(defun bili-api--response-data (status &optional accepted-codes binary-p)
  "Decode STATUS after HTTP and byte-size validation.
ACCEPTED-CODES permits provider codes; BINARY-P returns raw bytes, not JSON."
  (when-let* ((transport-error (plist-get status :error)))
    (error "Bilibili transport failed: %s" transport-error))
  (unless (and (integerp url-http-response-status)
               (<= 200 url-http-response-status 299))
    (error "Bilibili HTTP request failed with status %s"
           (or url-http-response-status "unknown")))
  (unless (and (integer-or-marker-p url-http-end-of-headers)
               (<= (point-min) url-http-end-of-headers)
               (< url-http-end-of-headers (point-max))
               (eq (char-after url-http-end-of-headers) ?\n))
    (error "Bilibili response has no complete headers"))
  ;; url-http's marker is ON the final header LF, not after it.  A protobuf
  ;; body can itself start with LF (field 1, wire type 2); preserve that byte.
  (let ((body-start (1+ url-http-end-of-headers)))
    (when (> (- (position-bytes (point-max)) (position-bytes body-start))
             bili-api--response-limit)
      (error "Bilibili response exceeds %d bytes" bili-api--response-limit))
    (goto-char body-start)
    (if binary-p
        (let ((json-p
               (save-excursion
                 (goto-char (point-min))
                 (let ((case-fold-search t))
                   (re-search-forward
                    "^Content-Type:[ \t]*application/[^\n]*json"
                    url-http-end-of-headers t)))))
          (when (or json-p
                    (save-excursion
                      (condition-case nil
                          (let ((payload (json-parse-buffer
                                          :object-type 'alist :array-type 'list)))
                            (and (listp payload) (assq 'code payload)))
                        (error nil))))
            (bili-api--response-data status accepted-codes)
            (error "Bilibili returned JSON instead of danmaku protobuf"))
          (encode-coding-string
           (buffer-substring-no-properties body-start (point-max)) 'binary))
      (let* ((payload (json-parse-buffer
                       :object-type 'alist :array-type 'list
                       :null-object nil :false-object nil))
             (code (alist-get 'code payload)))
        (unless (and (integerp code) (memq code (cons 0 accepted-codes)))
          (error "Bilibili API error %s: %s"
                 (or code "unknown")
                 (or (alist-get 'message payload)
                     (alist-get 'msg payload) "unknown response")))
        (alist-get 'data payload)))))

(defun bili-api--finish-step
    (request step-callback status &optional accepted-codes binary-p)
  "Finish REQUEST with STATUS, delivering decoded data to STEP-CALLBACK.
ACCEPTED-CODES permits provider codes; BINARY-P selects raw response bytes."
  (let ((buffer (current-buffer)) result failure)
    (when (and (not (bili-api-request-settled-p request))
               (eq buffer (bili-api-request-buffer request))
               (or (null (bili-api-request-handle request))
                   (appkit-handle-alive-p (bili-api-request-handle request))))
      (condition-case error-data
          (setq result (bili-api--response-data status accepted-codes binary-p))
        (error (setq failure (bili-api--safe-error-message
                             error-data (bili-api-request-credential request)))))
      (setf (bili-api-request-buffer request) nil
            (bili-api-request-credential request) nil)
      (bili-api--discard-buffer buffer)
      (if failure (bili-api--fail request failure)
        (funcall step-callback result)))))

(defun bili-api--headers (credential &optional binary-p)
  "Return trusted API headers, optionally including CREDENTIAL.
BINARY-P requests protobuf rather than the default JSON representation."
  (append
   `(("Accept" . ,(if binary-p "application/octet-stream" "application/json"))
     ("Referer" . "https://www.bilibili.com/")
     ("User-Agent" . ,bili-api-user-agent))
   (when credential
     `(("Cookie" . ,(bili-auth-cookie-header credential))))))

(defun bili-api--dispatch-step
    (request url step-callback &optional accepted-codes binary-p)
  "Dispatch trusted URL as one GET step of REQUEST.
ACCEPTED-CODES permits provider codes; BINARY-P selects raw response bytes."
  (unless (bili-api--trusted-url-p url)
    (error "Bilibili request URL is not trusted: %S" url))
  (let* ((credential (bili-auth-credentials-if-available))
         (url-max-redirections 0)
         (url-http-attempt-keepalives (and url-http-attempt-keepalives
                                         (null credential)))
         (url-request-method "GET")
         (url-request-data nil)
         (url-request-extra-headers (bili-api--headers credential binary-p))
         buffer ready-p called-p pending-status)
    (setf (bili-api-request-credential request) credential)
    (let ((inhibit-quit t))
      (setq buffer
            (url-retrieve
             (encode-coding-string url 'us-ascii)
             (lambda (status)
               (if ready-p
                   (bili-api--finish-step request step-callback status
                                          accepted-codes binary-p)
                 (setq called-p t pending-status status)))
             nil t t)))
    (unless (buffer-live-p buffer)
      (error "Bilibili did not start the HTTP request"))
    (if (bili-api-request-settled-p request)
        (bili-api--discard-buffer buffer)
      (setf (bili-api-request-buffer request) buffer)
      (setq ready-p t)
      (when called-p
        (with-current-buffer buffer
          (bili-api--finish-step request step-callback pending-status
                                 accepted-codes binary-p))))
    buffer))

(defun bili-api--start (callback errback owner starter)
  "Create a request for CALLBACK and ERRBACK, then call STARTER.

OWNER is either a live AppKit lifecycle owner or `bili-api--effect-owner'.
Effect-owned requests return their opaque request as the transport
cancellation target and do not register a second lifecycle authority."
  (unless (functionp callback)
    (error "Bilibili request callback is not callable"))
  (let* ((error-fn (or errback (lambda (message) (message "%s" message))))
         (effect-owned-p (eq owner bili-api--effect-owner))
         (request-owner (and (not effect-owned-p)
                             (or owner (bili-core-app))))
         (request
           (bili-api--request-create
            :owner request-owner :callback callback :errback error-fn)))
    (unless (functionp error-fn)
      (error "Bilibili request error callback is not callable"))
    (unless (or effect-owned-p (appkit-owner-live-p request-owner))
      (error "Bilibili request owner is not live"))
    (unless effect-owned-p
      (setf (bili-api-request-handle request)
            (appkit-register-handle
             request-owner 'function request #'bili-api--cancel-owned)))
    (condition-case error-data
        (progn
          (funcall starter request)
          (unless (bili-api-request-settled-p request) request))
      ((error quit)
       (let ((quit-p (eq (car error-data) 'quit))
             (message
              (bili-api--safe-error-message
               error-data (bili-api-request-credential request))))
         (bili-api--fail request message)
         (when quit-p
           (signal 'quit nil))
         nil)))))

(cl-defun bili-api-get (root endpoint parameters callback &key errback owner)
  "GET ROOT ENDPOINT with PARAMETERS and deliver validated data to CALLBACK.

ERRBACK receives a readable failure string.  OWNER defaults to the bili.el
Appkit application and owns cancellation."
  (bili-api--start
   callback errback owner
   (lambda (request)
     (bili-api--dispatch-step
      request (bili-api--endpoint-url root endpoint parameters)
      (lambda (data) (bili-api--succeed request data))))))

(defun bili-api--cached-wbi-key ()
  "Return the current transport WBI key, or nil when stale."
  (when (and (stringp bili-api--wbi-key)
             (numberp bili-api--wbi-expires-at)
             (> bili-api--wbi-expires-at (float-time)))
    bili-api--wbi-key))

(defun bili-api--install-wbi-key (data)
  "Derive, cache, and return a WBI key from navigation DATA."
  (let* ((wbi (alist-get 'wbi_img data))
         (key
          (bili-api-wbi-mixin-key
           (alist-get 'img_url wbi) (alist-get 'sub_url wbi))))
    (setq bili-api--wbi-key key
          bili-api--wbi-expires-at (+ (float-time) 21600))
    key))

(defun bili-api--dispatch-wbi-endpoint
    (request root endpoint parameters mixin-key)
  "Dispatch signed WBI ROOT and ENDPOINT with PARAMETERS for REQUEST.

MIXIN-KEY signs the request."
  (let ((signed (bili-api-wbi-sign
                 parameters mixin-key (floor (float-time)))))
    (bili-api--dispatch-step
     request (bili-api--endpoint-url root endpoint signed)
     (lambda (data) (bili-api--succeed request data)))))

(cl-defun bili-api-wbi-get
    (root endpoint parameters callback &key errback owner)
  "GET signed WBI ROOT ENDPOINT with PARAMETERS and deliver to CALLBACK.

ERRBACK and OWNER have the same meanings as in `bili-api-get'."
  (let ((request-owner (or owner (bili-core-app))))
    (bili-api--start
     callback errback request-owner
     (lambda (request)
       (if-let* ((key (bili-api--cached-wbi-key)))
           (bili-api--dispatch-wbi-endpoint
            request root endpoint parameters key)
         (bili-api--dispatch-step
          request
          (bili-api--endpoint-url bili-api--web-root
                                  "/x/web-interface/nav" nil)
          (lambda (data)
            (condition-case error-data
                (bili-api--dispatch-wbi-endpoint
                 request root endpoint parameters
                 (bili-api--install-wbi-key data))
              (error
               (bili-api--fail
                request
                (bili-api--safe-error-message error-data nil)))))
          '(-101)))))))

(cl-defun bili-api-video (bvid callback &key errback owner)
  "Read BVID detail and pass its data object to CALLBACK.

ERRBACK receives failures; OWNER controls request cancellation."
  (bili-api-get
   bili-api--web-root "/x/web-interface/view" `((bvid . ,bvid)) callback
   :errback errback :owner owner))

(cl-defun bili-api-video-playurl (bvid cid callback &key errback owner)
  "Read BVID and CID progressive playback data, then call CALLBACK.

ERRBACK receives failures; OWNER controls request cancellation."
  (bili-api-wbi-get
   bili-api--web-root "/x/player/wbi/playurl"
   `((bvid . ,bvid) (cid . ,cid) (qn . 64) (fnver . 0) (fnval . 0)
     (fourk . 1) (high_quality . 1))
   callback :errback errback :owner owner))

(cl-defun bili-api-popular
    (page callback &key (page-size 20) errback owner)
  "Read popular video PAGE of PAGE-SIZE and call CALLBACK with its data."
  (unless (and (integerp page-size) (<= 1 page-size 50))
    (error "Bilibili popular page size must be between 1 and 50"))
  (bili-api-get
   bili-api--web-root "/x/web-interface/popular"
   `((pn . ,page) (ps . ,page-size))
   callback :errback errback :owner owner))

(cl-defun bili-api-recommended-feed
    (page callback &key (page-size 20) errback owner)
  "Read account-personalized recommendation PAGE and call CALLBACK.

PAGE-SIZE must be between 1 and 30.  ERRBACK receives failures; OWNER controls
request cancellation."
  (unless (and (integerp page) (> page 0))
    (error "Bilibili recommendation page must be positive"))
  (unless (and (integerp page-size) (<= 1 page-size 30))
    (error "Bilibili recommendation page size must be between 1 and 30"))
  (bili-api-wbi-get
   bili-api--web-root "/x/web-interface/wbi/index/top/feed/rcmd"
   `((fresh_type . 4)
     (ps . ,page-size)
     (fresh_idx . ,page)
     (fresh_idx_1h . ,page)
     (brush . ,page)
     (fetch_row . ,(1+ (* (1- page) page-size)))
     (feed_version . "V8")
     (homepage_ver . 1)
     (web_location . 1430650))
   callback :errback errback :owner owner))

(cl-defun bili-api-search-videos
    (query page callback &key (page-size 20) errback owner)
  "Search PAGE-SIZE videos for QUERY at PAGE, then call CALLBACK.

ERRBACK receives failures; OWNER controls request cancellation."
  (unless (and (integerp page-size) (<= 1 page-size 50))
    (error "Bilibili search page size must be between 1 and 50"))
  (bili-api-wbi-get
   bili-api--web-root "/x/web-interface/wbi/search/type"
   `((search_type . "video") (keyword . ,query) (page . ,page)
     (page_size . ,page-size))
   callback :errback errback :owner owner))

(cl-defun bili-api-video-comments
    (aid callback &key offset (mode 3) errback owner)
  "Read one cursor page of comments for video AID.

OFFSET is the opaque cursor returned by the previous response.  MODE is 3 for
popular order or 2 for newest order.  CALLBACK receives response data;
ERRBACK receives failures and OWNER controls request cancellation."
  (unless (and (integerp aid) (> aid 0))
    (error "Bilibili comment AID must be positive"))
  (unless (memq mode '(2 3))
    (error "Bilibili comment mode must be 2 or 3"))
  (unless (or (null offset)
              (and (stringp offset) (not (string-empty-p offset))))
    (error "Bilibili comment cursor must be non-empty"))
  (bili-api-wbi-get
   bili-api--web-root "/x/v2/reply/wbi/main"
   (append
    `((type . 1) (oid . ,aid) (mode . ,mode) (web_location . 1315875))
    (when offset
      `((pagination_str . ,(json-encode `((offset . ,offset)))))))
   callback :errback errback :owner owner))

(cl-defun bili-api-live-room-init (room-id callback &key errback owner)
  "Resolve ROOM-ID and pass canonical room data to CALLBACK.

ERRBACK receives failures; OWNER controls request cancellation."
  (bili-api-get
   bili-api--live-root "/room/v1/Room/room_init"
   `((id . ,room-id)) callback :errback errback :owner owner))

(cl-defun bili-api-live-room (room-id callback &key errback owner)
  "Read ROOM-ID metadata and pass its data object to CALLBACK.

ERRBACK receives failures; OWNER controls request cancellation."
  (bili-api-get
   bili-api--live-root "/xlive/web-room/v1/index/getRoomBaseInfo"
   `((req_biz . "web_room_componet") (room_ids . ,room-id))
   callback :errback errback :owner owner))

(cl-defun bili-api-live-play-info (room-id callback &key errback owner)
  "Read ROOM-ID live playback candidates and call CALLBACK.

ERRBACK receives failures; OWNER controls request cancellation."
  (bili-api-get
   bili-api--live-root "/xlive/web-room/v2/index/getRoomPlayInfo"
   `((room_id . ,room-id) (protocol . "0,1") (format . "0,1,2")
     (codec . "0,1") (qn . 10000) (platform . "web") (ptype . 8))
   callback :errback errback :owner owner))

(cl-defun bili-api-live-list (page callback &key errback owner)
  "Read recommended live-room PAGE and call CALLBACK.

ERRBACK receives failures; OWNER controls request cancellation."
  (bili-api-get
   bili-api--live-root "/xlive/web-interface/v1/webMain/getMoreRecList"
   `((platform . "web") (web_location . "444.8") (page . ,page))
   callback :errback errback :owner owner))

(defun bili-api-effect-cancellation (request)
  "Return transport cancellation for effect-owned REQUEST, or nil."
  (when (bili-api-request-p request)
    (appkit-cancellation-create
     :kind 'transport
     :cancel (lambda () (bili-api-cancel request)))))

(cl-defun bili-api-danmaku-segment (cid segment-index callback &key errback owner)
  "Read raw protobuf danmaku for CID and one-based SEGMENT-INDEX.
Each segment spans 360 seconds.  CALLBACK receives an unibyte string,
including an empty string for an empty pool.  ERRBACK and OWNER follow
`bili-api-get'; credentials never leave the trusted API transport."
  (unless (and (integerp cid) (> cid 0))
    (error "Bilibili danmaku CID must be positive"))
  (unless (and (integerp segment-index) (<= 1 segment-index 240))
    (error "Bilibili danmaku segment index must be between 1 and 240"))
  (bili-api--start
   callback errback owner
   (lambda (request)
     (bili-api--dispatch-step
      request (bili-api--endpoint-url
               bili-api--web-root "/x/v2/dm/web/seg.so"
               `((type . 1) (oid . ,cid) (segment_index . ,segment-index)))
      (lambda (data) (bili-api--succeed request data)) nil t))))

(provide 'bili-api)

;;; bili-api.el ends here
