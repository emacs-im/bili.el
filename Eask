;; -*- mode: eask; lexical-binding: t -*-

(package "bili"
         "0.1.0"
         "Browse and play Bilibili in Emacs")

(website-url "https://github.com/0WD0/bili.el")
(keywords "multimedia" "convenience")

(package-file "bili.el")

(source "gnu")
(source 'melpa)

(depends-on "emacs" "32.0")
(depends-on "appkit" :file "../appkit.el")
(depends-on "browser-session" :file "../browser-session")
(depends-on "video" :file "../video.el")

(script "refresh-local-deps"
        "eask -f install-file ../appkit.el && eask -f install-file ../browser-session && eask -f install-file ../video.el")
(script "test" "eask test ert --quiet test/*.el")
(script "test-local"
        "eask -f install-file ../appkit.el && eask -f install-file ../browser-session && eask -f install-file ../video.el && eask recompile && eask test ert --quiet test/*.el")
