# bili.el

A read-only Bilibili client for Emacs, built on Appkit and video.el.

It provides comprehensive popular videos, account-personalized recommendations,
video search, recommended live rooms, video and live-room details, comments,
and direct Canvas playback inside Emacs.

## Requirements

- Emacs 32.0 or newer
- [appkit.el](https://github.com/emacs-im/appkit.el)
- [browser-session](https://github.com/0WD0/browser-session)
- [video.el](https://github.com/0WD0/video.el), including its native GStreamer module

`bili.el` does not invoke yt-dlp, mpv, ffmpeg, or a local HTTP proxy at runtime.

## Installation

Install this repository and its three dependencies with your package manager.
When using this monorepo checkout, build the video.el native module first:

```sh
make -C ../video.el module
```

## Commands

| Command | Purpose |
| --- | --- |
| `M-x bili` | Open Bilibili's comprehensive popular-video catalog. |
| `M-x bili-recommended` | Open recommendations personalized for the logged-in account. |
| `M-x bili-search` | Search videos. |
| `M-x bili-live` | Open the recommended live-room directory. |
| `M-x bili-open` | Open a BV id, video URL, live-room id, or live-room URL. |
| `M-x bili-login` | Import the minimal browser session required by the API. |
| `M-x bili-logout` | Delete imported credentials. |
| `M-x bili-core-stop` | Stop the canonical App, generated surfaces, and owned effects. |

`M-x bili` uses `/x/web-interface/popular`. It is Bilibili's comprehensive
popular list, not an account-personalized home feed. Use
`M-x bili-recommended` for the separate personalized feed.

Catalogs load additional pages automatically near the visible window edge.
There is no manual “next page” command.

## Generated surfaces

### Catalogs

- `RET`: open the item detail
- `n` / `p`: next or previous card
- `g`: refresh
- `h`: comprehensive popular videos
- `f`: personalized recommendations
- `/`: video search
- `e`: edit the current search
- `l`: recommended live rooms
- `o`: open a URL or identifier
- `q`: return

Catalog rows are stable-key projections. Covers are shared declarative Appkit
Resources rendered as display-only line-prefix slices; source buffer text is
not padded for visual alignment.

### Video and live-room details

- `RET`: activate the action at point
- `P`: play the selected video part or live room
- `c`: open video comments
- `s`: select a video part
- `g`: refresh metadata
- `o`: open in a browser
- `n` / `p`: next or previous action
- `q`: return

Video comments use an Appkit Generated Surface and `appkit-discussion`: stable
root/reply identities, parent and depth metadata, connectors, circular avatars,
right-aligned timestamps, and semantic entry navigation. Avatars are
declarative Resources. Main-comment pagination follows Bilibili's opaque cursor
and loads on scroll; nested reply previews are displayed below their roots.

### Evil

When Evil is loaded, bili.el installs state-local bindings through
`appkit-evil`. Native Evil grammar such as `gg`, `/`, and `n` remains intact.
Application navigation is available under `g` prefixes, including `g f` for
the personalized feed, `g c` for comments, and `g j` / `g k` for semantic row
navigation.

## Runtime ownership

The canonical App stores immutable video, live-room, and comment entities and
runs keyed metadata Effects. Generated Surfaces own catalog membership,
pagination, selection, presentation phases, and stale-request fencing.
Projections render canonical entities by stable key. Cover and avatar
acquisition is declarative Resource demand; playback resolution is a
Surface-owned Effect.

## Authentication and transport boundaries

`bili-login` uses browser-session and stores only:

- `SESSDATA`
- `bili_jct`

The credential file is written atomically with mode `0600`.

Account cookies are sent only by `bili-api.el` to these exact HTTPS origins:

- `https://api.bilibili.com`
- `https://api.live.bilibili.com`

Cover and avatar downloads accept only HTTPS on port 443 from `hdslb.com`,
`biliimg.com`, or their real subdomains. Public image requests never receive
account cookies.

Video CDN requests receive only:

- `Referer: https://www.bilibili.com/`
- the public bili.el user agent

Live CDN requests receive only:

- `Referer: https://live.bilibili.com/<canonical-room-id>`
- the public bili.el user agent

Signed CDN URLs are transient transport data. They are not canonical Appkit
resource identities. Live streams use no disk cache.

## Current playback scope

- Video: one safe progressive MP4 segment
- Live: HTTPS HTTP-FLV with AVC
- Multi-part videos: each part has a stable BVID/CID cache identity
- Live streams: explicitly non-seekable and shown as live by video.el

DASH audio/video merging, HLS, replay/round playback, danmaku, and all write
operations are outside the current read-only scope.

## License

MIT. See [LICENSE](LICENSE).
