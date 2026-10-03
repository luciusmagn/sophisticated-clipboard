# sophisticated-clipboard

Portable system clipboard access for Common Lisp. Text and typed data move
between your image and the clipboard of the desktop you are running on, with
no compiled glue code.

## Installation

```lisp
(ql:quickload :sophisticated-clipboard)
```

The system depends on UIOP and flexi-streams everywhere, and on CFFI only on
Windows.

## Usage

```lisp
(setf (sophisticated-clipboard:clipboard-text) "put text!")
(sophisticated-clipboard:clipboard-text)
;; => "put text!"

(sophisticated-clipboard:clipboard-types)
;; => (#<CLIPBOARD-TYPE text/plain;charset=utf-8> #<CLIPBOARD-TYPE image/png>)

(sophisticated-clipboard:clipboard-get "image/png")
;; => #(137 80 78 71 ...)
(setf (sophisticated-clipboard:clipboard-image "image/png") png-octets)
```

Text types take and return strings; every other type takes and returns an
octet vector. `clipboard-has-type-p` answers whether a type is on offer, and
`clipboard-image` reads the first image type, preferring PNG.

## Backends

A backend is detected on every call unless `*clipboard-backend*` is bound.

| Host | Backend | Requirements |
| --- | --- | --- |
| Wayland (Linux, BSDs) | `wl-copy` and `wl-paste` | `WAYLAND_DISPLAY` set and wl-clipboard installed |
| X11 (Linux, BSDs, XWayland) | `xclip`, or `xsel` for text only | `DISPLAY` set and one of the tools installed |
| macOS | `pbcopy`, `pbpaste`, `osascript` | the stock tools; images use AppleScript coercions |
| Windows x86-64 | Win32 clipboard API through CFFI | nothing extra |

A Wayland session without wl-clipboard falls back to the X11 tools when
`DISPLAY` is also set. Helper programs are located through `PATH` without
spawning a shell, and on Windows the `PATHEXT` extensions are honoured.

`clipboard-detect-backend` returns the backend that applies, or signals
`clipboard-unavailable` when no display is reachable and its subtype
`not-installed` when a display exists but none of its tools do. Pin a backend
explicitly when the default is wrong for your session:

```lisp
(let ((sophisticated-clipboard:*clipboard-backend*
        (make-instance 'sophisticated-clipboard:x11-backend :tool :xsel)))
  (sophisticated-clipboard:clipboard-text))
```

Every backend answers the same generic functions: `backend-types`,
`backend-get`, `backend-set`, and `backend-text`. Subclass `clipboard-backend`
to add another transport.

## Conditions

All conditions inherit from `sophisticated-clipboard-error`:

- `clipboard-unavailable`: no backend reaches a clipboard from this process
- `not-installed`: a display is present but its clipboard commands are missing
- `clipboard-command-failed`: a helper command exited unsuccessfully
- `clipboard-unsupported-type`: the backend cannot carry the requested type,
  such as binary data through `xsel`

## Testing

```lisp
(asdf:test-system :sophisticated-clipboard)
```

Detection, type classification, and parsing are tested with replaced
environment and executable lookups. The live round trip runs only when a
clipboard is actually reachable and is skipped otherwise.

## Authors

sophisticated fork: Lukáš Hozda (me@mag.wiki)
original: SANO Masatoshi (snmsts@gmail.com)

## Project

https://github.com/lambda-symbolics/sophisticated-clipboard

## License

Licensed under the MIT License.
