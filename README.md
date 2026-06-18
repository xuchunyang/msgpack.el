# MessagePack Emacs Lisp library
[![Melpa](https://melpa.org/packages/msgpack-badge.svg)](https://melpa.org/#/msgpack)

msgpack.el is an Emacs Lisp library for reading and writing [MessagePack](https://msgpack.org/).

``` emacs-lisp
(msgpack-read-from-string "\xA5Hello")
;; => "Hello"

(msgpack-encode "Hello")
;; => "\245Hello"
```

## API

### `(msgpack-read-from-string STRING &rest ARGS)`

Read the MessagePack object in unibyte STRING and return it.  ARGS are the same
keyword arguments accepted by `msgpack-read`.

### `(msgpack-decode STRING &rest ARGS)`

Alias-style decoding API for reading the first MessagePack object in unibyte
STRING.  ARGS are the same keyword arguments accepted by `msgpack-read`.

### `(msgpack-read &key array-type map-type key-type bin-type null-value false-value)`

Parse and return the MessagePack object following point.
Advances point just past MessagePack object.

Keyword arguments override the dynamic defaults for this read and nested values.
Defaults preserve historical behavior: MessagePack `false` decodes as `nil` and
MessagePack `bin` decodes as a raw unibyte string.

### `(msgpack-read-file FILE &rest ARGS)` / `(msgpack-decode-file FILE &rest ARGS)`

Read the first MessagePack object contained in FILE and return it.

### `(msgpack-encode OBJ)`

Return MessagePack representation of OBJ.

### `msgpack-false` (defaults to `:msgpack-false`)

Value encoded as MessagePack `false`.  Decoding still defaults false to `nil`;
use `:false-value` to override that per call.

### `msgpack-null` (defaults to `nil`)

### `msgpack-array-type` (defaults to `list`)

### `msgpack-map-type` (defaults to `alist`)

### `msgpack-key-type` (defaults to `nil`)

### `msgpack-bin-type` (defaults to `string`)

## Requirements

- Emacs 25.1
