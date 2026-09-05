# messagepack-protocol

CLOS **MessagePack** encode/decode for [cl-stack](https://github.com/egao1980/cl-stack). Same Lisp mapping as [`json-protocol`](https://github.com/egao1980/json-protocol), plus `bin` and `ext`. Implements [`serdes-protocol`](https://github.com/egao1980/serdes-protocol) `:messagepack` and `:msgpack`.

OCI **0.1.0** — `ghcr.io/egao1980/cl-systems/messagepack-protocol:0.1.0`

```lisp
(asdf:load-system "messagepack-protocol")   ; nick stack-messagepack; registers :messagepack / :msgpack

(stack-messagepack:encode 42)
(serdes-protocol:encode ht :format :msgpack)
(serdes-protocol:format-media-type :messagepack)   ; "application/msgpack"
```

`:null` → MessagePack nil (`0xc0`); Lisp `nil` → false (`0xc2`). Timestamp ext type `-1` decodes to `msgpack-timestamp`.

## License

MIT — see [LICENSE](LICENSE).
