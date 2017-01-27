## menu

A dmenu-like multi-purpose menu bar, built using [dmlenu](https://github.com/the-lambda-church/dmlenu).

## Build

```
opam pin add dmlenu https://github.com/the-lambda-church/dmlenu.git
opam pin add papiers https://github.com/Armael/papiers.git
opam install cmdliner batteries lwt cohttp yojson
make
```

Some dependencies could be make optional, and can be easily removed by removing
the corresponding feature in `menu.ml`:

- `papiers` is only used by the corresponding plugin in `papiers.ml`
- `cohttp` and `yojson` are only needed by the twitch.tv feature, in `twitch.ml`