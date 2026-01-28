# sketchthis-lang

A domain specific language aims to help language models create sketch art leveraging their understanding of natural language and programming. Built in OCaml, outputs both G-code and SVG.

Powers [@sketchthis](https://x.com/sketchthis)
See the [language spec](https://github.com/TheMaslowsDilemma/sketchthis-dsl/blob/main/LANGUAGE_SPEC.md) for more details.

## Build

```bash
opam install dune
dune build
```

## Install to PATH

```bash
sudo cp _build/default/bin/main.exe /usr/local/bin/sketchlang
```

Or symlink:

```bash
sudo ln -s $(pwd)/_build/default/bin/main.exe /usr/local/bin/sketchlang
```

## Usage

```bash
sketchlang drawing.sketch              # → drawing.txt (G-code)
sketchlang drawing.sketch --svg        # → drawing.svg
sketchlang drawing.sketch --gcode --svg  # → both
sketchlang drawing.sketch -scale 0.5   # half size, centered
sketchlang drawing.sketch -pos 10,10 -size 100,100  # position and constrain
```

Run `sketchlang --help` for all options.