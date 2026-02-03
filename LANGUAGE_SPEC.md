# Sketch DSL Language Specification

A minimal language for generating pen plotter artwork via G-code.

## Types

Types are inferred. There are three value types:

- `number` — floating point
- `vec` — 2D point `(x, y)`
- `sketch` — drawable primitive or composition of sketches

## Statements

```
statement   := let_binding | render_command
let_binding := "let" IDENT "=" expr
render       := ("trace" | "draw" | "scribble") expr
```

## Expressions

### Precedence (lowest to highest)

```
pipe    := add ("|>" transform)*
add     := mul (("+"|"-") mul)*
mul     := unary (("*"|"/") unary)*
unary   := "-" unary | atom
```

### Atoms

```
atom := NUMBER | IDENT
      | "(" expr "," expr ")"         -- vec
      | "(" expr ")"                  -- grouping
      | "origin"                      -- (0, 0)
      | "x_axis"                      -- (1, 0)
      | "y_axis"                      -- (0, 1)
      | "center" "of" atom
      | "dot" atom
      | "dash" atom
      | "stroke" add "->" add ["~" "[" expr_list "]"]
      | "[" expr_list "]"             -- sketch list
      | transform_prefix
```

### Transforms

Pipe form (left side is the sketch being transformed):

```
expr |> translate atom
expr |> scale atom
expr |> rotate atom
expr |> mirror atom
expr |> at atom
```

Prefix form (sketch is the first argument):

```
translate atom atom
scale atom atom
rotate atom atom
mirror atom atom
at atom atom
```

| Transform   | Arg type | Effect                                    |
|-------------|----------|-------------------------------------------|
| `translate` | vec      | Shift by delta                            |
| `scale`     | number   | Scale around sketch center                |
| `rotate`    | number   | Rotate CCW (degrees) around sketch center |
| `mirror`    | vec      | Reflect across axis through sketch center |
| `at`        | vec      | Move sketch center to point               |

## Render Commands

| Command    | Effect                           |
|------------|----------------------------------|
| `trace`    | Exact, no noise                  |
| `draw`     | Slight wobble, hand-drawn feel   |
| `scribble` | Heavy noise, sketchy style       |

## Flow Field

`dash` orientation is determined by all strokes drawn before it.
Strokes contribute to a flow field weighted by inverse-square
distance. Default direction is horizontal if no strokes exist.

## Rules

=== RULE 1 ===
Transform arguments parse as atoms. Atoms never consume
past themselves. No parens needed unless you're doing
inline math.

=== RULE 2 ===
Transforms operate relative to sketch center. After "mirror"
use "translate" or "at" to align, otherwise sketches will overlap.

## Examples

```
# Symmetric shapes are not difficult, just remember how they should meet!
let wing = stroke (0, 0) -> (15, 10) ~ [(5, 12)]
let bird = [wing, wing |> mirror y_axis |> translate (15, 0)] # RULE 2
draw bird |> at (100, 200)
```

```
let s = stroke (0, 0) -> (10, 0)
draw s |> translate (5 + 3, 10)
draw s |> scale (2 * 3) |> scale 2 |> rotate 45 |> translate (10, 10) # RULE 1
```

## Notes

- No dot notation. `v.x` is not valid.
- No variable reassignment. Each name is bound once.
- Comments start with `#`
- Coordinates are in mm
- `~` points create smooth Catmull-Rom splines
- Noise magnitude: scribble > draw > trace (none)
- Avoid duplicate strokes over the same path