# Sketch DSL Language Specification

A minimal language for generating pen plotter artwork via G-code.

## Types

Types are inferred. There are four value types:

- `number` — floating point
- `vec` — 2D point `(x, y)`
- `sketch` — drawable primitive or composition of sketches
- `region` — closed polygon (vec list), produced by `regionof`

## Statements

```
statement   := let_binding | render_command
let_binding := "let" IDENT "=" expr
render      := ("trace" | "draw" | "scribble") expr
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
      | "(" expr "," expr ")"            -- vec
      | "(" expr ")"                     -- grouping
      | "origin"                         -- (0, 0)
      | "x_axis"                         -- (1, 0)
      | "y_axis"                         -- (0, 1)
      | "centerof" atom                  -- centroid of sketch
      | "regionof" atom                  -- convex hull of sketch
      | "shade" atom                     -- fill region with dashes
      | "dot" atom                       -- single dot
      | "dot" "[" expr_list "]"          -- multiple dots
      | "dash" atom                      -- single dash
      | "dash" "[" expr_list "]"         -- multiple dashes
      | "stroke" "->" "[" expr_list "]"  -- straight line segments
      | "stroke" "~>" "[" expr_list "]"  -- smooth Catmull-Rom spline
      | "[" expr_list "]"               -- sketch list
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

## Regions and Shading

`regionof` computes the convex hull of a sketch, returning a
closed polygon. `shade` fills a region with random dashes. Shade
accepts both regions and sketches (auto-computes hull). Stack
multiple shade passes for denser fill.

```
let shape = stroke -> [(0,0), (40,0), (40,30), (0,30), (0,0)]
scribble shade shape
scribble shade shape
```

## Rules

=== RULE 1 ===
Transform arguments parse as atoms. Atoms never consume
past themselves. No parens needed unless you're doing
inline math.

=== RULE 2 ===
Transforms operate relative to sketch center. After "mirror"
use "translate" or "at" to align, otherwise sketches will overlap.

## Examples

### Basics
```
let a = (0, 0)
let b = (100, 50)
trace stroke -> [a, b]
```

### Curves
```
let center = (50, 50)
let r = 30
draw stroke ~> [center - (r, 0), (50, 80), center + (r, 0)]
```

### Multi-dot and multi-dash
```
let eyes = dot [(18, 93), (32, 93)]
let freckles = dash [(10, 85), (14, 83), (18, 86)]
draw [eyes, freckles]
```

### Reuse and pipes
```
let eye = stroke ~> [(0,0), (4,3), (8,0)]
draw [
  eye |> at (35, 60),
  eye |> at (55, 60)
]
```

### Mirror with translate (Rule 2)
```
let wing = stroke ~> [(0,0), (5,12), (15,10)]
let bird = [wing, wing |> mirror y_axis |> translate (15, 0)]
draw bird |> at (100, 200)
```

### Transform chain (Rule 1)
```
let s = stroke -> [(0, 0), (10, 0)]
draw s |> translate (5 + 3, 10)
draw s |> scale (2 * 3) |> rotate 45 |> translate (10, 10)
```

### Centroid
```
let tri = [
  stroke -> [(0,0), (40,0)],
  stroke -> [(40,0), (20,35)],
  stroke -> [(20,35), (0,0)]
]
draw tri
draw dot centerof tri
```

### Shading
```
let leaf = stroke ~> [(0,0), (5,8), (3,15), (0,20), (-3,15), (-5,8)]
let shaded_leaf = [leaf, shade leaf]
draw [
  shaded_leaf |> at (50, 100),
  shaded_leaf |> scale 0.7 |> rotate 30 |> at (80, 110)
]
```

## Notes

- No dot notation. `v.x` is not valid.
- No variable reassignment. Each name is bound once.
- Comments start with `#`
- Coordinates are in mm
- Newlines separate statements
- `~>` points create smooth Catmull-Rom splines
- `->` points create straight line segments
- Noise magnitude: scribble > draw > trace (none)
- Avoid duplicate strokes over the same path