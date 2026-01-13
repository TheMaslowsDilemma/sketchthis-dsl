# Sketch DSL Language Specification

A minimal language for generating pen plotter artwork via G-code.

## Types

- `number` - floating point value
- `vec` - 2D point `(x, y)`
- `sketch` - drawable primitive or list of sketches

## Syntax

```
statement := let_binding | render_command
let_binding := "let" IDENT ":" type "=" expr
render_command := ("trace" | "draw" | "scribble") sketch_expr

type := "number" | "vec" | "sketch"
```

## Expressions

### Numbers
```
num_expr := NUMBER | IDENT | "-" num_expr
          | num_expr ("+" | "-" | "*" | "/") num_expr
          | "(" num_expr ")"
```

### Vectors
```
vec_expr := "(" num_expr "," num_expr ")"  -- construct
          | IDENT                           -- variable
          | "origin"                        -- (0, 0)
          | "center" "of" sketch_expr       -- centroid
          | "flow" "at" vec_expr            -- flow field direction
          | vec_expr ("+" | "-") vec_expr   -- arithmetic
          | vec_expr "*" num_expr           -- scale
```

### Sketches
```
sketch_expr := primitive | IDENT | "[" sketch_list "]"
sketch_list := sketch_expr ("," sketch_expr)*

primitive := "dot" "at" vec_expr
           | "dash" "at" vec_expr
           | "stroke" "from" vec_expr "to" vec_expr ["via" vec_list]

vec_list := "[" vec_expr ("," vec_expr)* "]"
```

## Render Commands

| Command | Effect |
|---------|--------|
| `trace` | Exact rendering, no noise |
| `draw` | Slight wobble, hand-drawn feel |
| `scribble` | Heavy noise, sketchy style |

## Flow Field

`dash` orientation is determined by nearby `stroke` directions. Strokes contribute to a flow field weighted by inverse-square distance. Default direction is horizontal if no strokes exist.

## Compilation

1. Evaluate expressions to intermediate representation (IR)
2. Apply noise based on render command
3. Convert curves to line segments via Catmull-Rom splines
4. Optimize path order (greedy nearest-neighbor + 2-opt)
5. Generate G-code (G0 rapid, G1 linear, M3/M5 pen control)

## Examples

### Simple line
```
trace stroke from (0, 0) to (100, 100)
```

### Variables and composition
```
let p1 : vec = (10, 10)
let p2 : vec = (90, 90)
let line : sketch = stroke from p1 to p2
draw line
```

### Curves with control points
```
let curve : sketch = stroke from (0, 50) to (100, 50) via [(50, 0)]
trace curve
```

### Multiple sketches
```
let box : sketch = [
  stroke from (0, 0) to (10, 0),
  stroke from (10, 0) to (10, 10),
  stroke from (10, 10) to (0, 10),
  stroke from (0, 10) to (0, 0)
]
draw box
```

### Dots and dashes
```
let s : sketch = stroke from (0, 0) to (100, 0)
let pattern : sketch = [
  s,
  dash at (20, 5),
  dash at (50, 5),
  dash at (80, 5),
  dot at (50, -5)
]
draw pattern
```

### Using center
```
let shape : sketch = stroke from (0, 0) to (100, 50)
let c : vec = center of shape
let marker : sketch = dot at c
trace [shape, marker]
```

### Computed positions
```
let base : vec = (50, 50)
let offset : vec = (10, 0)
let p1 : vec = base - offset
let p2 : vec = base + offset
draw stroke from p1 to p2
```

### Grid of dots
```
let row1 : sketch = [dot at (0,0), dot at (10,0), dot at (20,0)]
let row2 : sketch = [dot at (0,10), dot at (10,10), dot at (20,10)]
let row3 : sketch = [dot at (0,20), dot at (10,20), dot at (20,20)]
scribble [row1, row2, row3]
```

### Sketch reuse
```
let corner : sketch = [
  stroke from (0, 0) to (5, 0),
  stroke from (0, 0) to (0, 5)
]
draw corner
```

## Notes

- Coordinates are in mm
- Comments start with `#` or `--`
- Newlines separate statements
- Flow field only affects `dash`, not `stroke` or `dot`
- `via` points create smooth Catmull-Rom splines
- Noise magnitude: scribble > draw > trace (none)