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

## Examples

### Simple line
```
trace stroke from (0, 0) to (100, 100)
```

### Variable Arithmetic and Sketch Composition
```

let base : vec = (50, 50)
let offset : vec = (10, 0)
let p1 : vec = (20, 0) - offset * 2
let p2 : vec = (90, 100) + offset

let line : sketch = stroke from p1 to p2
let some_dashes : sketch = [
  dash at offset * 3,
  dash at offset * 4,
  dash at offset * 5
]
draw [ line, 
```

### Curves with control points
```
let curve : sketch = stroke from (0, 50) to (100, 50) via [(50, 0)]
trace curve
```

### Using Center in a minimal Poem
```
# Build outward from a shape's own centroid
let triangle : sketch = [
  stroke from (50, 10) to (10, 90),
  stroke from (10, 90) to (90, 90),
  stroke from (90, 90) to (50, 10)
]
let heart : vec = center of triangle
let spokes : sketch = [
  stroke from heart to (50, 10),
  stroke from heart to (10, 90),
  stroke from heart to (90, 90)
]
trace [triangle, spokes]

# A little Haiku
scribble stroke from origin to center of stroke from heart to (20, 26)
```


## Important Notes

- dot notation is such as vec1.x or vec1.y is NOT SUPPORTED
- variable re-assignment is NOT SUPPORTED
- Dashes can be helpful with shading
- Comments start with `#`
- Comments are helpful to plan and label sections
- Coordinates are in mm
- Newlines separate statements
- Make an effor not to make duplicate lines and strokes
- Flow field only affects `dash`, not `stroke` or `dot`
- `via` points create smooth Catmull-Rom splines
- Noise magnitude: scribble > draw > trace (none)