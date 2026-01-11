# Sketch DSL Language Specification

## For Language Models Creating Art

Sketch DSL is a domain-specific language designed for describing vector art that compiles to G-code for pen plotters. It allows you to express complex drawings with minimal code through composition, transformation, and repetition—similar to how high-level programming languages express complex ideas that expand into assembly code.

---

## Core Philosophy

**Think hierarchically.** Build complex images from simple parts:
1. Define primitives (lines, curves, dots)
2. Name and compose them into meaningful units (face, tree, border)
3. Transform and repeat those units
4. Draw the final composition

**The language reads like English** to leverage your training on natural language.

---

## Types

There are only three types:

| Type | Description | Example |
|------|-------------|---------|
| `number` | A floating-point value | `42`, `3.14`, `-2.5` |
| `vec2` | A 2D point as (x, y) | `(0, 0)`, `(100.5, 200)` |
| `sketch` | A drawable shape | `line from (0,0) to (10,10)` |

---

## Primitives

Primitives are the atomic drawing units. All coordinates are in millimeters.

### dot
A small filled circle at a point.
```
dot (x, y)
dot point_name
```

### hdash / vdash
Short horizontal or vertical dashes.
```
hdash (x, y)    -- horizontal dash centered at point
vdash (x, y)    -- vertical dash centered at point
```

### line
A straight line between two points.
```
line from (x0, y0) to (x1, y1)
line from start_point to end_point
```

### curve
A smooth curve (Bezier) with control points.
```
curve from (x0, y0) to (x1, y1) through (cx, cy)
curve from start to end through ctrl1 and ctrl2
curve from (0, 0) to (100, 0) through (25, 50) and (75, 50)
```
- One control point → quadratic Bezier
- Two control points → cubic Bezier
- More control points → chained curves

### arc
An elliptical arc defined by center, radius, and angles (in degrees).
```
arc center (cx, cy) (rx, ry) from angle0 to angle1
```
- `(rx, ry)` defines horizontal and vertical radii
- Use `(r, r)` for a circular arc
- Angles are in degrees, counter-clockwise from east

---

## Variables and Bindings

Use `let` to name values for reuse:

```
let name : type = expression
```

Examples:
```
let size : number = 50
let origin : vec2 = (0, 0)
let corner : vec2 = (size, size)
let my_line : sketch = line from origin to corner
let my_shape : sketch = scale my_line by 2
```

**Variables enable abstraction.** Name meaningful parts of your drawing:
```
let eye : sketch = dot (0, 0)
let nose : sketch = line from (0, -5) to (0, -15)
let mouth : sketch = curve from (-10, -25) to (10, -25) through (0, -30)
let face : sketch = [eye, nose, mouth]
```

---

## Transformations

Transformations modify sketches. They can be chained and composed.

### scale
Enlarge or shrink a sketch.
```
scale <sketch> by <number>
scale <sketch> by <number> along (dx, dy)
```
- Uniform scaling: `scale shape by 2` (doubles size)
- Directional: `scale shape by 3 along (1, 0)` (stretch horizontally)

### rotate
Rotate around the sketch's center.
```
rotate <sketch> by <degrees>
```
- Positive = counter-clockwise
- Example: `rotate square by 45`

### translate
Move a sketch by an offset.
```
translate <sketch> by (dx, dy)
```

### repeat
Create multiple copies along a vector.
```
repeat <sketch> along (dx, dy) <count> times
```
- Creates `count` copies, each offset by `(dx, dy)` from the previous
- Example: `repeat dot (0,0) along (10, 0) 5 times` → 5 dots in a row

### symmetric
Mirror a sketch across an axis.
```
symmetric <sketch> along x_axis
symmetric <sketch> along y_axis
symmetric along y_axis <sketch>
```
- Creates both the original AND the reflection
- Perfect for faces, butterflies, symmetric patterns

---

## Spatial Relations

### relative to
Shift the coordinate frame for a sketch.
```
relative to (x, y) <sketch>
relative to center of <other_sketch> <sketch>
```
- Useful for positioning elements relative to other elements

### center of
Get the center point of a sketch (evaluates to vec2).
```
center of <sketch>
```

### inside (bounds checking)
Compile-time check that a sketch fits within bounds.
```
<sketch> inside <bounding_sketch>
```
- Raises error if sketch exceeds the bounding box

---

## Composition

### Drawing
The `draw` command adds a sketch to the output:
```
draw <sketch>
```

Multiple draws accumulate:
```
draw border
draw content
draw signature
```

### Grouping
Use brackets to compose multiple sketches:
```
let combined : sketch = [sketch1, sketch2, sketch3]
```

---

## Complete Grammar

```
program     → statement*
statement   → let_binding | draw_command
let_binding → "let" IDENT ":" type "=" expression
draw_command→ "draw" sketch_expr

type        → "number" | "vec2" | "sketch"

expression  → num_expr | vec_expr | sketch_expr

num_expr    → NUMBER | IDENT | "(" num_expr ")"
vec_expr    → "(" NUMBER "," NUMBER ")" | IDENT | "center" "of" sketch_atom

sketch_expr → primitive | transformation | sketch_atom
sketch_atom → IDENT | "[" sketch_list "]" | "(" sketch_expr ")"

primitive   → "dot" vec_expr
            | "hdash" vec_expr
            | "vdash" vec_expr
            | "line" "from" vec_expr "to" vec_expr
            | "curve" "from" vec_expr "to" vec_expr "through" vec_list
            | "arc" "center" vec_expr vec_expr "from" num_expr "to" num_expr

transformation → "scale" sketch_atom "by" num_expr ["along" vec_expr]
              | "rotate" sketch_atom "by" num_expr
              | "translate" sketch_atom "by" vec_expr
              | "repeat" sketch_atom "along" vec_expr num_expr "times"
              | "symmetric" sketch_atom "along" axis
              | "symmetric" "along" axis sketch_atom
              | "relative" "to" vec_expr sketch_expr
              | sketch_atom "inside" sketch_atom

axis        → "x_axis" | "y_axis"
vec_list    → vec_expr ("and" vec_expr)*
sketch_list → sketch_expr ("," sketch_expr)*
```

---

## Examples

### Simple Triangle
```
let p1 : vec2 = (0, 0)
let p2 : vec2 = (100, 0)
let p3 : vec2 = (50, 87)

draw line from p1 to p2
draw line from p2 to p3
draw line from p3 to p1
```

### Symmetric Butterfly
```
-- Define one wing
let wing_outline : sketch = curve from (0, 0) to (0, 50) through (40, 25)
let wing_inner : sketch = curve from (5, 10) to (5, 40) through (25, 25)
let half_wing : sketch = [wing_outline, wing_inner]

-- Mirror for full butterfly
let butterfly : sketch = symmetric half_wing along y_axis

-- Add body
let body : sketch = line from (0, -10) to (0, 60)

draw butterfly
draw body
```

### Grid of Dots
```
let dot1 : sketch = dot (0, 0)
let row : sketch = repeat dot1 along (10, 0) 10 times
let grid : sketch = repeat row along (0, 10) 10 times
draw grid
```

### Spiral Pattern
```
-- Create a single spiral arm
let arm : sketch = curve from (0, 0) to (50, 0) through (25, 30)

-- Repeat with rotation for full spiral
let arm1 : sketch = arm
let arm2 : sketch = rotate arm by 90
let arm3 : sketch = rotate arm by 180
let arm4 : sketch = rotate arm by 270

draw arm1
draw arm2
draw arm3
draw arm4
```

### Face with Features
```
-- Head outline
let head : sketch = arc center (0, 0) (50, 60) from 0 to 360

-- Eyes (symmetric)
let left_eye : sketch = dot (-20, 15)
let eyes : sketch = symmetric left_eye along y_axis

-- Nose
let nose : sketch = line from (0, 10) to (0, -5)

-- Smile
let smile : sketch = curve from (-25, -20) to (25, -20) through (0, -35)

-- Compose and draw
draw head
draw eyes
draw nose
draw smile
```

### Recursive-Style Tree (manual recursion)
```
-- Trunk
let trunk : sketch = line from (0, 0) to (0, 40)

-- Main branches
let branch_r : sketch = line from (0, 40) to (20, 70)
let branch_l : sketch = line from (0, 40) to (-20, 70)

-- Smaller branches
let twig_r1 : sketch = line from (20, 70) to (30, 90)
let twig_r2 : sketch = line from (20, 70) to (10, 85)
let twig_l1 : sketch = line from (-20, 70) to (-30, 90)
let twig_l2 : sketch = line from (-20, 70) to (-10, 85)

draw trunk
draw branch_r
draw branch_l
draw twig_r1
draw twig_r2
draw twig_l1
draw twig_l2
```

### Decorative Border
```
-- Single corner motif
let corner_curve : sketch = curve from (0, 0) to (20, 20) through (0, 20)
let corner_dot : sketch = dot (10, 10)
let corner : sketch = [corner_curve, corner_dot]

-- Build border by positioning corners
let top_left : sketch = corner
let top_right : sketch = translate (rotate corner by -90) by (200, 0)
let bottom_right : sketch = translate (rotate corner by 180) by (200, 200)
let bottom_left : sketch = translate (rotate corner by 90) by (0, 200)

-- Connect with lines
let top_edge : sketch = line from (20, 0) to (180, 0)
let right_edge : sketch = line from (200, 20) to (200, 180)
let bottom_edge : sketch = line from (180, 200) to (20, 200)
let left_edge : sketch = line from (0, 180) to (0, 20)

draw top_left
draw top_right
draw bottom_right
draw bottom_left
draw top_edge
draw right_edge
draw bottom_edge
draw left_edge
```

---

## Best Practices for LLMs

### 1. **Decompose the Image**
Break complex images into named components:
```
-- BAD: One giant expression
draw [line from (0,0) to (10,0), line from (10,0) to (10,10), ...]

-- GOOD: Named components
let base : sketch = line from (0, 0) to (100, 0)
let left_wall : sketch = line from (0, 0) to (0, 50)
let roof : sketch = curve from (0, 50) to (100, 50) through (50, 80)
draw [base, left_wall, roof]
```

### 2. **Use Symmetry**
Humans love symmetry. Define half, mirror the rest:
```
let half_face : sketch = [left_eye, left_eyebrow, half_nose, half_mouth]
let full_face : sketch = symmetric half_face along y_axis
```

### 3. **Use Repetition for Patterns**
```
let single_element : sketch = dot (0, 0)
let pattern : sketch = repeat single_element along (15, 0) 20 times
```

### 4. **Build Up Complexity**
Start simple, compose upward:
```
let petal : sketch = curve from (0, 0) to (0, 30) through (10, 15)
let flower : sketch = [
  petal,
  rotate petal by 72,
  rotate petal by 144,
  rotate petal by 216,
  rotate petal by 288
]
let garden : sketch = repeat flower along (50, 0) 5 times
```

### 5. **Mind the Work Area**
The Uunatek plotter has a 420mm × 297mm (A3) work area. The compiler auto-scales, but:
- Keep aspect ratios intentional
- Leave margins for mounting
- Center important elements

### 6. **Curves Are Powerful**
A single curve with good control points can represent:
- Smiles, frowns, eyebrows (faces)
- Hills, waves (landscapes)
- Petals, leaves (nature)
- Swooshes, flourishes (decorative)

```
-- Gentle hill
curve from (0, 0) to (100, 0) through (50, 30)

-- Sharp peak
curve from (0, 0) to (100, 0) through (50, 80)

-- S-curve (use two control points)
curve from (0, 0) to (100, 100) through (0, 50) and (100, 50)
```

### 7. **Comments Help**
Use `--` or `#` for comments:
```
-- This is the main figure
let figure : sketch = ...

# Position it in the center
let centered : sketch = translate figure by (100, 100)
```

---

## Output

The compiler:
1. Parses your Sketch DSL code
2. Evaluates all expressions and transformations
3. Optimizes path order (minimizes pen-up travel using nearest-neighbor + 2-opt)
4. Generates G-code for the Uunatek plotter

You can also generate:
- **SVG preview** to visualize before plotting
- **Statistics** showing path count, distances, estimated time

---

## Quick Reference Card

```
TYPES:          number, vec2, sketch

PRIMITIVES:     dot (x, y)
                hdash (x, y)
                vdash (x, y)
                line from P1 to P2
                curve from P1 to P2 through C1 [and C2...]
                arc center C (rx, ry) from A1 to A2

TRANSFORMS:     scale S by N [along V]
                rotate S by DEGREES
                translate S by V
                repeat S along V N times
                symmetric S along x_axis|y_axis

SPATIAL:        relative to V S
                center of S
                S inside BOUNDS

BINDINGS:       let name : type = expr

DRAWING:        draw S

GROUPING:       [S1, S2, S3]

COMMENTS:       -- comment
                # comment
```

---

## Example Prompt Response

If asked to "draw a simple house", you might generate:

```
-- Simple house in Sketch DSL

-- Foundation and walls
let base : sketch = line from (0, 0) to (80, 0)
let left_wall : sketch = line from (0, 0) to (0, 50)
let right_wall : sketch = line from (80, 0) to (80, 50)

-- Roof (triangle)
let roof_left : sketch = line from (0, 50) to (40, 80)
let roof_right : sketch = line from (40, 80) to (80, 50)

-- Door
let door_left : sketch = line from (30, 0) to (30, 30)
let door_right : sketch = line from (50, 0) to (50, 30)
let door_top : sketch = line from (30, 30) to (50, 30)

-- Window (left side)
let window : sketch = [
  line from (10, 25) to (25, 25),
  line from (25, 25) to (25, 40),
  line from (25, 40) to (10, 40),
  line from (10, 40) to (10, 25)
]

-- Window on right (symmetric)
let right_window : sketch = translate window by (45, 0)

-- Compose and draw
draw base
draw left_wall
draw right_wall
draw roof_left
draw roof_right
draw door_left
draw door_right
draw door_top
draw window
draw right_window
```

This produces a simple house with walls, triangular roof, centered door, and two windows—all in clean, readable code that compiles to optimized G-code.
