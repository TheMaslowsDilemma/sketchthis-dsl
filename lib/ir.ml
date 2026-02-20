(*---------------------------------------------------------
ir.ml - intermediate representation - definitions of ir,
segment, and path
---------------------------------------------------------*)

type segment = { p0 : Vector.vec; p1 : Vector.vec }
type path = { start : Vector.vec; segments : segment list }
type ir = path list
