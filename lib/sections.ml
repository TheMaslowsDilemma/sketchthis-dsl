(*---------------------------------------------------------
sections.ml - logic and definitions needed to find and
provide warning for section overlap
---------------------------------------------------------*)


(* 	section is just a sketch list 
	sketches that are defined in a section
	and are renderered are put in that
	sections list. this way we avoid warning
	about sketches that aren't even rendered.

	section warnings should look similar to
	error format

	{
		msg: "intersecting sections found",
		interections: [
			{
				neighborhood: [ (x1, y1), (x2, y2) ],
				sections: [
					{
						"name": "some_section_name",
						"sketches": [ "sk_name_1", "sk_name_2", ],
					},
					{
						"name": "some_other_name
						"sketches": [ sk_name_14 ]
					}
				]
			},
		]
	}

	this allows for multiple reports of intersections. we specify
	the neighborhood of the intersection, which sections were invovled
	and which sketches of those sketches were involved. 
*)