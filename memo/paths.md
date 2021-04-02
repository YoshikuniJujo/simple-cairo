Paths memo
==========

functions
---------

* `cairo_copy_path`
* `cairo_copy_path_flat`
* `cairo_path_destroy`
* `cairo_append_path`
* `cairo_has_current_point`
* `cairo_get_current_point`
* `cairo_new_path`
* `cairo_new_sub_path`
* `cairo_close_path`
* `cairo_arc`
* `cairo_arc_negative`
* `cairo_curve_to`
* `cairo_line_to`
* `cairo_move_to`
* `cairo_rectangle`
* `cairo_glyph_path`
* `cairo_text_path`
* `cairo_rel_curve_to`
* `cairo_rel_line_to`
* `cairo_rel_move_to`
* `cairo_path_extents`

necessary
---------

* [x] basic
	+ [x] `cairo_new_path`
	+ [x] `cairo_new_sub_path`
	+ [x] `cairo_move_to`
	+ [x] `cairo_line_to`
	+ [x] `cairo_curve_to`
	+ [x] `cairo_close_path`
	+ [x] `cairo_rectangle`
	+ [x] `cairo_arc`
	+ [x] `cairo_arc_negative`
	+ [x] `cairo_path_extents`
* [x] relative
	+ [x] `cairo_rel_move_to`
	+ [x] `cairo_rel_line_to`
	+ [x] `cairo_rel_curve_to`

optional
--------

* [x] `cairo_copy_path`
* [x] `cairo_copy_path_flat`
* [x] `cairo_append_path`
* [ ] `cairo_has_current_point`
* [ ] `cairo_get_current_point`

won't implement
---------------

* `cairo_text_path`

for GC
------

* [x] `cairo_path_destroy`

CairoPathT
----------

* [ ] define `isCairoPatchPath :: CairoPathT -> Bool`
	+ MoveTo ==> (LineTo or CurveTo) * 4
	+ MoveTo x0 y0: the goal of last LineTo or CurveTo is (x0, y0)
	+ [x] define `cairoPathCheckPaths :: CairoPathT -> [#{type cairo_path_data_type_t} -> Bool] -> Bool`
	+ [ ] define `cairoPathGetGoal :: Int -> (CDouble, CDouble)
