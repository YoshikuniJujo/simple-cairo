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

* [ ] basic
	+ [x] `cairo_new_path`
	+ [ ] `cairo_new_sub_path`
	+ [ ] `cairo_move_to`
	+ [ ] `cairo_line_to`
	+ [ ] `cairo_curve_to`
	+ [ ] `cairo_close_path`
	+ [ ] `cairo_rectangle`
	+ [ ] `cairo_arc`
	+ [ ] `cairo_arc_negative`
	+ [ ] `cairo_path_extents`
* [ ] relative
	+ [ ] `cairo_rel_move_to`
	+ [ ] `cairo_rel_line_to`
	+ [ ] `cairo_rel_curve_to`

optional
--------

* [ ] `cairo_copy_path`
* [ ] `cairo_copy_path_flat`
* [ ] `cairo_append_path`
* [ ] `cairo_has_current_point`
* [ ] `cairo_get_current_point`

won't implement
---------------

* `cairo_text_path`

for GC
------

* [ ] `cairo_path_destroy`
