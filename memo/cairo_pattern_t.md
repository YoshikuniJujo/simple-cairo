cairo\__pattern\_t memo
========================

functions
---------

* `cairo_pattern_add_color_stop_rgb`
* `cairo_pattern_add_color_stop_rgba`
* `cairo_pattern_get_color_stop_count`
* `cairo_pattern_get_color_stop_rgba`
* `cairo_pattern_create_rgb`
* `cairo_pattern_create_rgba`
* `cairo_pattern_get_rgba`
* `cairo_pattern_create_rfor_surface`
* `cairo_pattern_get_surface`
* `cairo_pattern_get_linear_points`
* `cairo_pattern_create_mesh`
* `cairo_mesh_pattern_begin_patch`
* `cairo_mesh_pattern_end_patch`
* `cairo_mesh_pattern_move_to`
* `cairo_mesh_pattern_line_to`
* `cairo_mesh_pattern_curve_to`
* `cairo_mesh_pattern_set_control_point`
* `cairo_mesh_pattern_set_corner_color_rgb`
* `cairo_mesh_pattern_set_corner_color_rgba`
* `cairo_mesh_pattern_get_path`
* `cairo_mesh_pattern_get_control_point`
* `cairo_mesh_pattern_get_corner_color_rgba`
* `cairo_pattern_reference`
* `cairo_pattern_destroy`
* `cairo_pattern_status`
* `cairo_pattern_set_extend`
* `cairo_pattern_get_extend`
* `cairo_pattern_set_filter`
* `cairo_pattern_get_filter`
* `cairo_pattern_set_matrix`
* `cairo_pattern_get_matrix`
* `cairo_pattern_get_type`
* `cairo_pattern_get_reference_count`
* `cairo_pattern_set_user_data`
* `cairo_pattern_get_user_data`

basic
-----

### get type

* [ ] `cairo_pattern_get_type`

### solid pattern

* [ ] `cairo_pattern_create_rgb`
	+ [x] define it
	+ [ ] use `Data.Color.Rgb`
* [ ] `cairo_pattern_create_rgba`
* [ ] `cairo_pattern_get_rgba`

### gradient pattern

* [ ] common
	+ [ ] `cairo_pattern_add_color_stop_rgb`
	+ [ ] `cairo_pattern_add_color_stop_rgba`
	+ [ ] `cairo_pattern_get_color_stop_count`
	+ [ ] `cairo_pattern_get_color_stop_rgba`
* [ ] linear
	+ [ ] `cairo_pattern_create_linear`
	+ [ ] `cairo_pattern_get_linear_points`
* [ ] radial
	+ [ ] `cairo_pattern_radial`
	+ [ ] `cairo_pattern_get_radial_circles`

### mesh pattern

* [ ] `cairo_pattern_create_mesh`
* [ ] `cairo_mesh_pattern_begin_patch`
* [ ] `cairo_mesh_pattern_end_patch`
* [ ] `cairo_mesh_pattern_move_to`
* [ ] `cairo_mesh_pattern_line_to`
* [ ] `cairo_mesh_pattern_curve_to`
* [ ] `cairo_mesh_pattern_set_control_point`
* [ ] `cairo_mesh_pattern_set_corner_color_rgb`
* [ ] `cairo_mesh_pattern_set_corner_color_rgba`
* [ ] `cairo_mesh_pattern_get_patch_count`
* [ ] `cairo_mesh_pattern_get_path`
* [ ] `cairo_mesh_pattern_get_control_point`
* [ ] `cairo_mesh_pattern_get_corner_color_rgba`

### pattern for surface

* [ ] `cairo_pattern_create_for_surface`
* [ ] `cairo_pattern_get_surface`

### setting

* [ ] `cairo_pattern_set_foo` and `cairo_pattern_get_foo`
	+ [ ] `extend`
	+ [ ] `filter`
	+ [ ] `matrix`

optional
--------

* [ ] `cairo_pattern_set_user_data`
* [ ] `cairo_pattern_get_user_data`

for GC
------

* [ ] `cairo_pattern_reference`
* [ ] `cairo_pattern_destroy`
* [ ] `cairo_pattern_get_reference_count`

for exception
-------------

* [ ] `cairo_pattern_status`
