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

### common

* [x] class `IsCairoPatternT`
	+ [x] instance IsCairoPatternT CairoPatternT
	+ [x] instance IsCairoPatternT CairoPatternSolidT
	+ [x] instance IsCairoPatternGradient pt => CairoPatternT pt
	+ [ ] others

### get type

* [ ] `cairo_pattern_get_type`
	+ [x] `enum cairo_pattern_type_t`
	+ [x] function `cairo_pattern_get_type`
	+ [x] this function may be pure
	+ [ ] define patterns
		- [x] `pattern CairoPatternTSolid :: CairoPatternSolidT s -> CairoPatternT s`
		- [ ] others

### solid pattern

* [x] `cairo_pattern_create_rgb`
	+ [x] define it
	+ [x] use `Data.Color.Rgb`
	+ [x] exception
* [x] `cairo_pattern_create_rgba`
	+ [x] define it
	+ [x] exception
* [x] `cairo_pattern_get_rgba`
	+ [x] foreign import `c_cairo_pattern_get_rgba`
	+ [x] define `cairoPatternGetRgba`

### gradient pattern

* [x] common
	+ [x] class `IsCairoPatternGradient`
	+ [x] define `CairoPatternGradientT`
	+ [x] define `pattern CairoPatternTGradient`
	+ [x] `cairo_pattern_add_color_stop_rgb`
		- [x] define
		- [x] use Data.Color.Rgb
	+ [x] `cairo_pattern_add_color_stop_rgba`
		- [x] define
		- [x] use Data.Color.Rgba
	+ [x] `cairoPatternGetColorStopRgbaList`
		- [x] `cairo_pattern_get_color_stop_count`
		- [x] `cairo_pattern_get_color_stop_rgba`
		- [x] `cairoPatternGetColorStopRgbaList`
* [x] linear
	+ [x] define `CairoPatternLinearT`
	+ [x] define `pattern CairoPatternGradientTLinear`
	+ [x] `instance IsCairoPatternGradientT CairoPatternLinearT`
	+ [x] `cairo_pattern_create_linear`
	+ [x] `cairo_pattern_get_linear_points`
* [x] radial
	+ [x] define `CairoPatternRadialT`
	+ [x] define `pattern CairoPatternGradientTRadial`
		- [x] define `cairoPatternGradientRadialT`
		- [x] `pattern CairoPatternGradientTRadial`
	+ [x] `instance IsCairoPatternGradientT CairoPatternRadialT`
	+ [x] `cairo_pattern_create_radial`
	+ [x] `cairo_pattern_get_radial_circles`

### pattern for surface

* [ ] `cairo_pattern_create_for_surface`
* [ ] `cairo_pattern_get_surface`

### setting

* [ ] `cairo_pattern_set_foo` and `cairo_pattern_get_foo`
	+ [ ] `extend`
	+ [ ] `filter`
	+ [ ] `matrix`

mesh pattern
------------

* [x] define `CairoPatternMeshT`
* [x] define `pattern CairoPattermTMesh`
	+ [x] define `cairoPatternMeshT`
	+ [x] define `pattern CairoPatternTMesh`
* [x] `instance IsCairoPatternT CairoPatternMeshT`
* [ ] define ADTs
	+ [ ] `data MoveTo`
	+ [ ] `data LineCurveTo`
	+ [ ] `data CloseTo`
	+ [ ] `data Color`
	+ [ ] `data Point`
* [ ] `cairo_pattern_create_mesh`
* [ ] `cairoMeshPatternAddPatch`
	+ [ ] `cairo_mesh_pattern_begin_patch`
	+ [ ] `cairo_mesh_pattern_end_patch`
	+ [ ] `cairo_mesh_pattern_move_to`
	+ [ ] `cairo_mesh_pattern_line_to`
	+ [ ] `cairo_mesh_pattern_curve_to`
	+ [ ] `cairo_mesh_pattern_set_control_point`
	+ [ ] `cairo_mesh_pattern_set_corner_color_rgb`
	+ [ ] `cairo_mesh_pattern_set_corner_color_rgba`
* [ ] `cairoMeshPatternGetPatchList`
	+ [ ] `cairo_mesh_pattern_get_patch_count`
	+ [ ] `cairo_mesh_pattern_get_path`
	+ [ ] `cairo_mesh_pattern_get_control_point`
	+ [ ] `cairo_mesh_pattern_get_corner_color_rgba`

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
	+ [x] foreign import it
	+ [ ] define `raiseIfErrorPattern`

TODO
----

* [x] make new module for `CairoPatternMeshT`
