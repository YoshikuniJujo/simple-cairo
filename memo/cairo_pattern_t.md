cairo\_pattern\_t memo
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

### get type

* [x] `cairo_pattern_get_type`
	+ [x] `enum cairo_pattern_type_t`
	+ [x] function `cairo_pattern_get_type`
	+ [x] this function may be pure
	+ [x] define patterns
		- [x] `pattern CairoPatternTSolid :: CairoPatternSolidT s -> CairoPatternT s`

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

* [x] define `CairoPatternSurfaceT`
* [x] define `pattern CairoPatternTSurface :: CairoPatternSurfaceT s -> CairoPatternT s`
	+ [x] define `cairoPatternTSurface :: CairoPatternT s -> Maybe (CairoPatternSurfaceT s)`
	+ [x] define `pattern CairoPatternTSurface`
* [x] `cairo_pattern_create_for_surface`
* [x] `cairo_pattern_get_surface`

### setting

* [x] `cairo_pattern_set_foo` and `cairo_pattern_get_foo`
	+ [x] `extend`
	+ [x] `filter`
	+ [x] `matrix`

mesh pattern
------------

* [x] define `CairoPatternMeshT`
* [x] define `pattern CairoPattermTMesh`
	+ [x] define `cairoPatternMeshT`
	+ [x] define `pattern CairoPatternTMesh`
* [x] `instance IsCairoPatternT CairoPatternMeshT`
* [x] define ADTs
	+ [x] `data MoveTo`
	+ [x] `data LineCurveTo`
	+ [x] `data CloseTo`
	+ [x] `data Color`
	+ [x] `data Point`
* [x] `cairo_pattern_create_mesh`
* [x] `cairoMeshPatternAddPatch`
	+ [x] `cairo_mesh_pattern_begin_patch`
	+ [x] `cairo_mesh_pattern_end_patch`
	+ [x] `cairo_mesh_pattern_move_to`
	+ [x] `cairo_mesh_pattern_line_to`
	+ [x] `cairo_mesh_pattern_curve_to`
	+ [x] `cairo_mesh_pattern_set_control_point`
	+ [x] `cairo_mesh_pattern_set_corner_color_rgb`
	+ [x] `cairo_mesh_pattern_set_corner_color_rgba`
	+ [x] `cairoMeshPatternAddPatch`
* [x] `cairoMeshPatternGetPatchList`
	+ [x] `cairo_mesh_pattern_get_patch_count`
	+ [x] `cairo_mesh_pattern_get_path`
	+ [x] `cairo_mesh_pattern_get_control_point`
	+ [x] `cairo_mesh_pattern_get_corner_color_rgba`
	+ [x] `cairoMeshPatternGetPatch1`
	+ [x] `cairoMeshPatternGetPatchList`
	+ [x] use `unsafeInterleaveIO`
* [x] define `CairoPatchPathT`
	+ [x] define `pattern CairoPathTPatch :: CairoPatchPathT -> CairoPathT`

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

* [x] make a new module for `CairoPatternMeshT`
* [ ] make a module `Graphics.Cairo.Drawing.CairoPatternT.Basic.Internal`
