cairo\_t memo
=============

functions
---------

* cairo\_create
* cairo\_reference
* cairo\_destroy
* cairo\_sattus
* cairo\_save
* cairo\_restore
* cairo\_get\_target
* cairo\_push\_group
* cairo\_push\_group\_with\_content
* cairo\_pop\_group
* cairo\_pop\_group\_to\_source
* cairo\_get\_group\_target
* cairo\_set\_source\_rgb
* cairo\_set\_source\_rgba
* cairo\_set\_source
* cairo\_set\_antialias
* cairo\_get\_angialias
* cairo\_set\_dash
* cairo\_get\_dash\_count
* cairo\_get\_dash
* cairo\_set\_fill\_rule
* cairo\_get\_fill\_rule
* cairo\_set\_line\_cap
* cairo\_get\_line\_cap
* cairo\_set\_line\_join
* cairo\_get\_line\_join
* cairo\_set\_line\_width
* cairo\_get\_line\_width
* cairo\_set\_miter\_limit
* cairo\_get\_miter\_limit
* cairo\_set\_operator
* cairo\_get\_operator
* cairo\_set\_tolerance
* cairo\_get\_tolerance
* cairo\_clip
* cairo\_clip\_preserve
* cairo\_clip\_extents
* cairo\_in\_clip
* cairo\_reset\_clip
* cairo\_rectangle\_list\_destroy
* cairo\_copy\_clip\_rectangle\_list
* cairo\_fill
* cairo\_fill\_preserve
* cairo\_fill\_extents
* cairo\_in\_fill
* cairo\_mask
* cairo\_mask\_surface
* cairo\_paint
* cairo\_paint\_with\_alpha
* cairo\_stroke
* cairo\_stroke\_presserve
* cairo\_stroke\_extents
* cairo\_in\_stroke
* cairo\_copy\_page
* cairo\_show\_page
* cairo\_get\_reference\_count
* cairo\_set\_user\_data
* cairo\_get\_user\_data

necessary
----------

* [x] basic
	+ [x] cairo\_create
	+ [x] source
		- [x] cairo\_set\_source\_rgb
		- [x] cairo\_set\_source\_rgba
		- [x] cairo\_set\_source
		- [x] cairo\_set\_source\_surface
	+ [x] stroke
		- [x] cairo\_stroke
		- [x] cairo\_stroke\_preserve
		- [x] cairo\_stroke\_extents
		- [x] cairo\_in\_stroke
	+ [x] fill
		- [x] cairo\_fill
		- [x] cairo\_fill\_preserve
		- [x] cairo\_fill\_extents
		- [x] cairo\_in\_fill
	+ [x] paint
		- [x] cairo\_paint
		- [x] cairo\_paint\_with\_alpha
	+ [x] mask
		- [x] cairo\_mask
		- [x] cairo\_mask\_surface
* [x] save and restore
	+ [x] cairo\_save
	+ [x] cairo\_restore
* [x] clip
	+ [x] cairo\_clip
	+ [x] cairo\_clip\_preserve
	+ [x] cairo\_clip\_extents
	+ [x] cairo\_in\_clip
	+ [x] cairo\_reset\_clip
* [ ] setting
	+ [x] cairo\_set\_line\_width
	+ [x] cairo\_set\_dash
	+ [ ] cairo\_set\_fill\_rule
	+ [ ] cairo\_set\_line\_cap
	+ [ ] cairo\_set\_line\_join
	+ [ ] cairo\_set\_miter\_limit
	+ [ ] cairo\_set\_operator
* [ ] getting
	+ [ ] cairo\_get\_line\_width
	+ [ ] cairo\_get\_dash\_count
	+ [ ] cairo\_get\_dash
	+ [ ] cairo\_get\_fill\_rule
	+ [ ] cairo\_get\_line\_cap
	+ [ ] cairo\_get\_line\_join
	+ [ ] cairo\_get\_miter\_limit
	+ [ ] cairo\_get\_operator

optional
--------

* [ ] cairo\_get\_target
* [ ] cairo\_push\_group
* [ ] cairo\_push\_group\_with\_content
* [ ] cairo\_pop\_group
* [ ] cairo\_pop\_group\_to\_source
* [ ] cairo\_get\_group\_target
* [ ] cairo\_get\_source
* [ ] cairo\_set\_antialias
* [ ] cairo\_get\_antialias
* [ ] cairo\_set\_tolerance
* [ ] cairo\_get\_tolerance
* [ ] cairo\_rectangle\_list\_destroy
* [ ] cairo\_copy\_clip\_rectangle\_list
* [ ] cairo\_copy\_page
* [ ] cairo\_show\_page
* [ ] cairo\_set\_user\_data
* [ ] cairo\_get\_user\_data

for GC
------

* [ ] cairo\_reference
* [ ] cairo\_destroy
* [ ] cairo\_get\_reference\_count

for exception
-------------

* [ ] cairo\_status

todo
----

* [x] move function from module Graphics.Cairo.Drawing.CairoT
	to module Graphics.Cairo.Drawing.CairoT.Basic
	+ [x] function `cairo_create`
	+ [x] function `cairo_set_source_rgb`
	+ [x] function `cairo_set_source_rgba`
	+ [x] function `cairo_set_source`
	+ [x] function `cairo_stroke`
	+ [x] function `cairo_fill`
	+ [x] function `cairo_paint`
	+ [x] function `cairo_paint_with_alpha`
	+ [x] function `cairo_mask`
* [x] create function in module Graphics.Cairo.Drawing.CairoT.Basic
	+ [x] function `cairo_set_source_surface`
	+ [x] function `cairo_stroke_preserve`
	+ [x] function `cairo_stroke_extents`
	+ [x] function `cairo_in_stroke`
	+ [x] function `cairo_fill_preserve`
	+ [x] function `cairo_fill_extents`
	+ [x] function `cairo_in_fill`
	+ [x] function `cairo_mask_surface`
* [x] review the function `cairo_create`
	+ [x] reconsider the module which has the function `unPrimIO`
	+ [x] reconsider the module which has the function `makeCairoT`
	+ [x] reconsider the module which has the function `raiseIfError`
	+ [x] reconsider the function `cairo_create`
* [x] create function in module Graphics.Cairo.Drawing.CairoT.SaveAndRestore
	+ [x] function `cairo_save`
	+ [x] function `cairo_restore`
* [x] create function in module Graphics.Cairo.Drawing.CairoT.Clip
	+ [x] function `cairo_clip`
	+ [x] function `cairo_clip_preserve`
	+ [x] function `cairo_clip_extents`
	+ [x] function `cairo_in_clip`
	+ [x] function `cairo_reset_clip`
* [x] separate extents from Graphics.Cairo.Drawing.CairoT.Basic
* [x] move the function `makeCairoT` to module `...CairoT.Basic`
* [x] move the data type `CairoT` to module `Data.CairoContext`
* [x] move the data type `CairoSurfaceT` to module `...Surfaces.CairoSurfaceT`
* [x] move argCairoPatternT and returnCairoPatternT to `...CairoPatternT`
* [x] move argCairoT to `...CairoT`
* [x] flip argument of argCairoT
* [x] rename the function argCairoT to withCairoT
* [ ] move a function from Graphics.Cairo.Drawing.CairoT
	to Graphics.Cairo.Drawing.CairoT.Setting
	+ [x] function `cairo_set_line_width`
* [x] define `class CairoSetting`
* [ ] create functions in module Graphics.Cairo.Drawing.CairoT.Setting
	+ [x] function `cairo_set_dash`
		- [x] use `withArrayLen`
		- [x] throw `CAIRO_STATU_INVALID_DASH`
	+ [x] function `cairo_get_line_width`
	+ [ ] function `cairo_set_fill_rule`
	+ [ ] function `cairo_set_line_cap`
	+ [ ] function `cairo_set_line_join`
	+ [ ] function `cairo_set_miter_limit`
	+ [ ] function `cairo_set_operator`
* [ ] use Color in CairoPatternT
* [ ] move CairoPatternT to the module Graphics.Cairo.Drawing.CairoPatternT
* [ ] use CairoFormatT pattens of the package cairo-image
	instead of CairoFormatT values of the module Graphics.Cairo.Values
