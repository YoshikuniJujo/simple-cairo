cairo\_surface\_t memo
=======================

functions
---------

* `cairo_surface_create_similar`
* `cairo_surface_create_similar_image`
* `cairo_surface_create_for_rectangle`
* `cairo_surface_reference`
* `cairo_surface_destroy`
* `cairo_surface_status`
* `cairo_surface_finish`
* `cairo_surface_flush`
* `cairo_surface_get_device`
* `cairo_surface_get_font_options`
* `cairo_surface_mark_dirty`
* `cairo_surface_mark_dirty_rectangle`
* `cairo_surface_set_device_offset`
* `cairo_surface_get_device_offset`
* `cairo_surface_get_device_scale`
* `cairo_surface_set_device_scale`
* `cairo_surface_set_fallback_resolution`
* `cairo_surface_get_fallback_resolution`
* `cairo_surface_get_type`
* `cairo_surface_get_reference_count`
* `cairo_surface_set_user_data`
* `cairo_surface_get_user_data`
* `cairo_surface_copy_page`
* `cairo_surface_show_page`
* `cairo_surface_has_show_text_glyphs`
* `cairo_surface_set_mime_data`
* `cairo_surface_get_mime_data`
* `cairo_surface_supports_mime_type`
* `cairo_surface_map_to_image`
* `cairo_surface_unmap_image`

todo
----

* [x] make module `Graphics.Cairo.Surfaces.CairoSurfaceT.Internal`
* [x] `cairo_surface_destroy`
	+ [x] rename makeCairoSurfaceT
	+ [x] rename makeCairoSurfaceT'
* [x] `cairo_surface_finish`
	+ [x] remove `cairoSurfaceFinish`
* [x] remove `cairo_surface_flush`
* [ ] `cairo_surface_get_type`
* [ ] `cairo_surface_status`
* [ ] `cairo_surface_get_content`
