PDF Surfaces memo
=================

functions
---------

* `cairo_pdf_surface_create`
* `cairo_pdf_surface_create_for_stream`
* `cairo_pdf_surface_restrict_to_version`
* `cairo_pdf_get_versions`
* `cairo_pdf_version_to_string`
* `cairo_pdf_surface_set_size`
* `cairo_pdf_surface_add_outline`
* `cairo_pdf_surface_set_metadata`
* `cairo_pdf_surface_set_page_label`
* `cairo_pdf_surface_set_thumbnail_size`

create
------

* [x] define `CairoSurfacePdfT`
	+ [x] define ADT
	+ [x] `instance IsCairoSurfaceT CairoSurfacePdfT`
	+ [x] `pattern CairoSurfaceTPdf`
		- [x] `cairoSurfaceTPdf :: CairoSurfaceT s ps -> Maybe (CairoSurfacePdfT s ps)`
		- [x] `pattern CairoSurfaceTPdf`
* [x] `cairoPdfSurfaceWith`
	+ [x] `cairoPdfSurfaceCreate`
		- [x] add exception
		- [x] use CairoSurfacePdfT
	+ [x] define `cairoPdfSurfaceWith`
* [x] make module `Graphics.Cairo.Surfaces.WriteFunc`
	+ [x] move `WriteResult` from `Graphics.Cairo.Surfaces.SvgSurfaces`
* [x] `cairoPdfSurfaceWithForStream`
	+ [x] `cairoPdfSurfaceCreateForStream`
	+ [x] define `cairoPdfSurfaceWithForStream`

outline
-------

* [x] define `cairoTagBegin` and `cairoTagEnd` for `Graphics.Cairo.Drawing.CairoT.TagsAndLinks`
* [ ] cairoPdfSurfaceAddOutline
	+ [x] use Maybe Pos instead of Pos
	+ [ ] use CairoPdfDestinationT

setting
-------

* [x] `cairoPdfSurfaceSetSize`
* [ ] `cairoPdfSurfaceSetMetadata`
* [ ] `cairoPdfSurfaceSetPageLabel`
* [ ] `cairoPdfSurfaceSetThumbnailSize`

version
-------

* [ ] cairoPdfSurfaceRestrictToVersion
* [ ] cairoPdfGetVersions
* [ ] cairoPdfVersionToString

todo
----

* [x] define `cairoPdfSurfaceCreate` for test
	+ use `CairoSurfaceT`
* [x] `unsafeCairoSurfaceFinish` for test
