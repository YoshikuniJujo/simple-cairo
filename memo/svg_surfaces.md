SVG Surfaces memo
=================

functions
---------

* `cairo_svg_surface_create`
* `cairo_svg_surface_create_for_stream`
* `cairo_svg_surface_get_document_unit`
* `cairo_svg_surface_set_document_unit`
* `cairo_svg_surface_restrict_to_version`
* `cairo_svg_surface_get_versions`
* `cairo_svg_version_to_string`

types and values
-----------------

* `CAIRO_HAS_SVG_SURFACE`
* `cairo_svg_version_t`
* `cairo_svg_unit_t`

CairoSurfaceSvgT
----------------

* [x]  define `CairoSurfaceSvgT`
* [x] `instance IsCairoSurfaceT CairoSurfaceSvgT`
* [x] `pattern CairoSurfaceTSvg`
* [x] `newtype CairoSurfaceSvgT s st`

create SVG Surfaces
--------------------

* [x] cairoSvgSurfaceWith
	+ [x] return `m a` instead of `m ()`
* [x] cairoSvgSurfaceWithForStream
	+ [x] return `m a` instead of `m ()`

settings
--------

* [x] CairoSvgUnitT
* [x] cairoSvgSurfaceGetDocumentUnit
* [x] cairoSvgSurfaceSetDocumentUnit

version
-------

* [x] CairoSvgVersionT
* [x] cairoSvgSurfaceRestrictToVersion
* [ ] cairoSvgSurfaceGetVersions
* [ ] cairoSvgVersionToString
