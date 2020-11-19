memo
====

* [ ] clean module hierarchy
* [ ] output image data
	+ [ ] CAIRO\_FORMAT\_INVALID
	+ [x] CAIRO\_FORMAT\_ARGB32 -> ImageRGBA8
		- [x] define cairoImageSurfaceGetImage
		- [x] process pre-multiplied
	+ [ ] CAIRO\_FORMAT\_RGB24 -> ImageRGB8
	+ [ ] CAIRO\_FORMAT\_A8
	+ [ ] CAIRO\_FORMAT\_A1
	+ [ ] CAIRO\_FORMAT\_RGB16\_565 -> ImageRGB8
	+ [ ] CAIRO\_FORMAT\_RGB30 -> ImageRGB16
* [ ] input image data
	+ [x] ImageRGBA8 -> CAIRO\_FORMAT\_ARGB32
		- [x] imageRgba8ToFormatArgb32
		- [x] others
	+ [ ] ImageRGBA16
	+ [ ] ImageRGB8 -> CAIRO\_FORMAT\_RGB24
	+ [ ] ImageRGB16 -> CAIRO\_FORMAT\_RGB30
	+ [ ] ImageYA16
	+ [ ] ImageYA8
	+ [ ] ImageY16
	+ [ ] ImageY8
* [ ] make Cairo Image Format
	+ [x] define CairoImage
		- [x] define CairoImage data type
		- [x] define pattern CairoImageArgb32
		- [x] define pixelAt
		- [x] define imageSize for Argb32
		- [x] define createImage for Argb32
		- [x] define cairoImageSurfaceCreateForCairoImage
			* [x] define
			* [x] test
	+ [x] define CairoImageMut
		- [x] define `type CairoImageMut s = ...`
		- [x] define cairoImageSurfaceGetCairoImageMut
		- [x] define pattern CairoImageMutArgb32
		- [x] define `instance ImageMut Argb32Mut`
		- [x] define imageMutSize for Arb32Mut
		- [x] define newImageMut for Argb32Mut
		- [x] define cairoImageSurfaceCreateForCairoImageMut
			* [x] define
			* [x] test
	+ [ ] define unsafe functions
		- get ForeignPtr or Ptr etc
* [x] repair class CairoMonad
	+ to add finalizer to CairoSurfaceT
* [x] use MonadPrim instead of CairoMonad
* [x] repair throwIfError and throwIfErrorRegion
	+ use IO instead of PrimMonad
* [ ] define functions of Regions
	+ [x] make module Graphics.Cairo.Drawing.Regions
	+ [x] define function throwIfErrorRegion
	+ [x] define cairoRegionCreate
		- use cairo\_region\_create
		- use cairo\_region\_destory
		- use throwIfErrorRegion
	+ [x] define cairoRegionCreateRectangle
* [ ] move modules to Graphics.Cairo.Drawing
	+ [x] move Graphics.Cairo.CairoT to it
	+ [ ] move Graphics.Cairo.Paths to it
	+ [ ] move Graphics.Cairo.CairoPatternT to it
	+ [ ] move Graphics.Cairo.Text to it
* [ ] throw exception from functions
	+ [x] cairoCreate
	+ [ ] others
* [ ] define functions of Drawing
	+ [ ] Graphics.Cairo.CairoT from API reference 'cairo\_t'
		- [x] cairoCreate from cairo_create
		- [ ] cairo_save
		- [ ] cairo_restore
		- [ ] cairo_get_target
		- [ ] others
	+ [ ] Graphics.Cairo.Paths from API reference 'Paths'
	+ [ ] cairo\_pattern\_t
	+ [ ] Regions
	+ [ ] Transformations
	+ no text
	+ [ ] Raster Sources
	+ [ ] Tags and Links
* [ ] remove Graphics.Cairo.Drawing.Text
	+ [ ] use pango
* not define functions of Fonts
* [ ] define functions of Surfaces
	+ [ ] read cairo\_device\_t
	+ [ ] maybe define functions of cairo\_device\_t
	+ [ ] cairo\_surface\_t
	+ [ ] Image Surfaces
	+ not implement others
* [ ] define functions of Utilities
	+ [ ] cairo\_matrix\_t
	+ [ ] Error handling
	+ [ ] Version Information
	+ [ ] Types
* [ ] move converter for JuicyPixels to Graphics.Cairo.JuicyPixels
* [ ] clean 'stack test'
* [ ] change CairoRegionT to Storable instance
* [x] cairo\_identity\_matrix

cairo\_format\_t
----------------

* CAIRO\_FORMAT\_INVALID
* CAIRO\_FORMAT\_ARGB32
* CAIRO\_FORMAT\_RGB24
* CAIRO\_FORMAT\_A8
* CAIRO\_FORMAT\_A1
* CAIRO\_FORMAT\_RGB16\_565
* CAIRO\_FORMAT\_RGB30
