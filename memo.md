memo
====

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
	+ [ ] define CairoImage
		- [x] define CairoImage data type
		- [x] define pattern CairoImageArgb32
		- [x] define pixelAt
		- [ ] read function
		- [ ] create function
	+ [ ] define CairoImageMut
		- [x] define `type CairoImageMut s = ...`
		- [x] define cairoImageSurfaceGetCairoImageMut
		- [x] define pattern CairoImageMutArgb32
		- [x] define `instance ImageMut Argb32Mut`
		- [ ] others
* [x] repair class CairoMonad
	+ to add finalizer to CairoSurfaceT
* [ ] use MonadPrim instead of CairoMonad
* [ ] move converter for JuicyPixels to Graphics.Cairo.JuicyPixels
* [ ] throw exception from functions
* [ ] clean 'stack test'

cairo\_format\_t
----------------

* CAIRO\_FORMAT\_INVALID
* CAIRO\_FORMAT\_ARGB32
* CAIRO\_FORMAT\_RGB24
* CAIRO\_FORMAT\_A8
* CAIRO\_FORMAT\_A1
* CAIRO\_FORMAT\_RGB16\_565
* CAIRO\_FORMAT\_RGB30
