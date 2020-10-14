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
	+ [ ] ImageRGBA8 -> CAIRO\_FORMAT\_ARGB32
		- [x] imageRgba8ToFormatArgb32
		- [ ] others
	+ [ ] ImageRGBA16
	+ [ ] ImageRGB8 -> CAIRO\_FORMAT\_RGB24
	+ [ ] ImageRGB16 -> CAIRO\_FORMAT\_RGB30
	+ [ ] ImageYA16
	+ [ ] ImageYA8
	+ [ ] ImageY16
	+ [ ] ImageY8
* [ ] repair class CairoMonad
	+ to add finalizer to CairoSurfaceT
* [ ] throw exception from functions
* [ ] clean 'stack test'
