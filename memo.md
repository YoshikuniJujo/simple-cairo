memo
====

* [ ] separate cairo-image
	+ [ ] try Tuple n in another project
	+ [ ] consider whether or not to use Tuple n
	+ [x] consider to rename module
		- Graphics.Cairo.CairoImage to Data.Cairo.Image or Data.CairoImage
	+ [x] separate Graphics.Cairo.CairoImage to itself and Graphics.Cairo.CairoImage.Internal
		- [x] move to ....Internal
		- [x] make Graphics.Cairo.CairoImage
		- [x] make export list of Graphics.Cairo.CairoImage
	+ [x] make CairoImage instance of Eq
		- [x] define compareBytes
		- [x] make CairoImage instance of Eq
	+ [ ] extend Graphics.Cairo.CairoImage
		- [x] define Rgb24
			* [x] type PixelRgb24
			* [x] pattern PixelRgb24
			* [x] pattern CairoImagRgb24
				+ [x] define cairoImageToRgb24
				+ [x] define pattern
			* [x] type Rgb24
			* [x] make Rgb24 instance of Image
				+ [x] ptrRgb24
				+ [x] generateRgb24PrimM
				+ [x] define instance
			* [x] pattern CairoImageMutRgb24
				+ [x] define cairoImageMutToRgb24
				+ [x] define pattern
			* [x] type Rgb24Mut
			* [x] make Rgb24Mut instance of ImageMut
				+ [x] newRgb24Mut
				+ [x] define instance
		- [x] define A8
			* [x] type PixelA8
			* [x] type A8
			* [x] pattern CairoImageA8
				+ [x] define cairoImageToA8
				+ [x] define pattern
			* [x] make A8 instance of Image
			* [x] type A8Mut
			* [x] pattern CairoImageMutA8
			* [x] make A8Mut instance of ImageMut
		- [ ] define A1
			* [ ] type PixelA1
			* [ ] pattern PixelA1
				+ `pattern PixelA1 :: Tuple 32 Bool -> PixelA1`
			* [ ] type A1
			* [ ] pattern CairoImageA1
			* [ ] make A1 instance of Image
			* [ ] make A1Mut
			* [ ] pattern CairoImageMutA1
			* [ ] make A1Mut instance of ImageMut
		- [ ] define Rgb16565
		- [ ] define Rgb30
	* [x] try to use A8
	+ [ ] refactor Data.CairoImage
		- [x] others 1
		- [x] others 2
		- [x] structure of export list
		- [x] with haddock
			* [x] Data.CairoImage
				+ [x] class Image
				+ [x] class ImageMut
				+ [x] type CairoImage
				+ [x] type CairoImageMut
				+ [x] type PixelArgb32
				+ [x] pattern CairoImageArgb32
				+ [x] type Argb32
				+ [x] pattern CairoImageMutArgb32
				+ [x] type Argb32Mut
			* [x] Data.CairoImage.Internal
				+ [x] type CairoImage
				+ [x] type CairoImageMut
		- [x] remove Data.CairoImage.Private
		- [x] define cairoImageDataCopy
		- [x] define cairoImageFreeze
		- [x] define cairoImageThaw
		- [ ] refactor Data.CairoImage
			* [ ] export list
			* [ ] import list
			* [ ] structure
			* [ ] body
		- [ ] refactor Data.CairoImage.Internal
			* [ ] export list
			* [ ] import list
			* [ ] structure
			* [ ] body
	+ [ ] separate cairo-image package
* [x] make package typecheck-plugin-nat-simple
* [x] make package ranged-list
* [x] define repeatMin, repeatMax, repeat
* [ ] try to encode/decode bits to/from Word32 in little or big endian machine
* [ ] separate JuicyPixels
* [ ] clean module hierarchy
	+ [x] remove Graphics.Cairo
	+ [x] move some modules under Drawing
		- [x] CairoT
		- [x] Paths
		- [x] CairoPatternT
		- [x] Transformations
		- [x] Text
	+ [x] remove Tips
	+ [x] move some modules under Surfaces
		- [x] ImageSurfaces
		- [x] PngSupport
	+ [ ] consider to remove Graphics.Cairo.Surfaces.PngSupport
	+ [ ] others
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
* [ ] select public and private modules

cairo\_format\_t
----------------

* CAIRO\_FORMAT\_INVALID
* CAIRO\_FORMAT\_ARGB32
* CAIRO\_FORMAT\_RGB24
* CAIRO\_FORMAT\_A8
* CAIRO\_FORMAT\_A1
* CAIRO\_FORMAT\_RGB16\_565
* CAIRO\_FORMAT\_RGB30
