memo
====

Cairo structure
---------------

* Drawing
	+ cairo\_t
	+ Paths
	+ cairo\_pattern\_t
	+ Regions
	+ Transformations
	+ text (will delete)
	+ Raster Sources (will implement in the distant future)
	+ Tags and LInks (won't implement)
* Fonts (won't implement)
* Surfaces
	+ cairo\_device\_t
	+ cairo\_surface\_t
	+ Image Surfaces (will implement soone)
	+ PDF Surfaces (will implement)
	+ PNG Surfaces (toy API)
	+ Recording Surfaces (will implement in the distant future)
	+ Win32 Surfaces (won't implement)
	+ SVG Surfaces
	+ Quartz Surfaces (won't implement)
	+ XCB Surfaces
	+ XLib Surfaces (won't implement)
	+ XLib-XRender Backend (won't implement)
	+ Script Surfaces (will implement in the distant future)
* Utilities
	+ cairo\_matrix\_t
	+ Error handling
	+ Version Information
	+ Types

todo
----

* [x] Drawing
	+ [x] cairo\_t
	+ [x] Paths
	+ [x] cairo\_pattern\_t
	+ [x] Transformations
	+ [x] Tags and Links
* [x] Surfaces
	+ [x] cairo\_surface\_t
	+ [x] Image Surfaces
	+ [x] SVG Surfaces
		- [x] cairo\_svg\_surface\_create
		- [x] cairo\_svg\_surface\_create\_for\_stream
		- [x] others
	+ [x] PDF Surfaces
* [x] Utilities
	+ [x] cairo\_matrix\_t
* [x] move a module `Graphics.Cairo.Drawing.CairoT.Extents`
	to `Graphics.Cairo.Drawing.Extents`
* [x] add one more variable to `CairoT`
* [ ] repair simple-pango
* [ ] repair try-gdk
* [ ] separate package to export
	+ [ ] `CairoT`
	+ [ ] `CairoSurfaceT`
* [ ] use package `simple-cairo` from
	+ [ ] pango
	+ [ ] gdk
* [ ] use GHC.Foreign.withCString instead of Foreign.C.String.withCString
* [ ] rename a module `Data.Color` to `Data.CairoColor`
* [ ] make new package cairo-regions
	+ [ ] Regions

to do next
----------

* [ ] Recording Surfaces
* [ ] Script Surfaces

old
---

* [x] define `data Rgb`
	+ [x] define `data Rgb = RgbWord8_ Word8 Word8 Word8 | RgbDouble_ Double Double Double`
	+ [x] define `pattern RgbWord8`
		- [x] add COMPLETE pragma
	+ [x] define `pattern RgbDouble`
	+ [x] define constructor `rgbDouble :: Double -> Double -> Double -> Rgb`
	+ [x] use `#{type double}` instead of Double
	+ [x] use CDouble instead of `#{type double}`
	+ [x] rename `foo.hsc` to `foo.hs`
* [x] use `data Rgb` in cairoSetSourceRgb
* [x] define `data Rgba`
	+ [x] define `data Rgba`
	+ [x] define `pattern RgbaWord8`
	+ [x] define `pattern RgbaDouble`
	+ [x] define constructor `rgbaDouble :: Double -> Double -> Double -> Double -> Rgba`
	+ [x] use CDouble instead of Double
* [x] use `data Rgba` in cairoSetSourceRgba
* [x] define cairoRotate
	+ [x] test/test-rotate.hs
	+ [x] prepare a sample .png file
	+ [x] read a sample .png file and write to a new .png file
	+ [x] define cairoRotate
	+ [x] rotate a new .png file
* [x] separate cairo-image
	+ [x] consider whether or not to use Tuple n
	+ [x] consider to rename module
		- Graphics.Cairo.CairoImage to Data.Cairo.Image or Data.CairoImage
	+ [x] separate Graphics.Cairo.CairoImage to itself and Graphics.Cairo.CairoImage.Internal
		- [x] move to ....Internal
		- [x] make Graphics.Cairo.CairoImage
		- [x] make export list of Graphics.Cairo.CairoImage
	+ [x] make CairoImage instance of Eq
		- [x] define compareBytes
		- [x] make CairoImage instance of Eq
* [x] extend Graphics.Cairo.CairoImage
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
	- [x] define A1
		* [x] type PixelA1
		* [x] type A1
		* [x] pattern CairoImageA1
		* [x] make A1 instance of Image
		* [x] make A1Mut
		* [x] pattern CairoImageMutA1
		* [x] make A1Mut instance of ImageMut
	- [x] define Rgb16565
	- [x] define Rgb30
* [x] try to use A8
	+ [x] refactor Data.CairoImage
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
		- [x] refactor Data.CairoImage
			* [x] export list
			* [x] import list
			* [x] structure
			* [x] body
		- [x] refactor Data.CairoImage.Internal
			* [x] export list
			* [x] import list
			* [x] structure
			* [x] body
	+ [x] separate cairo-image package
* [x] make package typecheck-plugin-nat-simple
* [x] make package ranged-list
* [x] define repeatMin, repeatMax, repeat
* [x] try to encode/decode bits to/from Word32 in little or big endian machine
* [x] separate JuicyPixels
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
* [x] make Cairo Image Format
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
* [x] move modules to Graphics.Cairo.Drawing
	+ [x] move Graphics.Cairo.CairoT to it
	+ [x] move Graphics.Cairo.Paths to it
	+ [x] move Graphics.Cairo.CairoPatternT to it
	+ [x] move Graphics.Cairo.Text to it
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
	+ [x] Tags and Links
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
* [x] move converter for JuicyPixels to Graphics.Cairo.JuicyPixels
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

separate JuicyPixels
--------------------

* [x] make a converter from/to cairo image to/from JuicyPixels image
	+ [x] make module Data.JuicyCairo
	+ [x] converter from/to instance of Cairo Image to/from Juicy Image a
		- [x] converter from instance of Cairo Image to Juicy Image a
		- [x] converter from Juicy Image a to instance of Cairo Image
	+ [x] converter from/to instance of ImageMut to/from Image a
	+ [x] converter from/to Argb32 to/from Image PixelRGBA8
		- [x] converter from Argb32 to Image PixelRGBA8
		- [x] converter from Image PixelRGBA8 to Argb32
	+ [x] converter from/to Argb32Mut to/from  Image PixelRGBA8
	+ [x] others
* [x] use the converter
* [x] to make package to convert cairo image and JuicyPixels image
	+ the name is JuicyCairo
* [x] to use the above package and simple-cairo in try-cairo
* [x] to try ARGB 32 in try-cairo
	+ [x] read
	+ [x] write
