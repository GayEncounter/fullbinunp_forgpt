unit WGLExt;

interface
uses Windows;

// WGL_ARB_extensions_string
type
  PFNWGLGETEXTENSIONSSTRINGARBPROC = function(_hdc : HDC) : PAnsiChar; stdcall;

// WGL_ARB_multisample
const
  WGL_SAMPLE_BUFFERS_ARB            = $2041;
  WGL_SAMPLES_ARB                   = $2042;

// WGL_ARB_pixel_format
const
  WGL_NUMBER_PIXEL_FORMATS_ARB      = $2000;
  WGL_DRAW_TO_WINDOW_ARB            = $2001;
  WGL_DRAW_TO_BITMAP_ARB            = $2002;
  WGL_ACCELERATION_ARB              = $2003;
  WGL_NEED_PALETTE_ARB              = $2004;
  WGL_NEED_SYSTEM_PALETTE_ARB       = $2005;
  WGL_SWAP_LAYER_BUFFERS_ARB        = $2006;
  WGL_SWAP_METHOD_ARB               = $2007;
  WGL_NUMBER_OVERLAYS_ARB           = $2008;
  WGL_NUMBER_UNDERLAYS_ARB          = $2009;
  WGL_TRANSPARENT_ARB               = $200A;
  WGL_TRANSPARENT_RED_VALUE_ARB     = $2037;
  WGL_TRANSPARENT_GREEN_VALUE_ARB   = $2038;
  WGL_TRANSPARENT_BLUE_VALUE_ARB    = $2039;
  WGL_TRANSPARENT_ALPHA_VALUE_ARB   = $203A;
  WGL_TRANSPARENT_INDEX_VALUE_ARB   = $203B;
  WGL_SHARE_DEPTH_ARB               = $200C;
  WGL_SHARE_STENCIL_ARB             = $200D;
  WGL_SHARE_ACCUM_ARB               = $200E;
  WGL_SUPPORT_GDI_ARB               = $200F;
  WGL_SUPPORT_OPENGL_ARB            = $2010;
  WGL_DOUBLE_BUFFER_ARB             = $2011;
  WGL_STEREO_ARB                    = $2012;
  WGL_PIXEL_TYPE_ARB                = $2013;
  WGL_COLOR_BITS_ARB                = $2014;
  WGL_RED_BITS_ARB                  = $2015;
  WGL_RED_SHIFT_ARB                 = $2016;
  WGL_GREEN_BITS_ARB                = $2017;
  WGL_GREEN_SHIFT_ARB               = $2018;
  WGL_BLUE_BITS_ARB                 = $2019;
  WGL_BLUE_SHIFT_ARB                = $201A;
  WGL_ALPHA_BITS_ARB                = $201B;
  WGL_ALPHA_SHIFT_ARB               = $201C;
  WGL_ACCUM_BITS_ARB                = $201D;
  WGL_ACCUM_RED_BITS_ARB            = $201E;
  WGL_ACCUM_GREEN_BITS_ARB          = $201F;
  WGL_ACCUM_BLUE_BITS_ARB           = $2020;
  WGL_ACCUM_ALPHA_BITS_ARB          = $2021;
  WGL_DEPTH_BITS_ARB                = $2022;
  WGL_STENCIL_BITS_ARB              = $2023;
  WGL_AUX_BUFFERS_ARB               = $2024;
  WGL_NO_ACCELERATION_ARB           = $2025;
  WGL_GENERIC_ACCELERATION_ARB      = $2026;
  WGL_FULL_ACCELERATION_ARB         = $2027;
  WGL_SWAP_EXCHANGE_ARB             = $2028;
  WGL_SWAP_COPY_ARB                 = $2029;
  WGL_SWAP_UNDEFINED_ARB            = $202A;
  WGL_TYPE_RGBA_ARB                 = $202B;
  WGL_TYPE_COLORINDEX_ARB           = $202C;
  
type
  PFNWGLGETPIXELFORMATATTRIBIVARBPROC = function(_hdc : HDC; iPixelFormat, iLayerPlane : Longint; nAttributes : Longword; piAttributes, piValues : PLongint) : BOOL; stdcall; 
  PFNWGLGETPIXELFORMATATTRIBFVARBPROC = function(_hdc : HDC; iPixelFormat, iLayerPlane : Longint; nAttributes : Longword; piAttributes : PLongint; pfValues : PSingle) : BOOL; stdcall;
  PFNWGLCHOOSEPIXELFORMATARBPROC = function(_hdc : HDC; piAttribIList : PLongint; pfAttribFList : PSingle; nMaxFormats : Longword; piFormats : PLongint; nNumFormats : PLongword) : BOOL; stdcall;

// WGL_3DFX_multisample
const
  WGL_SAMPLE_BUFFERS_3DFX           = $2060;
  WGL_SAMPLES_3DFX                  = $2061;
  
// WGL_EXT_depth_float
const
  WGL_DEPTH_FLOAT_EXT               = $2040;
  
// WGL_EXT_extensions_string
type
  PFNWGLGETEXTENSIONSSTRINGEXTPROC = function : PAnsiChar; stdcall;
  
// WGL_EXT_multisample
const
  WGL_SAMPLE_BUFFERS_EXT            = $2041;
  WGL_SAMPLES_EXT                   = $2042;
  
// WGL_EXT_pixel_format
const
  WGL_NUMBER_PIXEL_FORMATS_EXT      = $2000;
  WGL_DRAW_TO_WINDOW_EXT            = $2001;
  WGL_DRAW_TO_BITMAP_EXT            = $2002;
  WGL_ACCELERATION_EXT              = $2003;
  WGL_NEED_PALETTE_EXT              = $2004;
  WGL_NEED_SYSTEM_PALETTE_EXT       = $2005;
  WGL_SWAP_LAYER_BUFFERS_EXT        = $2006;
  WGL_SWAP_METHOD_EXT               = $2007;
  WGL_NUMBER_OVERLAYS_EXT           = $2008;
  WGL_NUMBER_UNDERLAYS_EXT          = $2009;
  WGL_TRANSPARENT_EXT               = $200A;
  WGL_TRANSPARENT_VALUE_EXT         = $200B;
  WGL_SHARE_DEPTH_EXT               = $200C;
  WGL_SHARE_STENCIL_EXT             = $200D;
  WGL_SHARE_ACCUM_EXT               = $200E;
  WGL_SUPPORT_GDI_EXT               = $200F;
  WGL_SUPPORT_OPENGL_EXT            = $2010;
  WGL_DOUBLE_BUFFER_EXT             = $2011;
  WGL_STEREO_EXT                    = $2012;
  WGL_PIXEL_TYPE_EXT                = $2013;
  WGL_COLOR_BITS_EXT                = $2014;
  WGL_RED_BITS_EXT                  = $2015;
  WGL_RED_SHIFT_EXT                 = $2016;
  WGL_GREEN_BITS_EXT                = $2017;
  WGL_GREEN_SHIFT_EXT               = $2018;
  WGL_BLUE_BITS_EXT                 = $2019;
  WGL_BLUE_SHIFT_EXT                = $201A;
  WGL_ALPHA_BITS_EXT                = $201B;
  WGL_ALPHA_SHIFT_EXT               = $201C;
  WGL_ACCUM_BITS_EXT                = $201D;
  WGL_ACCUM_RED_BITS_EXT            = $201E;
  WGL_ACCUM_GREEN_BITS_EXT          = $201F;
  WGL_ACCUM_BLUE_BITS_EXT           = $2020;
  WGL_ACCUM_ALPHA_BITS_EXT          = $2021;
  WGL_DEPTH_BITS_EXT                = $2022;
  WGL_STENCIL_BITS_EXT              = $2023;
  WGL_AUX_BUFFERS_EXT               = $2024;
  WGL_NO_ACCELERATION_EXT           = $2025;
  WGL_GENERIC_ACCELERATION_EXT      = $2026;
  WGL_FULL_ACCELERATION_EXT         = $2027;
  WGL_SWAP_EXCHANGE_EXT             = $2028;
  WGL_SWAP_COPY_EXT                 = $2029;
  WGL_SWAP_UNDEFINED_EXT            = $202A;
  WGL_TYPE_RGBA_EXT                 = $202B;
  WGL_TYPE_COLORINDEX_EXT           = $202C;
  
type
  PFNWGLGETPIXELFORMATATTRIBIVEXTPROC = function(_hdc : HDC; iPixelFormat, iLayerPlane : Longint; nAttributes : Longword; piAttributes, piValues : PLongint) : BOOL; stdcall; 
  PFNWGLGETPIXELFORMATATTRIBFVEXTPROC = function(_hdc : HDC; iPixelFormat, iLayerPlane : Longint; nAttributes : Longword; piAttributes : PLongint; pfValues : PSingle) : BOOL; stdcall;
  PFNWGLCHOOSEPIXELFORMATEXTPROC = function(_hdc : HDC; piAttribIList : PLongint; pfAttribFList : PSingle; nMaxFormats : Longword; piFormats : PLongint; nNumFormats : PLongword) : BOOL; stdcall;


implementation

end.