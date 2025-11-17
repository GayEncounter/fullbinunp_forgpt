unit FreeImage;

interface

type
	FIBITMAP = Pointer;
	FIBOOL = Longint;
	
const
	FI_DLL = 'FreeImage.dll';

const
	FIF_UNKNOWN = -1;
	FIF_BMP     = 0;
	FIF_ICO     = 1;
	FIF_JPEG    = 2;
	FIF_JNG     = 3;
	FIF_KOALA   = 4;
	FIF_LBM     = 5;
	FIF_IFF     = FIF_LBM;
	FIF_MNG     = 6;
	FIF_PBM     = 7;
	FIF_PBMRAW  = 8;
	FIF_PCD     = 9;
	FIF_PCX     = 10;
	FIF_PGM     = 11;
	FIF_PGMRAW  = 12;
	FIF_PNG     = 13;
	FIF_PPM     = 14;
	FIF_PPMRAW  = 15;
	FIF_RAS     = 16;
	FIF_TARGA   = 17;
	FIF_TIFF    = 18;
	FIF_WBMP    = 19;
	FIF_PSD     = 20;
	FIF_CUT     = 21;
	FIF_XBM     = 22;
	FIF_XPM     = 23;
	FIF_DDS     = 24;
	FIF_GIF     = 25;
	FIF_HDR     = 26;
	FIF_FAXG3   = 27;
	FIF_SGI     = 28;
	FIF_EXR     = 29;
	FIF_J2K     = 30;
	FIF_JP2     = 31;
	FIF_PFM     = 32;
	FIF_PICT    = 33;
	FIF_RAW     = 34;
	FIF_WEBP    = 35;
	FIF_JXR     = 36;
	
{$IFDEF WIN64}
function  FreeImage_GetFIFFromFilename(filename : PAnsiChar) : Longword; stdcall; external FI_DLL;
	
function  FreeImage_Load(format : Longword; filename : PAnsiChar; flags : Longint = 0) : FIBITMAP; stdcall; external FI_DLL;
procedure FreeImage_Unload(dib : FIBITMAP); stdcall; external FI_DLL;

function  FreeImage_GetBits(dib : FIBITMAP) : PByte; stdcall; external FI_DLL;
function  FreeImage_GetScanLine(dib : FIBITMAP; scanline : Longint) : PByte; stdcall; external FI_DLL;

function  FreeImage_GetBPP(dib : FIBITMAP) : Longword; stdcall; external FI_DLL;
function  FreeImage_GetWidth(dib : FIBITMAP) : Longword; stdcall; external FI_DLL;
function  FreeImage_GetHeight(dib : FIBITMAP) : Longword; stdcall; external FI_DLL;

function  FreeImage_ConvertTo32Bits(dib : FIBITMAP) : FIBITMAP; stdcall; external FI_DLL;

function  FreeImage_FlipVertical(dib : FIBITMAP) : FIBOOL; stdcall; external FI_DLL;
function  FreeImage_FlipHorizontal(dib : FIBITMAP) : FIBOOL; stdcall; external FI_DLL;
{$ELSE}
function  FreeImage_GetFIFFromFilename(filename : PAnsiChar) : Longword; stdcall; external FI_DLL name '_FreeImage_GetFIFFromFilename@4';
	
function  FreeImage_Load(format : Longword; filename : PAnsiChar; flags : Longint = 0) : FIBITMAP; stdcall; external FI_DLL name '_FreeImage_Load@12';
procedure FreeImage_Unload(dib : FIBITMAP); stdcall; external FI_DLL name '_FreeImage_Unload@4';

function  FreeImage_GetBits(dib : FIBITMAP) : PByte; stdcall; external FI_DLL name '_FreeImage_GetBits@4';
function  FreeImage_GetScanLine(dib : FIBITMAP; scanline : Longint) : PByte; stdcall; external FI_DLL name '_FreeImage_GetScanLine@8';

function  FreeImage_GetBPP(dib : FIBITMAP) : Longword; stdcall; external FI_DLL name '_FreeImage_GetBPP@4';
function  FreeImage_GetWidth(dib : FIBITMAP) : Longword; stdcall; external FI_DLL name '_FreeImage_GetWidth@4';
function  FreeImage_GetHeight(dib : FIBITMAP) : Longword; stdcall; external FI_DLL name '_FreeImage_GetHeight@4';

function  FreeImage_ConvertTo32Bits(dib : FIBITMAP) : FIBITMAP; stdcall; external FI_DLL name '_FreeImage_ConvertTo32Bits@4';

function  FreeImage_FlipVertical(dib : FIBITMAP) : FIBOOL; stdcall; external FI_DLL name '_FreeImage_FlipVertical@4';
function  FreeImage_FlipHorizontal(dib : FIBITMAP) : FIBOOL; stdcall; external FI_DLL name '_FreeImage_FlipHorizontal@4';
{$ENDIF}

implementation

end.