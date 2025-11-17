unit crnlib;

interface

type
	PPLongword = ^PLongword;

const
	cCRNFileTypeCRN            = 0;
	cCRNFileTypeDDS            = 1;
	
const
	cCRNFmtDXT1                = 0;
	cCRNFmtDXT3                = 1;
	cCRNFmtDXT5                = 2;
	cCRNFmtDXT5_CCxY           = 3;
	cCRNFmtDXT5_xGxR           = 4;
	cCRNFmtDXT5_xGBR           = 5;
	cCRNFmtDXT5_AGBR           = 6;
	cCRBFmtDXN_XY              = 7;
	cCRBFmtDXN_YX              = 8;
	cCRNFmtDXT5A               = 9;
	cCRNFmtETC1                = 10;

const
	cCRNMaxLevelResolution     = 4096;
	cCRNMinPaletteSize         = 8;
	cCRNMaxPaletteSize         = 8192;
	cCRNMaxFaces               = 6;
	cCRNMaxLevels              = 16;
	cCRNMaxHelperThreads       = 16;
	cCRNMinQualityLevel        = 0;
	cCRNMaxQualityLevel        = 255;
	
const
	// Enables perceptual colorspace distance metrics if set.
	// Important: Be sure to disable this when compressing non-sRGB colorspace images, like normal maps!
	// Default: Set
	cCRNCompFlagPerceptual = 1;
	
	// Enables (up to) 8x8 macroblock usage if set. If disabled, only 4x4 blocks are allowed.
	// Compression ratio will be lower when disabled, but may cut down on blocky artifacts because the process used to determine
	// where large macroblocks can be used without artifacts isn't perfect.
	// Default: Set.
	cCRNCompFlagHierarchical = 2;
	
	// cCRNCompFlagQuick disables several output file optimizations - intended for things like quicker previews.
	// Default: Not set.
	cCRNCompFlagQuick = 4;
	
	// DXT1: OK to use DXT1 alpha blocks for better quality or DXT1A transparency.
	// DXT5: OK to use both DXT5 block types.
	// Currently only used when writing to .DDS files, as .CRN uses only a subset of the possible DXTn block types.
	// Default: Set.
	cCRNCompFlagUseBothBlockTypes = 8;
	
	// OK to use DXT1A transparent indices to encode black (assumes pixel shader ignores fetched alpha).
	// Currently only used when writing to .DDS files, .CRN never uses alpha blocks.
	// Default: Not set.
	cCRNCompFlagUseTransparentIndicesForBlack = 16;
	
	// Disables endpoint caching, for more deterministic output.
	// Currently only used when writing to .DDS files.
	// Default: Not set.
	cCRNCompFlagDisableEndpointCaching = 32;
	
	// If enabled, use the cCRNColorEndpointPaletteSize, etc. params to control the CRN palette sizes. Only useful when writing to .CRN files.
	// Default: Not set.
	cCRNCompFlagManualPaletteSizes = 64;
	
	// If enabled, DXT1A alpha blocks are used to encode single bit transparency.
	// Default: Not set.
	cCRNCompFlagDXT1AForTransparency = 128;
	
	// If enabled, the DXT1 compressor's color distance metric assumes the pixel shader will be converting the fetched RGB results to luma (Y part of YCbCr).
	// This increases quality when compressing grayscale images, because the compressor can spread the luma error amoung all three channels (i.e. it can generate blocks
	// with some chroma present if doing so will ultimately lead to lower luma error).
	// Only enable on grayscale source images.
	// Default: Not set.
	cCRNCompFlagGrayscaleSampling = 256;
	
	// If enabled, debug information will be output during compression.
	// Default: Not set.
	cCRNCompFlagDebugging = $80000000;

const
	cCRNDXTQualitySuperFast = 0;
	cCRNDXTQualityFast      = 1;
	cCRNDXTQualityNormal    = 2;
	cCRNDXTQualityBetter    = 3;
	cCRNDXTQualityUber      = 4;
   
const
	cCRNDXTCompressorCRN    = 0; // Use crnlib's ETC1 or DXTc block compressor (default, highest quality, comparable or better than ati_compress or squish, and crnlib's ETC1 is a lot fasterw with similiar quality to Erricson's)
	cCRNDXTCompressorCRNF   = 1; // Use crnlib's "fast" DXTc block compressor
	cCRNDXTCompressorRYG    = 2; // Use RYG's DXTc block compressor (low quality, but very fast)
	cCRNDXTCompressorATI    = 3;
	cCRNDXTCompressorSquish = 4;

type
	crn_progress_callback_func = function(phase_index, total_phases, subphase_index, total_subphases : Longword; pUserData_ptr : Pointer) : Longint; cdecl;

type
	crn_comp_params = record
		m_size_of_obj                           : Longword;
		m_file_type                             : Longword;
		m_faces                                 : Longword;
		m_width                                 : Longword;
		m_height                                : Longword;
		m_levels                                : Longword;
		m_format                                : Longword;
		m_flags                                 : Longword;
		m_pImages                               : array[0..cCRNMaxFaces-1] of array[0..cCRNMaxLevels-1] of PLongword;
		m_target_bitrate                        : Single;
		m_quality_level                         : Longword;
		m_dxt1a_alpha_threshold                 : Longword;
		m_dxt_quality                           : Longword;
		m_dxt_compressor_type                   : Longword;
		m_alpha_component                       : Longword;
		m_crn_adaptive_tile_color_psnr_derating : Single;
		m_crn_adaptive_tile_alpha_psnr_derating : Single;
		m_crn_color_endpoint_palette_size       : Longword;
		m_crn_color_selector_palette_size       : Longword;
		m_crn_alpha_endpoint_palette_size       : Longword;
		m_crn_alpha_selector_palette_size       : Longword;
		m_num_helper_threads                    : Longword;
		m_userdata0                             : Longword;
		m_userdata1                             : Longword;
		m_pProgress_func                        : crn_progress_callback_func;
		m_pProgress_func_data                   : Pointer;
		
		procedure Clear;
	end;
	
{$IFDEF WIN64}
const
	crn_free_block_Proc = '?crn_free_block@@YAXPEAX@Z';
	crn_decompress_crn_to_dds_Proc = '?crn_decompress_crn_to_dds@@YAPEAXPEBXAEAI@Z';
	crn_compress_Proc = '?crn_compress@@YAPEAXAEBUcrn_comp_params@@AEAIPEAIPEAM@Z';
	crn_decompress_dds_to_images_Proc = '?crn_decompress_dds_to_images@@YA_NPEBXIPEAPEAIAEAUcrn_texture_desc@@@Z';
	crn_free_all_images_Proc = '?crn_free_all_images@@YAXPEAPEAIAEBUcrn_texture_desc@@@Z';
	crn_create_block_compressor_Proc = '?crn_create_block_compressor@@YAPEAXAEBUcrn_comp_params@@@Z';
	crn_compress_block_Proc = '?crn_compress_block@@YAXPEAXPEBI0@Z';
	crn_free_block_compressor_Proc = '?crn_free_block_compressor@@YAXPEAX@Z';
	crn_decompress_block_Proc = '?crn_decompress_block@@YA_NPEBXPEAIW4crn_format@@@Z';
{$ELSE}
const
	crn_free_block_Proc = '?crn_free_block@@YAXPAX@Z';
	crn_decompress_crn_to_dds_Proc = '?crn_decompress_crn_to_dds@@YAPAXPBXAAI@Z';
	crn_compress_Proc = '?crn_compress@@YAPAXABUcrn_comp_params@@AAIPAIPAM@Z';
	crn_decompress_dds_to_images_Proc = '?crn_decompress_dds_to_images@@YA_NPBXIPAPAIAAUcrn_texture_desc@@@Z';
	crn_free_all_images_Proc = '?crn_free_all_images@@YAXPAPAIABUcrn_texture_desc@@@Z';
	crn_create_block_compressor_Proc = '?crn_create_block_compressor@@YAPAXABUcrn_comp_params@@@Z';
	crn_compress_block_Proc = '?crn_compress_block@@YAXPAXPBI0@Z';
	crn_free_block_compressor_Proc = '?crn_free_block_compressor@@YAXPAX@Z';
	crn_decompress_block_Proc = '?crn_decompress_block@@YA_NPBXPAIW4crn_format@@@Z';
{$ENDIF}

procedure crn_free_block(block : Pointer); cdecl; 
external 'crnlib.dll' name crn_free_block_Proc;

function  crn_decompress_crn_to_dds(data : Pointer; file_size : PLongint) : Pointer; cdecl; 
external 'crnlib.dll' name crn_decompress_crn_to_dds_Proc;
	
function crn_compress(
	var comp_params : crn_comp_params; 
	out compressed_size : Longword; 
	pActual_quality_level : PLongword = nil; 
	pActual_bitrate : PSingle = nil) : pointer; cdecl;
external 'crnlib.dll' name crn_compress_Proc;

type
	crn_texture_desc = record
		m_faces : Longword;
		m_width : Longword;
		m_height : Longword;
		m_levels : Longword;
		m_fmt_fourcc : Longword;
	end;
	
function crn_decompress_dds_to_images(
	pDDS_file_data : Pointer; 
	dds_file_size : Longword; 
	ppImages : PPLongword; 
	out tex_desc : crn_texture_desc) : Boolean; cdecl;
external 'crnlib.dll' name crn_decompress_dds_to_images_Proc;

procedure crn_free_all_images(ppImages : PLongword; var dest : crn_texture_desc); cdecl;
external 'crnlib.dll' name crn_free_all_images_Proc;

type
	crn_block_compressor_context_t = Pointer;
	
function crn_create_block_compressor(var params : crn_comp_params) : crn_block_compressor_context_t; cdecl;
external 'crnlib.dll' name crn_create_block_compressor_Proc;

procedure crn_compress_block(pContext : crn_block_compressor_context_t; pPixels : PLongword; pDst_block : Pointer); cdecl;
external 'crnlib.dll' name crn_compress_block_Proc;

procedure crn_free_block_compressor(pContext : crn_block_compressor_context_t); cdecl;
external 'crnlib.dll' name crn_free_block_compressor_Proc;

function crn_decompress_block(pSrc_block : Pointer; pDst_pixels : PLongword; crn_fmt : Longword) : Boolean; cdecl;
external 'crnlib.dll' name crn_decompress_block_Proc;

implementation

procedure crn_comp_params.Clear;
begin
	m_size_of_obj := Sizeof(crn_comp_params);
	m_file_type := cCRNFileTypeCRN;
	m_faces := 1;
	m_width := 0;
	m_height := 0;
	m_levels := 1;
	m_format := cCRNFmtDXT1;
	m_flags := cCRNCompFlagPerceptual or cCRNCompFlagHierarchical or cCRNCompFlagUseBothBlockTypes;
	
	FillChar(m_pImages, Sizeof(m_pImages), #0);
	
	m_target_bitrate := 0.0;
	m_quality_level := cCRNMaxQualityLevel;
	m_dxt1a_alpha_threshold := 128;
	m_dxt_quality := cCRNDXTQualityUber;
	m_dxt_compressor_type := cCRNDXTCompressorCRN;
	m_alpha_component := 3;
	
	m_crn_adaptive_tile_color_psnr_derating := 2.0;
	m_crn_adaptive_tile_alpha_psnr_derating := 2.0;
	m_crn_color_endpoint_palette_size := 0;
	m_crn_color_selector_palette_size := 0;
	m_crn_alpha_endpoint_palette_size := 0;
	m_crn_alpha_selector_palette_size := 0;
	
	m_num_helper_threads := 0;
	m_userdata0 := 0;
	m_userdata1 := 0;
	m_pProgress_func := nil;
	m_pProgress_func_data := nil;
end;

end.