unit XBox360Swizzle;

interface

const
	DXT1_BLOCK_SZ = 8;
	DXT5_BLOCK_SZ = 16;	

function TiledBlockCoord(x, y, blockwidth, blocksize : Longint) : Longint;

implementation

// a bit reworked code from Xenia emulator
// I don't understand how it works
// https://github.com/xenia-project/xenia/blob/7dd715ea6fdd008c242f3d8b6d53d82e0ac348ad/src/xenia/gpu/texture_conversion.cc#L93
function TiledBlockCoord(x, y, blockwidth, blocksize : Longint) : Longint;
var
	log2_bpp : Longint;
	macro1, micro1 : Longint;
	row_offset : Longint;
	macro2, micro2 : Longint;
	offset1, offset2 : Longint;
	block_id : Longint;
begin
	log2_bpp := (blocksize div 4) + ((blocksize div 2) shr (blocksize div 4));
	macro1 := ((y div 32) * (blockwidth div 32)) shl (log2_bpp + 7);
	micro1 := ((y and 6) shl 2) shl log2_bpp;
	row_offset := macro1 + ((micro1 and (not $F)) shl 1) + (micro1 and $F) + ((y and 8) shl (3 + log2_bpp)) + ((y and 1) shl 4);
	macro2 := (x div 32) shl (log2_bpp + 7);
	micro2 := (x and 7) shl log2_bpp;
	offset1 := row_offset + (macro2 + ((micro2 and (not $F)) shl 1) + (micro2 and $F));
	offset2 := ((offset1 and (not $1FF)) shl 3) + ((offset1 and $1C0) shl 2) + (offset1 and $3F) + ((y and 16) shl 7) + (((((y and 8) shr 2) + (x shr 3)) and 3) shl 6);
	block_id := offset2 shr log2_bpp;
	TiledBlockCoord := block_id;
end;

end.