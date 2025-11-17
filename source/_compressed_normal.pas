// _compressed_normal.cpp от сталкера, переписанный на делфи (для укрытий)

unit _compressed_normal;

interface
uses vmath;

function pvCompress(vec : TVec3) : Word;
function pvDecompress(mVec : Word) : TVec3;

implementation

const
	// upper 3 bits
	pvSIGN_MASK		= $e000;
	pvXSIGN_MASK	= $8000;
	pvYSIGN_MASK	= $4000;
	pvZSIGN_MASK	= $2000;

	// middle 6 bits - xbits
	pvTOP_MASK		= $1f80;

	// lower 7 bits - ybits
	pvBOTTOM_MASK	= $007f;

var
	// static lookup table for unit vector3 decompression
	pvUVAdjustment : array[0..$1FFF] of Single;
	
procedure pvInitializeStatics;
var
	I : Longint;
	xbits, ybits : Longint;
	x, y, z : Single;
begin
	for I := 0 to $1FFF do
	begin
		xbits := I shr 7;
		ybits := I and pvBOTTOM_MASK;
		
		// map the numbers back to the triangle (0,0)-(0,127)-(127,0)
		if ( xbits + ybits ) >= 127 then 
		begin
			xbits := 127 - xbits; 
			ybits := 127 - ybits; 
		end;
		
		// convert to 3D vectors
		x := xbits;
		y := ybits;
		z := 126 - xbits - ybits;
		
		// calculate the amount of normalization required
		pvUVAdjustment[I] := 1.0 / Sqrt( y*y + z*z + x*x );
	end;
end;

function pvCompress(vec : TVec3) : Word;
var
	mVec : Word;
	w : Single;
	xbits : Longint;
	ybits : Longint;
begin
	// input vector3 does not have to be unit length
	mVec := 0;

	if vec.x < 0 then begin mVec := mVec or pvXSIGN_MASK; vec.x := -vec.x end;
	if vec.y < 0 then begin mVec := mVec or pvYSIGN_MASK; vec.y := -vec.y end;
	if vec.z < 0 then begin mVec := mVec or pvZSIGN_MASK; vec.z := -vec.z end;

	// project the normal onto the plane that goes through
	// X0=(1,0,0),Y0=(0,1,0),Z0=(0,0,1).

	// on that plane we choose an (projective!) coordinate system
	// such that X0->(0,0), Y0->(126,0), Z0->(0,126),(0,0,0)->Infinity

	// a little slower... old pack was 4 multiplies and 2 adds. 
	// This is 2 multiplies, 2 adds, and a divide....
	w := 126.0 / ( vec.x + vec.y + vec.z );
	xbits := Trunc( vec.x * w );
	ybits := Trunc( vec.y * w );

	{
	VERIFY( xbits <  127 );
	VERIFY( xbits >= 0   );
	VERIFY( ybits <  127 );
	VERIFY( ybits >= 0   );
	}

	// Now we can be sure that 0<=xp<=126, 0<=yp<=126, 0<=xp+yp<=126

	// however for the sampling we want to transform this triangle 
	// into a rectangle.
	if xbits >= 64 then 
	begin
		xbits := 127 - xbits; 
		ybits := 127 - ybits; 
	end;

	// now we that have xp in the range (0,127) and yp in the range (0,63), 
	// we can pack all the bits together
	mVec := mVec or ( xbits << 7 );           
	mVec := mVec or ybits;

	Result := mVec;
end;

function pvDecompress(mVec : Word) : TVec3;
var
	uvadj : Single;
	xbits, ybits : Longint;
begin
	// if we do a straightforward backward transform
	// we will get points on the plane X0,Y0,Z0
	// however we need points on a sphere that goes through these points.
	// therefore we need to adjust x,y,z so that x^2+y^2+z^2=1
	
	// by normalizing the vector3. We have already precalculated the amount
	// by which we need to scale, so all we do is a table lookup and a 
	// multiplication
	
	// get the x and y bits
	xbits := (( mVec and pvTOP_MASK ) shr 7 );
	ybits := ( mVec and pvBOTTOM_MASK );
	
	// map the numbers back to the triangle (0,0)-(0,126)-(126,0)
	if ( xbits + ybits ) >= 127 then
	begin
		xbits := 127 - xbits; 
		ybits := 127 - ybits; 
	end;
	
	// do the inverse transform and normalization
	// costs 3 extra multiplies and 2 subtracts. No big deal.         
	uvadj := pvUVAdjustment[mVec and (not pvSIGN_MASK)];
	Result.x := uvadj * xbits;
	Result.y := uvadj * ybits;
	Result.z := uvadj * ( 126 - xbits - ybits );
	
	// set all the sign bits
	if ( mVec and pvXSIGN_MASK ) <> 0 then Result.x := -Result.x;
	if ( mVec and pvYSIGN_MASK ) <> 0 then Result.y := -Result.y;
	if ( mVec and pvZSIGN_MASK ) <> 0 then Result.z := -Result.z;
end;

initialization pvInitializeStatics;

end.