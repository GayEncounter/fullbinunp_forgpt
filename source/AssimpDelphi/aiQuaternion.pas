unit aiQuaternion;

interface

type TaiQuaternion = packed record
   w, x, y, z: single;
end;
type PaiQuaternion = ^TaiQuaternion;

function Interpolate(const qStart : TaiQuaternion; qEnd : TaiQuaternion; factor : Single) : TaiQuaternion;

implementation
uses Math;

// ---------------------------------------------------------------------------
// Performs a spherical interpolation between two quaternions
// Implementation adopted from the gmtl project. All others I found on the net fail in some cases.
// Congrats, gmtl!
function Interpolate(const qStart : TaiQuaternion; qEnd : TaiQuaternion; factor : Single) : TaiQuaternion;
var
	cosom,
	omega,
	sinom,
	sclp,
	sclq : Single;
begin
	// calc cosine theta
	cosom := qStart.x * qEnd.x + qStart.y * qEnd.y + qStart.z * qEnd.z + qStart.w * qEnd.w;
	
	// adjust signs (if necessary)
	if cosom < 0.0 then
	begin
		cosom := -cosom;
		qEnd.x := -qEnd.x;
		qEnd.y := -qEnd.y;
		qEnd.z := -qEnd.z;
		qEnd.w := -qEnd.w;
	end;
	
	// Calculate coefficients
	if (1.0 - cosom) > 0.0001 then // 0.0001 -> some epsillon
	begin
		// Standard case (slerp)
		omega := arccos(cosom);  // extract theta from dot product's cos theta
		sinom := Sin(omega);
		sclp := (1.0 - factor) * omega / sinom;
		sclq := factor * omega / sinom;
	end else
	begin
		// Very close, do linear interp (because it's faster)
		sclp := 1.0 - factor;
		sclq := factor;
	end;
	
	Result.x := sclp * qStart.x + sclq * qEnd.x;
	Result.y := sclp * qStart.y + sclq * qEnd.y;
	Result.z := sclp * qStart.z + sclq * qEnd.z;
	Result.w := sclp * qStart.w + sclq * qEnd.w;
end;

end.
