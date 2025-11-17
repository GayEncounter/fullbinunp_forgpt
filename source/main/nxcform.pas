unit nxcform;

interface
uses chunkedFile, vmath, fouramdl;

const
	NXCFORM_VERSION_14 = 14; // Exodus obsolete
	NXCFORM_VERSION_15 = 15; // Arktika.1
	NXCFORM_VERSION_16 = 16; // Exodus

type
	TNxCformSurface = record
		materialid : Word;
		shader, texture, material : String;
		sectorid : Word;
		points : array of TVec3;
		indices : array of LongWord;
	end;

	TNxCform = class
		level : Boolean;
		surfaces : array of TNxCformSurface;

		constructor Create(forlevel : Boolean);

		function GetSurface(mtlid : Word) : Integer; overload;
		function GetSurface(shader, texture, material : String) : Integer; overload;

		procedure Add4AModel(mdl : T4AModelSimple); overload;
		procedure Add4AModel(mdl : T4AModelRef; from : T4ALevel); overload;

		procedure Save(w : TMemoryWriter);
		procedure SaveLL(w : TMemoryWriter);
		procedure SaveRedux(w : TMemoryWriter);
	end;

implementation
uses sysutils, PhysX;

constructor TNxCform.Create(forlevel : Boolean);
begin
	level := forlevel;
end;

function TNxCform.GetSurface(mtlid: Word) : Integer;
var I : Integer;
begin
	GetSurface := -1;
	for I := 0 to Length(surfaces) - 1 do
		if surfaces[I].materialid = mtlid then
		begin
			GetSurface := I;
			Break;
		end;
end;

function TNxCform.GetSurface(shader, texture, material : String) : Integer;
var I : Integer;
begin
	GetSurface := -1;
	for I := 0 to Length(surfaces) - 1 do
		if (surfaces[I].shader = shader) and
			 (surfaces[I].texture = texture) and
			 (surfaces[I].material = material) then
		begin
			GetSurface := I;
			Break;
		end;
end;

procedure TNxCform.Add4AModel(mdl : T4AModelSimple);
var
	startvtx, startidx, S, I : Integer;
begin
	S := GetSurface(mdl.shader, mdl.texture, mdl.material);
	if S = -1 then
	begin
		S := Length(surfaces);
		SetLength(surfaces, Length(surfaces)+1);
		surfaces[S].shader := mdl.shader;
		surfaces[S].texture := mdl.texture;
		surfaces[S].material := mdl.material;
	end;

	startvtx := Length(surfaces[S].points);
	SetLength(surfaces[S].points, Length(surfaces[S].points)+Length(mdl.vertices));
	for I := 0 to Length(mdl.vertices) - 1 do
		surfaces[S].points[startvtx+I] := mdl.vertices[I].point;

	startidx := Length(surfaces[S].indices);
	SetLength(surfaces[S].indices, Length(surfaces[S].indices)+Length(mdl.indices));
	for I := 0 to (Length(mdl.indices) div 3) - 1 do
	begin
		surfaces[S].indices[startidx+(I*3)	] := mdl.indices[I*3  ] + startvtx;
		surfaces[S].indices[startidx+(I*3)+1] := mdl.indices[I*3+1] + startvtx;
		surfaces[S].indices[startidx+(I*3)+2] := mdl.indices[I*3+2] + startvtx;
	end;
end;

procedure TNxCform.Add4AModel(mdl : T4AModelRef; from : T4ALevel);
var
	v : P4AVertLevel;
	startvtx, startidx, S, I : Integer;
begin
	S := GetSurface(mdl.shaderid);
	//if S = -1 then
	begin
		S := Length(surfaces);
		SetLength(surfaces, Length(surfaces)+1);
		surfaces[S].materialid := mdl.shaderid;
		
		// for debug purposes
		surfaces[S].shader   := from.materials[mdl.shaderid].shader;
		surfaces[S].texture  := from.materials[mdl.shaderid].texture;
		surfaces[S].material := from.materials[mdl.shaderid].material;
		
		// remember sector number (for Last Light)
		surfaces[S].sectorid := from.materials[mdl.shaderid].flags and $FFFF;
	end;

	startvtx := Length(surfaces[S].points);
	SetLength(surfaces[S].points, Length(surfaces[S].points)+mdl.vertexcount);
	v := @from.vbuffer[mdl.vertexoffset];
	for I := 0 to mdl.vertexcount - 1 do
	begin
		surfaces[S].points[startvtx+I] := v^.point;
		Inc(v);
	end;

	startidx := Length(surfaces[S].indices);
	SetLength(surfaces[S].indices, Length(surfaces[S].indices)+mdl.indexcount);
	for I := 0 to (mdl.indexcount div 3) - 1 do
	begin
		surfaces[S].indices[startidx+(I*3)	] := from.ibuffer[mdl.indexoffset+(I*3)  ] + startvtx;
		surfaces[S].indices[startidx+(I*3)+1] := from.ibuffer[mdl.indexoffset+(I*3)+1] + startvtx;
		surfaces[S].indices[startidx+(I*3)+2] := from.ibuffer[mdl.indexoffset+(I*3)+2] + startvtx;
	end;
end;

procedure TNxCform.Save(w : TMemoryWriter);
var
	data : Pointer;
	size : Longword;

	I, res : Integer;
	
	isLE : Boolean;
	
	function SW(src : Word) : Word;
	begin
		if isLE then
			SW := NtoLE(src)
		else
			SW := NtoBE(src)
	end;
	
	function LW(src : Longword) : Longword;
	begin
		if isLE then
			LW := NtoLE(src)
		else
			LW := NtoBE(src)
	end;
begin
	if InitCooking = 0 then
		raise Exception.Create('InitCooking failed!');
	
	try	
		isLE := True;
	
		w.WriteByte(Byte(isLE));
		w.WriteLongword(LW($00001FDF));
		w.WriteLongword(LW(5)); // version (probably)
		w.WriteLongword(LW($44BB8000)); // checksum
		w.WriteLongword(LW(Length(surfaces)));
		for I := 0 to Length(surfaces) - 1 do
		begin
			if not level then
			begin
				w.WriteByte(0);
				w.WriteLongword(LW(Length(surfaces[I].shader)+1));
				w.WriteStringZ(surfaces[I].shader);
				w.WriteLongword(LW(Length(surfaces[I].texture)+1));
				w.WriteStringZ(surfaces[I].texture);
				w.WriteLongword(LW(Length(surfaces[I].material)+1));
				w.WriteStringZ(surfaces[I].material);
			end else
			begin
				w.WriteByte(1);
				w.WriteWord(SW(surfaces[I].materialid));
			end;
	
			res := CookTriangleMesh(
			Length(surfaces[I].points), Length(surfaces[I].indices) div 3,
			Pointer(surfaces[I].points), Pointer(surfaces[I].indices),
			Sizeof(TVec3), Sizeof(LongWord)*3,
			NX_MF_HARDWARE_MESH, @data, @size);
	
			if res = 0 then
				raise Exception.Create('CookTriangleMesh failed, texture: ' + surfaces[I].texture);
	
			w.WriteLongword(LW(size));
			w.Write(data^, size);
		end;
	finally
		CloseCooking;
	end;
end;

procedure TNxCform.SaveLL(w : TMemoryWriter);
var
	data : Pointer;
	size : Longword;

	I, res : Integer;
	
	isLE : Boolean;
	version : Longint;
	
	function SW(src : Word) : Word;
	begin
		if isLE then
			SW := NtoLE(src)
		else
			SW := NtoBE(src)
	end;
	
	function LW(src : Longword) : Longword;
	begin
		if isLE then
			LW := NtoLE(src)
		else
			LW := NtoBE(src)
	end;
begin
	if InitCooking = 0 then
		raise Exception.Create('InitCooking failed!');
	
	//SetCookingParams(NX_PLATFORM_XENON, 0.025, 1);
	
	try	
		isLE := False;
		
		if level then version := 11
		else version := 8;
	
		w.WriteByte(Byte(isLE));
		w.WriteLongword(LW($00001FDF));
		w.WriteLongword(LW(version)); // version (probably)
		w.WriteLongword(LW($44BB8000)); // checksum
		w.WriteLongword(LW(Length(surfaces)));
		for I := 0 to Length(surfaces) - 1 do
		begin
			if not level then
			begin
				w.WriteByte(0);
				w.WriteLongword(LW(Length(surfaces[I].shader)+1));
				w.WriteStringZ(surfaces[I].shader);
				w.WriteLongword(LW(Length(surfaces[I].texture)+1));
				w.WriteStringZ(surfaces[I].texture);
				w.WriteLongword(LW(Length(surfaces[I].material)+1));
				w.WriteStringZ(surfaces[I].material);

				// unknown (last light)	
				case version of
					// такая версия используется для статических моделей 
					8: begin
						w.WriteWord(SW($FFFF));
						w.WriteWord(SW($0000));
						w.WriteByte(1);
					end;
					
					// такая версия используется для уровней в билде за декабрь 2012-ого. Работает и в релизе.
					9: begin
						w.WriteWord(SW($0000)); // подозреваю что это номер сектора...
					end;
					
					// такая версия используется для уровней в релизе.
					11: begin
						w.WriteWord(SW(surfaces[I].sectorid));
						w.WriteWord(SW($0000));
						w.WriteByte(1);
					end;
				end;
				
			end else
			begin
				w.WriteByte(1);
				w.WriteWord(SW(surfaces[I].materialid));
			end;
	
			res := CookTriangleMesh(
			Length(surfaces[I].points), Length(surfaces[I].indices) div 3,
			Pointer(surfaces[I].points), Pointer(surfaces[I].indices),
			Sizeof(TVec3), Sizeof(LongWord)*3,
			NX_MF_HARDWARE_MESH, @data, @size);
	
			if res = 0 then
				raise Exception.Create('CookTriangleMesh failed, texture: ' + surfaces[I].texture);
	
			w.WriteLongword(LW(size));
			w.Write(data^, size);
		end;
	finally
		CloseCooking;
	end;
end;

procedure TNxCform.SaveRedux(w : TMemoryWriter);
var
	data : Pointer;
	size : Longword;

	I, res : Integer;
	
	isLE : Boolean;
	version : Longint;
	
	function SW(src : Word) : Word;
	begin
		if isLE then
			SW := NtoLE(src)
		else
			SW := NtoBE(src)
	end;
	
	function LW(src : Longword) : Longword;
	begin
		if isLE then
			LW := NtoLE(src)
		else
			LW := NtoBE(src)
	end;
begin
	if InitCooking3 = 0 then
		raise Exception.Create('InitCooking3 failed!');

	try
		isLE := True;
		
		if level then version := 13
		else version := 8;
	
		w.WriteByte(Byte(isLE));
		w.WriteLongword(LW($00001BDF));
		w.WriteLongword(LW(version)); // version (probably)
		w.WriteLongword(LW($44BB8000)); // checksum
		w.WriteLongword(LW(Length(surfaces)));
		for I := 0 to Length(surfaces) - 1 do
		begin
			if True {not level} then
			begin
				w.WriteByte(0);
				w.WriteLongword(LW(Length(surfaces[I].shader)+1));
				w.WriteStringZ(surfaces[I].shader);
				w.WriteLongword(LW(Length(surfaces[I].texture)+1));
				w.WriteStringZ(surfaces[I].texture);
				w.WriteLongword(LW(Length(surfaces[I].material)+1));
				w.WriteStringZ(surfaces[I].material);
	
				// unknown 
				case version of
					// такая версия используется для статических моделей 
					8: begin
						w.WriteWord(SW($FFFF));
						w.WriteWord(SW($0000));
						w.WriteByte(1);
					end;
					
					// такая версия используется для уровней.
					13: begin
						w.WriteWord(SW(surfaces[I].sectorid));
						w.WriteWord(SW($0000));
						w.WriteByte(1);
						
						if version >= NXCFORM_VERSION_15 then
							w.WriteByte(0); // boolean ?
						if version >= NXCFORM_VERSION_16 then
							w.WriteByte(1); // also boolean ?
					end;
				end;
				
			end else
			begin
				w.WriteByte(1);
				w.WriteWord(SW(surfaces[I].materialid));
			end;
	
			res := CookTriangleMesh3(
			Length(surfaces[I].points), Length(surfaces[I].indices) div 3,
			Pointer(surfaces[I].points), Pointer(surfaces[I].indices),
			Sizeof(TVec3), Sizeof(LongWord)*3,
			NX_MF_HARDWARE_MESH, @data, @size);
	
			if res = 0 then
				raise Exception.Create('CookTriangleMesh3 failed, texture: ' + surfaces[I].texture);
	
			w.WriteLongword(LW(size));
			w.Write(data^, size);
		end;
	finally
		CloseCooking3;
	end;
end;

end.