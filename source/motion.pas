unit motion;

interface
uses vmath, chunkedFile, skeleton;

type 
	I4AMotion = class
		function  LengthSec : Single; virtual; abstract;
		function  AffectsBone(id : Longint) : Boolean; virtual; abstract;
		procedure GetTransform(id : Longint; t : Single; out m : TMatrix); virtual; abstract;
	end;

const
	flUsePosition = $01;
	flUseRotation = $02;
	flModernRotation = $20;

type
	TBoneMotion = record
		flags : Byte;
		rotation : array of Smallint;
		position : array of Smallint;
		position_origin : TVec3;
		position_scale : TVec3;
	end;

	T4AMotion = class(I4AMotion)
		version : Longint;
		skeleton_crc : Longint;
		
		unk1 : Word;
		speed : Single;
		accrue : Single;
		falloff : Single;
		frame_count : Longint;
		unk2 : Longint;
		unk3 : set of Byte;
		affected_bones : set of Byte;
		
		data : array of TBoneMotion;
	
		constructor CreateAndLoad(const fn : String); overload;
		constructor CreateAndLoad(reader : TMemoryReader); overload;
	
		procedure Load(const fn : String); overload;
		procedure Load(reader : TMemoryReader); overload;
		
		procedure Save(w : TMemoryWriter);
		
		function  LengthSec : Single; override;
		function  AffectsBone(id : Longint) : Boolean; override;
		procedure GetTransform(id : Longint; t : Single; out m : TMatrix); override;
	end;

type
	TMotionCurveLL = record
		curve_header_unkn : Word; // upper three nibbles
		curve_type : Byte;
		
		// type 2
		curve_constant : TVec4;
		
		// type 3
		curve_unkn_3 : Longword;
		
		// type 4, 5
		curve_time_divisor : Single;
		curve_time : array of Word;
		curve_value : array of Smallint;
		
		// type 4
		curve_origin : TVec3;
		curve_scale : TVec3;
		
		// type 6
		curve_unkn_6_f1 : Single;
		curve_unkn_6_v1 : TVec4S16;
	end;
	
	TBoneMotionLL = record
		id_curve_rot : Longint;
		id_curve_pos : Longint;
		id_curve_scale : Longint; // or not scale ? I don't quite get it
	end;
	
	TMotionLocatorNameLL = record
		name : String;
		unk1 : Byte;
	end;
	
	// arktika.1 uskeleton::time_tag
	TMotionTimeTagLL = record
		time : Single;
		tag : String;
		flags : Longword; // $01 - editor_type
	end;
	
	// arktika.1 usketelon::motion_ik_rule
	TMotionIKRuleLL = record
		quat : TVec4;
		pos : TVec3;
		start : Single;
		peak : Single;
		tail : Single;
		_end : Single;
		height : Single;
		radius : Single;
		floor : Single;
		chain : String;
		_type : Byte; 
		pad : Word; // padding for 4-byte alignment of next member ?
		frame : Longword;
		locator : String;
		attach_type : String;
	end;
	
	T4AMotionLL = class(I4AMotion)
		// we might need bind-pose at some point
		parent_skeleton : T4ASkeleton; 
		
		// CHUNK 0
		version : Longint;
		skeleton_crc : Longint;
		m_num_bones : Word;

		hdr_compression : Byte; // version >= 20; 1 = Raw, 2 = ACL
		hdr_unkn0       : Byte; // version >= 20; usually 4
		hdr_abracadabra0 : Longint; // version >= 20, whole number, usually 30 (FPS?)
		hdr_abracadabra1 : Longint; // version >= 20, usually $38D1B717, probably float
		hdr_abracadabra2 : Longint; // version >= 20, usually $3CF5C28F, probably float

		num_locators : Word;
		header_unk1 : Longint; // usually zero
		hdr_frame_count : Word;

		position_offset : TVec3;
		rotation_offset : TVec3; // things aren't clear with this
		
		// CHUNK 1
		motion_flags : Word;
		speed : Single;
		accrue : Single;
		falloff : Single;
		frame_count : Longint;
		jump_frame : Word;
		land_frame : Word;
		jump_frame2 : Word; // version >= 19
		land_frame2 : Word; // version >= 19
		hq_bones : set of Byte;
		m_motions_size : Longword;
		m_curves_offset : Longword;
		affected_bones : set of Byte;

		// CHUNK 9	
		affected_bones2 : set of Byte;
		num_locators2 : Word;
		num_xforms : Word;
		// motions_data_size : Longword;
		motions_data_unk2 : Longword; // float ?
		motions_data_unk3 : Longword; // float ?
		curves : array of TMotionCurveLL;
		bone_motions : array of TBoneMotionLL;
		
		// other chunks
		locator_names : array of TMotionLocatorNameLL;
		time_tags : array of TMotionTimeTagLL;
		was_8 : Boolean;
		something3_1 : Longword;
		something3_2 : Longword;
		ik_rules : array of TMotionIKRuleLL;
		
		constructor CreateAndLoad(const fn : String); overload;
		constructor CreateAndLoad(reader : TMemoryReader); overload;
		
		procedure Load(const fn : String); overload;
		procedure Load(reader : TMemoryReader); overload;
		
		procedure Save(const fn : String); overload;
		procedure Save(w : TMemoryWriter); overload;

		procedure SaveCurvesCompressed(m : TMemoryWriter; var curve_offsets : array of Longword);
		procedure SaveCurvesRaw(w : TMemoryWriter);
		
		function  LengthSec : Single; override;
		function  AffectsBone(id : Longint) : Boolean; override;

		function  GetTransformCurve(id_r, id_p, id_s : Longint; t : Single; out p : TVec3; out q : TVec4; out s : TVec3) : Byte; 
		procedure GetTransformBone(id : Longint; t : Single; out p : TVec3; out q : TVec4; out s : TVec3); 
		procedure GetTransform(id : Longint; t : Single; out m : TMatrix); override;
	end;

implementation
uses 
	Math, 
	uCrc, 
	aiQuaternion,
	sysutils; // for Exception

constructor T4AMotion.CreateAndLoad(const fn : String);
begin
	inherited Create;
	Load(fn);
end;

constructor T4AMotion.CreateAndLoad(reader : TMemoryReader);
begin
	inherited Create;
	Load(reader);
end;

procedure T4AMotion.Load(const fn : String); overload;
var
	r : TMemoryReader;
begin
	r := TMemoryReader.CreateFromFile(fn);
	try
		Load(r);
	finally
		r.Free;
	end;
end;

procedure T4AMotion.Load(reader : TMemoryReader); overload;
var
	I : Longint;
	r : TMemoryReader;
begin
	r := reader.OpenChunk(0);
	try
		version := r.ReadLongword;
		skeleton_crc := r.ReadLongword;
		SetLength(data, r.ReadWord);
		r.Read(affected_bones, 16);
	finally
		r.Free;
	end;
	
	r := reader.OpenChunk(1);
	
	try
		unk1 := r.ReadWord; // flags ?
		speed := r.ReadSingle;
		accrue := r.ReadSingle;
		falloff := r.ReadSingle;
		frame_count := r.ReadLongword;
		unk2 := r.ReadLongword; 
		r.Read(unk3, 32);
	finally
		r.Free;
	end;
	
	r := reader.OpenChunk(2);
	try
		for I := 0 to Length(data) - 1 do
			if AffectsBone(I) then
			begin
				data[I].flags := r.ReadByte;
				
				r.ReadLongword; // CRC
				r.ReadLongword; // Big-Endian CRC
				
				if (data[I].flags and flUseRotation) <> 0 then
					SetLength(data[I].rotation, frame_count*3)
				else
					SetLength(data[I].rotation, 3);
					
				r.Read(data[I].rotation[0], Length(data[I].rotation) * Sizeof(Smallint));
				
				if (data[I].flags and flUsePosition) <> 0 then
				begin
					r.ReadLongword; // CRC
					r.ReadLongword; // Big-Endian CRC
					SetLength(data[I].position, frame_count*3);
					r.Read(data[i].position[0], Length(data[I].position) * Sizeof(Smallint));
					
					r.Read(data[I].position_scale, Sizeof(TVec3));
					r.Read(data[I].position_origin, Sizeof(TVec3));
				end else
					r.Read(data[I].position_origin, Sizeof(TVec3));
			end;
	finally
		r.Free;
	end;
end;

procedure T4AMotion.Save(w : TMemoryWriter);
var
	I : Longint;
	
	function GetCRC(data : array of Smallint) : Longint;
	var buffer : String;
	begin
		// well, it's stupid, but i'm lazy
		SetLength(buffer, Length(data)*Sizeof(Smallint));
		Move(data[0], buffer[1], Length(buffer));
		Result := GetStringCrc(buffer);
	end;
	
	function GetCRC_BE(data : array of Smallint) : Longint;
	var
		data_be : array of Smallint;
		I : Longint;
	begin
		SetLength(data_be, Length(data));
		for I := 0 to Length(data_be)-1 do
			data_be[I] := NtoBE(data[I]);
			
		Result := GetCRC(data_be);
	end;
begin
	w.OpenChunk(0);
	
	w.WriteLongword(version);
	w.WriteLongword(skeleton_crc);
	w.WriteWord(Length(data)); // bone count
	w.Write(affected_bones, 16);
	
	w.CloseChunk;
	
	w.OpenChunk(1);
	
	w.WriteWord(unk1);
	w.WriteSingle(speed);
	w.WriteSingle(accrue);
	w.WriteSingle(falloff);
	w.WriteLongword(frame_count);
	w.WriteLongword(unk2);
	w.Write(unk3, 32);
	
	w.CloseChunk;
	
	w.OpenChunk(2);
	
	for I := 0 to Length(data) - 1 do
		if AffectsBone(I) then
		begin
			w.WriteByte(data[I].flags);
			
			w.WriteLongword(GetCRC(data[I].rotation)); // CRC
			w.WriteLongword(GetCRC_BE(data[I].rotation)); // Big-Endian CRC
				
			w.Write(data[I].rotation[0], Length(data[I].rotation) * Sizeof(Smallint));
			
			if (data[I].flags and flUsePosition) <> 0 then
			begin
				w.WriteLongword(GetCRC(data[I].position)); // CRC
				w.WriteLongword(GetCRC_BE(data[I].position)); // Big-Endian CRC
				
				w.Write(data[I].position[0], Length(data[I].position) * Sizeof(Smallint));
				
				w.Write(data[I].position_scale, Sizeof(TVec3));
				w.Write(data[I].position_origin, Sizeof(TVec3));
			end else
				w.Write(data[I].position_origin, Sizeof(TVec3));
		end;
	
	w.CloseChunk;
end;

function T4AMotion.LengthSec : Single;
begin
	LengthSec := frame_count / 30;
end;

function T4AMotion.AffectsBone(id : Longint) : Boolean;
begin
	Result := Byte(id) in affected_bones;
end;

procedure T4AMotion.GetTransform(id : Longint; t : Single; out m : TMatrix);
var
	I, rm : Longint;
	x, y, z : Smallint;
	
	q : TVec4;
	
	values : array[0..3] of Single;
	scale : Single;
	
	p : TVec3;
	
	function _Sqrt(v : Single) : Single;
	begin
		if v < 0.0001 then
			_Sqrt := 0.0
		else
			_Sqrt := Sqrt(v)
	end;
begin
	I := Trunc(t * frame_count);
	I := Min(I, frame_count-1);
	I := Max(I, 0);
	
	if (data[id].flags and flUseRotation) <> 0 then
	begin
		x := data[id].rotation[I*3  ];
		y := data[id].rotation[I*3+1];
		z := data[id].rotation[I*3+2];
	end else
	begin
		x := data[id].rotation[0];
		y := data[id].rotation[1];
		z := data[id].rotation[2];
	end;
	
	if (data[id].flags and flModernRotation) <> 0 then
	begin
		rm := ((x and $01) shl 1) or (y and $01);
		x := x and $FFFE;
		y := y and $FFFE;
		
		scale := (Sqrt(2.0) / 2.0) / 32767;

		values[0] := x * scale;
		values[1] := y * scale;
		values[2] := z * scale;
		values[3] := _Sqrt(1.0 - values[0]*values[0] - values[1]*values[1] - values[2]*values[2]);
		
		case rm of
			0: begin
				q.x := values[3];
				q.y := values[0];
				q.z := values[1];
				q.w := values[2];
			end;
			1: begin
				q.x := values[0];
				q.y := values[3];
				q.z := values[1];
				q.w := values[2];
			end;
			2: begin
				q.x := values[0];
				q.y := values[1];
				q.z := values[3];
				q.w := values[2];
			end;
			3: begin
				q.x := values[0];
				q.y := values[1];
				q.z := values[2];
				q.w := values[3];
			end;
		end;
	end else
	begin
		scale := 1.0 / 32767;
		
		q.x := x * scale;
		q.y := y * scale;
		q.z := z * scale;
		q.w := _Sqrt(1.0 - q.x*q.x - q.y*q.y - q.z*q.z);
	end;
	
	if (data[id].flags and flUsePosition) <> 0 then
	begin
		x := data[id].position[I*3  ];
		y := data[id].position[I*3+1];
		z := data[id].position[I*3+2];
		
		p.x := data[id].position_origin.x + x * data[id].position_scale.x;
		p.y := data[id].position_origin.y + y * data[id].position_scale.y;
		p.z := data[id].position_origin.z + z * data[id].position_scale.z;
	end else
		p := data[id].position_origin;
		
	RotateQuaternion(m, q);
	m[4,1] := p.x; m[4,2] := p.y; m[4,3] := p.z;
end;

constructor T4AMotionLL.CreateAndLoad(const fn : String);
begin
	inherited Create;
	Load(fn);
end;

constructor T4AMotionLL.CreateAndLoad(reader : TMemoryReader);
begin
	inherited Create;
	Load(reader);
end;

procedure T4AMotionLL.Load(const fn : String); overload;
var
	r : TMemoryReader;
begin
	r := TMemoryReader.CreateFromFile(fn);
	try
		Load(r);
	finally
		r.Free;
	end;
end;

procedure T4AMotionLL.Load(reader : TMemoryReader);
var
	r : TMemoryReader;
	I, J : Longint;
	
	num_curves : Longint;
	offsets : array of Longword;
	
	header : LongWord;
	num_points : Longint;
	num_elements : Longint;
	
	temp : array[0..7] of Longword;
	
	isLE : Boolean;
	function _BEtoNw(w:Word):Word;
	begin
		if isLE then _BEtoNw := w
		else _BEtoNw := BEtoN(w);
	end;
	function _BEtoNl(w:LongWord):LongWord;
	begin
		if isLE then _BEtoNl := w
		else _BEtoNl := BEtoN(w);
	end;	
begin
	r := reader.OpenChunk(0);
	try
		version := r.ReadLongword;
		skeleton_crc := r.ReadLongword;
		m_num_bones := r.ReadWord;

		if version >= 20 then
		begin
			hdr_compression := r.ReadByte;
			hdr_unkn0       := r.ReadByte;
			hdr_abracadabra0 := r.ReadLongword;
			hdr_abracadabra1 := r.ReadLongword;
			hdr_abracadabra2 := r.ReadLongword;
		end;

		num_locators := r.ReadWord;		
		header_unk1 := r.ReadLongint;
		hdr_frame_count := r.ReadWord;
		
		// Redux
		if version >= 15 then
			r.Read(position_offset, Sizeof(TVec3));
		// Exodus
		if version >= 19 then
			r.Read(rotation_offset, Sizeof(TVec3));
	finally
		r.Free;
	end;
	
	r := reader.OpenChunk(1);
	try
		motion_flags := r.ReadWord;
		speed := r.ReadSingle;
		accrue := r.ReadSingle;
		falloff := r.ReadSingle;
		frame_count := r.ReadLongword;
		if frame_count <> hdr_frame_count then
			WriteLn('Warning: motion frame_count <> hdr_frame_count');
		jump_frame := r.ReadWord;
		land_frame := r.ReadWord;
		if version >= 19 then
		begin
			jump_frame2 := r.ReadWord;
			land_frame2 := r.ReadWord;
			// should we set these two to jump_frame1/land_frame1 if version is less than 19?
		end;
		if version >= 16 then
			r.Read(hq_bones, 32)
		else
			r.Read(hq_bones, 16);
		m_motions_size := r.ReadLongword;
		m_curves_offset := r.ReadLongword;
		if version >= 16 then
			r.Read(affected_bones, 32)
		else
			r.Read(affected_bones, 16);
	finally
		r.Free;
	end;
	
	r := reader.OpenChunk(9);
	try
		if version >= 16 then
		begin
			r.Read(affected_bones2, 32);
			
			num_locators2 := r.ReadWord;
			num_xforms := r.ReadWord;
			if r.ReadLongword <> r.size then
				WriteLn('! Invalid motion data size found');
			motions_data_unk2 := r.ReadLongword;
			motions_data_unk3 := r.ReadLongword;
		end else
		begin
			temp[0] := BEtoN(r.ReadLongword);
			temp[1] := BEtoN(r.ReadLongword);
			temp[2] := BEtoN(r.ReadLongword);
			temp[3] := BEtoN(r.ReadLongword);
			Move(temp, affected_bones2, 16);
			
			num_locators2 := BEtoN(r.ReadWord);
			num_xforms := BEtoN(r.ReadWord);
			if BEtoN(r.ReadLongword) <> r.size then
				WriteLn('! Invalid motion data size found');
			motions_data_unk2 := BEtoN(r.ReadLongword);
			motions_data_unk3 := BEtoN(r.ReadLongword);
		end;
		
		if version <= 15 then
			num_curves := (m_curves_offset - $20) div 4
		else
			num_curves := (m_curves_offset - $30) div 4;
		
		// read curve offsets
		SetLength(offsets, num_curves);
		r.Read(offsets[0], num_curves*Sizeof(Longword));
		
		// read curves
		SetLength(curves, num_curves);
		for I := 0 to num_curves - 1 do
		begin
			if offsets[I] > r.size then
			begin
				isLE := False;
				r.pos := BEtoN(offsets[I]);
			end else
			begin
				isLE := True;
				r.pos := offsets[I];
			end;
			
			header := _BEtoNl(r.ReadLongword);
			num_points := (header and $FFFF);
			num_elements := (header and $0F000000) shr 24;
			
			curves[I].curve_header_unkn := (header and $FFF00000) shr 16;
			
			curves[I].curve_type := (header shr 16) and $000F;
			
			if (curves[I].curve_type = 3) and (num_points <> 1) then 
				WriteLn('ERROR curve type 3, num_points is not 1');
			if (curves[I].curve_type = 6) and (num_points <> 2) then 
				WriteLn('ERROR curve type 6, num_points is not 2');
			
			case curves[I].curve_type of
				2: begin // constant
					curves[I].curve_constant.x := Single(_BEtoNl(r.ReadLongword));
					if num_elements >= 2 then
						curves[I].curve_constant.y := Single(_BEtoNl(r.ReadLongword));
					if num_elements >= 3 then
						curves[I].curve_constant.z := Single(_BEtoNl(r.ReadLongword));
					if num_elements >= 4 then
						curves[I].curve_constant.w := Single(_BEtoNl(r.ReadLongword));
				end;
				3: begin
					curves[I].curve_unkn_3 := _BEtoNl(r.ReadLongword);
				end;
				4, 5: begin // compressed position/rotation
					SetLength(curves[I].curve_time, num_points);
					SetLength(curves[I].curve_value, num_points*3);
					
					curves[I].curve_time_divisor := Single(_BEtoNl(r.ReadLongword));
					
					if curves[I].curve_type = 4 then
					begin
						curves[I].curve_scale.x := Single(_BEtoNl(r.ReadLongword));
						curves[I].curve_scale.y := Single(_BEtoNl(r.ReadLongword));
						curves[I].curve_scale.z := Single(_BEtoNl(r.ReadLongword));
						
						curves[I].curve_origin.x := Single(_BEtoNl(r.ReadLongword));
						curves[I].curve_origin.y := Single(_BEtoNl(r.ReadLongword));
						curves[I].curve_origin.z := Single(_BEtoNl(r.ReadLongword));
					end;
					
					for J := 0 to num_points - 1 do
						curves[I].curve_time[J] := _BEtoNw(r.ReadWord);
					
					for J := 0 to num_points - 1 do
					begin
						curves[I].curve_value[J*3  ] := _BEtoNw(r.ReadSmallint);
						curves[I].curve_value[J*3+1] := _BEtoNw(r.ReadSmallint);
						curves[I].curve_value[J*3+2] := _BEtoNw(r.ReadSmallint);
					end;		
				end;
				6: begin
					curves[I].curve_unkn_6_f1 := r.ReadSingle;
					curves[I].curve_unkn_6_v1.x := r.ReadSmallint;
					curves[I].curve_unkn_6_v1.y := r.ReadSmallint;
					curves[I].curve_unkn_6_v1.z := r.ReadSmallint;
					curves[I].curve_unkn_6_v1.w := r.ReadSmallint;
				end;				
				7: ; // empty
				else
					WriteLn('Unsupported curve type ', curves[I].curve_type);
			end;
			
			// version 19 also have one zero two byte word between curves
		end;
		
		SetLength(bone_motions, m_num_bones);
		J := 0;
		for I := 0 to m_num_bones - 1 do
		begin
			if AffectsBone(I) then
			begin
				if version >= 15 then
				begin
					bone_motions[I].id_curve_rot := J*3;
					bone_motions[I].id_curve_pos := J*3+1;
					bone_motions[I].id_curve_scale := J*3+2;
				end else
				begin
					bone_motions[I].id_curve_rot := J*2;
					bone_motions[I].id_curve_pos := J*2+1;
					bone_motions[I].id_curve_scale := -1;
				end;
				Inc(J);
			end else
			begin
				bone_motions[I].id_curve_rot := -1;
				bone_motions[I].id_curve_pos := -1;
				bone_motions[I].id_curve_scale := -1;
			end;
		end;
	finally
		r.Free;
	end;
	
	// locator names
	r := reader.OpenChunk(10);
	if Assigned(r) then
		try
			SetLength(locator_names, r.ReadWord);
			for I := 0 to Length(locator_names) - 1 do
			begin
				locator_names[I].name := r.ReadStringZ;
				locator_names[I].unk1 := r.ReadByte;
			end;

			if version >= 19 then
				if r.ReadWord <> 0 then
					WriteLN('Warning: motion locators unknown word <> 0');
		finally
			r.Free;
		end;
	
	// time tags
	r := reader.OpenChunk(7);
	if Assigned(r) then
		try
			SetLength(time_tags, r.ReadLongword);
			for I := 0 to Length(time_tags) - 1 do
			begin
				time_tags[I].time := r.ReadSingle;
				time_tags[I].tag := r.ReadStringZ;
				time_tags[I].flags := r.ReadLongword;
			end;
		finally
			r.Free;
		end;
	
	// ?????
	r := reader.OpenChunk(8);
	if Assigned(r) then
		try
			was_8 := True;
			something3_1 := r.ReadLongword;
			something3_2 := r.ReadLongword;
		finally
			r.Free;
		end;
	
	// ik-rules
	r := reader.OpenChunk(6);
	if Assigned(r) then
		try
			SetLength(ik_rules, r.ReadLongword);
			for I := 0 to Length(ik_rules) - 1 do
			begin
				r.Read(ik_rules[I].quat, Sizeof(TVec4));
				r.Read(ik_rules[I].pos, Sizeof(TVec3));
				ik_rules[I].start := r.ReadSingle;
				ik_rules[I].peak := r.ReadSingle;
				ik_rules[I].tail := r.ReadSingle;
				ik_rules[I]._end := r.ReadSingle;
				ik_rules[I].height := r.ReadSingle;
				ik_rules[I].radius := r.ReadSingle;
				ik_rules[I].floor := r.ReadSingle;
				ik_rules[I].chain := r.ReadStringZ;
				ik_rules[I]._type := r.ReadByte;
				ik_rules[I].pad := r.ReadWord;
				ik_rules[I].frame := r.ReadLongword;
				ik_rules[I].locator := r.ReadStringZ;
				ik_rules[I].attach_type := r.ReadStringZ;
			end;
		finally
			r.Free;
		end;
	
end;

procedure T4AMotionLL.Save(const fn : String); overload;
var
	w : TMemoryWriter;
begin
	w := TMemoryWriter.Create;
	try
		Save(w);
		w.SaveTo(fn);
	finally
		w.Free;
	end;
end;

procedure T4AMotionLL.Save(w : TMemoryWriter); overload;
var
	I : Longint;
	
	m : TMemoryWriter;
	curve_offsets : array of Longword;
	
	curves_offset : Longword;
	motion_data_sz : Longword;

	num_affected_bones : Longint;
begin
	if version < 16 then
		raise Exception.Create('saving motions with version < 16 is not implemented');
		
	// make curves data first
	m := TMemoryWriter.Create;

	if version >= 20 then
	begin
		SaveCurvesRaw(m);
	end else
	begin
		SetLength(curve_offsets, Length(curves));
		SaveCurvesCompressed(m, curve_offsets);
	end;

	// write header 1...
	w.OpenChunk(0);
	w.WriteLongword(version);
	w.WriteLongword(skeleton_crc);
	w.WriteWord(m_num_bones);

	if version >= 20 then
	begin
		w.WriteByte(hdr_compression);
		w.WriteByte(hdr_unkn0);
		w.WriteLongword(hdr_abracadabra0);
		w.WriteLongword(hdr_abracadabra1);
		w.WriteLongword(hdr_abracadabra2);
	end;

	w.WriteWord(num_locators);
	w.WriteLongword(header_unk1);
	w.WriteWord(hdr_frame_count);

	num_affected_bones := 0;
	for I := 0 to Longint(m_num_bones) - 1 do
		if AffectsBone(I) then
			Inc(num_affected_bones);

	// Redux
	if version >= 15 then
		w.Write(position_offset, Sizeof(TVec3));
	// Exodus
	if version >= 19 then
		w.Write(rotation_offset, Sizeof(TVec3));
	w.CloseChunk;
	
	// calculate curves offset and motion data size
	if version >= 20 then
		curves_offset := (num_affected_bones+num_xforms)*3*Sizeof(Longword) + $30 // due to locators forcibly removed
	else if version >= 16 then
		curves_offset := Length(curves)*Sizeof(Longword) + $30
	else
		curves_offset := Length(curves)*Sizeof(Longword) + $20;	

	if version >= 20 then
		motion_data_sz := $30 + m.size // there's no offsets in version 20, so, obivously, they don't contribute to size, but we still need curve_offset :\
	else
		motion_data_sz := curves_offset + m.size;
	
	// write header 2....
	w.OpenChunk(1);
	w.WriteWord(motion_flags);
	w.WriteSingle(speed);
	w.WriteSingle(accrue);
	w.WriteSingle(falloff);
	w.WriteLongword(frame_count);
	w.WriteWord(jump_frame);
	w.WriteWord(land_frame);
	if version >= 19 then
	begin
		w.WriteWord(jump_frame2);
		w.WriteWord(land_frame2);	
	end;

	if version >= 16 then
		w.Write(hq_bones, 32)
	else
		w.Write(hq_bones, 16);

	w.WriteLongword(motion_data_sz);
	w.WriteLongword(curves_offset); // this value make no sense for version 20, but engine still calculates it as in previous ver and checks against calculated value

	if version >= 16 then
		w.Write(affected_bones, 32)
	else
		w.Write(affected_bones, 16);

	w.CloseChunk;
	
	// actual motions
	w.OpenChunk(9);
	if version >= 20 then
	begin
		w.Write(affected_bones2, 32);
		w.WriteWord(0); // num locators, not supported
		w.WriteWord(num_xforms); // num xforms
		
		w.WriteLongword(motion_data_sz);

		w.WriteByte(1); // compression type; 1 = Raw; 2 = ACL
		for I := 0 to 6 do
			w.WriteByte($CD); // align to 8

		// write motion data
		w.Write(m.data[0], m.size);
		m.Free;
	end else
	if version >= 16 then 
	begin
		w.Write(affected_bones2, 32);
		w.WriteWord(num_locators2);
		w.WriteWord(num_xforms);
		w.WriteLongword(motion_data_sz);
		w.WriteLongword(motions_data_unk2);
		w.WriteLongword(motions_data_unk3);
		
		// write offsets
		for I := 0 to Length(curves) - 1 do
			w.WriteLongword(curve_offsets[I] + curves_offset);
		
		// write motion data
		w.Write(m.data[0], m.size);
		m.Free;
	end else
	begin
		// this stuff should be written in big-endian (partially in case or redux) ...
		// NOT IMPLEMENTED
		w.Write(affected_bones2, 16);
	end;
	w.CloseChunk;
	
	// locator names
	if (Length(locator_names) > 0) and (version < 20) { force remove locators for version 20 } then
	begin
		w.OpenChunk(10);
		w.WriteWord(Length(locator_names));
			for I := 0 to Length(locator_names) - 1 do
			begin
				w.WriteStringZ(locator_names[I].name);
				w.WriteByte(locator_names[I].unk1);
			end;
			
		// exodus mots also have one zero word at end of that chunk !
		if version >= 19 then
			w.WriteWord(0);
			
		w.CloseChunk;
	end;
	
	// time tags
	if Length(time_tags) > 0 then
	begin
		w.OpenChunk(7);
		w.WriteLongword(Length(time_tags));
		for I := 0 to Length(time_tags) - 1 do
		begin
			w.WriteSingle(time_tags[I].time);
			w.WriteStringZ(time_tags[I].tag);
			w.WriteLongword(time_tags[I].flags);
		end;
		w.CloseChunk;
	end;
	
	// ?????
	// вот и чё я должен делать с этим?
	if was_8 then
	begin
		w.OpenChunk(8);
		w.WriteLongword(something3_1);
		w.WriteLongword(something3_2);
		w.CloseChunk;		
	end;
	
	// ik-rules
	if Length(ik_rules) > 0 then
	begin
		w.OpenChunk(6);
		w.WriteLongword(Length(ik_rules));
		for I := 0 to Length(ik_rules) - 1 do
		begin
			w.Write(ik_rules[I].quat, Sizeof(TVec4));
			w.Write(ik_rules[I].pos, Sizeof(TVec3));
			w.WriteSingle(ik_rules[I].start);
			w.WriteSingle(ik_rules[I].peak);
			w.WriteSingle(ik_rules[I].tail);
			w.WriteSingle(ik_rules[I]._end);
			w.WriteSingle(ik_rules[I].height);
			w.WriteSingle(ik_rules[I].radius);
			w.WriteSingle(ik_rules[I].floor);
			w.WriteStringZ(ik_rules[I].chain);
			w.WriteByte(ik_rules[I]._type);
			w.WriteWord(ik_rules[I].pad);
			w.WriteLongword(ik_rules[I].frame);
			w.WriteStringZ(ik_rules[I].locator);
			w.WriteStringZ(ik_rules[I].attach_type);
		end;
		w.CloseChunk;
	end;
end;

procedure T4AMotionLL.SaveCurvesCompressed(m : TMemoryWriter; var curve_offsets : array of Longword);
var
	I, J : Longint;
	num_points : Longint;
	num_elements : Longint;
	header : Longword;
begin
	for I := 0 to Length(curves) - 1 do
	begin
		// remember offset of this curve
		curve_offsets[I] := m.pos;
		
		// write curve
		case curves[I].curve_type of
			2, 3: num_points := 1;
			4, 5: num_points := Length(curves[I].curve_value) div 3;
			6: num_points := 2;
			7: num_points := 0;
		end;
		num_elements := (curves[I].curve_header_unkn and $0F00) shr 8;
		
		header := 
			(curves[I].curve_header_unkn shl 16) or
			((curves[I].curve_type and $0F) shl 16) or 
			(num_points and $FFFF);
			
		m.WriteLongword(header);
		
		case curves[I].curve_type of
			2: begin
				m.WriteSingle(curves[I].curve_constant.x);
				if num_elements > 1 then
					m.WriteSingle(curves[I].curve_constant.y);
				if num_elements > 2 then
					m.WriteSingle(curves[I].curve_constant.z);
				if num_elements > 3 then
					m.WriteSingle(curves[I].curve_constant.w);
			end;
			3: begin
				m.WriteLongword(curves[I].curve_unkn_3);
			end;
			4, 5: begin
				m.WriteSingle(curves[I].curve_time_divisor);
				
				if curves[I].curve_type = 4 then
				begin
					m.WriteSingle(curves[I].curve_scale.x);
					m.WriteSingle(curves[I].curve_scale.y);
					m.WriteSingle(curves[I].curve_scale.z);
					
					m.WriteSingle(curves[I].curve_origin.x);
					m.WriteSingle(curves[I].curve_origin.y);
					m.WriteSingle(curves[I].curve_origin.z);
				end;
				
				for J := 0 to num_points - 1 do
					m.WriteWord(curves[I].curve_time[J]);
					
				for J := 0 to num_points - 1 do
				begin
					m.WriteSmallint(curves[I].curve_value[J*3  ]);
					m.WriteSmallint(curves[I].curve_value[J*3+1]);
					m.WriteSmallint(curves[I].curve_value[J*3+2]);
				end;
			end;
			6: begin
				m.WriteSingle(curves[I].curve_unkn_6_f1);
				m.WriteSmallint(curves[I].curve_unkn_6_v1.x);
				m.WriteSmallint(curves[I].curve_unkn_6_v1.y);
				m.WriteSmallint(curves[I].curve_unkn_6_v1.z);
				m.WriteSmallint(curves[I].curve_unkn_6_v1.w);
			end;
			7: ; // empty
		end;
	end;
end;

procedure T4AMotionLL.SaveCurvesRaw(w : TMemoryWriter);
var
	I, J : Longint;
	f_mul : Longint; // frame multiplier
	num_affected_bones : Longint;
	sz : Longint;

	p : TVec3;
	q : TVec4;
	s : TVec3;

	id_r, id_p, id_s : Longint;
begin
	f_mul := hdr_abracadabra0 div 30;

	num_affected_bones := 0;
	for I := 0 to Longint(m_num_bones) - 1 do
	begin
		if AffectsBone(I) then
			Inc(num_affected_bones);
	end;

	sz := $10 + (num_affected_bones+num_xforms) * frame_count * f_mul * $30;

	// summary size of motion
	w.WriteLongword(sz);
	// num keyframes
	w.WriteLongword(frame_count * f_mul); // * (hdr_abracadabra0 div 30) ?
	// length of animation in seconds, float (acually depends on abracadabra0)
	w.WriteSingle(LengthSec);
	// alignment shit
	w.WriteLongword($CDCDCDCD);

	for I := 0 to Longint(m_num_bones) - 1 do
	begin
		if AffectsBone(I) then
		begin
			for J := 0 to (frame_count * f_mul) - 1 do
			begin
				GetTransformBone(I, J / ((frame_count * f_mul) - 1), p, q, s);
				
				// position
				w.Write(p, Sizeof(TVec3));
				// alignment shit
				w.WriteLongword($CDCDCDCD);

				// rotation
				w.Write(q, Sizeof(TVec4));

				// scale or something
				w.Write(s, Sizeof(TVec3));
				// alignment shit
				w.WriteLongword($CDCDCDCD);
			end;
		end;
	end;

	// matrices which is not bone nor locator
	// transforms whole object I guess
	for I := 0 to Longint(num_xforms) - 1 do
	begin
		id_r := num_affected_bones * 3 + num_locators2 * 4 + I*3;
		id_p := num_affected_bones * 3 + num_locators2 * 4 + I*3 + 1;
		id_s := num_affected_bones * 3 + num_locators2 * 4 + I*3 + 2;

		for J := 0 to (frame_count * f_mul) - 1 do
		begin
			GetTransformCurve(id_r, id_p, id_s, J / ((frame_count * f_mul) - 1), p, q, s);
				
			// position
			w.Write(p, Sizeof(TVec3));
			// alignment shit
			w.WriteLongword($CDCDCDCD);

			// rotation
			w.Write(q, Sizeof(TVec4));

			// scale or something
			w.Write(s, Sizeof(TVec3));
			// alignment shit
			w.WriteLongword($CDCDCDCD);
		end;		
	end;
end;

function T4AMotionLL.LengthSec : Single;
begin
	LengthSec := frame_count / 30;
end;

function T4AMotionLL.AffectsBone(id : Longint) : Boolean;
begin
	Result := (Byte(id) in affected_bones) and (Byte(id) in affected_bones2);
end;

const
	tQuat = 1;
	tPos = 2;
	tScale = 4;

function T4AMotionLL.GetTransformCurve(id_r, id_p, id_s : Longint; t : Single; out p : TVec3; out q : TVec4; out s : TVec3) : Byte; 
const
	quat_identity : TVec4 = (X:0; Y:0; Z:0; W:1);
	pos_identity : TVec3 = (X:0; Y:0; Z:0);
var
	I, L : Longint;
	
	q1 : TVec4;
	q2 : TVec4;
	
	p1 : TVec3;
	p2 : TVec3;
	p3 : TVec3;
	
	aq : TaiQuaternion absolute q;
	aq1 : TaiQuaternion absolute q1;
	aq2 : TaiQuaternion absolute q2;
	
	time1 : Single;
	time2 : Single;
	factor : Single;
	
	//matrix : TMatrix;
	
	function _Sqrt(v : Single) : Single;
	begin
		if v < 0.0001 then
			_Sqrt := 0.0
		else
			_Sqrt := Sqrt(v)
	end;
	
	function UnpackQuat(x, y, z : Smallint) : TVec4;
	var	
		rm : Longint;
		scale : Single;
		values : array[0..3] of Single;
	begin
		rm := ((x and $01) shl 1) or (y and $01);
		x := x and $FFFE;
		y := y and $FFFE;
		
		scale := (Sqrt(2.0) / 2.0) / 32767;
		
		values[0] := x * scale;
		values[1] := y * scale;
		values[2] := z * scale;
		values[3] := _Sqrt(1.0 - values[0]*values[0] - values[1]*values[1] - values[2]*values[2]);
		if (z and 1) <> 0 then
			values[3] := -values[3];
		
		case rm of
			0: begin
				Result.x := values[3];
				Result.y := values[0];
				Result.z := values[1];
				Result.w := values[2];
			end;
			1: begin
				Result.x := values[0];
				Result.y := values[3];
				Result.z := values[1];
				Result.w := values[2];
			end;
			2: begin
				Result.x := values[0];
				Result.y := values[1];
				Result.z := values[3];
				Result.w := values[2];
			end;
			3: begin
				Result.x := values[0];
				Result.y := values[1];
				Result.z := values[2];
				Result.w := values[3];
			end;
		end;
	end;
	
begin
	Result := tQuat or tPos or tScale;

	if id_r <> -1 then
	begin
		case curves[id_r].curve_type of
			2: q := curves[id_r].curve_constant;
			5: begin
				I := 0;
				L := Length(curves[id_r].curve_time);
				while (I < L) and ((curves[id_r].curve_time[I] / 65535) < t) do
					Inc(I);
					
				if I = 0 then
				begin
					// copy first frame
					q := UnpackQuat(
						curves[id_r].curve_value[0], 
						curves[id_r].curve_value[1], 
						curves[id_r].curve_value[2]
					);
				end else
				if I = L then
				begin
					// copy last frame
					q := UnpackQuat(
						curves[id_r].curve_value[(L-1)*3+0], 
						curves[id_r].curve_value[(L-1)*3+1], 
						curves[id_r].curve_value[(L-1)*3+2]
					);
				end else
				begin
					// interpolate
					q1 := UnpackQuat(
						curves[id_r].curve_value[(I-1)*3+0], 
						curves[id_r].curve_value[(I-1)*3+1], 
						curves[id_r].curve_value[(I-1)*3+2]
					);
					
					q2 := UnpackQuat(
						curves[id_r].curve_value[(I)*3+0], 
						curves[id_r].curve_value[(I)*3+1], 
						curves[id_r].curve_value[(I)*3+2]
					);
					
					time1 := curves[id_r].curve_time[I-1] / 65535;
					time2 := curves[id_r].curve_time[I  ] / 65535;
					
					aq := aiQuaternion.Interpolate(aq1, aq2, (t - time1) / (time2 - time1));
					Normalize(q);
				end;
			end;
			7: begin
				Result := Result xor tQuat;
				q := quat_identity;
			end;
		end;
	end else
	begin
		Result := Result xor tQuat;
		q := quat_identity;
	end;
	
	if id_p <> -1 then
	begin
		case curves[id_p].curve_type of
			2: begin
				p.x := curves[id_p].curve_constant.x;
				p.y := curves[id_p].curve_constant.y;
				p.z := curves[id_p].curve_constant.z;
			end;
			4: begin
				I := 0;
				L := Length(curves[id_p].curve_time);
				while (I < L) and ((curves[id_p].curve_time[I] / 65535) < t) do
					Inc(I);
					
				if I = 0 then
				begin
					// copy first frame
					p.x := curves[id_p].curve_origin.x + Word(curves[id_p].curve_value[0]) * curves[id_p].curve_scale.x;
					p.y := curves[id_p].curve_origin.y + Word(curves[id_p].curve_value[1]) * curves[id_p].curve_scale.y;
					p.z := curves[id_p].curve_origin.z + Word(curves[id_p].curve_value[2]) * curves[id_p].curve_scale.z;
				end else
				if I = L then
				begin
					// copy last frame
					p.x := curves[id_p].curve_origin.x + Word(curves[id_p].curve_value[(L-1)*3  ]) * curves[id_p].curve_scale.x;
					p.y := curves[id_p].curve_origin.y + Word(curves[id_p].curve_value[(L-1)*3+1]) * curves[id_p].curve_scale.y;
					p.z := curves[id_p].curve_origin.z + Word(curves[id_p].curve_value[(L-1)*3+2]) * curves[id_p].curve_scale.z;
				end else
				begin
					// interpolate
					p1.x := Word(curves[id_p].curve_value[(I-1)*3  ]);
					p1.y := Word(curves[id_p].curve_value[(I-1)*3+1]);
					p1.z := Word(curves[id_p].curve_value[(I-1)*3+2]);
				
					p2.x := Word(curves[id_p].curve_value[I*3  ]);
					p2.y := Word(curves[id_p].curve_value[I*3+1]);
					p2.z := Word(curves[id_p].curve_value[I*3+2]);
					
					time1 := curves[id_p].curve_time[I-1] / 65535;
					time2 := curves[id_p].curve_time[I  ] / 65535;
					factor := (t - time1) / (time2 - time1);
					
					p3.x := p1.x + (p2.x - p1.x) * factor;
					p3.y := p1.y + (p2.y - p1.y) * factor;
					p3.z := p1.z + (p2.z - p1.z) * factor;
					
					p.x := curves[id_p].curve_origin.x + p3.x * curves[id_p].curve_scale.x;
					p.y := curves[id_p].curve_origin.y + p3.y * curves[id_p].curve_scale.y;
					p.z := curves[id_p].curve_origin.z + p3.z * curves[id_p].curve_scale.z;
				end;
			end;
			7: begin
				Result := Result xor tPos;
				p := pos_identity;
			end;
		end;
	end else
	begin
		Result := Result xor tPos;
		p := pos_identity;
	end;
	
	if id_s <> -1 then
	begin
		case curves[id_s].curve_type of
			2: begin
				s.x := curves[id_s].curve_constant.x;
				s.y := curves[id_s].curve_constant.y;
				s.z := curves[id_s].curve_constant.z;
			end;
			4: begin
				I := 0;
				L := Length(curves[id_s].curve_time);
				while (I < L) and ((curves[id_s].curve_time[I] / 65535) < t) do
					Inc(I);
					
				if I = 0 then
				begin
					// copy first frame
					s.x := curves[id_s].curve_origin.x + Word(curves[id_s].curve_value[0]) * curves[id_s].curve_scale.x;
					s.y := curves[id_s].curve_origin.y + Word(curves[id_s].curve_value[1]) * curves[id_s].curve_scale.y;
					s.z := curves[id_s].curve_origin.z + Word(curves[id_s].curve_value[2]) * curves[id_s].curve_scale.z;
				end else
				if I = L then
				begin
					// copy last frame
					s.x := curves[id_s].curve_origin.x + Word(curves[id_s].curve_value[(L-1)*3  ]) * curves[id_s].curve_scale.x;
					s.y := curves[id_s].curve_origin.y + Word(curves[id_s].curve_value[(L-1)*3+1]) * curves[id_s].curve_scale.y;
					s.z := curves[id_s].curve_origin.z + Word(curves[id_s].curve_value[(L-1)*3+2]) * curves[id_s].curve_scale.z;
				end else
				begin
					// interpolate
					p1.x := Word(curves[id_s].curve_value[(I-1)*3  ]);
					p1.y := Word(curves[id_s].curve_value[(I-1)*3+1]);
					p1.z := Word(curves[id_s].curve_value[(I-1)*3+2]);
				
					p2.x := Word(curves[id_s].curve_value[I*3  ]);
					p2.y := Word(curves[id_s].curve_value[I*3+1]);
					p2.z := Word(curves[id_s].curve_value[I*3+2]);
					
					time1 := curves[id_s].curve_time[I-1] / 65535;
					time2 := curves[id_s].curve_time[I  ] / 65535;
					factor := (t - time1) / (time2 - time1);
					
					p3.x := p1.x + (p2.x - p1.x) * factor;
					p3.y := p1.y + (p2.y - p1.y) * factor;
					p3.z := p1.z + (p2.z - p1.z) * factor;
					
					s.x := curves[id_s].curve_origin.x + p3.x * curves[id_s].curve_scale.x;
					s.y := curves[id_s].curve_origin.y + p3.y * curves[id_s].curve_scale.y;
					s.z := curves[id_s].curve_origin.z + p3.z * curves[id_s].curve_scale.z;
				end;
			end;
			7: begin
				Result := Result xor tScale;
				s:= pos_identity;
			end;
		end;
	end else
	begin
		Result := Result xor tScale;
		s:= pos_identity;
	end;
end;

procedure T4AMotionLL.GetTransformBone(id : Longint; t : Single; out p : TVec3; out q : TVec4; out s : TVec3); 
var
	id_r : Longint;
	id_p : Longint;
	id_s : Longint;

	x : Byte;
begin
	id_r := bone_motions[id].id_curve_rot;
	id_p := bone_motions[id].id_curve_pos;
	id_s := bone_motions[id].id_curve_scale;

	x := GetTransformCurve(id_r, id_p, id_s, t, p, q, s);

	if (x and tQuat) = 0 then
	begin
		if (parent_skeleton <> nil) and (id < Length(parent_skeleton.bones)) then
			q := parent_skeleton.bones[id].q
	end;

	if (x and tPos) = 0 then
	begin
		if (parent_skeleton <> nil) and (id < Length(parent_skeleton.bones)) then
			p := parent_skeleton.bones[id].position
	end;	
end;

procedure T4AMotionLL.GetTransform(id : Longint; t : Single; out m : TMatrix); 
var
	p : TVec3;
	q : TVec4;
	s : TVec3;
begin
	GetTransformBone(id, t, p, q, s);
	RotateQuaternion(m, q);
	m[4,1] := p.x; m[4,2] := p.y; m[4,3] := p.z;
	
	//Scale(matrix, s);
	//Mul44(m, matrix);
end;

end.
