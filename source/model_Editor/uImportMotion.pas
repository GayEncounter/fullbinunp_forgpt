unit uImportMotion;

interface
uses chunkedFile, skeleton;

function ImportMotion(const path : String; skeleton : T4ASkeleton) : TMemoryWriter;

implementation

uses sysutils, uCrc, vmath, Iup, uImport, motion, OGF,
     assimp, aiAnimation, aiTypes, aiVector3D, aiQuaternion, aiScene;

procedure QNormalize(var q : TAIQuaternion);
var
	m : Single;
begin
	m := Sqrt(q.w*q.w + q.x*q.x + q.z*q.z + q.y*q.y);
	q.w := q.w / m;
	q.x := q.x / m;
	q.y := q.y / m;
	q.z := q.z / m;
end;

function FindBone(a : PAIAnimation; const name : String) : PAINodeAnim;
var
	I : Longint;
begin
	I := 0;
	while (I < a.mNumChannels) and (name <> aiStringToDelphiString(a.mChannels[I].mNodeName)) do
		Inc(I);
		
	if I >= a.mNumChannels then
		raise Exception.Create('Bone ''' + name + ''' not found in animation ''' + aiStringToDelphiString(a.mName) + '''!');
		
	Result := a.mChannels[I];
end;

function EvalTranslation(n : PAINodeAnim; time : Double) : TAIVector3D;
var
	J : Longint;
	t1, t2 : TAIVectorKey;
begin
	J := 0;
	while (J < n.mNumPositionKeys) and (n.mPositionKeys[J].mTime < time) do
		Inc(J);
		
	if J = 0 then // just copy first key
		Result := n.mPositionKeys[0].mValue
	else if J >= n.mNumPositionKeys then // just copy last key
		Result := n.mPositionKeys[n.mNumPositionKeys-1].mValue
	else begin // interpolate
		t1 := n.mPositionKeys[J-1];
		t2 := n.mPositionKeys[J];
		
		Result := aiVector3D.Interpolate(t1.mValue, t2.mValue, (time - t1.mTime) / (t2.mTime - t1.mTime));
	end;
end;

function EvalRotation(n : PAINodeAnim; time : Double) : TAIQuaternion;
var
	J : Longint;
	k1, k2 : TAIQuatKey;
	q : TAIQuaternion;
begin
	J := 0;
	while (J < n.mNumRotationKeys) and (n.mRotationKeys[J].mTime < time) do
		Inc(J);
		
	if J = 0 then // just copy first key
		q := n.mRotationKeys[0].mValue
	else if J >= n.mNumPositionKeys then // just copy last key
		q := n.mRotationKeys[n.mNumRotationKeys-1].mValue
	else begin // interpolate
		k1 := n.mRotationKeys[J-1];
		k2 := n.mRotationKeys[J];
		
		QNormalize(k1.mValue);
		QNormalize(k2.mValue);
		
		q := aiQuaternion.Interpolate(k1.mValue, k2.mValue, (time - k1.mTime) / (k2.mTime - k1.mTime));
	end;
	
	// normalize
	QNormalize(q);
	
	Result := q;
end;

procedure PackRotation(q : TAIQuaternion; var v1, v2, v3 : Smallint; flags : Byte);
var
	I, Largest : Byte;
	quat : array[1..4] of Single;
	scale : Single;
begin
	if (flags and $20) <> 0 then
	begin
	
		quat[1] := q.x;
		quat[2] := q.y;
		quat[3] := q.z;
		quat[4] := q.w;
	
		Largest := 1;
		for I := 2 to 4 do
			if Abs(quat[I]) > Abs(quat[Largest]) then
				Largest := I;
				
		if quat[Largest] < 0.0 then
			for I := 1 to 4 do
				quat[I] := -quat[I];
				
		scale := Sqrt(2.0) * 32767;
		
		case largest of
			1: begin
				v1 := Trunc(quat[2] * scale);
				v2 := Trunc(quat[3] * scale);
				v3 := Trunc(quat[4] * scale);
			end;
			2: begin
				v1 := Trunc(quat[1] * scale);
				v2 := Trunc(quat[3] * scale);
				v3 := Trunc(quat[4] * scale);
			end;
			3: begin
				v1 := Trunc(quat[1] * scale);
				v2 := Trunc(quat[2] * scale);
				v3 := Trunc(quat[4] * scale);
			end;
			4: begin
				v1 := Trunc(quat[1] * scale);
				v2 := Trunc(quat[2] * scale);
				v3 := Trunc(quat[3] * scale);
			end;
		end;
		
		Dec(Largest);
			
		v1 := (v1 and $FFFE) or ((Largest and $02) shr 1);
		v2 := (v2 and $FFFE) or (Largest and $01);
		
	end else
	begin
	
		if q.w < 0.0 then
		begin
			v1 := Trunc(-q.x * 32767);
			v2 := Trunc(-q.y * 32767);
			v3 := Trunc(-q.z * 32767);
		end else
		begin
			v1 := Trunc(q.x * 32767);
			v2 := Trunc(q.y * 32767);
			v3 := Trunc(q.z * 32767);
		end;
		
	end;
end;

function FromAISceneToMotion(scene : PAIScene; motion_id : Longint; cflags : TConvFlags; s : T4ASkeleton) : TMemoryWriter;
var
	m : T4AMotion;
	motion : PAIAnimation;
	
	flags : Byte;
	
	index : Byte;
	
	speed : Single;
	accrue : Single;
	falloff : Single;
	
	bone : PAINodeAnim;
	useRotation, usePosition : Boolean;
	
	I, J : Longint;
	
	q : TAIQuaternion;
	t : TAIVector3D;
	
	t_keys : array of TAIVector3D;
	t_origin : TVec3;
	t_scale : TVec3;
	
	vec, half_size : TVec3;
	bb : TAABB;
	
	x, y, z : Smallint;
	
	w : TMemoryWriter;
begin
	motion := scene.mAnimations[motion_id];

	WriteLN('mDuration = ', motion.mDuration);
	WriteLN('mTicksPerSecond = ', motion.mTicksPerSecond);
	
	useRotation := True;
	usePosition := True;
	speed := 1.0;
	accrue := 1.0;
	falloff := 1.0;
	
	if 0 = IupGetParam(
		'Options', nil, nil, 
		'Use rotation: %b'#10'Use position: %b'#10'Speed: %r'#10'Accrue(?): %r'#10'Falloff(?): %r'#10,
		@useRotation, @usePosition, @speed, @accrue, @falloff)
	then begin
		Result := nil;
		Exit;
	end;
	
	m := T4AMotion.Create;
	
	m.version := 8;
	m.skeleton_crc := s.crc;
	
	SetLength(m.data, Length(s.bones));
	for index := 0 to Length(s.bones)-1 do
		Include(m.affected_bones, index);
		
	m.unk1 := 0;
	m.speed := speed;
	m.accrue := accrue;
	m.falloff := falloff;
	m.frame_count := Trunc(motion.mDuration / motion.mTicksPerSecond * 30.0); // force 30 FPS
	m.unk2 := 0;
	for index := 0 to 255 do
		Include(m.unk3, index);
	
	
	for I := 0 to Length(s.bones)-1 do
	begin
		flags := 0;
		if usePosition then
			flags := flags or $01;
		if useRotation then
			flags := flags or $02;
			
		// quaternion swizzle
		flags := flags or $20;
			
		m.data[I].flags := flags;
		
		bone := FindBone(motion, s.bones[I].name);
		
		// calculate rotation keys
		if useRotation then
		begin
			SetLength(m.data[I].rotation, m.frame_count*3);
		
			for J := 0 to m.frame_count-1 do
			begin
				q := EvalRotation(bone, J / (m.frame_count-1) * motion.mDuration);
			
				if cfMirrorZ in cflags then
					q.z := -q.z;
					
				PackRotation(q, x, y, z, flags);
				
				m.data[i].rotation[J*3+0] := x;
				m.data[i].rotation[J*3+1] := y;
				m.data[i].rotation[J*3+2] := z;
			end;
		end else
		begin
			q := EvalRotation(bone, 0.0);
		
			if cfMirrorZ in cflags then
				q.z := -q.z;
				
			PackRotation(q, x, y, z, flags);
			
			SetLength(m.data[I].rotation, 3);
			m.data[i].rotation[0] := x;
			m.data[i].rotation[1] := y;
			m.data[i].rotation[2] := z;
		end;
		
		if usePosition then
		begin
			// calculate position keys & bounding box
			SetLength(t_keys, m.frame_count);
			for J := 0 to m.frame_count-1 do
			begin
				t_keys[J] := EvalTranslation(bone, J / (m.frame_count-1) * motion.mDuration);
				
				if cfMirrorZ in cflags then
					t_keys[J].z := -t_keys[J].z;
				
				Move(t_keys[J], vec, Sizeof(TVec3));
				if J = 0 then
				begin
					bb.min := vec;
					bb.max := vec;
				end else
					AABBMergePoint(bb, vec);	
			end;
			
			half_size.x := (bb.max.x - bb.min.x) / 2;
			half_size.y := (bb.max.y - bb.min.y) / 2;
			half_size.z := (bb.max.z - bb.min.z) / 2;
			
			AABBCenter(t_origin, bb);
			t_scale.x := 1 / (32767 / half_size.x);
			t_scale.y := 1 / (32767 / half_size.y);
			t_scale.z := 1 / (32767 / half_size.z);
			
			// write position data
			SetLength(m.data[I].position, m.frame_count*3);
			
			for J := 0 to m.frame_count-1 do
			begin
				t := t_keys[J];
				
				x := Trunc((t.x - t_origin.x) / t_scale.x);
				y := Trunc((t.y - t_origin.y) / t_scale.y);
				z := Trunc((t.z - t_origin.z) / t_scale.z);
				
				m.data[I].position[J*3+0] := x;
				m.data[I].position[J*3+1] := y;
				m.data[I].position[J*3+2] := z;
			end;	
			
			m.data[I].position_scale := t_scale;
			m.data[I].position_origin := t_origin;
		end else
		begin
			t := EvalTranslation(bone, 0.0);
			Move(t, m.data[I].position_origin, Sizeof(TVec3));
		end;
		
	end;
	
	w := TMemoryWriter.Create;
	m.Save(w);
	m.Free;
	
	Result := w;
end;

function FromOMFFileToMotion(omf : TOMFFile; id : Longint; s : T4ASkeleton) : TMemoryWriter;
var
	m : T4AMotion;
	
	flags : Byte;
	index : Byte;
	
	speed : Single;
	accrue : Single;
	falloff : Single;
	rotateX90deg : Longint;
	
	I, J : Longint;
	
	data_idx : Longint;
	bid : Longint;
	part_id : Longint;
	q1, q2 : TVec4;
	
	w : TMemoryWriter;
	
	function _FindBone(const name : String) : Longint;
	var I, J : Longint;
	begin
		Result := -1;
		for I := 0 to Length(omf.bone_parts)-1 do
			for J := 0 to Length(omf.bone_parts[I].bone_names)-1 do
				if omf.bone_parts[I].bone_names[J] = name then
				begin
					Result := omf.bone_parts[I].bone_ids[J];
					Exit;
				end;
	end;
	
begin
	speed := omf.motions_params[id].speed;
	accrue := omf.motions_params[id].accrue;
	falloff := omf.motions_params[id].falloff;
	rotateX90deg := 0;
	
	if 0 = IupGetParam(
		'Options', nil, nil, 
		'Speed: %r'#10'Accrue(?): %r'#10'Falloff(?): %r'#10'Rotate by X 90 deg.: %b'#10,
		@speed, @accrue, @falloff, @rotateX90deg)
	then begin
		Result := nil;
		Exit;
	end;
	
	data_idx := omf.motions_params[id].data_idx;
	
	m := T4AMotion.Create;
	
	m.version := 8;
	m.skeleton_crc := s.crc;
	SetLength(m.data, Length(s.bones));
	
	part_id := omf.motions_params[id].part_id;
	if part_id = 65535 then
	begin
		for index := 0 to Length(s.bones)-1 do
			Include(m.affected_bones, index);
	end else
	begin
		for I := 0 to Length(omf.bone_parts[part_id].bone_names)-1 do
		begin
			bid := s.GetBoneID(omf.bone_parts[part_id].bone_names[I]);
			if bid = -1 then
				raise Exception.Create('Can''t find bone '+omf.bone_parts[part_id].bone_names[I]+' in skeleton');
				
			Include(m.affected_bones, Byte(bid));
		end;
	end;
		
	m.unk1 := 0;
	m.speed := speed;
	m.accrue := accrue;
	m.falloff := falloff;
	m.frame_count := omf.motions[data_idx].frame_count;
	m.unk2 := 0;
	for index := 0 to 255 do
		Include(m.unk3, index);
	
	
	for I := 0 to Length(s.bones)-1 do
	begin
		bid := _FindBone(s.bones[I].name);
		if bid = -1 then
			raise Exception.Create('Can''t find bone '+s.bones[I].name+' in OMF file');
	
		flags := 0;
		if (omf.motions[data_idx].data[bid].flags and $01) <> 0 then
			flags := flags or $01;
		if (omf.motions[data_idx].data[bid].flags and $02) = 0 then
			flags := flags or $02;
			
		m.data[I].flags := flags;
		
		// calculate rotation keys
		if (flags and $02) <> 0 then
		begin
			SetLength(m.data[I].rotation, m.frame_count*3);
		end else
		begin
			SetLength(m.data[I].rotation, 3);
		end;
		
		for J := 0 to Length(m.data[I].rotation) div 3 - 1 do
		begin
			q1.x := omf.motions[data_idx].data[bid].rotation[J*4+0] / 32767;
			q1.y := omf.motions[data_idx].data[bid].rotation[J*4+1] / 32767;
			q1.z := omf.motions[data_idx].data[bid].rotation[J*4+2] / 32767;
			q1.w := omf.motions[data_idx].data[bid].rotation[J*4+3] / 32767;
		
			if (s.bones[I].parent_name = '') and (rotateX90deg > 0) then
			begin
				QuatRotateAxis(q2, 1, 0, 0, PI/2.0);
				QuatMul(q1, q2);
			end;
			
			if q1.w < 0.0 then
			begin
				m.data[i].rotation[J*3+0] := -Trunc(q1.x * 32767);
				m.data[i].rotation[J*3+1] := -Trunc(q1.y * 32767);
				m.data[i].rotation[J*3+2] := -Trunc(q1.z * 32767);
			end else
			begin
				m.data[i].rotation[J*3+0] := Trunc(q1.x * 32767);
				m.data[i].rotation[J*3+1] := Trunc(q1.y * 32767);
				m.data[i].rotation[J*3+2] := Trunc(q1.z * 32767);
			end;
		end;
		
		if (flags and $01) <> 0 then
		begin
			SetLength(m.data[I].position, m.frame_count*3);
			Move(omf.motions[data_idx].data[bid].position[0], m.data[I].position[0], Length(m.data[I].position)*Sizeof(Smallint));
			
			m.data[I].position_scale := omf.motions[data_idx].data[bid].position_scale;
			m.data[I].position_origin := omf.motions[data_idx].data[bid].position_origin;
		end else
		begin
			m.data[I].position_origin := omf.motions[data_idx].data[bid].position_origin;
		end;
		
	end;
	
	w := TMemoryWriter.Create;
	m.Save(w);
	m.Free;
	
	Result := w;
end;

function ImportMotion(const path : String; skeleton : T4ASkeleton) : TMemoryWriter;
var
	scene : PAIScene;
	flags : TConvFlags;
	
	omf : TOMFFile;
	r : TMemoryReader;
	
	I : Longint;
	anims : array of String;
	ret : Longint;
begin
	Result := nil;
	
	if LowerCase(ExtractFileExt(path)) = '.omf' then
	begin
		omf := TOMFFile.Create;
		
		r := TMemoryReader.CreateFromFile(path);
		omf.Load(r);
		r.Free;
		
		SetLength(anims, Length(omf.motions_params));
		for I := 0 to Length(anims)-1 do
			anims[I] := omf.motions_params[I].name;
			
		ret := iup.ListDialog('Select Animation', anims, 0, 24, 40);
		if ret <> -1 then
			Result := FromOMFFileToMotion(omf, ret, skeleton);
			
		omf.Free;
	end else
	begin
		scene := ImportAIScene(path, flags);
		try
			if scene^.mNumAnimations > 0 then
			begin	
				SetLength(anims, scene^.mNumAnimations);
				for I := 0 to Length(anims) - 1 do
					anims[I] := aiStringToDelphiString(scene^.mAnimations[I].mName);
				
				ret := iup.ListDialog('Select Animation', anims, 0, 24, 40);
				if ret <> -1 then
					Result := FromAISceneToMotion(scene, ret, flags, skeleton);
			end else
			begin
				IupMessage('Error', 'Imported scene doesn''t have any motion');
			end;
		finally
			aiReleaseImport(scene);
		end;
	end;
end;

end.