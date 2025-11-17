program script_impl;
uses sysutils, Konfig;

var
	sect_counter : Longint;

procedure ProcessSection(sect : TSection; r : String);
var
	I : Longint;
	P : TSimpleValue;
begin
	I := 0;
	while I < sect.ParamCount do
	begin
		P := sect.GetParam(I);
		
		if P is TSection then
		begin
		
			Inc(sect_counter);
			WriteLn('var section'+IntToStr(sect_counter) + ' = ' + r + '.ReadSection("' + P.name + '")');
			ProcessSection(TSection(P), 'section'+IntToStr(sect_counter));
			
		end else
		if P is TIntegerValue then
		begin
		
			if P.vtype = 'u8' then
				WriteLn(r + '.ReadU8("' + P.name + '")')
			else if P.vtype = 's8' then
				WriteLn(r + '.ReadS8("' + P.name + '")')
			else if P.vtype = 'u16' then
				WriteLn(r + '.ReadU16("' + P.name + '")')
			else if P.vtype = 's16' then
				WriteLn(r + '.ReadS16("' + P.name + '")')
			else if P.vtype = 'u32' then
				WriteLn(r + '.ReadU32("' + P.name + '")')
			else if P.vtype = 's32' then
				WriteLn(r + '.ReadS32("' + P.name + '")')
			else if (P.vtype = 'fp32_q8') or (P.vtype = 'bool8') then
				WriteLn(r + '.ReadU8("' + P.name + '", "' + P.vtype + '")')
			else if P.vtype = 'u64' then
				WriteLn(r + '.ReadU64("' + P.name + '")')
			else if P.vtype = 's64' then
				WriteLn(r + '.ReadS64("' + P.name + '")')
			else if (P.vtype = 'entity_link, uobject_link') or
				 (P.vtype = 'cover_link, ucover_link') then
				WriteLn(r + '.ReadU16("' + P.name + '", "' + P.vtype + '")')
			else
				raise Exception.Create('define integer type ' + P.vtype + ', please');
				
		end else
		if P is TSingleValue then
		begin
		
			Write(r + '.ReadFP32("' + P.name + '"');
			if P.vtype <> 'fp32' then
				Write(', "' + P.vtype + '"');
			WriteLn(')');
			
		end else
		if P is TStringValue then
		begin
		
			WriteLn(r + '.ReadString("' + P.Name + '")');
			
		end else
		if P is TByteArrayValue then
		begin
		
			Write(r + '.ReadU8Array("' + P.name + '"');
			if P.vtype <> 'u8_array' then
				Write(', "' + P.vtype + '"');
			WriteLn(')');
			
		end else
		if P is TIntegerArrayValue then
		begin
		
			if P.vtype = 'vec4s16' then
				WriteLn(r + '.ReadVec4S16("' + P.name + '")')
			else if P.vtype = 'vec3i' then
				WriteLn(r + '.ReadVec3i("' + P.name + '")')
			else if P.vtype = 'vec4i' then
				WriteLn(r + '.ReadVec4i("' + P.name + '")')
			else if P.vtype = 'u16_array' then
				WriteLn(r + '.ReadU16Array16("' + P.name + '")')
			else if P.vtype = 'u32_array' then
				WriteLn(r + '.ReadU32Array16("' + P.name + '")')
			else
				raise Exception.Create('define integer array type ' + P.vtype + ', please');
			
		end else
		if P is TFloatArrayValue then
		begin
		
			if P.vtype = 'vec2f' then
				WriteLn(r + '.ReadVec2("' + P.name + '")')
			else if P.vtype = 'ang2f' then
				WriteLn(r + '.ReadVec2("' + P.name + '", "ang2f")')
			else if P.vtype = 'vec3f' then
				WriteLn(r + '.ReadVec3("' + P.name + '")')
			else if P.vtype = 'vec4f' then
				WriteLn(r + '.ReadVec4("' + P.name + '")')
			else if P.vtype = 'color, vec4f' then
				WriteLn(r + '.ReadVec4("' + P.name + '", "color, vec4f")')
			else if P.vtype = 'fp32_array' then
				WriteLn(r + '.ReadFP32Array("' + P.name + '")')
			else if P.vtype = 'pose, matrix' then
				WriteLn(r + '.ReadMatrix44("' + P.name + '")')
			else if P.vtype = 'pose, matrix_43T' then
				WriteLn(r + '.ReadMatrix43("' + P.name + '")')
			else
				raise Exception.Create('define integer array type ' + P.vtype + ', please');
			
		end else
		if P is TBoolValue then
		begin
		
			WriteLn(r + '.ReadBool("' + P.Name + '")');
			
		end else
		begin
		
			if (P.vtype = 'str_array') or (P.vtype = 'str_array16') or (P.vtype = 'str_array32') then
			begin
				
				if P.Vtype = 'str_array16' then
					WriteLn(r + '.ReadStrArray16("' + P.Name + '")')
				else if P.Vtype = 'str_array32' then
					WriteLn(r + '.ReadStrArray32("' + P.Name + '")')
				else
					WriteLn(r + '.ReadStrArray32("' + P.Name + '", "' + P.VType + '")');
					
				if I < sect.ParamCount-1 then
				begin
					while sect.GetParam(I+1).name = P.Name do
						Inc(I);
				end;
			
			end else
			if (I < sect.ParamCount-1) and (sect.GetParam(I+1) is TStringValue) then
			begin
				WriteLn(r + '.ReadHintStr("' + P.Name + '", "' + P.VType + '")');
				Inc(I);
			end else
				WriteLn(r + '.ReadHint("' + P.Name + '", "' + P.VType + '")');
				
		end;
		
		Inc(I);
	end;
end;

var
	TK : TTextKonfig;
begin

	if ParamCount >= 1 then
	begin
		TK := TTextKonfig.Create;
		TK.LoadFromFile(ParamStr(1));
		
		WriteLn('// Auto-generated file');
		WriteLn;
		
		ProcessSection(TK.root, 'reader');
		
		TK.Free;
	end;
	
end.