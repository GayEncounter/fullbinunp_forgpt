program roxanne;
uses chunkedFile, skeleton, motion;

var
	I : Longint;
	mot : T4AMotionLL;
	skl : T4ASkeleton;
	r : TMemoryReader;
begin
	if ParamStr(1) = '-skl' then
	begin
		skl := T4ASkeleton.Create;
		
		r := TMemoryReader.CreateFromFile(ParamStr(2));
		skl.LoadKonfig(r);
		r.Free;
		
		I := 3;
	end else
	begin
		I := 1;
		skl := nil;
	end;
	
	mot := T4AMotionLL.CreateAndLoad(ParamStr(I));
	mot.parent_skeleton := skl;
	mot.version := 20;

	if (skl <> nil) and (Longword(mot.skeleton_crc) <> skl.crc) then
		WriteLn('Warning: skeleton crc doesn''t match!');

	mot.hdr_compression := 1;
	mot.hdr_unkn0 := 4;
	mot.hdr_abracadabra0 := 30;
	mot.hdr_abracadabra1 := $38D1B717;
	mot.hdr_abracadabra2 := $3CF5C28F;

	if ParamCount > I then
		mot.Save(ParamStr(I+1))
	else
		mot.Save('proba.m2');
		
	mot.Free;
	skl.Free;
end.