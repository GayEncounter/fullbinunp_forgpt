program mshk_frede;
uses motion;

var
	mot : T4AMotionLL;
begin
	mot := T4AMotionLL.CreateAndLoad(ParamStr(1));
	mot.version := 16;
	if ParamCount > 1 then
		mot.Save(ParamStr(2))
	else
		mot.Save('proba.m2');
	mot.Free;
end.