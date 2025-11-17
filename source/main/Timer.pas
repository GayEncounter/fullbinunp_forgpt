unit Timer;

interface

type
	TTimer = object
		start_qpc : QWord;
		
		procedure Start;
		function  CurrentTime : Double;
	end;
	
implementation
uses Windows;

procedure TTimer.Start;
begin
	QueryPerformanceCounter(@start_qpc);
end;

function TTimer.CurrentTime : Double;
var
	qpf : QWord;
	qpc : QWord;
begin
	QueryPerformanceFrequency(@qpf);
	QueryPerformanceCounter(@qpc);
	CurrentTime := (qpc - start_qpc) / qpf;
end;

end.