unit uProgressDlg;

interface
uses Iup;

type
	TProgressLine = record
		bar : Ihandle;
		msg : Ihandle;
	end;
	
	TProgressDlg = class
		dlg : Ihandle;
		lines : array of TProgressLine;
		CancelAsserted : Boolean;
	
		constructor Create;
		constructor CreateMultiline(nlines : Integer);
		destructor Destroy; override;
		
		procedure SetText(line : Integer; const str : String);
		procedure SetProgress(line : Integer; progress : Integer);
		procedure SetAmount(line : Integer; amount : Integer);
		
		procedure SetTitle(const title : String);
		function GetTitle : String;
		
		procedure SetParentDialog(ih : Ihandle);
		function GetParentDialog : Ihandle;
		
		property Title : String read GetTitle write SetTitle;
		property ParentDialog : Ihandle read GetParentDialog write SetParentDialog;
	end;

implementation
uses sysutils;

function btn_cancel_cb(ih : Ihandle) : Longint; cdecl;
var
	_this : TProgressDlg;
begin
	_this := TProgressDlg(IupGetAttribute(ih, 'TProgressDlg->this'));
	_this.CancelAsserted := True;
	Result := IUP_DEFAULT;
end;

constructor TProgressDlg.Create;
begin
	CreateMultiline(1);
end;

constructor TProgressDlg.CreateMultiline(nlines : Integer);
var
	I : Integer;
	vbox : array of Ihandle;
	cancel_btn : Ihandle;
begin
	inherited Create;
	
	SetLength(lines, nlines);
	SetLength(vbox, nlines*2 + 2);
	
	for I := 0 to nlines-1 do
	begin
		lines[I].bar := IupProgressBar;
		lines[I].msg := IupLabel(nil);
		
		IupSetAttribute(lines[I].bar, 'EXPAND', 'HORIZONTAL');
		IupSetAttribute(lines[I].msg, 'EXPAND', 'HORIZONTAL');
		IupSetAttribute(lines[I].msg, 'TITLE', PAnsiChar('Label'+IntToStr(I)));
		
		vbox[I*2+0] := lines[I].msg;
		vbox[I*2+1] := lines[I].bar;
	end;
	
	cancel_btn := IupButton('Cancel', nil);
	IupSetAttribute(cancel_btn, 'PADDING', '3x3');
	IupSetCallback(cancel_btn, 'ACTION', @btn_cancel_cb);
	
	vbox[nlines*2 + 0] := cancel_btn;
	vbox[nlines*2 + 1] := nil;
	
	dlg := IupDialog(
		IupSetAttributes(
			IupVboxV(@vbox[0]),
			'MARGIN=5x5, GAP=5x5, ALIGNMENT=ACENTER'
		)
	);
	
	IupSetAttribute(dlg, 'MODAL', 'YES'); 
	IupSetAttribute(dlg, 'RESIZE', 'NO');
	IupSetAttribute(dlg, 'RASTERSIZE', '300x');
	IupSetAttribute(dlg, 'TProgressDlg->this', Pointer(self));
end;

destructor TProgressDlg.Destroy;
begin
	IupDestroy(dlg);
	inherited;
end;

procedure TProgressDlg.SetText(line : Integer; const str : String);
begin
	IupSetStrAttribute(lines[line].msg, 'TITLE', PAnsiChar(str));
end;

procedure TProgressDlg.SetProgress(line : Integer; progress : Integer);
begin
	IupSetInt(lines[line].bar, 'VALUE', progress);
end;

procedure TProgressDlg.SetAmount(line : Integer; amount : Integer);
begin
	IupSetInt(lines[line].bar, 'MAX', amount);
end;

procedure TProgressDlg.SetTitle(const title : String);
begin
	IupSetStrAttribute(self.dlg, 'TITLE', PAnsiChar(title));
end;

function TProgressDlg.GetTitle : String;
begin
	Result := IupGetAttribute(self.dlg, 'TITLE');
end;

procedure TProgressDlg.SetParentDialog(ih : Ihandle);
begin
	IupSetAttribute(self.dlg, 'PARENTDIALOG', ih);
end;

function TProgressDlg.GetParentDialog : Ihandle;
begin
	Result := IupGetAttribute(self.dlg, 'PARENTDIALOG');
end;

end.