unit script_block_descs;

interface
uses Konfig, Iup, glfont;

const
	BlockSizeX = 150;
	BlockSizeY = 150;
	
	BlockMarginX = 10;
	BlockMarginY = 5;
	BlockMarginPointX = 20;

type
	TBlockDesc = class
		clsid : String;
		
		in_names : array of String;
		out_names : array of String;
		
		props : TSection;
		
		constructor Create;
		destructor Destroy; override;
	end;
	
var	
	block_descs : array of TBlockDesc;
	
procedure LoadDescs(const fn : String; tree : Ihandle);
procedure UnloadDescs;

function GetBlockDesc(const clsid : String; blk : TSection; out unique : Boolean) : TBlockDesc;
function NeedUpdateBlockDesc(const clsid : String; const name,vtype : String; data : TSection) : Boolean;

procedure CalcBlockSize(desc : TBlockDesc; font : TGLFont; out width, height : Longint);

implementation
uses sysutils, Math, uEditorUtils, framework;

var
	script_descs : TFramework;

constructor TBlockDesc.Create;
begin
	inherited Create;
	props := TSection.Create;
end;

destructor TBlockDesc.Destroy;
begin
	props.Free;
	inherited;
end;

procedure _LoadDescs(sect : TSection; tree : Ihandle; ref : Longint);
var
	I, J : Integer;
	
	s : TSection;
	desc : TBlockDesc;
	
	props : TSection;
begin
	for I := sect.ParamCount - 1 downto 0 do
	begin
		s := sect.GetParam(I) as TSection;
		
		if s.GetParam('clsid', 'stringz') <> nil then
		begin
			J := Length(block_descs);
			SetLength(block_descs, J+1);
			
			block_descs[J] := TBlockDesc.Create;
			desc := block_descs[J];
			
			desc.clsid := s.GetStr('clsid');
			
			try
				desc.in_names := SplitString(s.GetStr('in_names'), ',');
				desc.out_names := SplitString(s.GetStr('out_names'), ',');
			except 
				on E: Exception do begin
					WriteLn('Last clsid: ', desc.clsid);
					raise;
				end;
			end;
			
			props := s.GetSect('properties', False);
			if Assigned(props) then
				for J := 0 to props.items.Count - 1 do
					desc.props.items.Add(TSimpleValue(props.items[J]).Copy);
					
			IupSetStrAttributeId(tree, 'ADDLEAF', ref, PAnsiChar(s.name));
			J := IupGetInt(tree, 'LASTADDNODE');
			IupSetAttributeId(tree, 'USERDATA', J, Pointer(desc));
		end else
		begin
			IupSetStrAttributeId(tree, 'ADDBRANCH', ref, PAnsiChar(s.name));
			J := IupGetInt(tree, 'LASTADDNODE');
			IupSetAttributeId(tree, 'USERDATA', J, nil);
			
			_LoadDescs(s, tree, J);
		end;
	end;
end;
	
procedure LoadDescs(const fn : String; tree : Ihandle);
var
	I : Integer;
	k : TTextKonfig;
begin
	script_descs := TFramework.Create;
	script_descs.ExecuteScript('js\editor\' + fn + '.js');

	SetLength(block_descs, 1);
	
	// block_descs[0] is default desc
	block_descs[0] := TBlockDesc.Create;
	SetLength(block_descs[0].in_names, 5);
	for I := 0 to Length(block_descs[0].in_names) - 1 do
		block_descs[0].in_names[I] := 'in'+IntToStr(I);
	SetLength(block_descs[0].out_names, 5);
	for I := 0 to Length(block_descs[0].out_names) - 1 do
		block_descs[0].out_names[I] := 'out'+IntToStr(I);

	k := TTextKonfig.Create;
	try
		k.LoadFromFile('editor_data\' + fn + '.txt');		
		_LoadDescs(k.root, tree, -1);
	finally
		k.Free;
	end;
end;

procedure UnloadDescs;
var
	I : Integer;
begin
	for I := 0 to Length(block_descs) - 1 do
		block_descs[I].Free;
	SetLength(block_descs, 0);
	
	script_descs.Free;
end;

function GetBlockDesc(const clsid : String; blk : TSection; out unique : Boolean) : TBlockDesc;
var
	desc : TBlockDesc;
	I : Longint;	
	scdesc : TFrameworkBlockDesc;
begin
	if script_descs.GetBlockDesc(clsid, blk, scdesc) then
	begin
		unique := True;
		
		desc := TBlockDesc.Create;
		desc.clsid := clsid;
		
		desc.in_names := scdesc.in_names;
		desc.out_names := scdesc.out_names;
	end else
	begin
		unique := False;
		
		desc := block_descs[0]; // default
		for I := 0 to Length(block_descs) - 1 do
			if block_descs[I].clsid = clsid then
			begin
				desc := block_descs[I];
				Break;
			end;
	end;
	
	Result := desc;
end;

function NeedUpdateBlockDesc(const clsid : String; const name,vtype : String; data : TSection) : Boolean;
begin
	Result := script_descs.NeedUpdateBlockDesc(clsid, name, vtype, data);
end;

procedure CalcBlockSize(desc : TBlockDesc; font : TGLFont; out width, height : Longint);
const
	GAP_POINT = 10;
var
	I : Longint;
	maxln : Longint;
	w : Longint;
begin
	width := Max(BlockSizeX, font.StringWidth(desc.clsid) + BlockMarginX*2);
	
	maxln := Max(Length(desc.in_names), Length(desc.out_names));
	for I := 0 to maxln-1 do
	begin
		w := GAP_POINT + BlockMarginPointX*2;
		if I < Length(desc.in_names) then
			Inc(w, font.StringWidth(desc.in_names[I]));
		if I < Length(desc.out_names) then
			Inc(w, font.StringWidth(desc.out_names[I]));
			
		width := Max(w, width);
	end;
	
	height := BlockMarginY*2 + font.cell_height*2 + maxln * font.cell_height;
end;

end.
