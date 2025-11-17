unit uHashTable;

interface
uses fgl;

type
	TIntArray = TFPGList<Longint>;

type
	XHashTable = class
		name : String;
		max_entries : Longint;
	
		strings : TFPGList<String>;
		strings_map : TFPGMap<Longint,TIntArray>;
		
		constructor Create(const name : String; max_entries : Longint);
		destructor Destroy; override;
		
		function Add(const str : String) : Longint;
		function Get(index : Longint) : String;
	end;

implementation
uses windows, uCrc;

constructor XHashTable.Create(const name : String; max_entries : Longint);
begin
	inherited Create;

	self.name := name;
	self.max_entries := max_entries;

	strings := TFPGList<String>.Create;
	strings_map := TFPGMap<Longint,TIntArray>.Create;
	strings_map.Sorted := True;
end;

destructor XHashTable.Destroy;
var
	I : Longint;
begin
	for I := 0 to strings_map.Count - 1 do
		strings_map.Data[I].Free;
		
	strings.Free;
	strings_map.Free;
	
	inherited Destroy;
end;

function XHashTable.Add(const str : String) : Longint;
var
	I : Longint;
	crc : Longint;
	idx, idx2 : Integer;
	arr : TIntArray;
begin
	crc := GetStringCrc(str);
	if not strings_map.Find(crc, idx) then
	begin
		idx := strings.Add(str);
		arr := TIntArray.Create;
		arr.Add(idx);
		strings_map.Add(crc, arr);
		Result := idx;
	end else
	begin
		// 1. check if string is already added
		for I := 0 to strings_map.Data[idx].Count - 1 do
		begin
			idx2 := strings_map.Data[idx][I];
			if strings[idx2] = str then
			begin
				Result := idx2;
				Exit;
			end;
		end;
		
		// 2. and if not, add it
		if strings.Count >= max_entries then
		begin
			MessageBox(HWND(0), PAnsiChar(self.name + ' overflow. Application will now exit'), nil, MB_ICONERROR);
			ExitProcess(0);
		end;
		
		idx2 := strings.Add(str);
		strings_map.Data[idx].Add(idx2);
		Result := idx2;
	end;
end;

function XHashTable.Get(index : Longint) : String;
begin
	Result := strings.Items[index];
end;

end.