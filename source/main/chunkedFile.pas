unit ChunkedFile;

interface
uses classes;

type
	TMemoryReader = class
	public
		//data : array of Byte;
		data : Pointer;
		pos, size : Longword;
	
	private	
		// for slice support
		main : TMemoryReader;
		slice_count : Longword;
	public
		constructor Create(stream : TStream); overload;
		constructor Create(const buffer; size : Longword); overload;
		constructor CreateSlice(m : TMemoryReader; at, sz : Longword);
		constructor CreateFromFile(filename : String);
		destructor Destroy; override;

		procedure Read(var Buffer; count : Longword);

		function ReadShortint : Shortint;
		function ReadByte : Byte;
		function ReadSmallint : Smallint;
		function ReadWord : Word;
		function ReadLongint : Longint;
		function ReadLongword : Longword;
		function ReadInt64 : Int64;
		function ReadQWord : QWord;

		function ReadSingle : Single;
		function ReadDouble : Double;

		function ReadStringZ : String;

		function FindChunk(id : Longword; var sz : Longword; fromhere : Boolean = False) : Boolean;
		function OpenChunk(id : Longword; fromhere : Boolean = False) : TMemoryReader; overload;
		function OpenChunk : TMemoryReader; overload; // open chunk with any id at cursor
		
		function More : Boolean;
	end;

	TMemoryWriter = class
	public
		data : array of Byte;
		pos, size, capacity : Longword;
		delta : Longword;

		chunks : array[1..32] of Longword;
		curchunk : Longword;
	public
		constructor Create;

		procedure Clear;
		procedure SaveTo(filename : String);

		procedure Write(const Buffer; count : Longword);

		procedure WriteShortint(value : Shortint);
		procedure WriteByte(value : Byte);
		procedure WriteSmallint(value : Smallint);
		procedure WriteWord(value : Word);
		procedure WriteLongint(value : Longint);
		procedure WriteLongword(value : Longword);
		procedure WriteInt64(value : Int64);
		procedure WriteQWord(value : QWord);

		procedure WriteSingle(value : Single);
		procedure WriteDouble(value : Double);

		procedure WriteString(const value : String);
		procedure WriteStringZ(const value : String);

		procedure OpenChunk(id : Longword);
		procedure CloseChunk;
	end;

implementation
uses windows, SysUtils;

// TMemoryReader implementation
constructor TMemoryReader.Create(stream : TStream);
begin
	inherited Create;
	//SetLength(data, stream.Size);
	//stream.Read(data[0], stream.Size);
	data := GetMem(stream.Size);
	stream.ReadBuffer(data^, stream.Size);

	pos := 0;
	size := stream.Size;
end;

constructor TMemoryReader.Create(const buffer; size : Cardinal);
begin
	inherited Create;
	//SetLength(data, size);
	//Move(buffer, data[0], size);
	data := GetMem(size);
	Move(buffer, data^, size);

	pos := 0;
	self.size := size;
end;

constructor TMemoryReader.CreateSlice(m : TMemoryReader; at, sz : Longword);
begin
	if at+sz > m.size then
		raise Exception.Create('at+sz > m.size');
		
	main := m;
	Inc(main.slice_count);
	
	data := main.data+at;
	pos  := 0;
	size := sz;
end;

constructor TMemoryReader.CreateFromFile(filename : String);
var
	f : TFileStream;
begin
	f := TFileStream.Create(filename, fmOpenRead);
	try
		Create(f);
	finally
		f.Free;
	end;
end;

destructor TMemoryReader.Destroy;
begin
	if slice_count > 0 then
		raise Exception.Create('TMemoryReader.Destroy: slice_count > 0');
		
	if main = nil then
		FreeMem(data)
	else
		Dec(main.slice_count);
end;

procedure TMemoryReader.Read(var Buffer; count : Longword);
begin
	if (pos > size) or (size-pos < count) then
		raise Exception.Create('size-pos < count');

	//Move(data[pos], Buffer, count);
	Move((data+pos)^, Buffer, count);
	Inc(pos, count);
end;

function TMemoryReader.ReadShortint : Shortint;
begin
	Read(Result, Sizeof(Shortint));
end;

function TMemoryReader.ReadByte : Byte;
begin
	Read(Result, Sizeof(Byte));
end;

function TMemoryReader.ReadSmallint : Smallint;
begin
	Read(Result, Sizeof(Smallint));
end;

function TMemoryReader.ReadWord : Word;
begin
	Read(Result, Sizeof(Word));
end;

function TMemoryReader.ReadLongint : Longint;
begin
	Read(Result, Sizeof(Longint));
end;

function TMemoryReader.ReadLongword : Longword;
begin
	Read(Result, Sizeof(Longword));
end;

function TMemoryReader.ReadInt64 : Int64;
begin
	Read(Result, Sizeof(Int64));
end;

function TMemoryReader.ReadQWord : QWord;
begin
	Read(Result, Sizeof(QWord));
end;

function TMemoryReader.ReadSingle : Single;
begin
	Read(Result, Sizeof(Single));
end;

function TMemoryReader.ReadDouble : Double;
begin
	Read(Result, Sizeof(Double));
end;
{
function TMemoryReader.ReadStringZ : String;
var
	c : Char;
	str : String;
begin
	str := '';
	c := Char(ReadByte);
	while c <> #0 do
	begin
		str := str + c;
		c := Char(ReadByte);
	end;
	Result := str;
end;
}
function TMemoryReader.ReadStringZ : String;
var
	str : String;
	index : Longint;
begin
	if pos > size then
		raise Exception.Create('pos > size');
		
	index := IndexChar((data+pos)^, size-pos, #0);
	if index < 0 then
		raise Exception.Create('index < 0');
		
	SetLength(str, index);
	Read(str[1], index);
	Inc(pos); // skip #0
	
	Result := str;
end;

function TMemoryReader.FindChunk(id : Longword; var sz : Longword; fromhere : Boolean) : Boolean;
var
	chkid, chksize : Longword;
begin
	if not fromhere then
		pos := 0;

	Result := False;

	repeat
		chkid := ReadLongword;
		chksize := ReadLongword;

		if chkid = id then
		begin
			sz := chksize;
			Result := True
		end else
			pos := pos + chksize;
	until Result or (pos >= size);
end;

function TMemoryReader.OpenChunk(id : Longword; fromhere : Boolean) : TMemoryReader; overload;
var
	chksz : Longword;
begin
	if FindChunk(id, chksz, fromhere) then
	begin
		//Result := TMemoryReader.Create(data[pos], chksz);
		Result := TMemoryReader.CreateSlice(self, pos, chksz);
		Inc(pos, chksz);
	end else
		Result := nil;
end;

function TMemoryReader.OpenChunk : TMemoryReader; overload;
var
	chunksize : Longword;
begin
	Inc(pos, 4); // skip id
	chunksize := ReadLongword;
	
	if size-pos < chunksize then
		raise Exception.Create('size-pos < chunksize');
		
	//Result := TMemoryReader.Create(data[pos], chunksize);
	Result := TMemoryReader.CreateSlice(self, pos, chunksize);
	Inc(pos, chunksize);
end;

function TMemoryReader.More : Boolean;
begin
	Result := (pos < size);
end;

// TMemoryWriter implementation
constructor TMemoryWriter.Create;
begin
	inherited;

	pos := 0;
	size := 0;
	capacity := 4096;
	delta := 4096;
	SetLength(data, capacity);

	curchunk := 0;
end;

procedure TMemoryWriter.Clear;
begin
	pos := 0;
	size := 0;
	curchunk := 0;
end;

procedure TMemoryWriter.SaveTo(filename: string);
var
	f : TFileStream;
begin
	if curchunk >= Low(chunks) then
	begin
		WriteLn('Error: opened chunk not closed');
		ExitProcess(0);
	end;
	f := TFileStream.Create(filename, fmCreate);
	f.Write(data[0], size);
	f.Free;
end;

procedure TMemoryWriter.Write(const Buffer; count : Longword);
begin
	if pos+count > size then
		size := pos+count;
	if size > capacity then
	begin
		capacity := (size div delta)*delta + delta;
		SetLength(data, capacity);
	end;
	Move(Buffer, data[pos], count);
	pos := pos + count;
end;

procedure TMemoryWriter.WriteShortint(value: Shortint);
begin
	Write(value, Sizeof(Shortint));
end;

procedure TMemoryWriter.WriteByte(value: Byte);
begin
	Write(value, Sizeof(Byte));
end;

procedure TMemoryWriter.WriteSmallint(value: Smallint);
begin
	Write(value, Sizeof(Smallint));
end;

procedure TMemoryWriter.WriteWord(value: Word);
begin
	Write(value, Sizeof(Word));
end;

procedure TMemoryWriter.WriteLongint(value: Longint);
begin
	Write(value, Sizeof(Longint));
end;

procedure TMemoryWriter.WriteLongword(value: Longword);
begin
	Write(value, Sizeof(Longword));
end;

procedure TMemoryWriter.WriteInt64(value : Int64);
begin
	Write(value, Sizeof(Int64));
end;

procedure TMemoryWriter.WriteQWord(value : QWord);
begin
	Write(value, Sizeof(QWord));
end;

procedure TMemoryWriter.WriteSingle(value: Single);
begin
	Write(value, Sizeof(Single));
end;

procedure TMemoryWriter.WriteDouble(value: Double);
begin
	Write(value, Sizeof(Double));
end;

procedure TMemoryWriter.WriteString(const value: String);
begin
	Write(PChar(value)^, Length(value));
end;

procedure TMemoryWriter.WriteStringZ(const value: String);
begin
	Write(PChar(value)^, Length(value));
	WriteByte(0);
end;

procedure TMemoryWriter.OpenChunk(id : Longword);
begin
	WriteLongword(id);
	Inc(curchunk);
	if curchunk > High(chunks) then
	begin
		Writeln('Error: too much nested chunks');
		ExitProcess(0);
	end;
	chunks[curchunk] := pos;
	WriteLongword(0);
end;

procedure TMemoryWriter.CloseChunk;
var
	currentpos : Longword;
begin
	if curchunk < Low(chunks) then
	begin
		Writeln('Error: chunk was not opened');
		ExitProcess(0);
	end;
	currentpos := pos;
	pos := chunks[curchunk];
	WriteLongword(currentpos - (pos+4));
	pos := currentpos;
	Dec(curchunk);
end;

end.
