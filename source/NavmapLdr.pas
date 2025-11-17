unit NavmapLdr;

interface
uses vmath, chunkedFile, Konfig_reader;

type
	TAttributeType = ( atStringZ = 1, atLongint = 2, atSmallint = 3, atShortint = 4, atLongword = 5, atWord = 6, atByte = 7 );

type
	PTokAttr = ^TTokAttr;
	TTokAttr = record
		_name : String;
		val_str : String;
		val_numeric : Int64;
		_type : TAttributeType;
		{
		case _type : TAttributeType of
			atStringZ:  ();
			atLongint:  (val_s32 : Longint);
			atSmallint: (val_s16 : Smallint);
			atShortint: (val_s8  : Shortint);
			atLongword: (val_u32 : Longword);
			atWord:     (val_u16 : Word);
			atByte:     (val_u8  : Byte);
		}
	end;
	
	PTokNode = ^TTokNode;
	TTokNode = record
		name : String;
		attrs : array of TTokAttr;
		child : array of TTokNode;
		
		function FindChild(const name : String) : PTokNode;
		function FindAttr(const name : String) : PTokAttr;
	end;

	TTokFile = class
		node_names : array[1..256] of String;
		attr_names : array[1..256] of String;
		attr_types : array[1..256] of TAttributeType;
		
		root : TTokNode;
		
		procedure Load(r : TMemoryReader);
		function  LoadNode(var node : TTokNode; r : TMemoryReader) : Boolean;
	end;
	
type
	TNavmapEndPoint = record
		x, y, cell : Longint;
	end;
	
	TNavmapConnection = record
		point_from, point_to, penalty : Longint;
	end;
	
	TNavigationMap = class
		verts : array of TVec3;
		tris : array of Longword;
		
		// off-mesh conections
		end_points : array of TNavmapEndPoint;
		connections : array of TNavmapConnection;
		
		// converted end-points
		end_points_pos : array of TVec3;
		
		procedure LoadFromTok(tok : TTokFile);
		procedure LoadFromBin(r : TKonfigReader);
		
		procedure SaveRAW(w : TMemoryWriter); overload;
		procedure SaveRAW(const fn : String); overload;
		
		procedure Draw;
		
		procedure Cleanup;
	end;
	
function LoadNavmap2033(const fn : String) : TNavigationMap;
function LoadNavmapBin(const fn : String) : TNavigationMap;

implementation
uses sysutils, GL, GLExt, common, Konfig, uLEOptions, uCrc;

function TTokNode.FindChild(const name : String) : PTokNode;
var I : Longint;
begin
	for I := 0 to Length(child) - 1 do
		if child[I].name = name then
		begin
			Result := @child[I];
			Exit;
		end;
		
	Result := nil;
end;

function TTokNode.FindAttr(const name : String) : PTokAttr;
var I : Longint;
begin
	for I := 0 to Length(attrs) - 1 do
		if attrs[I]._name = name then
		begin
			Result := @attrs[I];
			Exit;
		end;
		
	Result := nil;
end;

procedure TTokFile.Load(r : TMemoryReader);
var
	count : Longint;
	str : String;
	at : Byte;
begin
	count := 0;
	str := r.ReadStringZ;
	while str <> '' do
	begin
		Inc(count);
		node_names[count] := str;
		str := r.ReadStringZ;
	end;
	
	count := 0;
	at := r.ReadByte;
	while at <> 0 do
	begin
		Inc(count);
		attr_names[count] := r.ReadStringZ;
		attr_types[count] := TAttributeType(at);
		at := r.ReadByte;
	end;
	
	LoadNode(root, r);
end;

function TTokFile.LoadNode(var node : TTokNode; r : TMemoryReader) : Boolean;
var
	attrid : Byte;
	nameid : Byte;
	idx : Longint;
	
	child : TTokNode;
begin
	nameid := r.ReadByte;
	if nameid <> 0 then
	begin
		node.name := node_names[nameid];
		
		// read node attributes
		attrid := r.ReadByte;
		while attrid <> 0 do
		begin
			idx := Length(node.attrs);
			SetLength(node.attrs, idx+1);
			
			node.attrs[idx]._name := attr_names[attrid];
			node.attrs[idx]._type := attr_types[attrid];
			
			case attr_types[attrid] of
{
				atStringZ: node.attrs[idx].val_str := r.ReadStringZ;
				atLongint: node.attrs[idx].val_s32 := r.ReadLongint;
				atSmallint: node.attrs[idx].val_s16 := r.ReadSmallint;
				atShortint: node.attrs[idx].val_s8  := r.ReadShortint;
				atLongword: node.attrs[idx].val_u32 := r.ReadLongword;
				atWord: node.attrs[idx].val_u16 := r.ReadWord;
				atByte: node.attrs[idx].val_u8 := r.ReadByte;
}
				atStringZ:  node.attrs[idx].val_str     := r.ReadStringZ;
				atLongint:  node.attrs[idx].val_numeric := r.ReadLongint;
				atSmallint: node.attrs[idx].val_numeric := r.ReadSmallint;
				atShortint: node.attrs[idx].val_numeric := r.ReadShortint;
				atLongword: node.attrs[idx].val_numeric := r.ReadLongword;
				atWord:     node.attrs[idx].val_numeric := r.ReadWord;
				atByte:     node.attrs[idx].val_numeric := r.ReadByte;
				else raise Exception.Create('unknown attr type ' + IntToStr(Integer(attr_types[attrid])));
			end;
		
			attrid := r.ReadByte;
		end;
	
		// read child nodes
		while LoadNode(child, r) do
		begin
			idx := Length(node.child);
			SetLength(node.child, idx+1);
			node.child[idx] := child;
			
			// clear for next iteration
			SetLength(child.attrs, 0);
			SetLength(child.child, 0);
		end;
	end else
	begin
		Result := False;
		Exit;
	end;
	
	Result := True;
end;

procedure TNavigationMap.LoadFromTok(tok : TTokFile);
var
	I : Longint;
	
	mesh3D : PTokNode;
	verts  : PTokNode;
	tris   : PTokNode;
	
	off_conns : PTokNode;
	end_points : PTokNode;
	connections : PTokNode;
	
	x : PTokAttr;
	y : PTokAttr;
	z : PTokAttr;
	_y : Single;
	
	err : Word;
begin
	mesh3D := tok.root.FindChild('mesh3D');
	
	verts := mesh3D.FindChild('verts');
	SetLength(self.verts, Length(verts.child));
	
	for I := 0 to Length(verts.child) - 1 do
	begin
		x := verts.child[I].FindAttr('x');
		y := verts.child[I].FindAttr('y');
		z := verts.child[I].FindAttr('z');
		
		if z.val_str[1] = 'x' then
		begin
			_y := Single(StrToInt(z.val_str));
		end else
		begin
			Val(z.val_str, _y, err);
			if err <> 0 then
				WriteLn('Error parsing "', z.val_str, '" as float');
		end;
		
		self.verts[I].x := x.val_numeric / 1000;
		self.verts[I].y := _y / 1000;
		self.verts[I].z := y.val_numeric / 1000;
	end;
	
	tris := mesh3D.FindChild('tris');
	SetLength(self.tris, Length(tris.child)*3);
	
	for I := 0 to Length(tris.child) - 1 do
	begin
		x := tris.child[I].FindAttr('edge0StartVert');
		y := tris.child[I].FindAttr('edge1StartVert');
		z := tris.child[I].FindAttr('edge2StartVert');
			
		self.tris[I*3+0] := x.val_numeric;
		self.tris[I*3+1] := y.val_numeric;
		self.tris[I*3+2] := z.val_numeric;
	end;
	
	// off-mesh connections
	off_conns := tok.root.FindChild('offMeshConnections');
	
	if off_conns <> nil then
	begin
		end_points := off_conns.FindChild('endPoints');
		SetLength(self.end_points, Length(end_points.child));
		
		for I := 0 to Length(end_points.child) - 1 do
		begin
			x := end_points.child[I].FindAttr('position');
			SScanf(x.val_str, '%d:%d,%d', [@self.end_points[I].cell, @self.end_points[I].x, @self.end_points[I].y]);
		end;
		
		connections := off_conns.FindChild('connections');
		SetLength(self.connections, Length(connections.child));
		
		for I := 0 to Length(connections.child) - 1 do
		begin
			self.connections[I].point_from := connections.child[I].FindAttr('from').val_numeric;
			self.connections[I].point_to := connections.child[I].FindAttr('to').val_numeric;
			self.connections[I].penalty := connections.child[I].FindAttr('penalty').val_numeric;
		end;
	end;
	
	//fortesting
	Cleanup;
end;

procedure TNavigationMap.LoadFromBin(r : TKonfigReader);
var
	I : Longint;
	
	bDebug : Boolean;
	
	v_major : Longint;
	v_minor : Longint;
	
	vert_count : Longint;
	tri_count : Longint;
	
	x : Longint;
	y : Longint;
	z : Single;
begin
	bDebug := (r.bin_flags and konfDebugInfo) <> 0;

	if bDebug and (r.ReadString('open_element') <> 'mesh') then
		WriteLn('TNavigationMap.LoadFromBin : ERROR open_element(mesh) not found');
		
	v_major := r.ReadS32('majorRelease');
	v_minor := r.ReadS32('minorRelease');
	
	if (v_major <> 5) or (v_minor <> 16) then
		WriteLn('TNavigationMap.LoadFromBin : unsupported version ', v_major, '.', v_minor, ', must be 5.16');
		
	if bDebug and (r.ReadString('open_element') <> 'mesh3D') then
		WriteLn('TNavigationMap.LoadFromBin : ERROR open_element(mesh3D) not found');
	
	// verts	
	if bDebug and (r.ReadString('open_element') <> 'verts') then
		WriteLn('TNavigationMap.LoadFromBin : ERROR open_element(verts) not found');

	vert_count := r.ReadS32('size');
	SetLength(self.verts, vert_count);
	
	for I := 0 to vert_count - 1 do
	begin
		if bDebug and (r.ReadString('open_element') <> 'vert') then
			WriteLn('TNavigationMap.LoadFromBin : ERROR open_element(vert) not found');
			
		x := r.ReadS32('x');
		y := r.ReadS32('y');
		z := r.ReadFP32('z');
			
		self.verts[I].x := x / 1000;
		self.verts[I].y := z / 1000;
		self.verts[I].z := y / 1000;
			
		if bDebug and (r.ReadString('close_element') <> 'vert') then
			WriteLn('TNavigationMap.LoadFromBin : ERROR close_element(vert) not found');
	end;
	
	if bDebug and (r.ReadString('close_element') <> 'verts') then
		WriteLn('TNavigationMap.LoadFromBin : ERROR close_element(verts) not found');
		
	// tris
	if bDebug and (r.ReadString('open_element') <> 'tris') then
		WriteLn('TNavigationMap.LoadFromBin : ERROR open_element(tris) not found');
		
	tri_count := r.ReadS32('size');
	SetLength(self.tris, tri_count*3);	

	for I := 0 to tri_count - 1 do
	begin
		if bDebug and (r.ReadString('open_element') <> 'tri') then
			WriteLn('TNavigationMap.LoadFromBin : ERROR open_element(tri) not found');
	
		r.ReadS32('surfaceType');
		r.ReadS32('sectionID');
		r.ReadS32('userData');
		self.tris[I*3+0] := r.ReadS32('edge0StartVert');
		r.ReadFP32('edge0StartZ');
		r.ReadS32('edge0Connection');
		self.tris[I*3+1] := r.ReadS32('edge1StartVert');
		r.ReadFP32('edge1StartZ');
		r.ReadS32('edge1Connection');
		self.tris[I*3+2] := r.ReadS32('edge2StartVert');
		r.ReadFP32('edge2StartZ');
		r.ReadS32('edge2Connection');
		
		if bDebug and (r.ReadString('close_element') <> 'tri') then
			WriteLn('TNavigationMap.LoadFromBin : ERROR close_element(tri) not found');	
	end;
		
	if bDebug and (r.ReadString('close_element') <> 'tris') then
		WriteLn('TNavigationMap.LoadFromBin : ERROR close_element(tris) not found');
		
	if bDebug and (r.ReadString('close_element') <> 'mesh3D') then
		WriteLn('TNavigationMap.LoadFromBin : ERROR close_element(mesh3D) not found');	
end;

procedure TNavigationMap.SaveRAW(w : TMemoryWriter); overload;
var
	new_points : array of TVec3;
	index_remap : array of Longint;

	I : Longint;

	function AddPoint(const p : TVec3) : Longint;
	var
		I : Longint;
	begin{
		for I := 0 to Length(new_points) - 1 do
			if Distance(new_points[I], p) < 0.001 then
			begin
				AddPoint := I;
				Exit;
			end;
			}
		Insert([p], new_points, Length(new_points));
		Result := Length(new_points) - 1;
	end;

begin

	// remove duplicate points
	SetLength(new_points, 0);
	SetLength(index_remap, Length(verts));
	
	for I := 0 to Length(verts) - 1 do
		index_remap[I] := AddPoint(verts[I]);

	// version
	w.WriteLongint(2);
	
	// save points
	w.WriteLongword(Length(new_points));
	w.Write(new_points[0], Length(new_points)*Sizeof(TVec3));
	
	// save triangles
	w.WriteLongword(Length(tris) div 3);
	for I := 0 to Length(tris) - 1 do
		w.WriteLongword(index_remap[tris[I]]);
	
	w.WriteLongword(Length(end_points));
	w.Write(end_points[0], Length(end_points)*Sizeof(TNavmapEndPoint));
	w.WriteLongword(Length(connections));
	w.Write(connections[0], Length(connections)*Sizeof(TNavmapConnection));	
end;

procedure TNavigationMap.SaveRAW(const fn : String); overload;
var
	w : TMemoryWriter;
begin
	w := TMemoryWriter.Create;
	SaveRAW(w);
	
	w.SaveTo(fn);
	w.Free;
end;

procedure TNavigationMap.Draw;
var
	I : Longint;
begin
	glDisable(GL_CULL_FACE);
	
	glEnableClientState(GL_VERTEX_ARRAY);
	glVertexPointer(3, GL_FLOAT, 12, @verts[0]);
	
	glEnable(GL_BLEND);
	glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
	
	glColor4ubv(@uLEOptions.navmap_face_clr);
	glDrawElements(GL_TRIANGLES, Length(tris), GL_UNSIGNED_INT, @tris[0]);
	
	glDisable(GL_BLEND);
	
	RenderWireframe;
	glColor4ubv(@uLEOptions.navmap_wire_clr);
	glDrawElements(GL_TRIANGLES, Length(tris), GL_UNSIGNED_INT, @tris[0]);
	RenderDefault;
	
	glDisableClientState(GL_VERTEX_ARRAY);
	
	if Length(end_points_pos) = Length(end_points) then
	begin
		glColor3f(0, 0, 1);
		glBegin(GL_LINES);
		
		for I := 0 to Length(connections) - 1 do
		begin
			glVertex3fv(@end_points_pos[connections[I].point_from].x);
			glVertex3fv(@end_points_pos[connections[I].point_to].x);
		end;
		
		glEnd;
		glColor3f(1, 1, 1);
	end;
	
	glEnable(GL_CULL_FACE);
end;

procedure TNavigationMap.Cleanup;
const
	VERT_THRESHOLD = 0.001;
	EDGE_THRESHOLD = 0.001;
var
	I, J : Longint;
	
	n_points : Longint;
	n_edges : Longint;
begin
	n_points := 0;
	n_edges := 0;
	
	for I := 0 to Length(verts) - 1 do
		for J := 0 to Length(verts) - 1 do
			if (I <> J) and (Distance(verts[I], verts[J]) < VERT_THRESHOLD) then
				Inc(n_points);
				
	for I := 0 to (Length(tris) div 3) - 1 do
	begin
		if Distance(verts[tris[I*3+0]], verts[tris[I*3+1]]) < EDGE_THRESHOLD then
			Inc(n_edges);
		if Distance(verts[tris[I*3+1]], verts[tris[I*3+2]]) < EDGE_THRESHOLD then
			Inc(n_edges);
		if Distance(verts[tris[I*3+2]], verts[tris[I*3+0]]) < EDGE_THRESHOLD then
			Inc(n_edges);
	end;
	
	WriteLn(n_points, ' unmerged points at same position');
	WriteLn(n_edges, ' edges with length leading to zero');
end;

function LoadNavmap2033(const fn : String) : TNavigationMap;
var
	r, mesh : TMemoryReader;
	tok : TTokFile;
	navmap : TNavigationMap;
begin
	r := TMemoryReader.CreateFromFile(fn);
	mesh := TMemoryReader.CreateSlice(r, 4, r.ReadLongword);
	
	tok := TTokFile.Create;
	tok.Load(mesh);
	
	navmap := TNavigationMap.Create;
	navmap.LoadFromTok(tok);
	
	tok.Free;
	mesh.Free;
	r.Free;
	
	Result := navmap;
end;

function LoadNavmapBin(const fn : String) : TNavigationMap;
var
	r, mesh : TMemoryReader;
	ground_size : Longword;
	K : TKonfig;
	KR : TKonfigReader;
	navmap : TNavigationMap;
	
	sign : Longword;
begin
	Result := nil;

	r := TMemoryReader.CreateFromFile(fn);
	
	sign := r.ReadLongword;

	if sign = Longword(GetStringCrc('ground_ver0001')) then
	begin
		// OK, LastLight or Redux
	end else
	if sign = Longword(GetStringCrc('ground_ver0002')) then
	begin
		// OK, Arktika1 or Exodus
	end else
	begin
		WriteLn('LoadNavmapBin : invalid ground_ver000X signature');
		r.Free;
		Exit;
	end;
	
	ground_size := r.ReadLongword;
	mesh := TMemoryReader.CreateSlice(r, r.pos, ground_size);
	
	K := TKonfig.Create;
	K.Load(mesh);
	
	KR := TKonfigReader.Create(K, nil, nil);
	
	navmap := TNavigationMap.Create;
	navmap.LoadFromBin(KR);
	
	KR.Free;
	K.Free;
	mesh.Free;
	r.Free;
	
	Result := navmap;
end;

end.