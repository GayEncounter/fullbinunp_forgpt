unit ConverterScripts;

interface
uses Konfig;

procedure CutEditorScriptBlocks(entities : TSection);

implementation
uses sysutils, classes, strutils, vmath;

type
	TBlock = class
		data : TSection;
	end;
	
	TLink = class
		block_from, block_to : TBlock;
		point_from, point_to : Integer;
	end;
	
	TScript = class
		blocks_version : Integer;
		
		blocks : TList;
		links : TList;
		
		constructor Create;
		destructor Destroy; override;
		
		procedure Load(script : TSection);
		procedure Save(script : TSection);
		
		procedure RemoveBlock(block : TBlock);
	end;
	
constructor TScript.Create;
begin
	inherited;
	
	blocks := TList.Create;
	links := TList.Create;
end;

destructor TScript.Destroy;
var
	I : Integer;
begin
	for I := 0 to blocks.Count - 1 do
	begin
		TBlock(blocks[I]).data.Free;
		TBlock(blocks[I]).Free;
	end;
	for I := 0 to links.Count - 1 do
		TLink(links[I]).Free;
		
	blocks.Free;
	links.Free;
	
	inherited;
end;
	
procedure TScript.Load(script : TSection);
var
	sect_blocks, blk : TSection;
	
	lnk_count : TIntegerValue;
	lnk : TIntegerArrayValue;
	
	I : Integer;
	
	block : TBlock;
	link : TLink;
begin
	sect_blocks := script.GetParam('blocks', 'section') as TSection;
	blocks_version := sect_blocks.GetInt('version', 0, 'u16');
	
	for I := 0 to sect_blocks.items.Count - 1 do
	begin
		if TObject(sect_blocks.items[I]) is TSection then
		begin
			blk := TSection(sect_blocks.items[I]);
		
	 		block := TBlock.Create;
	 		block.data := blk.Copy as TSection;
			blocks.Add(block);
		end;
	end;
		
	lnk_count := script.GetParam('link_count', 'u32') as TIntegerValue;
	for I := 0 to lnk_count.num - 1 do
	begin
		lnk := script.GetParam(IntToStr(I), 'vec4s16') as TIntegerArrayValue;
		 
		link := TLink.Create; 
		link.block_from := TBlock(blocks[lnk.data[0]]);
		link.point_from := lnk.data[1];
		link.block_to := TBlock(blocks[lnk.data[2]]);
		link.point_to := lnk.data[3];
		links.Add(link);
	end;
end;

procedure TScript.Save(script : TSection);
var
	I : Integer;
	sgroups, sblocks : TSection;
	
	block : TBlock;
	n : String;
	
	link : TLink;
	vlink : TVec4S16;
begin
	// ?
	script.AddHint('groups', 'array');
	sgroups := script.AddSect('groups');
	sgroups.AddInt('count', 0, 'u32');
	
	sblocks := script.AddSect('blocks');
	if blocks_version > 0 then
		sblocks.AddInt('version', { blocks_version } 59, 'u16');
	sblocks.AddInt('block_count', $FFFFFFFF, 'u32');
	sblocks.AddHint('array with no key', 'array');
	sblocks.AddInt('count', blocks.Count, 'u32');
	
	for I := 0 to blocks.Count - 1 do
	begin
		block := TBlock(blocks[I]);
		
		n := IntToStr(I);
		n := StringOfChar('0', 5-Length(n)) + n;
		
		sblocks.items.Add(block.data.Copy);
	end;
	
	script.AddInt('link_count', links.Count, 'u32');
	for I := 0 to links.Count - 1 do
	begin
		link := TLink(links[I]);

		vlink.x := blocks.IndexOf(link.block_from);
		vlink.y := link.point_from;
		vlink.z := blocks.IndexOf(link.block_to);
		vlink.w := link.point_to;
		
		script.AddVec4S16(IntToStr(I), vlink);
	end;
end;

procedure TScript.RemoveBlock(block : TBlock);
var
	I : Integer;
	del_links : TList;
	link : TLink;
begin
	// remove all links from- and to that block
	del_links := TList.Create;
	
	for I := 0 to links.Count - 1 do
	begin
		link := TLink(links[I]);
		if (link.block_from = block) or (link.block_to = block) then
			del_links.Add(link);
	end;
	
	for I := 0 to del_links.Count - 1 do
	begin
		links.Remove(del_links[I]);
		TLink(del_links[I]).Free;
	end;
	
	del_links.Free;
	
	// remove block itself
	blocks.Remove(block);
	block.data.Free;
	block.Free;
end;

procedure ProcessScript(vs : TSection);
var
	script : TScript;
	I, J : Integer;
	blk : TBlock;
	prev, sect : TSection;
begin
	script := TScript.Create;
	
	script.Load(vs);
	
	I := 0;
	while I < script.blocks.Count do
	begin
		blk := TBlock(script.blocks[I]);
		
		if AnsiStartsStr('subscript', blk.data.GetStr('clsid')) then
		begin
			script.RemoveBlock(blk);
			Dec(I);
		end else
		if (blk.data.GetStr('clsid') = 'actions/activate-deactivate') and 
		   (blk.data.GetParam('force_dead', 'bool') = nil) then
		begin
			blk.data.AddBool('force_dead', False);
		end else 
		if (blk.data.GetStr('clsid') = 'triggers/is fire') then
		begin
			if blk.data.GetSect('ak sammy slot', False) = nil then
			begin
				prev := blk.data.GetSect('rpk_slot');
				J := blk.data.items.IndexOf(prev);
				
				sect := TSection.Create('ak sammy slot');
				sect.AddInt('primary_ak sammy slot', 3, 'u8');
				sect.AddInt('secondary_ak sammy slot', 3, 'u8');
				
				blk.data.items.Insert(J+1, sect);
			end;
			
			if blk.data.GetSect('kolya slot', False) = nil then
			begin
				prev := blk.data.GetSect('ak sammy slot');
				J := blk.data.items.IndexOf(prev);
				
				sect := TSection.Create('kolya slot');
				sect.AddInt('primary_kolya slot', 3, 'u8');
				sect.AddInt('secondary_kolya slot', 3, 'u8');
				
				blk.data.items.Insert(J+1, sect);
			end;
			
			if blk.data.GetSect('vyhlop', False) = nil then
			begin
				prev := blk.data.GetSect('kolya slot');
				J := blk.data.items.IndexOf(prev);
				
				sect := TSection.Create('vyhlop');
				sect.AddInt('primary_vyhlop', 3, 'u8');
				sect.AddInt('secondary_vyhlop', 3, 'u8');
				
				blk.data.items.Insert(J+1, sect);
			end;			
		end else
		if (blk.data.GetStr('clsid') = 'actions/attach vs') then
		begin
			if (blk.data.GetStr('vs') <> '') and (blk.data.GetSect('exposed_blocks', False) = nil) then
			begin
				blk.data.AddHint('exposed_blocks', 'array');
				blk.data.AddSect('exposed_blocks').AddInt('count', 0, 'u32');
			end;
		end;
		
		Inc(I);
	end;
	
	vs.Clear;
	script.Save(vs);
	
	script.Free;
end;

procedure CutEditorScriptBlocks(entities : TSection);
var
	E : Longint;
	ent, vss : TSection;
begin
	for E := 1 to entities.ParamCount - 1 do
	begin
		ent := entities.GetParam(E) as TSection;
		
		vss := ent.GetSect('vss_ver_7', False);
		if vss = nil then
			vss := ent.GetSect('vss_ver_6');
			
		if vss.GetInt('count', 'u32') > 0 then
			ProcessScript(vss.GetParam(1) as TSection);
	end;
end;

end.