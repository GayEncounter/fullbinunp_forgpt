unit egeoms;

interface
uses fouramdl;

function LoadLevelEGeoms(const filename : String) : T4AModelHierrarhy;

implementation
uses classes, chunkedFile;

function LoadLevelEGeoms(const filename : String) : T4AModelHierrarhy;
var
	reader : TMemoryReader;
	
	visuals : TMemoryReader;
	visual : TMemoryReader;
	
	header : TMemoryReader;
	model_data : TMemoryReader;
	
	list : TList;
	
	mesh : T4AModelSimple;
	model : T4AModelHierrarhy;
begin
	reader := TMemoryReader.CreateFromFile(filename);
	
	list := TList.Create;
	
	visuals := reader.OpenChunk(1);
	while visuals.More do
	begin
		visual := visuals.OpenChunk;
		
		header := visual.OpenChunk(0);
		header.Free;
		
		model_data := visual.OpenChunk(1);
		
		mesh := T4AModelSimple.Create;
		mesh.Load(model_data);
		list.Add(mesh);
		
		model_data.Free;
		
		visual.Free;
	end;
	visuals.Free;
	
	model := T4AModelHierrarhy.Create;
	SetLength(model.meshes, list.Count);
	Move(list.List^[0], model.meshes[0], list.Count*Sizeof(T4AModelSimple));
	
	list.Free;
	
	reader.Free;
	
	Result := model;
end;

end.
