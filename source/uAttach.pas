unit uAttach;

interface
uses classes, uEntity, Konfig;

function  AttachEntities(entities : TList; parent : TEntity) : Boolean;
procedure DetachEntities(entities : TList);

function  AddShapes(victim : TEntity; from : TEntity) : Boolean;
function  RemoveShape(e : TEntity; shape : TSection) : Boolean;

implementation
uses SysUtils, vmath, skeleton, uChoose, uEditorUtils;

function AttachEntities(entities : TList; parent : TEntity) : Boolean;
var
	I : Longint;
	e : TEntity;
	skeleton : T4ASkeleton;
	bone : String;
	
	base_mat, inv_base_mat, att_mat : TMatrix;
begin
	Result := False;
	skeleton := parent.GetSkeleton;
	if skeleton <> nil then
	begin
		bone := '';
		if ChooseLocator(skeleton, bone) then
		begin
			parent.GetBoneTransform(bone, base_mat);
			Invert43(inv_base_mat, base_mat);
			
			for I := 0 to entities.Count-1 do
			begin
				e := TEntity(entities[I]);
				
				e.ParentID := parent.ID;
				e.AttachBone := bone;
				
				att_mat := inv_base_mat;
				Mul44(att_mat, e.Matrix);
				
				e.AttachOffset := att_mat;
			end;
			
			Result := True;
		end;
	end;
end;

procedure DetachEntities(entities : TList);
var
	I : Longint;
	E : TEntity;
	id : TMatrix;
begin
	Identity(id);
	
	for I := 0 to entities.Count-1 do
	begin
		E := TEntity(entities[I]);
		E.ParentID := 65535;
		E.AttachBone := '';
		E.AttachOffset := id;
	end;
end;

procedure _RenumberShapes(shapes : TSection);
begin
	uEditorUtils.RenumberArray(shapes, 'shape_', 2);
end;

function AddShapes(victim : TEntity; from : TEntity) : Boolean;
var
	I : Longint;
	
	src_shapes : TSection;
	dst_shapes : TSection;
	
	v : TSimpleValue;
	f : TFloatArrayValue;
	shape, box, sphere : TSection;
	
	shape_type : Longword;
	mat, offset, new_offset : TMatrix;
	center : TVec3;
begin
	Result := False;
	
	src_shapes := from.data.GetSect('shapes', False);
	dst_shapes := victim.data.GetSect('shapes', False);
	
	if (src_shapes <> nil) and (dst_shapes <> nil) then
	begin
		for I := 0 to src_shapes.items.Count-1 do
		begin
			v := TSimpleValue(src_shapes.items[I]);
			if v is TSection then
			begin
				shape := v.Copy as TSection;
				shape_type := shape.GetInt('type', 'u32');
				
				case shape_type of
					1: begin
						box := shape.GetSect('box');
						
						f := box.GetParam('', 'pose, matrix') as TFloatArrayValue;
						if Length(f.data) = 16 then
							f.GetMatrix44(offset)
						else
							f.GetMatrix43(offset);
					
						mat := from.Matrix;
						Mul44(mat, offset);
						
						Invert43(new_offset, victim.Matrix);
						Mul44(new_offset, mat);
						
						if Length(f.data) = 16 then
							f.SetMatrix44(new_offset)
						else
							f.SetMatrix43(new_offset);
					end;
					0: begin
						sphere := shape.GetSect('sphere');
						
						f := sphere.GetParam('center', 'vec3f') as TFloatArrayValue;
						
						center.x := from.Matrix[4,1] + f.data[0];
						center.y := from.Matrix[4,2] + f.data[1];
						center.z := from.Matrix[4,3] + f.data[2];
						
						f.data[0] := center.x - victim.Matrix[4,1];
						f.data[1] := center.y - victim.Matrix[4,2];
						f.data[2] := center.z - victim.Matrix[4,3];
					end;
				end;
				
				dst_shapes.items.Add(shape);
			end;
		end;
		
		_RenumberShapes(dst_shapes);
		victim.UpdateShapes;
		Result := True;
	end;
end;

function RemoveShape(e : TEntity; shape : TSection) : Boolean;
var
	shapes : TSection;
begin
	Result := False;
	
	shapes := e.data.GetSect('shapes', False);
	if shapes <> nil then
	begin
		if shapes.items.Remove(shape) >= 0 then
		begin
			shape.Free;
			_RenumberShapes(shapes);
			e.UpdateShapes;
			
			Result := True;
		end;
	end;
end;

end.
