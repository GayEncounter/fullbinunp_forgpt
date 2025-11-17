unit uDrawUtils;

interface
uses fouramdl, skeleton, motion;

procedure DrawSkeleton(skeleton : T4ASkeleton; sel : Longint; mot : I4AMotion = nil; time : Single = 0.0);
procedure DrawBoneOBB(ms : T4AModelSkeleton; sel : Longint);

procedure DrawNormals(model : T4AModel);

implementation
uses vmath, GL;

procedure DrawSkeleton(skeleton : T4ASkeleton; sel : Longint; mot : I4AMotion = nil; time : Single = 0.0);
var
	I : Longint;
	mt : TMatrix;
const
	pvt_scale : Single = 0.02;
	
	procedure MotTransform(const name : String; out m : TMatrix);
	var
		id : Longint;
		parent : TMatrix;
		mat : TMatrix;
	begin
		id := skeleton.GetBoneID(name);
		if skeleton.bones[id].parent_name <> '' then
			MotTransform(skeleton.bones[id].parent_name, parent)
		else
			Identity(parent);
			
		mot.GetTransform(id, time, mat);
		Mul44(parent, mat);
		m := parent;
	end;
	
	procedure GetTransform(const name : String; out m : TMatrix);
	var
		id : Longint;
	begin
		id := skeleton.GetBoneID(name);
		if (mot <> nil) and mot.AffectsBone(id) then
			MotTransform(name, m)
		else
			skeleton.GetTransform(name, m);
	end;
	
begin
	glDisable(GL_DEPTH_TEST);
	
	
	glBegin(GL_LINES);
	
	for I := 0 to Length(skeleton.bones)-1 do
	begin
		if sel = I then
			glColor3f(1.0, 1.0, 1.0)
		else
			glColor3f(0.8, 0.8, 0.0);
			
		if skeleton.bones[I].parent_name <> '' then
		begin
			GetTransform(skeleton.bones[I].name, mt);
			glVertex3f(mt[4,1], mt[4,2], mt[4,3]);
			
			GetTransform(skeleton.bones[I].parent_name, mt);
			glVertex3f(mt[4,1], mt[4,2], mt[4,3]);
		end;
	end;
	
	glEnd;
	
	glPointSize(5.0);
	glBegin(GL_POINTS);
	
	for I := 0 to Length(skeleton.bones)-1 do
	begin
		if sel = I then
			glColor3f(0.0, 0.0, 1.0)
		else
			glColor3f(1.0, 0.0, 0.0);
	
		GetTransform(skeleton.bones[I].name, mt);
		glVertex3f(mt[4,1], mt[4,2], mt[4,3]);
	end;
	
	glEnd;
	{
	glBegin(GL_LINES);
	
	for I := 0 to Length(skeleton.bones)-1 do
	begin
		GetTransform(skeleton.bones[I].name, mt);

		glColor3f(1.0, 0.0, 0.0);
		glVertex3f(mt[4,1],mt[4,2],mt[4,3]);
		glVertex3f(mt[4,1]+mt[1,1]*pvt_scale,mt[4,2]+mt[1,2]*pvt_scale,mt[4,3]+mt[1,3]*pvt_scale);
		
		glColor3f(0.0, 1.0, 0.0);
		glVertex3f(mt[4,1],mt[4,2],mt[4,3]);
		glVertex3f(mt[4,1]+mt[2,1]*pvt_scale,mt[4,2]+mt[2,2]*pvt_scale,mt[4,3]+mt[2,3]*pvt_scale);
		
		glColor3f(0.0, 0.0, 1.0);
		glVertex3f(mt[4,1],mt[4,2],mt[4,3]);
		glVertex3f(mt[4,1]+mt[3,1]*pvt_scale,mt[4,2]+mt[3,2]*pvt_scale,mt[4,3]+mt[3,3]*pvt_scale);
	end;
	
	glEnd;
	}
	glColor3f(1.0, 1.0, 1.0);
	glPointSize(1.0);
	glEnable(GL_DEPTH_TEST);
end;

procedure _DrawBoneOBB(m : T4AModelSkinnedMesh; skeleton : T4ASkeleton; sel : Longint);
var
	I, bone_id : Longint;
	mat : TMatrix;
	b : TMatrix;
begin
	glDisable(GL_CULL_FACE);
	glPolygonMode(GL_FRONT_AND_BACK, GL_LINE);
	
	for I := 0 to Length(m.bone_obb)-1 do
		with m.bone_obb[I] do
		begin
			bone_id := m.bone_ids[I];
			skeleton.GetTransform(skeleton.bones[bone_id].name, b);
		
			mat[1,1] := rot[1,1]; mat[1,2] := rot[1,2]; mat[1,3] := rot[1,3]; mat[1,4] := 0.0;
			mat[2,1] := rot[2,1]; mat[2,2] := rot[2,2]; mat[2,3] := rot[2,3]; mat[2,4] := 0.0;
			mat[3,1] := rot[3,1]; mat[3,2] := rot[3,2]; mat[3,3] := rot[3,3]; mat[3,4] := 0.0;
			mat[4,1] := offset.x; mat[4,2] := offset.y; mat[4,3] := offset.z; mat[4,4] := 1.0;
			
			Mul44(b, mat);
			
			glPushMatrix;
			glMultMatrixf(@b);
			
			if sel = bone_id then
				glColor3f(0.0, 1.0, 0.0)
			else
				glColor3f(1.0, 1.0, 1.0);
			
			glBegin(GL_QUADS);
			
			glVertex3f( half_size.x,  half_size.y, -half_size.z);
			glVertex3f( half_size.x, -half_size.y, -half_size.z);
			glVertex3f( half_size.x, -half_size.y,  half_size.z);
			glVertex3f( half_size.x,  half_size.y,  half_size.z);
			
			glVertex3f(-half_size.x,  half_size.y, -half_size.z);
			glVertex3f(-half_size.x,  half_size.y,  half_size.z);
			glVertex3f(-half_size.x, -half_size.y,  half_size.z);
			glVertex3f(-half_size.x, -half_size.y, -half_size.z);
			
			glVertex3f( half_size.x,  half_size.y, -half_size.z);
			glVertex3f( half_size.x,  half_size.y,  half_size.z);
			glVertex3f(-half_size.x,  half_size.y,  half_size.z);
			glVertex3f(-half_size.x,  half_size.y, -half_size.z);
			
			glVertex3f( half_size.x, -half_size.y, -half_size.z);
			glVertex3f(-half_size.x, -half_size.y, -half_size.z);
			glVertex3f(-half_size.x, -half_size.y,  half_size.z);
			glVertex3f( half_size.x, -half_size.y,  half_size.z);
			
			glVertex3f(-half_size.x,  half_size.y,  half_size.z);
			glVertex3f( half_size.x,  half_size.y,  half_size.z);
			glVertex3f( half_size.x, -half_size.y,  half_size.z);
			glVertex3f(-half_size.x, -half_size.y,  half_size.z);
			
			glVertex3f( half_size.x,  half_size.y, -half_size.z);
			glVertex3f(-half_size.x,  half_size.y, -half_size.z);
			glVertex3f(-half_size.x, -half_size.y, -half_size.z);
			glVertex3f( half_size.x, -half_size.y, -half_size.z);
			
			glEnd;
			
			glColor3f(1.0, 1.0, 1.0);
			
			glPopMatrix;
		end;
		
	glEnable(GL_CULL_FACE);
	glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
end;

procedure DrawBoneOBB(ms : T4AModelSkeleton; sel : Longint);
var
	I, J : Longint;
begin
	for I := 0 to Length(ms.meshes[0])-1 do
		for J := 0 to Length(ms.meshes[0,I].meshes)-1 do
			_DrawBoneOBB(ms.meshes[0,I].meshes[J], ms.skeleton, sel);
end;

procedure _DrawNormals(model : T4AModelHierrarhy); overload;
var
	I, J : Integer;

	m : T4AModelSimple;
	v : P4AVertStatic;
	n, t, b : TVec3;
const
	scale = 0.1;
begin
	for I := 0 to Length(model.meshes) - 1 do
	begin
		m := model.meshes[I];
		v := P4AVertStatic(m.vertices);

		glBegin(GL_LINES);
		for J := 0 to Length(m.vertices) - 1 do
		begin
			UnpackNormal(n, v^.normal);
			UnpackNormal(t, v^.tangent);
			UnpackNormal(b, v^.binormal);

			n.x := n.x * scale;
			n.y := n.y * scale;
			n.z := n.z * scale;

			t.x := t.x * scale;
			t.y := t.y * scale;
			t.z := t.z * scale;

			b.x := b.x * scale;
			b.y := b.y * scale;
			b.z := b.z * scale;

			glColor3f(1, 0, 0);
			glVertex3fv(@v^.point);
			glVertex3f(v^.point.x + t.x, v^.point.y + t.y, v^.point.z + t.z);

			glColor3f(0, 1, 0);
			glVertex3fv(@v^.point);
			glVertex3f(v^.point.x + n.x, v^.point.y + n.y, v^.point.z + n.z);

			glColor3f(0, 0, 1);
			glVertex3fv(@v^.point);
			glVertex3f(v^.point.x + b.x, v^.point.y + b.y, v^.point.z + b.z);

			Inc(v);
		end;
		glEnd();
	end;

	glColor3f(1, 1, 1);
end;

procedure _DrawNormals(model : T4AModelSkinned); overload;
var
	I, J : Integer;

	m : T4AModelSkinnedMesh;
	v : P4AVertSkin;
	n, t, b : TVec3;
	
	p : TVec3;
	point_scale : Single;
const
	scale = 0.1;
begin
	for I := 0 to Length(model.meshes) - 1 do
	begin
		m := model.meshes[I];
		v := P4AVertSkin(m.vertices);

		point_scale := m.GetPointScale;

		glBegin(GL_LINES);
		for J := 0 to Length(m.vertices) - 1 do
		begin
			UnpackNormal(n, v^.normal);
			UnpackNormal(t, v^.tangent);
			UnpackNormal(b, v^.binormal);

			n.x := n.x * scale;
			n.y := n.y * scale;
			n.z := n.z * scale;

			t.x := t.x * scale;
			t.y := t.y * scale;
			t.z := t.z * scale;

			b.x := b.x * scale;
			b.y := b.y * scale;
			b.z := b.z * scale;
			
			p.x := v^.point.x * point_scale;
			p.y := v^.point.y * point_scale;
			p.z := v^.point.z * point_scale;

			glColor3f(1, 0, 0);
			glVertex3fv(@p);
			glVertex3f(p.x + t.x, p.y + t.y, p.z + t.z);

			glColor3f(0, 1, 0);
			glVertex3fv(@p);
			glVertex3f(p.x + n.x, p.y + n.y, p.z + n.z);

			glColor3f(0, 0, 1);
			glVertex3fv(@p);
			glVertex3f(p.x + b.x, p.y + b.y, p.z + b.z);

			Inc(v);
		end;
		glEnd();
	end;

	glColor3f(1, 1, 1);
end;

procedure _DrawNormals(ms : T4AModelSkeleton); overload;
var
	I : Longint;
begin
	for I := 0 to Length(ms.meshes[0])-1 do
		_DrawNormals(ms.meshes[0,I]);
end;

procedure _DrawNormals(model : T4AModelSoftbody); overload;
var
	I : Integer;

	v : P4AVertSoft;
	n, t, b : TVec3;
const
	scale = 0.1;
begin
	glBegin(GL_LINES);
	for I := 0 to Length(model.vertices) - 1 do
	begin
		v := @model.vertices[I];

		n := v^.normal;

//		UnpackNormal(n, v^.normal);
//		UnpackNormal(t, v^.tangent);
//		UnpackNormal(b, v^.binormal);

		n.x := n.x * scale;
		n.y := n.y * scale;
		n.z := n.z * scale;

//		t.x := t.x * scale;
//		t.y := t.y * scale;
//		t.z := t.z * scale;

//		b.x := b.x * scale;
//		b.y := b.y * scale;
//		b.z := b.z * scale;

//		glColor3f(1, 0, 0);
//		glVertex3fv(@v^.point);
//		glVertex3f(v^.point.x + t.x, v^.point.y + t.y, v^.point.z + t.z);

		glColor3f(0, 1, 0);
		glVertex3fv(@v^.point);
		glVertex3f(v^.point.x + n.x, v^.point.y + n.y, v^.point.z + n.z);

//		glColor3f(0, 0, 1);
//		glVertex3fv(@v^.point);
//		glVertex3f(v^.point.x + b.x, v^.point.y + b.y, v^.point.z + b.z);
	end;
	glEnd();

	glColor3f(1, 1, 1);
end;

procedure DrawNormals(model : T4AModel);
begin
	if model is T4AModelHierrarhy then
		_DrawNormals(T4AModelHierrarhy(model))
	else
	if model is T4AModelSkeleton then
		_DrawNormals(T4AModelSkeleton(model))
	else
	if model is T4AModelSoftbody then
		_DrawNormals(T4AModelSoftbody(model));
end;

end.