procedure DrawSprite(const p : TVec3; const d : TVec3; w, h : Single);
var
	p1, p2 : TVec3;
	hw, hh : Single;
begin
	hw := w/2;
	hh := h/2;

	p1.x := -d.z;
	p1.y := 0.0;
	p1.z := d.x;
	Normalize(p1);
	
	p2.x := d.z;
	p2.y := 0.0;
	p2.z := -d.x;
	Normalize(p2);
	
	glDisable(GL_CULL_FACE);
	
	glBegin(GL_QUADS);
	
	glVertex3f(p.x + p2.x*hw, p.y + hh, p.z + p2.z*hw);
	glTexCoord2f(0.0, 0.0);
	glVertex3f(p.x + p2.x*hw, p.y - hh, p.z + p2.z*hw);
	glTexCoord2f(0.0, 1.0);
	glVertex3f(p.x + p1.x*hw, p.y - hh, p.z + p1.z*hw);
	glTexCoord2f(1.0, 1.0);
	glVertex3f(p.x + p1.x*hw, p.y + hh, p.z + p1.z*hw);
	glTexCoord2f(1.0, 0.0);
	
	glEnd;
	
	glEnable(GL_CULL_FACE);
end;

procedure DrawSprite2(const p : TVec3; const d : TVec3; w, h : Single);
var
	p1, p2 : TVec3;
	p3, nd : TVec3;
	hw, hh : Single;
begin
	hw := w/2;
	hh := h/2;

	p1.x := -d.z;
	p1.y := 0.0;
	p1.z := d.x;
	Normalize(p1);
	
	p2.x := d.z;
	p2.y := 0.0;
	p2.z := -d.x;
	Normalize(p2);
	
	nd := d;
	Normalize(nd);
	
	Cross(p3, p1, nd);
	
	glDisable(GL_CULL_FACE);
	
	glBegin(GL_QUADS);
	
	glVertex3f(p.x + p2.x*hw + p3.x*hh, p.y + p3.y*hh, p.z + p2.z*hw + p3.z*hh);
	glTexCoord2f(0.0, 0.0);
	glVertex3f(p.x + p2.x*hw - p3.x*hh, p.y - p3.y*hh, p.z + p2.z*hw - p3.z*hh);
	glTexCoord2f(0.0, 1.0);
	glVertex3f(p.x + p1.x*hw - p3.x*hh, p.y - p3.y*hh, p.z + p1.z*hw - p3.z*hh);
	glTexCoord2f(1.0, 1.0);
	glVertex3f(p.x + p1.x*hw + p3.x*hh, p.y + p3.y*hh, p.z + p1.z*hw + p3.z*hh);
	glTexCoord2f(1.0, 0.0);

{
	glVertex3f(p.x + hw, p.y + p4.y*hh, p.z + p4.z*hh);
	glTexCoord2f(0.0, 0.0);
	glVertex3f(p.x + hw, p.y + p3.y*hh, p.z + p3.z*hh);
	glTexCoord2f(0.0, 1.0);
	glVertex3f(p.x - hw, p.y + p3.y*hh, p.z + p3.z*hh);
	glTexCoord2f(1.0, 1.0);
	glVertex3f(p.x - hw, p.y + p4.y*hh, p.z + p4.z*hh);
	glTexCoord2f(1.0, 0.0);
}
	glEnd;
	
	glEnable(GL_CULL_FACE);
end;
