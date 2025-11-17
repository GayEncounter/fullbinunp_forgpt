unit uModelEditorGlobals;

interface
uses common, fouramdl;

var
	edit_model : T4AModel;
	maler : IMaler;

	current_mtlset : Longint;
	mtlsets : P4AMaterialSetArray; // pointer to actual array in model
	
procedure UpdateMaler;

implementation
uses sysutils;

procedure UpdateMaler;
var
	mh : T4AModelHierrarhy;
	ms : T4AModelSkeleton;
	mb : T4AModelSoftbody;
	mk : T4AModelSkinned;
begin
	FreeAndNil(maler);
	
	if edit_model is T4AModelHierrarhy then
	begin
		mh := T4AModelHierrarhy(edit_model);
		maler := TStaticModelMaler.Create(mh);
	end else 
	if edit_model is T4AModelSkeleton then
	begin
		ms := T4AModelSkeleton(edit_model);
		maler := TSkeletonModelMaler.Create(ms);
	end else 
	if edit_model is T4AModelSoftBody then
	begin
		mb := T4AModelSoftBody(edit_model);
		maler := TSoftbodyModelMaler.Create(mb);
	end else
	if edit_model is T4AModelSkinned then
	begin
		mk := T4AModelSkinned(edit_model);
		maler := TSkinnedModelMaler.Create(nil, mk);
	end;
end;

end.