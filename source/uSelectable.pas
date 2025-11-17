unit uSelectable;

interface
uses vmath;

type
	ISelectable = class
		FSelected : Boolean;
		property Selected : Boolean read FSelected write FSelected;
		
		procedure GetBBox(out b : TAABB); virtual; abstract;
	end;

type
	TSelectableArray = array of ISelectable;

implementation

end.