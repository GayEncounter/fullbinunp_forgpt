{$POINTERMATH ON}
unit aiColor4D;

interface
uses aiTypes;

const AI_MAX_NUMBER_OF_COLOR_SETS = $04;

type TaiColor4D = packed record
   r, g, b, a: single;
end;
type PaiColor4D = ^TaiColor4D;
type PTaiColor4DArray = ^TaiColor4D;

implementation

end.
