unit PHGroups;

interface

const
	PH_GROUP_DEFAULT             : Longword = 0;
	PH_GROUP_MANIPULATOR         : Longword = 10;
	PH_GROUP_SHAPE               : Longword = 11;
	PH_GROUP_ENV_ZONE            : Longword = 12;
	PH_GROUP_UCOVER              : Longword = 13;
	PH_GROUP_UCOVER_LINK         : Longword = 14;
	PH_GROUP_UCOVER_LINK_PE      : Longword = 15;
	PH_GROUP_HUMAN_COMBAT_COVER  : Longword = 16;
	
	PH_GROUP_DEFAULT_MASK        : Longword = 1;
	PH_GROUP_MANIPULATOR_MASK    : Longword = 1 shl 10;
	PH_GROUP_SHAPE_MASK          : Longword = 1 shl 11;
	PH_GROUP_ENV_ZONE_MASK       : Longword = 1 shl 12;
	PH_GROUP_UCOVER_MASK         : Longword = 1 shl 13;
	PH_GROUP_UCOVER_LINK_MASK    : Longword = 1 shl 14;
	PH_GROUP_UCOVER_LINK_PE_MASK : Longword = 1 shl 15;
	PH_GROUP_HUMAN_COMBAT_COVER_MASK : Longword = 1 shl 16;
	
implementation

end.
