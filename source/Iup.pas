unit Iup;

interface
uses classes;

type
  PPPAnsiChar = ^PPAnsiChar;

  Ihandle = Pointer;
  PIhandle = ^Ihandle;

  Icallback = function(ih : Ihandle) : Longint; cdecl;
  Iparamcb = function(dialog : Ihandle; param_index : Longint; userdata : Pointer) : Longint; cdecl;

const
  IUP_IGNORE    = -1;
  IUP_DEFAULT   = -2;
  IUP_CLOSE     = -3;
  IUP_CONTINUE  = -4;

  IUP_CENTER        = $FFFF;
  IUP_LEFT          = $FFFE;
  IUP_RIGHT         = $FFFD;
  IUP_MOUSEPOS      = $FFFC;
  IUP_CURRENT       = $FFFB;
  IUP_CENTERPARENT  = $FFFA;
  IUP_TOP           = IUP_LEFT;
  IUP_BOTTOM        = IUP_RIGHT;

  IUP_MASK_FLOAT        = '[+/-]?(/d+/.?/d*|/./d+)';
  IUP_MASK_UFLOAT		    = '(/d+/.?/d*|/./d+)';
  IUP_MASK_EFLOAT		    = '[+/-]?(/d+/.?/d*|/./d+)([eE][+/-]?/d+)?';
  IUP_MASK_UEFLOAT		  = '(/d+/.?/d*|/./d+)([eE][+/-]?/d+)?';
  IUP_MASK_FLOATCOMMA	  = '[+/-]?(/d+/,?/d*|/,/d+)';
  IUP_MASK_UFLOATCOMMA	= '(/d+/,?/d*|/,/d+)';
  IUP_MASK_INT			    = '[+/-]?/d+';
  IUP_MASK_UINT			    = '/d+';

function  IupOpen(argc : PLongint; argv : PPPAnsiChar) : Longint; cdecl; external 'iup.dll';
procedure IupClose; cdecl; external 'iup.dll';

function  IupMainLoop : Longint; cdecl; external 'iup.dll';
function  IupLoopStep : Longint; cdecl; external 'iup.dll';
function  IupLoopStepWait : Longint; cdecl; external 'iup.dll';
function  IupMainLoopLevel : Longint; cdecl; external 'iup.dll';
procedure IupFlush; cdecl; external 'iup.dll';
procedure IupExitLoop; cdecl; external 'iup.dll';
procedure IupPostMessage(ih : Ihandle; s : PAnsiChar; i : Longint; d : Double; p : Pointer); cdecl; external 'iup.dll';

procedure IupUpdate(ih : Ihandle); cdecl; external 'iup.dll';
procedure IupUpdateChildren(ih : Ihandle); cdecl; external 'iup.dll';
procedure IupRedraw(ih : Ihandle; children : Longint); cdecl; external 'iup.dll';
procedure IupRefresh(ih : Ihandle); cdecl; external 'iup.dll';
procedure IupRefreshChildren(ih : Ihandle); cdecl; external 'iup.dll';

function  IupLoad(filename : PAnsiChar) : PAnsiChar; cdecl; external 'iup.dll';
function  IupLoadBuffer(buffer : PAnsiChar) : PAnsiChar; cdecl; external 'iup.dll';

procedure IupDestroy(ih : Ihandle); cdecl; external 'iup.dll';

function  IupGetNextChild(ih, child : Ihandle) : Ihandle; cdecl; external 'iup.dll';
function  IupGetBrother(ih : Ihandle) : Ihandle; cdecl; external 'iup.dll';
function  IupGetParent(ih : Ihandle) : Ihandle; cdecl; external 'iup.dll';
function  IupGetDialog(ih : Ihandle) : Ihandle; cdecl; external 'iup.dll';
function  IupGetDialogChild(ih : Ihandle; name : PAnsiChar) : Ihandle; cdecl; external 'iup.dll';

function  IupPopup(ih : Ihandle; x, y : Longint) : Longint; cdecl; external 'iup.dll';
function  IupShowXY(ih : Ihandle; x, y : Longint) : Longint; cdecl; external 'iup.dll';
function  IupShow(ih : Ihandle) : Longint; cdecl; external 'iup.dll';
function  IupHide(ih : Ihandle) : Longint; cdecl; external 'iup.dll';
function  IupMap(ih : Ihandle) : Longint; cdecl; external 'iup.dll';
procedure IupUnmap(ih : Ihandle); cdecl; external 'iup.dll';

function  IupSetAttributes(ih : Ihandle; attributes : PAnsiChar) : Ihandle; cdecl; external 'iup.dll';
function  IupGetAttributes(ih : Ihandle) : PAnsiChar; cdecl; external 'iup.dll';

procedure IupSetAttribute(ih : Ihandle; name, value : PAnsiChar); cdecl; external 'iup.dll';
procedure IupSetStrAttribute(ih : Ihandle; name, value : PAnsiChar); cdecl; external 'iup.dll';
procedure IupSetStrf(ih : Ihandle; name, format : PAnsiChar); cdecl; external 'iup.dll'; varargs;
procedure IupSetInt(ih : Ihandle; name : PAnsiChar; value : Longint); cdecl; external 'iup.dll';

function  IupGetAttribute(ih : Ihandle; name : PAnsiChar) : PAnsiChar; cdecl; external 'iup.dll';
function  IupGetInt(ih : Ihandle; name : PAnsiChar) : Longint; cdecl; external 'iup.dll';

procedure IupSetAttributeId(ih : Ihandle; name : PAnsiChar; id : Longint; value : PAnsiChar); cdecl; external 'iup.dll';
procedure IupSetStrAttributeId(ih : Ihandle; name : PAnsiChar; id : Longint; value : PAnsiChar); cdecl; external 'iup.dll';
procedure IupSetStrfId(ih : Ihandle; name : PAnsiChar; id : Longint; format : PAnsiChar); cdecl; external 'iup.dll'; varargs;
procedure IupSetIntId(ih : Ihandle; name : PAnsiChar; id : Longint; value : Longint); cdecl; external 'iup.dll';

function  IupGetAttributeId(ih : Ihandle; name : PAnsiChar; id : Longint) : PAnsiChar; cdecl; external 'iup.dll';
function  IupGetIntId(ih : Ihandle; name : PAnsiChar; id : Longint) : Longint; cdecl; external 'iup.dll';

function  IupGetCallback(ih : Ihandle; name : PAnsiChar) : Icallback; cdecl; external 'iup.dll';
function  IupSetCallback(ih : Ihandle; name : PAnsiChar; func : Icallback) : Icallback; cdecl; external 'iup.dll';
function  IupSetCallbacks(ih : Ihandle; name : PAnsiChar; func : Icallback) : PIhandle; cdecl; external 'iup.dll'; varargs;

function  IupGetFunction(name : PAnsiChar) : Icallback; cdecl; external 'iup.dll';
function  IupSetFunction(name : PAnsiChar; func : Icallback) : Icallback; cdecl; external 'iup.dll'; 

function  IupGetHandle(name : PAnsiChar) : Ihandle; cdecl; external 'iup.dll';
function  IupSetHandle(name : PAnsiChar; ih : Ihandle) : Ihandle; cdecl; external 'iup.dll';

procedure IupSetAttributeHandle(ih : Ihandle; name : PAnsiChar; ih_named : Ihandle); cdecl; external 'iup.dll';
function  IupGetAttributeHandle(ih : Ihandle; name : PAnsiChar) : Ihandle; cdecl; external 'iup.dll';

function  IupGetClassName(ih : Ihandle) : PAnsiChar; cdecl; external 'iup.dll';
function  IupGetClassType(ih : Ihandle) : PAnsiChar; cdecl; external 'iup.dll';
function  IupGetAllClasses(names : PPAnsiChar; n : Longint) : Longint; cdecl; external 'iup.dll';
function  IupGetClassAttributes(classname : PAnsiChar; names : PPAnsiChar; n : Longint) : Longint; cdecl; external 'iup.dll';
function  IupGetClassCallbacks(classname : PAnsiChar; names : PPAnsiChar; n : Longint) : Longint; cdecl; external 'iup.dll';
procedure IupSaveClassAttributes(ih : Ihandle); cdecl; external 'iup.dll';
procedure IupCopyClassAttributes(src_ih, dst_id : Ihandle); cdecl; external 'iup.dll';
procedure IupSetClassDefaultAttribute(classname, name, value : PAnsiChar); cdecl; external 'iup.dll';
function  IupClassMatch(ih : Ihandle; classname : PAnsiChar) : Longint; cdecl; external 'iup.dll';

{************************************************************************}
{*                        Elements                                      *}
{************************************************************************}

function  IupFill : Ihandle; cdecl; external 'iup.dll';
function  IupSpace : Ihandle; cdecl; external 'iup.dll';

function  IupRadio(child : Ihandle) : Ihandle; cdecl; external 'iup.dll';
function  IupVbox(child : Ihandle) : Ihandle; cdecl; external 'iup.dll'; varargs;
function  IupVboxv(child : PIhandle) : Ihandle; cdecl; external 'iup.dll';
function  IupZbox(child : Ihandle) : Ihandle; cdecl; external 'iup.dll'; varargs;
function  IupZboxv(child : PIhandle) : Ihandle; cdecl; external 'iup.dll';
function  IupHbox(child : Ihandle) : Ihandle; cdecl; external 'iup.dll'; varargs;
function  IupHboxv(child : PIhandle) : Ihandle; cdecl; external 'iup.dll';

function  IupNormalizer(ih_first : Ihandle) : Ihandle; cdecl; external 'iup.dll'; varargs;
function  IupNormalizerv(ih_list : PIhandle) : Ihandle; cdecl; external 'iup.dll';

function  IupCbox(child : Ihandle) : Ihandle; cdecl; external 'iup.dll'; varargs;
function  IupCboxv(children : PIhandle) : Ihandle; cdecl; external 'iup.dll';
function  IupSbox(child : Ihandle) : Ihandle; cdecl; external 'iup.dll';
function  IupSplit(child1, child2 : Ihandle) : Ihandle; cdecl; external 'iup.dll';
function  IupScrollBox(child : Ihandle) : Ihandle; cdecl; external 'iup.dll';
function  IupFlatScrollBox(child : Ihandle) : Ihandle; cdecl; external 'iup.dll';
function  IupGridBox(child : Ihandle) : Ihandle; cdecl; external 'iup.dll'; varargs;
function  IupGridBoxv(children : PIhandle) : Ihandle; cdecl; external 'iup.dll';
function  IupMultiBox(child : Ihandle) : Ihandle; cdecl; external 'iup.dll'; varargs;
function  IupMultiBoxv(children : PIhandle) : Ihandle; cdecl; external 'iup.dll';
function  IupExpander(child : Ihandle) : Ihandle; cdecl; external 'iup.dll';
function  IupDetachBox(child : Ihandle) : Ihandle; cdecl; external 'iup.dll';
function  IupBackgroundBox(child : Ihandle) : Ihandle; cdecl; external 'iup.dll';

function  IupFrame(child : Ihandle) : Ihandle; cdecl; external 'iup.dll';
function  IupFlatFrame(child : Ihandle) : Ihandle; cdecl; external 'iup.dll';

function  IupImage(width, height : Longint; pixmap : Pointer) : Ihandle; cdecl; external 'iup.dll';
function  IupImageRGB(width, height : Longint; pixmap : Pointer) : Ihandle; cdecl; external 'iup.dll';
function  IupImageRGBA(width, height : Longint; pixmap : Pointer) : Ihandle; cdecl; external 'iup.dll';

function  IupItem(title, action : PAnsiChar) : Ihandle; cdecl; external 'iup.dll';
function  IupSubmenu(title : PAnsiChar; child : Ihandle) : Ihandle; cdecl; external 'iup.dll';
function  IupSeparator : Ihandle; cdecl; external 'iup.dll';
function  IupMenu(child : Ihandle) : Ihandle; cdecl; external 'iup.dll'; varargs;
function  IupMenuv(chilren : PIhandle) : Ihandle; cdecl; external 'iup.dll';

function  IupButton(title, action : PAnsiChar) : Ihandle; cdecl; external 'iup.dll';
function  IupFlatButton(title : PAnsiChar) : Ihandle; cdecl; external 'iup.dll';
function  IupFlatToggle(title : PAnsiChar) : Ihandle; cdecl; external 'iup.dll';
function  IupDropButton(dropchild : Ihandle) : Ihandle; cdecl; external 'iup.dll';
function  IupFlatLabel(title : PAnsiChar) : Ihandle; cdecl; external 'iup.dll';
function  IupFlatSeparator : Ihandle; cdecl; external 'iup.dll';
function  IupCanvas(action : PAnsiChar) : Ihandle; cdecl; external 'iup.dll';
function  IupDialog(child : Ihandle) : Ihandle; cdecl; external 'iup.dll';
function  IupUser : Ihandle; cdecl; external 'iup.dll';
function  IupLabel(title : PAnsiChar) : Ihandle; cdecl; external 'iup.dll';
function  IupList(action : PAnsiChar) : Ihandle; cdecl; external 'iup.dll';
function  IupText(action : PAnsiChar) : Ihandle; cdecl; external 'iup.dll';
function  IupMultiLine(action : PAnsiChar) : Ihandle; cdecl; external 'iup.dll';
function  IupToggle(title, action : PAnsiChar) : Ihandle; cdecl; external 'iup.dll';
function  IupTimer : Ihandle; cdecl; external 'iup.dll';
function  IupClipboard : Ihandle; cdecl; external 'iup.dll';
function  IupProgressBar : Ihandle; cdecl; external 'iup.dll';
function  IupVal(orientation : PAnsiChar) : Ihandle; cdecl; external 'iup.dll';
function  IupFlatVal(orientation : PAnsiChar) : Ihandle; cdecl; external 'iup.dll';
function  IupFlatTree : Ihandle; cdecl; external 'iup.dll';
function  IupTabs(child : Ihandle) : Ihandle; cdecl; external 'iup.dll'; varargs;
function  IupTabsv(children : PIhandle) : Ihandle; cdecl; external 'iup.dll';
function  IupFlatTabs(first : Ihandle) : Ihandle; cdecl; external 'iup.dll'; varargs;
function  IupFlatTabsv(children : PIhandle) : Ihandle; cdecl; external 'iup.dll';
function  IupTree : Ihandle; cdecl; external 'iup.dll';
function  IupLink(url, title : PAnsiChar) : Ihandle; cdecl; external 'iup.dll';
function  IupAnimatedLabel(animation : Ihandle) : Ihandle; cdecl; external 'iup.dll';
function  IupDatePick : Ihandle; cdecl; external 'iup.dll';
function  IupCalendar : Ihandle; cdecl; external 'iup.dll';
function  IupColorbar : Ihandle; cdecl; external 'iup.dll';
function  IupGauge : Ihandle; cdecl; external 'iup.dll';
function  IupDial(orientation : PAnsiChar) : Ihandle; cdecl; external 'iup.dll';
function  IupColorBrowser : Ihandle; cdecl; external 'iup.dll';

function  IupConvertXYToPos(ih : Ihandle; x, y : Longint) : Longint; cdecl; external 'iup.dll';

function  IupFileDlg : Ihandle; cdecl; external 'iup.dll';
function  IupMessageDlg : Ihandle; cdecl; external 'iup.dll';
function  IupColorDlg : Ihandle; cdecl; external 'iup.dll';
function  IupFontDlg : Ihandle; cdecl; external 'iup.dll';
function  IupProgressDlg : Ihandle; cdecl; external 'iup.dll';

procedure IupMessage(title, msg : PAnsiChar); cdecl; external 'iup.dll';
procedure IupMessagef(title, msg : PAnsiChar); cdecl; external 'iup.dll'; varargs;
procedure IupMessageError(parent : Ihandle; message : PAnsiChar); cdecl; external 'iup.dll'; 
function  IupMessageAlarm(parent : Ihandle; title, message, buttons : PAnsiChar) : Longint; cdecl; external 'iup.dll';
function  IupAlarm(title, msg, b1, b2, b3 : PAnsiChar) : Longint; cdecl; external 'iup.dll';
function  IupScanf(format : PAnsiChar) : Longint; cdecl; external 'iup.dll'; varargs;
function  IupListDialog(_type : Longint; title : PAnsiChar; size : Longint; list : PPAnsiChar; op, max_col, max_lin : Longint; marks : PAnsiChar) : Longint; cdecl; external 'iup.dll';
function  IupGetText(title : PAnsiChar; _text : PAnsiChar; maxlen : Longint) : Longint; cdecl; external 'iup.dll'; 
function  IupGetColor(x, y : Longint; r, g, b : PByte) : Longint; cdecl; external 'iup.dll'; 

function  IupGetParam(title : PAnsiChar; action : Iparamcb; userdata : Pointer; format : PAnsiChar) : Longint; cdecl; external 'iup.dll'; varargs;
function  IupGetParamv(title : PAnsiChar; action : Iparamcb; userdata : Pointer; format : PAnsiChar; param_count, param_extra : Longint; param_data : PPointer) : Longint; cdecl; external 'iup.dll';

// OpenGL functions

procedure IupGLCanvasOpen; cdecl; external 'iupgl.dll';

function  IupGLCanvas(action : PAnsiChar) : Ihandle; cdecl; external 'iupgl.dll';
function  IupGLBackgroundBox(child : Ihandle) : Ihandle; cdecl; external 'iupgl.dll';

procedure IupGLMakeCurrent(ih : Ihandle); cdecl; external 'iupgl.dll';
function  IupGLIsCurrent(ih : Ihandle) : Longint; cdecl; external 'iupgl.dll';
procedure IupGLSwapBuffers(ih : Ihandle); cdecl; external 'iupgl.dll';
procedure IupGLPalette(ih : Ihandle; index : Longint; r, g, b : Single); cdecl; external 'iupgl.dll';
procedure IupGLUseFont(ih : Ihandle; first, count, list_base : Longint); cdecl; external 'iupgl.dll';
procedure IupGLWait(gl : Longint); cdecl; external 'iupgl.dll';

// OpenGL controls functions

function  IupGLControlsOpen : Longint;  cdecl; external 'iupglcontrols.dll';

function  IupGLCanvasBoxv(children : PIhandle) : Ihandle; cdecl; external 'iupglcontrols.dll';
function  IupGLCanvasBox(child : Ihandle) : Ihandle; cdecl; external 'iupglcontrols.dll'; varargs;

function  IupGLSubCanvas : Ihandle; cdecl; external 'iupglcontrols.dll';
function  IupGLLabel(title : PAnsiChar) : Ihandle; cdecl; external 'iupglcontrols.dll';
function  IupGLSeparator : Ihandle; cdecl; external 'iupglcontrols.dll';
function  IupGLButton(title : PAnsiChar) : Ihandle; cdecl; external 'iupglcontrols.dll';
function  IupGLToggle(title : PAnsiChar) : Ihandle; cdecl; external 'iupglcontrols.dll';
function  IupGLLink(title : PAnsiChar) : Ihandle; cdecl; external 'iupglcontrols.dll';
function  IupGLProgressBar : Ihandle; cdecl; external 'iupglcontrols.dll';
function  IupGLVal : Ihandle; cdecl; external 'iupglcontrols.dll';
function  IupGLFrame(child : Ihandle) : Ihandle; cdecl; external 'iupglcontrols.dll';
function  IupGLExpander(child : Ihandle) : Ihandle; cdecl; external 'iupglcontrols.dll';
function  IupGLScrollBox(child : Ihandle) : Ihandle; cdecl; external 'iupglcontrols.dll';
function  IupGLSizeBox(child : Ihandle) : Ihandle; cdecl; external 'iupglcontrols.dll';
function  IupGLText : Ihandle; cdecl; external 'iupglcontrols.dll';

procedure IupGLDrawImage(ih : Ihandle; name : PAnsiChar; x, y, active : Longint); cdecl; external 'iupglcontrols.dll';
procedure IupGLDrawText(ih : Ihandle; str : PAnsiChar; len, x, y : Longint); cdecl; external 'iupglcontrols.dll';
procedure IupGLDrawGetTextSize(ih : Ihandle; str : PAnsiChar; w, h : PLongint); cdecl; external 'iupglcontrols.dll';
procedure IupGLDrawGetImageInfo(name : PAnsiChar; w, h, bpp : PLongint); cdecl; external 'iupglcontrols.dll';

// helper functions
function  iup_isshift(_s : PAnsiChar) : Boolean;
function  iup_iscontrol(_s : PAnsiChar) : Boolean;
function  iup_isbutton1(_s : PAnsiChar) : Boolean;
function  iup_isbutton2(_s : PAnsiChar) : Boolean;
function  iup_isbutton3(_s : PAnsiChar) : Boolean;
function  iup_isdouble(_s : PAnsiChar) : Boolean;
function  iup_isalt(_s : PAnsiChar) : Boolean;
function  iup_issys(_s : PAnsiChar) : Boolean;
function  iup_isbutton4(_s : PAnsiChar) : Boolean;
function  iup_isbutton5(_s : PAnsiChar) : Boolean;

{******************************************************}
{* Modifiers use last 4 bits. Since IUP 3.9           *}
{* These modifiers definitions are specific to IUP    *}
{******************************************************}

function  iup_isShiftXkey(_c : Longint) : Boolean;
function  iup_isCtrlXkey(_c : Longint) : Boolean;
function  iup_isAltXkey(_c : Longint) : Boolean;
function  iup_isSysXkey(_c : Longint) : Boolean;

// my extensions. Will be called as iup.MenuItem()
procedure SetAttribute(ih : Ihandle; const name : String; value : PAnsiChar);
procedure SetStrAttribute(ih : Ihandle; const name, value : String);
procedure SetInt(ih : Ihandle; const name : String; value : Longint);

function  GetAttribute(ih : Ihandle; const name : String) : PAnsiChar;
function  GetInt(ih : Ihandle; const name : String) : Longint;

function Button(const title : String; callback : Icallback) : Ihandle;
function MenuItem(const title : String; callback : Icallback; state : Boolean = False) : Ihandle;
function Toggle(const title : String; callback : Icallback; state : Boolean = False) : Ihandle; 
function AutoToggle(const title : String; value : PBoolean) : Ihandle;

function ListDialog(const title : String; list : array of String; op, max_lin, max_col : Longint) : Longint; overload;
function ListDialogMulti(const title : String; list : array of String; op, max_lin, max_col : Longint; var marks_str : String) : Longint; overload;

function ListDialog(const title : String; list : TStringList; op, max_lin, max_col : Longint) : Longint; overload;
function ListDialogMulti(const title : String; list : TStringList; op, max_lin, max_col : Longint; var marks_str : String) : Longint; overload;

implementation

function iup_isshift(_s : PAnsiChar) : Boolean;
begin Result := _s[0] = 'S'; end;
function iup_iscontrol(_s : PAnsiChar) : Boolean;
begin Result := _s[1] = 'C'; end;
function iup_isbutton1(_s : PAnsiChar) : Boolean;
begin Result := _s[2] = '1'; end;
function iup_isbutton2(_s : PAnsiChar) : Boolean;
begin Result := _s[3] = '2'; end;
function iup_isbutton3(_s : PAnsiChar) : Boolean;
begin Result := _s[4] = '3'; end;
function iup_isdouble(_s : PAnsiChar) : Boolean;
begin Result := _s[5] = 'D'; end;
function iup_isalt(_s : PAnsiChar) : Boolean;
begin Result := _s[6] = 'A'; end;
function iup_issys(_s : PAnsiChar) : Boolean;
begin Result := _s[7] = 'Y'; end;
function iup_isbutton4(_s : PAnsiChar) : Boolean;
begin Result := _s[8] = '4'; end;
function iup_isbutton5(_s : PAnsiChar) : Boolean;
begin Result := _s[9] = '5'; end;

function  iup_isShiftXkey(_c : Longint) : Boolean;
begin Result := (_c and $10000000) <> 0 end;
function  iup_isCtrlXkey(_c : Longint) : Boolean;
begin Result := (_c and $20000000) <> 0 end;
function  iup_isAltXkey(_c : Longint) : Boolean;
begin Result := (_c and $40000000) <> 0 end;
function  iup_isSysXkey(_c : Longint) : Boolean;
begin Result := (_c and $80000000) <> 0 end;

procedure SetAttribute(ih : Ihandle; const name : String; value : PAnsiChar);
begin
	IupSetAttribute(ih, PAnsiChar(name), PAnsiChar(value));
end;

procedure SetStrAttribute(ih : Ihandle; const name, value : String);
begin
	IupSetStrAttribute(ih, PAnsiChar(name), PAnsiChar(value));
end;

procedure SetInt(ih : Ihandle; const name : String; value : Longint);
begin
	IupSetInt(ih, PAnsiChar(name), value);
end;

function GetAttribute(ih : Ihandle; const name : String) : PAnsiChar;
begin
	Result := IupGetAttribute(ih, PAnsiChar(name));
end;

function GetInt(ih : Ihandle; const name : String) : Longint;
begin
	Result := IupGetInt(ih, PAnsiChar(name));
end;

function Button(const title : String; callback : Icallback) : Ihandle;
var
	ih : Ihandle;
begin
	ih := IupButton(PAnsiChar(title), nil);
	IupSetCallback(ih, 'ACTION', callback);
	Result := ih;
end;

function MenuItem(const title : String; callback : Icallback; state : Boolean = False) : Ihandle;
var
	ih : Ihandle;
begin
	ih := IupItem(PAnsiChar(title), nil);
	IupSetCallback(ih, 'ACTION', callback);
	IupSetInt(ih, 'VALUE', Longint(state));
	Result := ih;
end;

function Toggle(const title : String; callback : Icallback; state : Boolean = False) : Ihandle; 
var
	ih : Ihandle;
begin
	ih := IupToggle(PAnsiChar(title), nil);
	IupSetCallback(ih, 'ACTION', callback);
	IupSetInt(ih, 'VALUE', Longint(state));
	Result := ih;
end;

function auto_toggle_cb(ih : Ihandle) : Longint; cdecl;
var
	value_ptr : PBoolean;
begin
	value_ptr := PBoolean(IupGetAttribute(ih, 'VALUE_PTR'));
	value_ptr^ := not (value_ptr^);
	Result := IUP_DEFAULT;
end;

function AutoToggle(const title : String; value : PBoolean) : Ihandle;
var
	ih : Ihandle;
begin
	ih := IupToggle(PAnsiChar(title), nil);
	IupSetCallback(ih, 'ACTION', @auto_toggle_cb);
	IupSetInt(ih, 'VALUE', Longint(value^));
	IupSetAttribute(ih, 'VALUE_PTR', Pointer(value));
	Result := ih;
end;
	

function ListDialog(const title : String; list : array of String; op, max_lin, max_col : Longint) : Longint;
var
	I : Longint;
	pointers : array of PAnsiChar;
begin
	SetLength(pointers, Length(list));
	for I := 0 to Length(list) - 1 do
		pointers[I] := PAnsiChar(list[I]);
		
	Result := IupListDialog(1, PAnsiChar(title), Length(pointers), PPAnsiChar(pointers), op, max_lin, max_col, nil);
end;

function ListDialogMulti(const title : String; list : array of String; op, max_lin, max_col : Longint; var marks_str : String) : Longint;
var
	I : Longint;
	pointers : array of PAnsiChar;
	marks : array of Longint;
begin
	SetLength(marks_str, Length(list));

	SetLength(pointers, Length(list));
	SetLength(marks, Length(marks_str));
	
	for I := 0 to Length(list) - 1 do
	begin
		pointers[I] := PAnsiChar(list[I]);
		if marks_str[I+1] = '+' then
			marks[I] := 1
		else
			marks[I] := 0;
	end;
		
	Result := IupListDialog(2, PAnsiChar(title), Length(pointers), PPAnsiChar(pointers), op, max_lin, max_col, @marks[0]);
	
	for I := 0 to Length(list) - 1 do
	begin
		if marks[I] <> 0 then
			marks_str[I+1] := '+'
		else
			marks_str[I+1] := '-';
	end;	
end;

function ListDialog(const title : String; list : TStringList; op, max_lin, max_col : Longint) : Longint;
var
	I : Longint;
	pointers : array of PAnsiChar;
begin
	SetLength(pointers, list.Count);
	for I := 0 to list.Count - 1 do
		pointers[I] := PAnsiChar(list[I]);
		
	Result := IupListDialog(1, PAnsiChar(title), Length(pointers), PPAnsiChar(pointers), op, max_lin, max_col, nil);
end;

function ListDialogMulti(const title : String; list : TStringList; op, max_lin, max_col : Longint; var marks_str : String) : Longint;
var
	I : Longint;
	pointers : array of PAnsiChar;
	marks : array of Longint;
begin
	SetLength(marks_str, list.Count);

	SetLength(pointers, list.Count);
	SetLength(marks, Length(marks_str));
	
	for I := 0 to list.Count - 1 do
	begin
		pointers[I] := PAnsiChar(list[I]);
		if marks_str[I+1] = '+' then
			marks[I] := 1
		else
			marks[I] := 0;
	end;
		
	Result := IupListDialog(2, PAnsiChar(title), Length(pointers), PPAnsiChar(pointers), op, max_lin, max_col, @marks[0]);
	
	for I := 0 to list.Count - 1 do
	begin
		if marks[I] <> 0 then
			marks_str[I+1] := '+'
		else
			marks_str[I+1] := '-';
	end;	
end;

end.
