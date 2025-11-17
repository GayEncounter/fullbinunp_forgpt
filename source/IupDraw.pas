unit IupDraw;

interface
uses Iup;

// all functions can be used only in IUP canvas and inside the ACTION callback 

procedure IupDrawBegin(ih : Ihandle); cdecl; external 'iup.dll';
procedure IupDrawEnd(ih : Ihandle); cdecl; external 'iup.dll';

// all functions can be called only between calls to Begin and End 

procedure IupDrawSetClipRect(ih : Ihandle; x1, y1, x2, y2 : Longint); cdecl; external 'iup.dll';
procedure IupDrawGetClipRect(ih : Ihandle; x1, y1, x2, y2 : PLongint); cdecl; external 'iup.dll';
procedure IupDrawResetClip(ih : Ihandle); cdecl; external 'iup.dll';

// color controlled by the attribute DRAWCOLOR
// line style or fill controlled by the attribute DRAWSTYLE

procedure IupDrawParentBackground(ih : Ihandle); cdecl; external 'iup.dll';
procedure IupDrawLine(ih : Ihandle; x1, y1, x2, y2 : Longint); cdecl; external 'iup.dll';
procedure IupDrawRectangle(ih : Ihandle; x1, y1, x2, y2 : Longint); cdecl; external 'iup.dll';
procedure IupDrawArc(ih : Ihandle; x1, y1, x2, y2 : Longint; a1, a2 : Double); cdecl; external 'iup.dll';
procedure IupDrawPolygon(ih : Ihandle; points : PLongint; count : Longint); cdecl; external 'iup.dll';
procedure IupDrawText(ih : Ihandle; _text : PAnsiChar; len, x, y, w, h : Longint); cdecl; external 'iup.dll';
procedure IupDrawImage(ih : Ihandle; name : PAnsiChar; len, x, y, w, h : Longint); cdecl; external 'iup.dll';
procedure IupDrawSelectRect(ih : Ihandle; x1, y1, x2, y2 : Longint); cdecl; external 'iup.dll';
procedure IupDrawFocusRect(ih : Ihandle; x1, y1, x2, y2 : Longint); cdecl; external 'iup.dll';

procedure IupDrawGetSize(ih : Ihandle; w, h : PLongint); cdecl; external 'iup.dll';
procedure IupDrawGetTextSize(ih : Ihandle; _text : PAnsiChar; len : Longint; w, h : PLongint); cdecl; external 'iup.dll';
procedure IupDrawGetImageInfo(name : PAnsiChar; w, h, bpp : PLongint); cdecl; external 'iup.dll';

implementation

end.
