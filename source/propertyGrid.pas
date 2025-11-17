// rewritten from propertyGrid.h, all original comments retained

//////////////////////////////////////////////////////////////////////////////
///
/// @file propertyGrid.h
///
/// @brief A property grid control in Win32 SDK C.
///
/// @author David MacDermot
///
/// @par Comments:
///         This source is distributed in the hope that it will be useful,
///         but WITHOUT ANY WARRANTY; without even the implied warranty of
///         MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
/// 
/// @date 4-16-21
///
/// @version 2.3
///
/// @todo 
///
/// @bug 
///
//////////////////////////////////////////////////////////////////////////////

unit propertyGrid;

interface
uses Windows;

const
	WC_PROPERTYGRIDCTLA : AnsiString = 'PropGridCtl';
	WC_PROPERTYGRIDCTLW : WideString = 'PropGridCtl';

{$IFDEF UNICODE}
	WC_PROPERTYGRIDCTL : WideString = 'PropGridCtl';
	PROPERTYGRIDDLLNAME = 'propertyGridUC.dll';
{$ELSE}
	WC_PROPERTYGRIDCTL : AnsiString = 'PropGridCtl';
	PROPERTYGRIDDLLNAME = 'propertyGrid.dll';
{$ENDIF}

{****************************************************************************}
// Public Messages

// List box message subset handled by the property grid.

// LB_ADDSTRING - PropGrid_AddItem()
// LB_DELETESTRING - PropGrid_DeleteItem()
// LB_GETCOUNT - PropGrid_GetCount()
// LB_GETCURSEL - PropGrid_GetCurSel()
// LB_GETHORIZONTALEXTENT - PropGrid_GetHorizontalExtent()
// LB_GETITEMDATA - PropGrid_GetItemData()
// LB_GETITEMHEIGHT - PropGrid_GetItemHeight()
// LB_GETITEMRECT - PropGrid_GetItemRect()
// LB_GETSEL - PropGrid_GetSel()
// LB_RESETCONTENT - PropGrid_ResetContent()
// LB_SETCURSEL - PropGrid_SetCurSel()
// LB_SETHORIZONTALEXTENT - PropGrid_SetHorizontalExtent()
// LB_SETITEMDATA - PropGrid_SetItemData()
// LB_SETITEMHEIGHT - PropGrid_SetItemHeight()

/// @name Property grid specific messages.
/// @{
const
	PG_EXPANDCATALOGS   = WM_USER + $01;  ///<PropGrid_ExpandCatalogs()
	PG_COLLAPSECATALOGS = WM_USER + $02;  ///<PropGrid_CollapseCatalogs()
	PG_SHOWTOOLTIPS     = WM_USER + $03;  ///<PropGrid_ShowToolTips()
	PG_SHOWPROPERTYDESC = WM_USER + $04;  ///<PropGrid_ShowPropertyDescriptions()
	PG_FLATCHECKS       = WM_USER + $05;  ///<PropGrid_SetFlatStyleChecks()
/// @}

{****************************************************************************}
// Property item types

const
	PIT_EDIT        = 0;   ///< Property item type: Edit
	PIT_COMBO       = 1;   ///< Property item type: Dropdownlist
	PIT_EDITCOMBO   = 2;   ///< Property item type: Dropdown(editable)
	PIT_CHECKCOMBO  = 3;   ///< Property item type: Checked dropdown //DWM 2.2:Added
	PIT_STATIC      = 4;   ///< Property item type: Not editable text
	PIT_COLOR       = 5;   ///< Property item type: Color
	PIT_FONT        = 6;   ///< Property item type: Font
	PIT_FILE        = 7;   ///< Property item type: File select dialog
	PIT_FOLDER      = 8;   ///< Property item type: Folder select dialog
	PIT_CHECK       = 9;   ///< Property item type: BOOL
	PIT_IP          = 10;  ///< Property item type: IP Address
	PIT_DATE        = 11;  ///< Property item type: Date
	PIT_TIME        = 12;  ///< Property item type: Time
	PIT_DATETIME    = 13;  ///< Property item type: Date & Time
	PIT_CATALOG     = 99;  ///< Property item type: Catalog

{****************************************************************************}
// Public structures and notifications

/// @var PROPGRIDFDITEM
/// @brief A property grid file dialog item object

/// @var LPPROPGRIDFDITEM
/// @brief Pointer to a property grid file dialog item

/// @struct tagPROPGRIDFDITEM
/// @brief This is additional data associated with a property grid file dialog item
type
	PROPGRIDFDITEM = record
		lpszDlgTitle : LPTSTR;    ///< Dialog title
		lpszFilePath : LPTSTR;    ///< Initial path
		lpszFilter : LPTSTR;  ///< Double null terminated filter string
		lpszDefExt : LPTSTR;  ///< Default extension
	end;
	LPPROPGRIDFDITEM = ^PROPGRIDFDITEM;

/// @var PROPGRIDFONTITEM
/// @brief A property grid font item object

/// @var LPPROPGRIDFONTITEM
/// @brief Pointer to a property grid font item

/// @struct tagPROPGRIDFONTITEM
/// @brief This is additional data associated with a property grid font item
type
	PROPGRIDFONTITEM = record
		_logFont : LOGFONT;    ///< Logical font struct
		crFont : COLORREF;    ///< Text color
	end;
	LPPROPGRIDFONTITEM = ^PROPGRIDFONTITEM;

/// @var PROPGRIDITEM
/// @brief A property grid item object

/// @var LPPROPGRIDITEM
/// @brief Pointer to a property grid item

/// @struct tagPROPGRIDITEM
/// @brief This is the data associated with a property grid item
type
	PROPGRIDITEM = record
		lpszCatalog : LPTSTR; ///< Catalog (group) name
		lpszPropName : LPTSTR;    ///< Property (item) name
		lpszzCmbItems : LPTSTR;   ///< Double null terminated list of strings
		lpszPropDesc : LPTSTR;    ///< Property (item) description
		lpCurValue : LPARAM;  ///< Property (item) value
		lpUserData : LPVOID;  ///< Additional user data
		iItemType : Longint;  ///< Property (item) type identifier
	end;
	LPPROPGRIDITEM = ^PROPGRIDITEM;

/// @var NMPROPGRID
/// @brief A property grid notification message data object

/// @var LPNMPROPGRID
/// @brief Pointer to property grid notification message data

/// @struct tagNMPROPGRID
/// @brief This is the data associated with a property grid notification
type
	NMPROPGRID = record
		hdr : NMHDR;  ///< Notification message header
		iIndex : Longint; ///< Index of a property grid item
	end;
	LPNMPROPGRID = ^NMPROPGRID;
	
const
	PGN_PROPERTYCHANGE = WM_USER + $2A; ///<property grid Property changed notification message

{****************************************************************************}
/// @name Macroes
/// @{

/// @def PropGrid_AddItem(hwndCtl,lpItem)
///
/// @brief Add an item to a property grid.  Items are appended to their respective
///         catalogs.
///
/// @param hwndCtl The handle of a property grid.
/// @param lpItem Pointer to a property grid item.
///
/// @returns The zero-based index of the item in the list box. If an error occurs,
///           the return value is LB_ERR. If there is insufficient space to store
///           the new string, the return value is LB_ERRSPACE. 
function PropGrid_AddItem(hwndCtl : HWND; lpItem : LPPROPGRIDITEM) : Longint;

/// @def PropGrid_DeleteItem(hwndCtl,index)
///
/// @brief Deletes the item at the specified location in a property grid.
///
/// @param hwndCtl The handle of a property grid.
/// @param index The zero-based index of the item to delete.
///
/// @returns A count of the items remaining in the grid. The return value is
///           LB_ERR if the index parameter specifies an index greater than the
///           number of items in the list. 
function PropGrid_DeleteItem(hwndCtl : HWND; index : Longint) : Longint;

/// @def PropGrid_Enable(hwndCtl,fEnable)
///
/// @brief Enables or disables a property grid control.
///
/// @param hwndCtl The handle of a property grid.
/// @param fEnable TRUE to enable the control, or FALSE to disable it.
///
/// @returns No return value.
function PropGrid_Enable(hwndCtl : HWND; fEnable : BOOL) : BOOL;

/// @def PropGrid_GetCount(hwndCtl)
///
/// @brief Gets the number of items in a property grid.
///
/// @param hwndCtl The handle of a property grid.
///
/// @returns The number of items.
function PropGrid_GetCount(hwndCtl : HWND) : Longint;

/// @def PropGrid_GetCurSel(hwndCtl)
///
/// @brief Gets the index of the currently selected item in a property grid.
///
/// @param hwndCtl The handle of a property grid.
///
/// @returns The zero-based index of the selected item. If there is no selection,
///           the return value is LB_ERR.
function PropGrid_GetCurSel(hwndCtl : HWND) : Longint;

/// @def PropGrid_GetHorizontalExtent(hwndCtl)
///
/// @brief Gets the width that a property grid can be scrolled horizontally
///         (the scrollable width).
///
/// @param hwndCtl The handle of a property grid.
///
/// @returns The scrollable width, in pixels, of the property grid.
function PropGrid_GetHorizontalExtent(hwndCtl : HWND) : Longint;

/// @def PropGrid_GetItemData(hwndCtl,index)
///
/// @brief Gets the PROPGRIDITEM associated with the specified property grid item.
///
/// @param hwndCtl The handle of a property grid.
/// @param index The zero-based index of the item.
///
/// @returns A pointer to a PROPGRIDITEM object.
function PropGrid_GetItemData(hwndCtl : HWND; index : Longint) : LPPROPGRIDITEM;

/// @def PropGrid_GetItemHeight(hwndCtl)
///
/// @brief Retrieves the height of all items in a property grid.
///
/// @param hwndCtl The handle of a property grid.
///
/// @returns The height, in pixels, of the items, or LB_ERR if an error occurs.
function PropGrid_GetItemHeight(hwndCtl : HWND) : Longint;

/// @def PropGrid_GetItemRect(hwndCtl,index,lprc)
///
/// @brief Gets the dimensions of the rectangle that bounds a property grid item
///         as it is currently displayed in the property grid.
///
/// @param hwndCtl The handle of a property grid.
/// @param index The zero-based index of the item in the property grid.
/// @param lprc A pointer to a RECT structure that receives the client
///              coordinates for the item in the property grid.
///
/// @returns If an error occurs, the return value is LB_ERR.
function PropGrid_GetItemRect(hwndCtl : HWND; index : Longint; lprc : LPRECT) : Longint;

/// @def PropGrid_GetSel(hwndCtl,index)
///
/// @brief Gets the selection state of an item.
///
/// @param hwndCtl The handle of a property grid.
/// @param index The zero-based index of the item.
///
/// @returns If the item is selected, the return value is greater than zero;
///           otherwise, it is zero. If an error occurs, the return value is LB_ERR.
function PropGrid_GetSel(hwndCtl : HWND; index : Longint) : Longint;

/// @def PropGrid_ResetContent(hwndCtl)
///
/// @brief Removes all items from a property grid.
///
/// @param hwndCtl The handle of a property grid.
///
/// @returns The return value is not meaningful.
function PropGrid_ResetContent(hwndCtl : HWND) : Longint;

/// @def PropGrid_SetCurSel(hwndCtl,index)
///
/// @brief Sets the currently selected item in a property grid.
///
/// @param hwndCtl The handle of a property grid.
/// @param index The zero-based index of the item to select, or –1 to clear the selection.
///
/// @returns If an error occurs, the return value is LB_ERR. If the index
///           parameter is –1, the return value is LB_ERR even though no error occurred. 
function PropGrid_SetCurSel(hwndCtl : HWND; index : Longint) : Longint;

/// @def PropGrid_SetHorizontalExtent(hwndCtl,cxExtent)
///
/// @brief Set the width by which a property grid can be scrolled horizontally
///         (the scrollable width). If the width of the property grid is smaller
///         than this value, the horizontal scroll bar horizontally scrolls items
///         in the property grid. If the width of the property grid is equal to or
///         greater than this value, the horizontal scroll bar is hidden.
///
/// @param hwndCtl The handle of a property grid.
/// @param cxExtent The number of pixels by which the list box can be scrolled.
///
/// @returns No return value.
function PropGrid_SetHorizontalExtent(hwndCtl : HWND; cxExtent : Longint) : Longint;

/// @def PropGrid_SetItemData(hwndCtl,index,data)
///
/// @brief Sets the PROPGRIDITEM associated with the specified property grid item.
///
/// @param hwndCtl The handle of a property grid.
/// @param index The zero-based index of the item.
/// @param data The item data to set.
///
/// @returns If an error occurs, the return value is LB_ERR.
function PropGrid_SetItemData(hwndCtl : HWND; index : Longint; data : LPPROPGRIDITEM) : Longint;

/// @def PropGrid_SetItemHeight(hwndCtl,cy)
///
/// @brief Sets the height of all items in a property grid.
///
/// @param hwndCtl The handle of a property grid.
/// @param cy The height of the items, in pixels.
///
/// @returns If the height is invalid, the return value is LB_ERR.
function PropGrid_SetItemHeight(hwndCtl : HWND; cy : Longint) : Longint;

/// @def PropGrid_ExpandCatalogs(hwndCtl, lpszzCatalogs)
///
/// @brief Expand certain specified catalogs in a property grid.
///
/// @param hwndCtl The handle of a property grid.
/// @param lpszzCatalogs The list of catalog names each terminated by a null (\\0).
///
/// @returns No return value.
procedure PropGrid_ExpandCatalogs(hwndCtl : HWND; lpszzCatalogs : LPTSTR);

/// @def PropGrid_ExpandAllCatalogs(hwndCtl)
///
/// @brief Expand all catalogs in a property grid.
///
/// @param hwndCtl The handle of a property grid.
///
/// @returns No return value.
procedure PropGrid_ExpandAllCatalogs(hwndCtl : HWND);

/// @def PropGrid_CollapseCatalogs(hwndCtl, lpszzCatalogs)
///
/// @brief Collapse certain specified catalogs in a property grid.
///
/// @param hwndCtl The handle of a property grid.
/// @param lpszzCatalogs The catalog names each terminated by a null (\\0).
///
/// @returns No return value.
procedure PropGrid_CollapseCatalogs(hwndCtl : HWND; lpszzCatalogs : LPTSTR);

/// @def PropGrid_CollapseAllCatalogs(hwndCtl)
///
/// @brief Collapse all catalogs in a property grid.
///
/// @param hwndCtl The handle of a property grid.
///
/// @returns No return value.
procedure PropGrid_CollapseAllCatalogs(hwndCtl : HWND);

/// @def PropGrid_ShowToolTips(hwndCtl,fShow)
///
/// @brief Show or hide tooltips in the property grid.
///
/// @param hwndCtl The handle of a property grid.
/// @param fShow TRUE for tooltips; FALSE do not show tooltips.
///
/// @returns No return value.
procedure PropGrid_ShowToolTips(hwndCtl : HWND; fShow : BOOL);

/// @def PropGrid_ShowPropertyDescriptions(hwndCtl,fShow)
///
/// @brief Show or hide the property discription pane in the property grid.
///
/// @param hwndCtl The handle of a property grid.
/// @param fShow TRUE for descriptions; FALSE do not show discription pane.
///
/// @returns No return value.
procedure PropGrid_ShowPropertyDescriptions(hwndCtl : HWND; fShow : BOOL);

/// @def PropGrid_SetFlatStyleChecks(hwndCtl, fFlat)
///
/// @brief Sets the appearance of the checkboxes.
///
/// @param hwndCtl The handle of a property grid.
/// @param fFlat TRUE for flat checkboxes, or FALSE for standard checkboxes.
///
/// @returns No return value.
procedure PropGrid_SetFlatStyleChecks(hwndCtl : HWND; fFlat : BOOL);


/// @def PropGrid_ItemInit(pgi)
///
/// @brief Initialize an item struct.
///
/// @param pgi The PROPGRIDITEM struct.
///
/// @returns No return value.
procedure PropGrid_ItemInit(var pgi : PROPGRIDITEM);

/// @}

{****************************************************************************}
// Exported function prototypes

function InitPropertyGrid(hInstance : {HINSTANCE}HMODULE) : ATOM; cdecl; external PROPERTYGRIDDLLNAME;
function New_PropertyGrid(hParent : HWND; dwID : DWORD) : HWND; cdecl; external PROPERTYGRIDDLLNAME;

implementation

function PropGrid_AddItem(hwndCtl : HWND; lpItem : LPPROPGRIDITEM) : Longint;
begin
	Result := SendMessage(hwndCtl, LB_ADDSTRING, 0, LPARAM(lpItem));
end;

function PropGrid_DeleteItem(hwndCtl : HWND; index : Longint) : Longint;
begin
	Result := SendMessage(hwndCtl, LB_DELETESTRING, WPARAM(index), 0);
end;

function PropGrid_Enable(hwndCtl : HWND; fEnable : BOOL) : BOOL;
begin
	Result := EnableWindow(hwndCtl, fEnable);
end;

function PropGrid_GetCount(hwndCtl : HWND) : Longint;
begin
	Result := SendMessage(hwndCtl, LB_GETCOUNT, 0, 0);
end;

function PropGrid_GetCurSel(hwndCtl : HWND) : Longint;
begin
	Result := SendMessage(hwndCtl, LB_GETCURSEL, 0, 0);
end;

function PropGrid_GetHorizontalExtent(hwndCtl : HWND) : Longint;
begin
	Result := SendMessage(hwndCtl, LB_GETHORIZONTALEXTENT, 0, 0);
end;

function PropGrid_GetItemData(hwndCtl : HWND; index : Longint) : LPPROPGRIDITEM;
begin
	Result := LPPROPGRIDITEM(SendMessage(hwndCtl, LB_GETITEMDATA, WPARAM(index), 0));
end;

function PropGrid_GetItemHeight(hwndCtl : HWND) : Longint;
begin
	Result := SendMessage(hwndCtl, LB_GETITEMHEIGHT, 0, 0);
end;

function PropGrid_GetItemRect(hwndCtl : HWND; index : Longint; lprc : LPRECT) : Longint;
begin
	Result := SendMessage(hwndCtl, LB_GETITEMRECT, WPARAM(index), LPARAM(lprc));
end;

function PropGrid_GetSel(hwndCtl : HWND; index : Longint) : Longint;
begin
	Result := SendMessage(hwndCtl, LB_GETSEL, WPARAM(index), 0);
end;

function PropGrid_ResetContent(hwndCtl : HWND) : Longint;
begin
	Result := SendMessage(hwndCtl, LB_RESETCONTENT, 0, 0);
end;

function PropGrid_SetCurSel(hwndCtl : HWND; index : Longint) : Longint;
begin
	Result := SendMessage(hwndCtl, LB_SETCURSEL, WPARAM(index), 0);
end;

function PropGrid_SetHorizontalExtent(hwndCtl : HWND; cxExtent : Longint) : Longint;
begin
	Result := SendMessage(hwndCtl, LB_SETHORIZONTALEXTENT, WPARAM(cxExtent), 0);
end;

function PropGrid_SetItemData(hwndCtl : HWND; index : Longint; data : LPPROPGRIDITEM) : Longint;
begin
	Result := SendMessage(hwndCtl, LB_SETITEMDATA, WPARAM(index), LPARAM(data));
end;

function PropGrid_SetItemHeight(hwndCtl : HWND; cy : Longint) : Longint;
begin
	Result := SendMessage(hwndCtl, LB_SETITEMHEIGHT, 0, MAKELPARAM(cy,0));
end;

procedure PropGrid_ExpandCatalogs(hwndCtl : HWND; lpszzCatalogs : LPTSTR);
begin
	SendMessage(hwndCtl, PG_EXPANDCATALOGS, 0, LPARAM(lpszzCatalogs));
end;

procedure PropGrid_ExpandAllCatalogs(hwndCtl : HWND);
begin
	SendMessage(hwndCtl, PG_EXPANDCATALOGS, 0, LPARAM(nil));
end;

procedure PropGrid_CollapseCatalogs(hwndCtl : HWND; lpszzCatalogs : LPTSTR);
begin
	SendMessage(hwndCtl, PG_COLLAPSECATALOGS, 0, LPARAM(lpszzCatalogs));
end;

procedure PropGrid_CollapseAllCatalogs(hwndCtl : HWND);
begin
	SendMessage(hwndCtl, PG_COLLAPSECATALOGS, 0, LPARAM(nil));
end;

procedure PropGrid_ShowToolTips(hwndCtl : HWND; fShow : BOOL);
begin
	SendMessage(hwndCtl, PG_SHOWTOOLTIPS, WPARAM(fShow), 0);
end;

procedure PropGrid_ShowPropertyDescriptions(hwndCtl : HWND; fShow : BOOL);
begin
	SendMessage(hwndCtl, PG_SHOWPROPERTYDESC, WPARAM(fShow), 0);
end;

procedure PropGrid_SetFlatStyleChecks(hwndCtl : HWND; fFlat : BOOL);
begin
	SendMessage(hwndCtl, PG_FLATCHECKS, WPARAM(fFlat), 0);
end;

procedure PropGrid_ItemInit(var pgi : PROPGRIDITEM);
begin
	pgi.lpszCatalog := nil;
	pgi.lpszPropName := nil;
	pgi.lpszzCmbItems := nil;
	pgi.lpszPropDesc := nil;
	pgi.lpCurValue := 0;
	pgi.lpUserData := nil;
	pgi.iItemType := 0;
end;

end.