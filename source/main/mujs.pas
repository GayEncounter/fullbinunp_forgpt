unit mujs;

interface

const
	DLL_NAME = 'mujs.dll';

type
	js_State = Pointer;

	js_Alloc = function(memctx, ptr : Pointer; size : Longint) : Pointer; cdecl;
	js_Panic = procedure(J : js_State); cdecl;
	js_CFunction = procedure(J : js_State); cdecl;
	js_Finalize = procedure(J : js_State; p : Pointer); cdecl; 
	js_HasProperty_t = function(J : js_State; p : Pointer; name : PAnsiChar) : Longint; cdecl;
	js_Put = function(J : js_State; p : Pointer; name : PAnsiChar) : Longint; cdecl;
	js_Delete = function(J : js_State; p : Pointer; name : PAnsiChar) : Longint; cdecl;
	js_Report_t = procedure(J : js_State; message : PAnsichar); cdecl;

function  js_newstate(alloc : js_Alloc; actx : Pointer; flags : Longint) : js_State; cdecl; external DLL_NAME;
procedure js_setcontext(J : js_State; uctx : Pointer); cdecl; external DLL_NAME;
function  js_getcontext(J : js_State) : Pointer; cdecl; external DLL_NAME;
procedure js_setreport(J : js_State; report : js_Report_t); cdecl; external DLL_NAME;
function  js_atpanic(J : js_State; panic : js_Panic) : js_Panic; cdecl; external DLL_NAME;
procedure js_freestate(J : js_State); cdecl; external DLL_NAME;
procedure js_gc(J : js_State; report : Longint); cdecl; external DLL_NAME;

function  js_dostring(J : js_State; source : PAnsiChar) : Longint; cdecl; external DLL_NAME;
function  js_dofile(J : js_State; filename : PAnsiChar) : Longint; cdecl; external DLL_NAME;
function  js_ploadstring(J : js_State; filename, source : PAnsiChar) : Longint; cdecl; external DLL_NAME;
function  js_ploadfile(J : js_State; filename : PAnsiChar) : Longint; cdecl; external DLL_NAME;
function  js_pcall(J : js_State; n : Longint) : Longint; cdecl; external DLL_NAME;
function  js_pconstruct(J : js_State; n : Longint) : Longint; cdecl; external DLL_NAME;

const
	JS_STRICT = 1;

	JS_REGEXP_G = 1;
	JS_REGEXP_I = 2;
	JS_REGEXP_M = 4;

	JS_READONLY = 1;
	JS_DONTENUM = 2;
	JS_DONTCONF = 4;

procedure js_report(J :  js_State; message : PAnsiChar); cdecl; external DLL_NAME;

procedure js_newerror(J : js_State; message : PAnsiChar); cdecl; external DLL_NAME;
procedure js_newevalerror(J : js_State; message : PAnsiChar); cdecl; external DLL_NAME;
procedure js_newrangeerror(J : js_State; message : PAnsiChar); cdecl; external DLL_NAME;
procedure js_newreferenceerror(J : js_State; message : PAnsiChar); cdecl; external DLL_NAME;
procedure js_newsyntaxerror(J : js_State; message : PAnsiChar); cdecl; external DLL_NAME;
procedure js_newtypeerror(J : js_State; message : PAnsiChar); cdecl; external DLL_NAME;
procedure js_newurierror(J : js_State; message : PAnsiChar); cdecl; external DLL_NAME;

procedure js_error(J : js_State; fmt : PAnsiChar); cdecl; external DLL_NAME; varargs;
procedure js_evalerror(J : js_State; fmt : PAnsiChar); cdecl; external DLL_NAME; varargs;
procedure js_rangeerror(J : js_State; fmt : PAnsiChar); cdecl; external DLL_NAME; varargs;
procedure js_referenceerror(J : js_State; fmt : PAnsiChar); cdecl; external DLL_NAME; varargs;
procedure js_syntaxerror(J : js_State; fmt : PAnsiChar); cdecl; external DLL_NAME; varargs;
procedure js_typeerror(J : js_State; fmt : PAnsiChar); cdecl; external DLL_NAME; varargs;
procedure js_urierror(J : js_State; fmt : PAnsiChar); cdecl; external DLL_NAME; varargs;
procedure js_throw(J : js_State); cdecl; external DLL_NAME;

procedure js_loadstring(J : js_State; filename, source : PAnsiChar); cdecl; external DLL_NAME; 
procedure js_loadfile(J : js_State; filename : PAnsiChar); cdecl; external DLL_NAME;

procedure js_eval(J : js_State); cdecl; external DLL_NAME;
procedure js_call(J : js_State; n : Longint); cdecl; external DLL_NAME;
procedure js_construct(J : js_State; n : Longint); cdecl; external DLL_NAME;

function  js_ref(J : js_State) : PAnsiChar; cdecl; external DLL_NAME;
procedure js_unref(J : js_State; ref : PAnsiChar); cdecl; external DLL_NAME;

procedure js_getregistry(J : js_State; name : PAnsiChar); cdecl; external DLL_NAME;
procedure js_setregistry(J : js_State; name : PAnsiChar); cdecl; external DLL_NAME;
procedure js_delregistry(J : js_State; name : PAnsiChar); cdecl; external DLL_NAME;

procedure js_getglobal(J : js_State; name : PAnsiChar); cdecl; external DLL_NAME;
procedure js_setglobal(J : js_State; name : PAnsiChar); cdecl; external DLL_NAME;
procedure js_defglobal(J : js_State; name : PAnsiChar; atts : Longint); cdecl; external DLL_NAME;

function  js_hasproperty(J : js_State; idx : Longint; name : PAnsiChar) : Longint; cdecl; external DLL_NAME;
procedure js_getproperty(J : js_State; idx : Longint; name : PAnsiChar); cdecl; external DLL_NAME;
procedure js_setproperty(J : js_State; idx : Longint; name : PAnsiChar); cdecl; external DLL_NAME;
procedure js_defproperty(J : js_State; idx : Longint; name : PAnsiChar; atts : Longint); cdecl; external DLL_NAME;
procedure js_delproperty(J : js_State; idx : Longint; name : PAnsiChar); cdecl; external DLL_NAME;
procedure js_defaccessor(J : js_State; idx : Longint; name : PAnsiChar; atts : Longint); cdecl; external DLL_NAME;

function  js_getlength(J : js_State; idx : Longint) : Longint; cdecl; external DLL_NAME;
procedure js_setlength(J : js_State; idx : Longint; len : Longint); cdecl; external DLL_NAME;
function  js_hasindex(J : js_State; idx : Longint; i : Longint) : Longint; cdecl; external DLL_NAME;
procedure js_getindex(J : js_State; idx : Longint; i : Longint); cdecl; external DLL_NAME; 
procedure js_setindex(J : js_State; idx : Longint; i : Longint); cdecl; external DLL_NAME;
procedure js_delindex(J : js_State; idx : Longint; i : Longint); cdecl; external DLL_NAME;

procedure js_currentfunction(J : js_State); cdecl; external DLL_NAME;
procedure js_pushglobal(J : js_State); cdecl; external DLL_NAME;
procedure js_pushundefined(J : js_State); cdecl; external DLL_NAME;
procedure js_pushnull(J : js_State); cdecl; external DLL_NAME;
procedure js_pushboolean(J : js_State; v : Longint); cdecl; external DLL_NAME;
procedure js_pushnumber(J : js_State; v : Double); cdecl; external DLL_NAME;
procedure js_pushstring(J : js_State; v : PAnsiChar); cdecl; external DLL_NAME;
procedure js_pushlstring(J : js_State; v : PAnsiChar; n : Longint); cdecl; external DLL_NAME;
procedure js_pushliteral(J : js_State; v : PAnsiChar); cdecl; external DLL_NAME;

procedure js_newobjectx(J : js_State); cdecl; external DLL_NAME;
procedure js_newobject(J : js_State); cdecl; external DLL_NAME;
procedure js_newarray(J : js_State); cdecl; external DLL_NAME;
procedure js_newboolean(J : js_State; v : Longint); cdecl; external DLL_NAME;
procedure js_newnumber(J : js_State; v : Double); cdecl; external DLL_NAME;
procedure js_newstring(J : js_State; v : PAnsiChar); cdecl; external DLL_NAME;
procedure js_newcfunction(J : js_State; fun : js_CFunction; name : PAnsiChar; length : Longint); cdecl; external DLL_NAME;
procedure js_newcconstructor(J : js_State; fun, con : js_CFunction; name : PAnsiChar; length : Longint); cdecl; external DLL_NAME;
procedure js_newuserdata(J : js_State; tag : PAnsiChar; data : Pointer; finalize : js_Finalize); cdecl; external DLL_NAME;
procedure js_newuserdatax(J : js_State; tag : PAnsiChar; data : Pointer; has : js_HasProperty_t; put : js_Put; del : js_Delete; finalize : js_Finalize); cdecl; external DLL_NAME;
procedure js_newregexp(J : js_State; pattern : PAnsiChar; flags : Longint); cdecl; external DLL_NAME;

procedure js_pushiterator(J : js_State; idx : Longint; own : Longint); cdecl; external DLL_NAME;
function  js_nextiterator(J : js_State; idx : Longint) : PAnsiChar; cdecl; external DLL_NAME;

function  js_isdefined(J : js_State; idx : Longint) : Longint; cdecl; external DLL_NAME;
function  js_isundefined(J : js_State; idx : Longint) : Longint; cdecl; external DLL_NAME;
function  js_isnull(J : js_State; idx : Longint) : Longint; cdecl; external DLL_NAME;
function  js_isboolean(J : js_State; idx : Longint) : Longint; cdecl; external DLL_NAME;
function  js_isnumber(J : js_State; idx : Longint) : Longint; cdecl; external DLL_NAME;
function  js_isstring(J : js_State; idx : Longint) : Longint; cdecl; external DLL_NAME;
function  js_isprimitive(J : js_State; idx : Longint) : Longint; cdecl; external DLL_NAME;
function  js_isobject(J : js_State; idx : Longint) : Longint; cdecl; external DLL_NAME;
function  js_isarray(J : js_State; idx : Longint) : Longint; cdecl; external DLL_NAME;
function  js_isregexp(J : js_State; idx : Longint) : Longint; cdecl; external DLL_NAME;
function  js_iscoercible(J : js_State; idx : Longint) : Longint; cdecl; external DLL_NAME;
function  js_iscallable(J : js_State; idx : Longint) : Longint; cdecl; external DLL_NAME;
function  js_isuserdata(J : js_State; idx : Longint; tag : PAnsiChar) : Longint; cdecl; external DLL_NAME;
function  js_iserror(J : js_State; idx : Longint) : Longint; cdecl; external DLL_NAME;

function  js_toboolean(J : js_State; idx : Longint) : Longint; cdecl; external DLL_NAME;
function  js_tonumber(J : js_State; idx : Longint) : Double; cdecl; external DLL_NAME;
function  js_tostring(J : js_State; idx : Longint) : PAnsiChar; cdecl; external DLL_NAME;
function  js_touserdata(J : js_State; idx : Longint; tag : PAnsiChar) : Pointer; cdecl; external DLL_NAME;

function  js_trystring(J : js_State; idx : Longint; error : PAnsiChar) : PAnsiChar; cdecl; external DLL_NAME;
function  js_trynumber(J : js_State; idx : Longint; error : Double) : Double; cdecl; external DLL_NAME;
function  js_tryinteger(J : js_State; idx : Longint; error : Longint) : Longint; cdecl; external DLL_NAME;
function  js_tryboolean(J : js_State; idx : Longint; error : Longint) : Longint; cdecl; external DLL_NAME;

function  js_tointeger(J : js_State; idx : Longint) : Longint; cdecl; external DLL_NAME;
function  js_toint32(J : js_State; idx : Longint) : Longint; cdecl; external DLL_NAME;
function  js_touint32(J : js_State; idx : Longint) : Longword; cdecl; external DLL_NAME;
function  js_toint16(J : js_State; idx : Longint) : Smallint; cdecl; external DLL_NAME;
function  js_touint16(J : js_State; idx : Longint) : Word; cdecl; external DLL_NAME;

function  js_gettop(J : js_State) : Longint; cdecl; external DLL_NAME;
procedure js_pop(J : js_State; n : Longint); cdecl; external DLL_NAME;
procedure js_rot(J : js_State; n : Longint); cdecl; external DLL_NAME;
procedure js_copy(J : js_State; n : Longint); cdecl; external DLL_NAME;
procedure js_remove(J : js_State; n : Longint); cdecl; external DLL_NAME;
procedure js_insert(J : js_State; n : Longint); cdecl; external DLL_NAME;
procedure js_replace(J : js_State; n : Longint); cdecl; external DLL_NAME;

procedure js_dup(J : js_State); cdecl; external DLL_NAME;
procedure js_dup2(J : js_State); cdecl; external DLL_NAME;
procedure js_rot2(J : js_State); cdecl; external DLL_NAME;
procedure js_rot3(J : js_State); cdecl; external DLL_NAME;
procedure js_rot4(J : js_State); cdecl; external DLL_NAME;
procedure js_rot2pop1(J : js_State); cdecl; external DLL_NAME;
procedure js_rot3pop2(J : js_State); cdecl; external DLL_NAME;

procedure js_concat(J : js_State); cdecl; external DLL_NAME;
function  js_compare(J : js_State; okay : PLongint) : Longint; cdecl; external DLL_NAME;
function  js_equal(J : js_State) : Longint; cdecl; external DLL_NAME;
function  js_strictequal(J : js_State) : Longint; cdecl; external DLL_NAME;
function  js_instanceof(J : js_State) : Longint; cdecl; external DLL_NAME;
function  js_typeof(J : js_State; idx : Longint) : PAnsiChar; cdecl; external DLL_NAME;

procedure js_repr(J : js_State; idx : Longint); cdecl; external DLL_NAME;
function  js_torepr(J : js_State; idx : Longint) : PAnsiChar; cdecl; external DLL_NAME;
function  js_tryrepr(J : js_State; idx : Longint; error : PAnsiChar) : PAnsiChar; cdecl; external DLL_NAME;

implementation

end.