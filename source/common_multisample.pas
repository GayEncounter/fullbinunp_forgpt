unit common_multisample;

interface

function ChooseMultisamplePixelFormat(nsamples : Longint) : Longint;

implementation
uses GL, Windows, WGLExt;

const
	DUMMY_WND_CLASS_NAME = 'DummyGLWnd';

function WindowProc(wnd : HWND; msg : UINT; _wparam : WPARAM; _lparam : LPARAM) : LRESULT; stdcall;
begin
	Result := DefWindowProc(wnd, msg, _wparam, _lparam);
end;

function ChooseMultisamplePixelFormat(nsamples : Longint) : Longint;
var
	hInst : HMODULE;
	wc : WNDCLASS;
	cls : ATOM;
	wnd : HWND;
	dc : HDC;
	pfd : PIXELFORMATDESCRIPTOR;
	iPixelFmt : Longint;
	gl_context : HGLRC;
	
	wglGetExtensionsStringARB : PFNWGLGETEXTENSIONSSTRINGARBPROC;
	wglGetExtensionsStringARB_ptr : Pointer absolute wglGetExtensionsStringARB;
	wglChoosePixelFormatARB : PFNWGLCHOOSEPIXELFORMATARBPROC;
	wglChoosePixelFormatARB_ptr : Pointer absolute wglChoosePixelFormatARB;
	
	attribs : array of Longint = [
		WGL_DRAW_TO_WINDOW_ARB, GL_TRUE,
		WGL_SUPPORT_OPENGL_ARB, GL_TRUE,
		WGL_DOUBLE_BUFFER_ARB,  GL_TRUE,
		WGL_PIXEL_TYPE_ARB,     WGL_TYPE_RGBA_ARB,
		WGL_COLOR_BITS_ARB,     32,
		WGL_RED_BITS_ARB,       8,
		WGL_GREEN_BITS_ARB,     8,
		WGL_BLUE_BITS_ARB,      8,
		WGL_ALPHA_BITS_ARB,     0,
		WGL_DEPTH_BITS_ARB,     24,
		WGL_STENCIL_BITS_ARB,   0,
		WGL_SAMPLE_BUFFERS_ARB, 1
		// completed later in code ..
	];
	fattribs : array of Single = [
		0, 0
	];
	
	format : Longint;
	nFormatsResult : Longword;
	
	I : Longint;
begin
	hInst := GetModuleHandle(nil);
	
	wc.style         := 0;
	wc.lpfnWndProc   := WindowProc;
	wc.cbClsExtra    := 0;
	wc.cbWndExtra    := 0;
	wc.hInstance     := hInst;
	wc.hIcon         := LoadIcon(0, IDI_APPLICATION);
	wc.hCursor       := LoadCursor(0, IDC_ARROW);
	wc.hbrBackground := 0;
	wc.lpszMenuName  := nil;
	wc.lpszClassName := DUMMY_WND_CLASS_NAME;
	
	cls := RegisterClass(@wc);
	if cls = 0 then
	begin
		WriteLn('Class registation failed!');
		Result := 0;
		Exit;
	end;
	
	wnd := CreateWindow(DUMMY_WND_CLASS_NAME, 'FakeWindow', WS_POPUP, 100, 100, 200, 200, 0, 0, hInst, nil);
	
	if wnd = 0 then
	begin
		WriteLn('Window creation failed!');
		UnregisterClass(DUMMY_WND_CLASS_NAME, hInst);
		Result := 0;
		Exit;
	end;
	
	pfd.nSize           := Sizeof(pfd);
	pfd.nVersion        := 1;
	pfd.dwFlags         := PFD_DRAW_TO_WINDOW or PFD_SUPPORT_OPENGL or PFD_DOUBLEBUFFER;
	pfd.iPixelType      := PFD_TYPE_RGBA;
	pfd.cColorBits      := 32;
	pfd.cRedBits        := 8;
	pfd.cRedShift       := 0;
	pfd.cGreenBits      := 8;
	pfd.cGreenShift     := 8;
	pfd.cBlueBits       := 8;
	pfd.cBlueShift      := 16;
	pfd.cAlphaBits      := 0;
	pfd.cAlphaShift     := 0;
	pfd.cAccumBits      := 0;
	pfd.cAccumRedBits   := 0;
	pfd.cAccumGreenBits := 0;	
	pfd.cAccumBlueBits  := 0;
	pfd.cAccumAlphaBits := 0;
	pfd.cDepthBits      := 32;
	pfd.cStencilBits    := 0;
	pfd.cAuxBuffers     := 0;
	pfd.iLayerType      := PFD_MAIN_PLANE;
	pfd.bReserved       := 0;
	pfd.dwLayerMask     := 0;
	pfd.dwVisibleMask   := 0;
	pfd.dwDamageMask    := 0;
	
	dc := GetDC(wnd);
	iPixelFmt := ChoosePixelFormat(dc, @pfd);
	if iPixelFmt = 0 then
	begin
		WriteLn('Cannot choose pixelformat for fake GL window');
		ReleaseDC(wnd, dc);
		DestroyWindow(wnd);
		UnregisterClass(DUMMY_WND_CLASS_NAME, hInst);
		Result := 0;
		Exit;
	end;
		
	if not SetPixelFormat(dc, iPixelFmt, @pfd) then
	begin
		WriteLn('Cannot set pixelformat for fake GL window');
		ReleaseDC(wnd, dc);
		DestroyWindow(wnd);
		UnregisterClass(DUMMY_WND_CLASS_NAME, hInst);
		Result := 0;
		Exit;
	end;
	
	gl_context := wglCreateContext(dc);
	if gl_context = 0 then
	begin
		WriteLn('Cannot create context for fake GL window');
		ReleaseDC(wnd, dc);
		DestroyWindow(wnd);
		UnregisterClass(DUMMY_WND_CLASS_NAME, hInst);
		Result := 0;
		Exit;
	end;
	
	if not wglMakeCurrent(dc, gl_context) then
	begin
		WriteLn('wglMakeCurrent failed for fake GL window');
		wglDeleteContext(gl_context);
		ReleaseDC(wnd, dc);
		DestroyWindow(wnd);
		UnregisterClass(DUMMY_WND_CLASS_NAME, hInst);
		Result := 0;
		Exit;
	end;
	
	wglGetExtensionsStringARB := wglGetProcAddress('wglGetExtensionsStringARB');
	if wglGetExtensionsStringARB_ptr = nil then
	begin
		WriteLn('wglGetProcAddress failed for wglGetExtensionsStringARB');
		wglMakeCurrent(0, 0);
		wglDeleteContext(gl_context);
		ReleaseDC(wnd, dc);
		DestroyWindow(wnd);
		UnregisterClass(DUMMY_WND_CLASS_NAME, hInst);
		Result := 0;
		Exit;
	end;
	
	if Pos('WGL_ARB_pixel_format', wglGetExtensionsStringARB(dc)) = 0 then
	begin
		WriteLn('WGL_ARB_pixel_format is unsupported');
		wglMakeCurrent(0, 0);
		wglDeleteContext(gl_context);
		ReleaseDC(wnd, dc);
		DestroyWindow(wnd);
		UnregisterClass(DUMMY_WND_CLASS_NAME, hInst);
		Result := 0;
		Exit;
	end;
	
	wglChoosePixelFormatARB := wglGetProcAddress('wglChoosePixelFormatARB');
	if wglChoosePixelFormatARB_ptr = nil then
	begin
		WriteLn('wglGetProcAddress failed for wglChoosePixelFormatARB');
		wglMakeCurrent(0, 0);
		wglDeleteContext(gl_context);
		ReleaseDC(wnd, dc);
		DestroyWindow(wnd);
		UnregisterClass(DUMMY_WND_CLASS_NAME, hInst);
		Result := 0;
		Exit;
	end;
	
	I := Length(attribs);
	SetLength(attribs, I+4);
	attribs[I+0] := WGL_SAMPLES_ARB;
	attribs[I+1] := nsamples;
	attribs[I+2] := 0;
	attribs[I+3] := 0;
	
	if not wglChoosePixelFormatARB(dc, @attribs[0], @fattribs[0], 1, @format, @nFormatsResult) then
	begin
		WriteLn('wglChoosePixelFormatEXT failed');
		wglMakeCurrent(0, 0);
		wglDeleteContext(gl_context);
		ReleaseDC(wnd, dc);
		DestroyWindow(wnd);
		UnregisterClass(DUMMY_WND_CLASS_NAME, hInst);
		Result := 0;
		Exit;
	end;
	
	if nFormatsResult <> 0 then
		Result := format
	else
		Result := 0;
		
	wglMakeCurrent(0, 0);
	wglDeleteContext(gl_context);
	ReleaseDC(wnd, dc);
	DestroyWindow(wnd);
	UnregisterClass(DUMMY_WND_CLASS_NAME, hInst);
end;

end.
