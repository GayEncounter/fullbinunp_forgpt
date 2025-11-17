unit uImages;

interface

procedure LoadImages;

implementation
uses Iup, FreeImage;

procedure LoadImage(const name, filename : String);
var
	dib, dib32 : FIBITMAP;
	
	width, height : Longint;
	I : Longint;
	buffer : array of Byte;
	tmp : Byte;
	
	handle : Ihandle;
begin
	dib := FreeImage_Load(FreeImage_GetFIFFromFilename(@filename[1]), @filename[1]);
	if dib <> nil then
	begin
		if FreeImage_FlipVertical(dib) <> 0 then
		begin
			if FreeImage_GetBPP(dib) <> 32 then
			begin
				dib32 := FreeImage_ConvertTo32Bits(dib);
				FreeImage_Unload(dib);
				dib := dib32;
			end;
			
			width := FreeImage_GetWidth(dib);
			height := FreeImage_GetHeight(dib);
			
			SetLength(buffer, width*height*4);
			Move(FreeImage_GetBits(dib)^, buffer[0], Length(buffer));
			
			// BGR -> RGB
			for I := 0 to (width*height) - 1 do
			begin
				tmp := buffer[I*4+0];
				buffer[I*4+0] := buffer[I*4+2];
				buffer[I*4+2] := tmp;
			end;
			
			handle := IupImageRGBA(width, height, @buffer[0]);
			IupSetHandle(PAnsiChar(name), handle);
			
		end else
			WriteLn('Cannot flip image ''', filename, '''!');
		
		FreeImage_Unload(dib);
	end else
		WriteLn('Cannot load image ''', filename, '''!');
end;

procedure LoadImages;
begin
	LoadImage('ICON_OBJECT', 'editor_data\icons\object.png');
	LoadImage('ICON_GROUP', 'editor_data\icons\group.png');
	LoadImage('ICON_WEATHER', 'editor_data\icons\weather.png');
	LoadImage('ICON_AO', 'editor_data\icons\ao.png');
	
	LoadImage('ICON_FOLDER1', 'editor_data\icons\folder1.png');
	LoadImage('ICON_FOLDER2', 'editor_data\icons\folder2.png');
	
	LoadImage('ICON_CURSOR', 'editor_data\icons\cursor.png');
	LoadImage('ICON_MOVE', 'editor_data\icons\move.png');
	LoadImage('ICON_ROTATE', 'editor_data\icons\rotate.png');
	LoadImage('ICON_SCALE', 'editor_data\icons\scale.png');
	
	LoadImage('ICON_DISKETTE', 'editor_data\icons\diskette.png');
	LoadImage('ICON_TRASHCAN', 'editor_data\icons\trashcan.png');
	LoadImage('ICON_FISHCAN', 'editor_data\icons\fishcan.png');
	
	LoadImage('ICON_PLAY', 'editor_data\icons\play.png');
	LoadImage('ICON_PAUSE', 'editor_data\icons\pause.png');
	
	LoadImage('ICON_RUN', 'editor_data\icons\run.png');
	
	LoadImage('ICON_TOOLS', 'editor_data\icons\tools.png');
	LoadImage('ICON_TEXTURE', 'editor_data\icons\texture.png');
end;

end.
