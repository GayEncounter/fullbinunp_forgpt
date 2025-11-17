unit m2033unp;

interface

procedure decompress(input, output : PChar; inlen : Longword);

implementation

var
  counts : array[0..15] of Integer = (4, 0, 1, 0,
                                      2, 0, 1, 0,
                                      3, 0, 1, 0,
                                      2, 0, 1, 0);

procedure xdecompress(input, output : PChar; outlen : Longint);
var
  outp, outlast, p : PChar;
  mask, bits, len, off : Longword;
  done : Boolean;
begin
  outp := output;
  outlast := outp + outlen - 1;
  mask := 1;
  done := false;

  while not done do
  begin
    if mask = 1 then
    begin
      mask := PLongword(input)^;
      input := input + 4;
    end;

    bits := PLongword(input)^;

    if mask and 1 <> 0 then
    begin
      mask := mask shr 1;
      len := 3;
      Inc(input);
      if bits and 3 <> 0 then
      begin
        Inc(input);
        if bits and 2 <> 0 then
        begin
          if bits and 1 <> 0 then
          begin
            Inc(input);
            if (bits and $7F) = 3 then
            begin
              Inc(input);
              off := bits shr 15;
              len := len + ((bits shr 7) and $FF);
            end else
            begin
              off := (bits shr 7) and $1FFFF;
              len := len + (((bits shr 2) and $1F) - 1);
            end;
          end else
          begin
            off := (bits shr 6) and $3FF;
            len := len + ((bits shr 2) and $F);
          end;
        end else
        begin
          off := (bits shr 2) and $3FFF;
        end;
      end else
      begin
        off := (bits shr 2) and $3F;
      end;
//      Writeln('backref bits = ', bits, ' off = ', off, ' len = ', len);
      p := outp;
      outp := outp + len;
      repeat
        PLongword(p)^ := PLongword(p-off)^;
        Inc(p, 3);
      until p >= outp;
    end
    else if outp < outlast - 10 then
    begin
//      Writeln('literal');
      PLongword(outp)^ := bits;
      len   := counts[mask and $0F];
      outp  := outp + len;
      input := input + len;
      mask  := mask shr len;
    end else
    begin
//      Writeln('tail');
      while outp <= outlast do
      begin
        if mask = 1 then
        begin
          mask := $80000000;
          input := input + 4;
        end;
        outp^ := input^;
        Inc(outp); Inc(input);
        mask := mask shr 1;
      end;
      done := true;
    end;
  end;
end;

procedure decompress(input, output : PChar; inlen : Longword);
var
  off, len, clen : Longword;
  outp, inp : PChar;
begin
  outp := output;
  inp := input;
  if inlen > 0 then
    repeat
      if PShortint(inp)^ and 2 <> 0 then
        off := 4
      else
        off := 1;

      clen := PLongword(inp+1)^;
      len := PLongword(inp+1+off)^;
      if off = 1 then
      begin
        clen := clen and $FF;
        len := len and $FF;
      end;

      if PShortint(inp)^ and 1 <> 0 then
        xdecompress(inp+1+2*off, outp, len)
      else
        Move((inp+1+2*off)^, outp^, len);

 //     if clen <> 0 then
 //       Writeln('clen = ', len);

      Inc(outp, len);
      Inc(inp, clen);
    until Longword(inp-input) >= inlen;
end;

end.