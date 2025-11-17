set UNIT_PATHS= ^
-Fu..\ ^
-Fu..\main

if not exist obj mkdir obj
ppcrossx64 %UNIT_PATHS% -FUobj -vw -gw -Mdelphi -vm6058 parse_lua.pas
if not [%ERRORLEVEL%]==[0] pause