set UNIT_PATHS= ^
-Fusource ^
-Fusource\main ^
-Fusource\AssimpDelphi 

ppcrossx64 -FE. -FUobj\il %UNIT_PATHS% -O2 -vw -vm6058 -gw -Mdelphi source\image_library\image_library.pas
if not [%ERRORLEVEL%]==[0] pause