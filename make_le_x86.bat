set UNIT_PATHS= ^
-Fusource ^
-Fusource\main ^
-Fusource\AssimpDelphi

ppc386 -FE. -FUobj\le %UNIT_PATHS% -O2 -vw -vm6058 -gw -Mdelphi source\level_Editor.pas
if not [%ERRORLEVEL%]==[0] pause