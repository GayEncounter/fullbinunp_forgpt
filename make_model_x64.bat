ppcrossx64 -Sd -FE. -vw -vm6058 -gw -Fusource\main -FUobj\model source\main\model.pas
if [%ERRORLEVEL%]==[0] goto :EOF 
pause