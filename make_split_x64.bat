ppcrossx64 -Sd -FE. -vw -vm6058 -gw -Fusource\main -FUobj\split source\main\split.pas
if [%ERRORLEVEL%]==[0] goto :EOF 
pause