ppcrossx64 -gw -vw -vm6058 -Sd -FE. -Fusource\main -FUobj\binunp source\main\binunp.pas
if [%ERRORLEVEL%]==[0] goto :EOF 
pause