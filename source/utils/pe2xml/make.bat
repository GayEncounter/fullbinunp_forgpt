if not exist obj mkdir obj

ppc386 -FUobj -Fu..\..\main -Sd pe2xml.pas
if not [%ERRORLEVEL%]==[0] pause