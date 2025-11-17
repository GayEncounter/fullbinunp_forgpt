if not exist obj mkdir obj

ppc386 -FUobj -Fu..\..\main -Sd anims_split.pas
if not [%ERRORLEVEL%]==[0] pause