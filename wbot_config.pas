{
 _    _  _         _____
| |  | || |       |_   _|   /|
| |  | || |___  ___ | |    / |__
| |/\| ||  _  |/ _ \| |   /_   /
| /  \ || (_) | (_) | |     | /
|__/\__||_____|\___/|_|     |/

}
unit WBot_Config;

{$i wbot.inc}

interface

uses
  SysUtils, LResources, IniFiles,
  // CEF
  uCEFApplication,
  // WBot
  WBot_Const;
                           
function CreateGlobalCEFApp: boolean;
procedure DestroyGlobalCEFApp;

implementation

var
  LocalIni: TIniFile = nil;

procedure CreateLocalIni;
begin
  LocalIni := TIniFile.Create(WBOT_INI);
end;

procedure DestroyLocalIni;
begin
  FreeAndNil(LocalIni);
end;

function CreateGlobalCEFApp: boolean;
begin
  {$IfDef wbot_debug}
  WriteLn(CreateGlobalCEFApp);
  {$EndIf}
  GlobalCEFApp := TCefApplication.Create;
  GlobalCEFApp.FrameworkDirPath :=
    UnicodeString(LocalIni.ReadString('Paths', 'FrameWork', 'cef'));
  GlobalCEFApp.ResourcesDirPath :=
    UnicodeString(LocalIni.ReadString('Paths', 'Resources', 'cef'));
  GlobalCEFApp.LocalesDirPath := 
    UnicodeString(LocalIni.ReadString('Paths', 'Locales', 'cef'+ PathDelim + 'locales'));
  GlobalCEFApp.Cache :=
    UnicodeString(LocalIni.ReadString('Paths', 'Cache', 'tmp' + PathDelim + 'cache'));
  GlobalCEFApp.UserDataPath :=
    UnicodeString(LocalIni.ReadString('Paths', 'UserData', 'tmp' + PathDelim + 'user'));

  Result := GlobalCEFApp.StartMainProcess;
end;

procedure DestroyGlobalCEFApp;
begin
  LocalIni.WriteString('Paths', 'FrameWork', string(GlobalCEFApp.FrameworkDirPath));
  LocalIni.WriteString('Paths', 'Resources', string(GlobalCEFApp.ResourcesDirPath));
  LocalIni.WriteString('Paths', 'Locales', string(GlobalCEFApp.LocalesDirPath));
  LocalIni.WriteString('Paths', 'Cache', string(GlobalCEFApp.Cache));
  LocalIni.WriteString('Paths', 'UserData', string(GlobalCEFApp.UserDataPath));
  FreeAndNil(GlobalCEFApp);   
  {$IfDef wbot_debug}
  WriteLn(DestroyGlobalCEFApp);
  {$EndIf}
end;

initialization
  CreateLocalIni;

finalization
  DestroyLocalIni;

end.
