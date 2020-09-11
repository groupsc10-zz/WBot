{
 _    _  _               _____
| |  | || |             |_   _|   /|
| |  | || |___  ___   ___ | |    / |__
| |/\| ||  _  |/ _ \ / _ \| |   /_   /
| /  \ || (_) | (_) | (_) | |     | /
|__/\__||_____|\___/ \___/|_|     |/

}
unit WBot_Config;

{$i wbot.inc}

interface

uses
  Classes, SysUtils, IniFiles, Forms, Dialogs, LCLType,
  // CEF
  uCEFApplication, uCEFChromium, uCEFTypes,
  // WBot
  WBot_Model, WBot_Const;

type

  { TCEFApp }

  TCEFApp = class(TCEFApplication)
  private
    FChromium: TChromium;
    FPathFramework: string;
    FPathResources: string;
    FPathLocales: string;
    FPathCache: string;
    FPathUserData: string; 
    FInitialized: boolean;
    FChanged: boolean;
    FIniFile: TIniFile;
    procedure SetChromium(const AValue: TChromium);
    procedure SetPathFramework(const AValue: string);
    procedure SetPathResources(const AValue: string);
    procedure SetPathLocales(const AValue: string);
    procedure SetPathCache(const AValue: string);
    procedure SetPathUserData(const AValue: string);
  protected
    procedure Load;
    procedure Save;
    function CheckVersion: boolean;
  public
    constructor Create;
    destructor Destroy; override;
    function StartMainProcess: boolean;
  published
    property Chromium: TChromium read FChromium write SetChromium;
    property PathFramework: string read FPathFramework write SetPathFramework;
    property PathResources: string read FPathResources write SetPathResources;
    property PathLocales: string read FPathLocales write SetPathLocales;
    property PathCache: string read FPathCache write SetPathCache;
    property PathUserData: string read FPathUserData write SetPathUserData;
    property Initialized: boolean read FInitialized;
  end;

var
  GlobalCEFApp: TCEFApp = nil;
                           
procedure InitializeGlobalCEFApp;
procedure DestroyGlobalCEFApp;

implementation

procedure InitializeGlobalCEFApp;
begin
  GlobalCEFApp := TCEFApp.Create;
  GlobalCEFApp.WindowlessRenderingEnabled := True;
  GlobalCEFApp.EnableHighDPISupport := True; 
  GlobalCEFApp.EnableGPU := True;
  GlobalCEFApp.DestroyApplicationObject := True; 
  GlobalCEFApp.DisableFeatures := 'NetworkService';
  GlobalCEFApp.StartMainProcess;
end;

procedure DestroyGlobalCEFApp;
begin
  FreeAndNil(GlobalCEFApp);
end;

{ TCEFApp }

procedure TCEFApp.SetChromium(const AValue: TChromium);
begin
  if (FChromium <> AValue) and (Assigned(AValue)) then
  begin
    FChromium := AValue;
  end;
end;

procedure TCEFApp.SetPathFramework(const AValue: string);
begin
  if (FPathFramework <> AValue) then
  begin
    FPathFramework := AValue;
    FChanged := True;
    FIniFile.WriteString('Paths', 'Framework', FPathFramework);
  end;
end;

procedure TCEFApp.SetPathResources(const AValue: string);
begin
  if (FPathResources <> AValue) then
  begin
    FPathResources := AValue;
    FChanged := True;
    FIniFile.WriteString('Paths', 'Resources', FPathResources);
  end;
end;

procedure TCEFApp.SetPathLocales(const AValue: string);
begin
  if (FPathLocales <> AValue) then
  begin
    FPathLocales := AValue;
    FChanged := True;
    FIniFile.WriteString('Paths', 'Locales', FPathLocales);
  end;
end;

procedure TCEFApp.SetPathCache(const AValue: string);
begin
  if (FPathCache <> AValue) then
  begin
    FPathCache := AValue;
    FChanged := True;
    FIniFile.WriteString('Paths', 'Cache', FPathCache);
  end;
end;

procedure TCEFApp.SetPathUserData(const AValue: string);
begin
  if (FPathUserData <> AValue) then
  begin
    FPathUserData := AValue;
    FChanged := True;
    FIniFile.WriteString('Paths', 'UserData', FPathUserData);
  end;
end;

procedure TCEFApp.Load;
begin
  if (not (Assigned(FIniFile))) then
  begin
    FIniFile := TIniFile.Create(WBOT_INI);
  end;

  FPathFramework := FIniFIle.ReadString('Paths', 'FrameWork', 'cef');
  FPathResources := FIniFIle.ReadString('Paths', 'Resources', 'cef');
  FPathLocales := FIniFIle.ReadString('Paths', 'Locales', 'cef' +
    PathDelim + 'locales');
  FPathCache := FIniFIle.ReadString('Paths', 'Cache', 'tmp' +
    PathDelim + 'cache');
  FPathUserData := FIniFIle.ReadString('Paths', 'UserData', 'tmp' +
    PathDelim + 'data_user');
  FChanged := False;
  FInitialized := True;

  FrameworkDirPath := UnicodeString(FPathFramework);
  ResourcesDirPath := UnicodeString(FPathResources);
  LocalesDirPath := UnicodeString(FPathLocales);
  Cache := UnicodeString(FPathCache);
  UserDataPath := UnicodeString(FPathUserData);
end;

procedure TCEFApp.Save;
begin
  FIniFIle.WriteString('Paths', 'FrameWork', string(FrameworkDirPath));
  FIniFIle.WriteString('Paths', 'Resources', string(ResourcesDirPath));
  FIniFIle.WriteString('Paths', 'Locales', string(LocalesDirPath));
  FIniFIle.WriteString('Paths', 'Cache', string(Cache));
  FIniFIle.WriteString('Paths', 'UserData', string(UserDataPath));
end;

function TCEFApp.CheckVersion: boolean;
begin
  Result := (CEF_VERSION_MAJOR > CEF_SUPPORTED_VERSION_MAJOR) or
    (CEF_VERSION_MAJOR = CEF_SUPPORTED_VERSION_MAJOR) and
    (CEF_VERSION_MINOR >= CEF_SUPPORTED_VERSION_MINOR) and
    (CEF_VERSION_REVIEW >= CEF_SUPPORTED_VERSION_BUILD);
end;

constructor TCEFApp.Create;
begin
  inherited Create;
  Load;
end;

destructor TCEFApp.Destroy;
begin
  Save;
  FreeAndNil(FIniFile);
  inherited Destroy;
end;

function TCEFApp.StartMainProcess: boolean;
var
  VVersionRequired: string;
  VVersionIdentified: string;
begin
  if (Status <> asInitialized) then
  begin
    if (not (CheckVersion)) then
    begin
      VVersionRequired := IntToStr(CEF_SUPPORTED_VERSION_MAJOR) + '.' +
        IntToStr(CEF_SUPPORTED_VERSION_MINOR) + '.' +
        IntToStr(CEF_SUPPORTED_VERSION_RELEASE);

      VVersionIdentified := IntToStr(CEF_VERSION_MAJOR) + '.' +
        IntToStr(CEF_VERSION_MINOR) + '.' +
        IntToStr(CEF_VERSION_REVIEW);

      Application.MessageBox(PChar(Format(EXCEPT_CEF_VERSION,
        [VVersionRequired, VVersionIdentified])),
        PChar(Application.Title), MB_ICONERROR + MB_OK);

      FInitialized := False;
      Result := False;
      Exit;
    end;
  end;
  inherited StartMainProcess;
end;

initialization
  InitializeGlobalCEFApp;

finalization
  DestroyGlobalCEFApp;

end.

