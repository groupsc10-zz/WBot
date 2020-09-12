{
 _    _  _         _____
| |  | || |       |_   _|   /|
| |  | || |___  ___ | |    / |__
| |/\| ||  _  |/ _ \| |   /_   /
| /  \ || (_) | (_) | |     | /
|__/\__||_____|\___/|_|     |/

}
unit WBot_Form;

{$i wbot.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls, LResources,
  // CEF
  uCEFWindowParent, uCEFChromium, uCEFTypes, uCEFInterfaces, uCEFApplication,
  // WBot
  WBot_Const, WBot_Utils, WBot_Model;

type

  { TWBotForm }

  TWBotForm = class(TForm)
    ChromiumWindow: TCEFWindowParent;
    Chromium: TChromium;
    QrCodeImg: TImage;
    StatusBar: TStatusBar;
    TimerConnect: TTimer;
    TimerMonitoring: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);  
    procedure FormShow(Sender: TObject);  
    procedure OnTimerConnect(Sender: TObject);
    procedure OnTimerMonitoring(Sender: TObject);
    // Chromium
    procedure ChromiumAfterCreated(Sender: TObject;
      const {%H-}ABrowser: ICefBrowser);
    procedure ChromiumConsoleMessage(Sender: TObject;
      const {%H-}ABrowser: ICefBrowser; {%H-}ALevel: TCefLogSeverity;
      const AMessage, {%H-}ASource: ustring; {%H-}ALine: Integer;
      out {%H-}AResult: Boolean);
    procedure ChromiumTitleChange(Sender: TObject;
      const {%H-}ABrowser: ICefBrowser; const {%H-}ATitle: ustring);
  private
    FAuthenticated: boolean;
    FConected: boolean;
    FBrowser: Boolean;
    FStep: NativeInt;
    FZoom: NativeInt;
    FLastQrCode: string; 
    FMonitorLowBattery: boolean;
    FMonitorUnreadMsgs: boolean;
    FOnError: TErrorEvent;
    FOnNotification: TNotificationEvent;
  private
    procedure SetZoom(const AValue: NativeInt);
  protected
    procedure CheckCEFApp;
    procedure InternalNotification(const AAction: TActionType;
      const AData: string = '');
    procedure InternalError(const AError: string;
      const AAdditionalInformation: string);
    procedure ExecuteScript(const AScript: string;
      const ADirect: boolean = False);
  protected
    procedure ProcessQrCode(const AData: string);
  public   
    procedure Connect;
  public
    procedure GetQrCode;
    procedure GetBatteryLevel; 
    procedure GetUnreadMessages;  
    procedure GetAllContacts; 
    procedure GetAllGroups;
    procedure SendMsg(const ANumber, AMsg: string);
  public
    property Conected: boolean read FConected;
    property Authenticated: boolean read FAuthenticated;
    property Browser: Boolean read FBrowser write FBrowser;
    property MonitorLowBattery: boolean
      read FMonitorLowBattery write FMonitorLowBattery;
    property MonitorUnreadMsgs: boolean
      read FMonitorUnreadMsgs write FMonitorUnreadMsgs;
    property OnNotification: TNotificationEvent
      read FOnNotification write FOnNotification;
    property OnError: TErrorEvent read FOnError write FOnError;
  end;

var
  WBotForm: TWBotForm;

implementation

{$R *.lfm}

{ TWBotForm }

procedure TWBotForm.FormCreate(Sender: TObject);
begin
  Chromium.DefaultUrl := WBOT_WHATSAPP;
  FConected := False;
  FAuthenticated := False;
  FBrowser := True;
  FStep := 0;
  FZoom := 1;
  FLastQrCode := EmptyStr;
  FMonitorLowBattery := True;
  FMonitorUnreadMsgs := True;
  FOnError := nil;
  FOnNotification:= nil;
end;

procedure TWBotForm.FormDestroy(Sender: TObject);
begin
  InternalNotification(atDestroy);
end;          

procedure TWBotForm.FormShow(Sender: TObject);
begin
  Caption := WBOT_NAME + ' - ' + WBOT_VERSION;
  StatusBar.SimpleText := EmptyStr;
  ChromiumWindow.Visible := False;
  QrCodeImg.Visible := False;
  if (FBrowser) then
  begin                      
    ChromiumWindow.Visible := True;
  end
  else
  begin                            
    QrCodeImg.Visible := True;
    StatusBar.SimpleText := QRCODE_LOADING;
  end;
end;

procedure TWBotForm.OnTimerConnect(Sender: TObject);
var
  VStatus: boolean;
  VScript: string;
begin
  TimerConnect.Enabled := False;
  VStatus := True;
  try
    if (FAuthenticated) then
    begin
      // Inject script     
      {$i wbot.lrs}
      VScript := ResourceToString('wbot');    
      {$IfDef wbot_debug}
      WriteLn('wbot script ', VScript);
      {$EndIf}
      ExecuteScript(VScript, True);
      Sleep(50);
      if (FBrowser) then
      begin
        //TODO: Self Close/Hide
      end;
      VStatus := False;
      InternalNotification(atInitialized);
    end
    else
    begin
      GetQrCode;
    end;
  finally
    TimerConnect.Enabled := VStatus;
  end;
end;

procedure TWBotForm.OnTimerMonitoring(Sender: TObject);
begin
  TimerMonitoring.Enabled := False;
  try
    if (FAuthenticated) then
    begin
      if (FMonitorLowBattery) then
      begin
        GetBatteryLevel;
      end;
      if (FMonitorUnreadMsgs) then
      begin
        GetUnreadMessages;
      end;
    end;
  finally
    TimerMonitoring.Enabled := True;
  end;
end;

procedure TWBotForm.ChromiumAfterCreated(Sender: TObject;
  const ABrowser: ICefBrowser);
begin
  TimerConnect.Enabled := True;
end;

procedure TWBotForm.ChromiumConsoleMessage(Sender: TObject;
  const ABrowser: ICefBrowser; ALevel: TCefLogSeverity;
  const AMessage, ASource: ustring; ALine: Integer; out AResult: Boolean);
var
  VModel: TResponseConsole;
  VAction: TActionType;
begin
  {$IfDef wbot_debug}  
  WriteLn(AMessage);
  {$EndIf}
  // Check JSON
  if (Copy(TrimLeft(AMessage), 1, 2) <> '{"') then
  begin
    Exit;
  end;
  VModel := TResponseConsole.Create;
  try  
    VModel.LoadJSON(string(AMessage));
    VAction := VModel.ActionType;
    // Redirect
    case VAction of
      atNone:
      begin
        InternalError(EXCEPT_JS_UNKNOWN, string(AMessage));
      end;  
      atGetQrCode:
      begin
        ProcessQrCode(VModel.SaveJSON);
        InternalNotification(VAction);
      end;
      else
      begin
        InternalNotification(VAction, VModel.Result);
      end;
    end;
  finally
    FreeAndNil(VModel);
  end;
end;

procedure TWBotForm.ChromiumTitleChange(Sender: TObject;
  const ABrowser: ICefBrowser; const ATitle: ustring);
begin
  Inc(FStep);
  if (FStep > 3) and (FStep < 10) and (not(FAuthenticated)) then
  begin
    // Established connection   
    FAuthenticated := True;
    InternalNotification(atConnected);
  end;
  if (FStep <= 3) then
  begin
    SetZoom(-2);
  end;
end;

procedure TWBotForm.InternalNotification(const AAction: TActionType;
  const AData: string);
begin
  if (Assigned(FOnNotification)) then
  begin
    FOnNotification(Self, AAction, AData);
  end;
end;

procedure TWBotForm.InternalError(const AError: string;
  const AAdditionalInformation: string);
begin
  if (Assigned(FOnError)) then
  begin
    FOnError(Self, AError, AAdditionalInformation);
  end;
end;

procedure TWBotForm.ExecuteScript(const AScript: string;
  const ADirect: boolean);
begin
  CheckCEFApp;  
  if (not (FConected)) and (not (ADirect))  then
  begin
    raise EWBot.Create(EXCEPT_CEF_CONNECT);
  end;
  Chromium.Browser.MainFrame.ExecuteJavaScript(UnicodeString(AScript),
    UnicodeString('about:blank'), 0);
end;

procedure TWBotForm.ProcessQrCode(const AData: string);
var
  VModel: TResponseQrCode;
begin
  if (FLastQrCode <> AData) then
  begin                  
    FLastQrCode := AData;
    VModel := TResponseQrCode.Create;
    try
      VModel.LoadJSON(FLastQrCode);
      QrCodeImg.Picture.Assign(VModel.Result.QrCodeImage);
      if (Assigned(QrCodeImg.Picture.Graphic))  and
        (not(QrCodeImg.Picture.Graphic.Empty)) then
      begin
        StatusBar.SimpleText := QRCODE_SUCCESS;
      end;
    finally
      FreeAndNil(VModel);
    end;
  end;
end;

procedure TWBotForm.SetZoom(const AValue: NativeInt);
var
  VIndex: NativeInt;
  VZoom: NativeInt;
begin
  if (FZoom <> AValue) then
  begin
    FZoom := AValue;
    Chromium.ResetZoomStep;
    VZoom := (FZoom * (-1));
    for VIndex := 0 to (VZoom - 1) do
    begin
      Chromium.DecZoomStep;
    end;
  end;
end;

procedure TWBotForm.CheckCEFApp;
begin
  if (GlobalCEFApp.Status <> asInitialized) then
  begin
    raise EWBot.Create(EXCEPT_CEF_APP);
  end;
end;

procedure TWBotForm.Connect;
var
  VStart: NativeUInt;
begin
  CheckCEFApp;
  try
    if (not(FConected)) then
    begin
      InternalNotification(atConnecting);
      VStart := GetTickCount64;
      FConected := Chromium.CreateBrowser(ChromiumWindow);
      repeat
        FConected := (Chromium.Initialized);
        if (not (FConected)) then
        begin
          Sleep(10);
          Application.ProcessMessages;
          if ((GetTickCount64 - VStart) > 20000) then
          begin
            Break;
          end;
        end;
      until FConected;
    end;
  finally
    TimerMonitoring.Enabled := FConected;
    if (FConected) then
    begin
      Chromium.OnConsoleMessage := @ChromiumConsoleMessage;
      Chromium.OnTitleChange := @ChromiumTitleChange;
      Show;
    end
    else
    begin
      InternalNotification(atDisconnected);
      raise EWBot.Create(EXCEPT_CEF_CONNECT);
    end;
  end;
end;

procedure TWBotForm.GetQrCode;
begin
  if (not(FBrowser)) then
  begin
    ExecuteScript(CMD_GET_QRCODE);
  end;
end;

procedure TWBotForm.GetBatteryLevel;
begin                                  
  ExecuteScript(CMD_GET_BATTERY_LEVEL);
end;

procedure TWBotForm.GetUnreadMessages;
begin
  ExecuteScript(CMD_GET_UNREAD_MESSAGES);
end;

procedure TWBotForm.GetAllContacts;
begin
  ExecuteScript(CMD_GET_ALL_CONTACTS);
end;

procedure TWBotForm.GetAllGroups;
begin
  ExecuteScript(CMD_GET_ALL_GROUPS);
end;

procedure TWBotForm.SendMsg(const ANumber, AMsg: string);
var
  VScript: string;
begin
  VScript := CMD_SEND_CHAT_STATE + LineEnding + CMD_SEND_MSG;
  VScript := ReplaceVAR(VScript, '<#MSG_PHONE#>', Trim(ANumber));
  VScript := ReplaceVAR(VScript, '<#MSG_BODY#>', Trim(AMsg));
  ExecuteScript(VScript);
end;

end.
