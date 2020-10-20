{
 _    _  _         _____
| |  | || |       |_   _|   /|
| |  | || |___  ___ | |    / |__
| |/\| ||  _  |/ _ \| |   /_   /
| /  \ || (_) | (_) | |     | /
|__/\__||_____|\___/|_|     |/

}
unit WBot_Core;

{$i wbot.inc}

interface

uses
  Classes, SysUtils, StrUtils, LResources,
  //WBot
  WBot_Model, WBot_Form;

type            
  TRequestChatEvent = procedure(const ASender: TObject;
    const AChats: TResponseChat) of object;
  TRequestContactsEvent = procedure(const ASender: TObject;
    const AContacts: TResponseContact) of object;  
  TRequestGroupsEvent = procedure(const ASender: TObject;
    const AGroups: TResponseGroups) of object;

  { TWBot }

  TWBot = class(TComponent)
  private
    FBrowser: Boolean;
    FLowBatteryLevel: NativeInt;
    FMonitorBattery: boolean;
    FMonitorUnreadMsgs: boolean;
    FOnConnected: TNotifyEvent;
    FOnDisconnected: TNotifyEvent;
    FOnError: TErrorEvent;
    FOnLowBatteryLevel: TNotifyEvent;
    FOnNotification: TNotificationEvent; 
    FOnRequestChat: TRequestChatEvent;
    FOnRequestContact: TRequestContactsEvent;
    FForm: TWBotForm;
    FOnRequestGroups: TRequestGroupsEvent;
    FVersion: string;
    function GetAuthenticated: boolean;
    function GetConected: boolean;
    procedure SetUnreadMsgs(const AValue: boolean);
  protected
    property Console: TWBotForm read FForm;
    procedure InternalError(const ASender: TObject; const AError: string;
      const AAdditionalInformation: string);  
    procedure InternalNotification(const ASender: TObject;
      const AAction: TActionType; const AData: string);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Connect;
    procedure Disconnect(const ALogout: boolean = False);
    procedure GetAllContacts;
    procedure GetAllGroups;
    procedure GetBatteryLevel;
    procedure GetUnreadMessages;
    procedure ReadMsg(const APhone: String);
    procedure SendContact(const APhone, AContact: string);
    procedure SendFile(const APhone, ACaption, AFileName: string);
    procedure SendMsg(const APhone, AMsg: string);
  public                
    property Authenticated: boolean read GetAuthenticated;
    property Conected: boolean read GetConected;
    // Events
    property OnError: TErrorEvent read FOnError write FOnError;
    property OnNotification: TNotificationEvent
      read FOnNotification write FOnNotification;
  published
    property Browser: Boolean read FBrowser write FBrowser default True;
    property LowBatteryLevel: NativeInt
      read FLowBatteryLevel write FLowBatteryLevel default 10;
    property MonitorBattery: boolean
      read FMonitorBattery write FMonitorBattery default True;
    property MonitorUnreadMsgs: boolean
      read FMonitorUnreadMsgs write SetUnreadMsgs default True;
    property Version: string read FVersion;
    // Events
    property OnConnected: TNotifyEvent
      read FOnConnected write FOnConnected;
    property OnDisconnected: TNotifyEvent
      read FOnDisconnected write FOnDisconnected; 
    property OnLowBatteryLevel: TNotifyEvent
      read FOnLowBatteryLevel write FOnLowBatteryLevel; 
    property OnRequestChat: TRequestChatEvent
      read FOnRequestChat write FOnRequestChat;
    property OnRequestContact: TRequestContactsEvent
      read FOnRequestContact write FOnRequestContact;  
    property OnRequestGroups: TRequestGroupsEvent
      read FOnRequestGroups write FOnRequestGroups;
  end;

procedure Register;

implementation

uses
  WBot_Const, WBot_Utils;

procedure Register;
begin        
  {$i wbot.lrs}
  RegisterComponents('WBot', [TWBot]);
end;

{ TWBot }

function TWBot.GetAuthenticated: boolean;
begin        
  if (csDesigning in ComponentState) then
  begin
    Exit;
  end;
  Result := FForm.Authenticated;
end;

function TWBot.GetConected: boolean;
begin
  if (csDesigning in ComponentState) then
  begin
    Exit;
  end;
  Result := FForm.Conected;
end;

procedure TWBot.SetUnreadMsgs(const AValue: boolean);
begin
  if (FMonitorUnreadMsgs<>AValue) then
  begin
    FMonitorUnreadMsgs:=AValue;    
    if (not(csDesigning in ComponentState)) then
    begin
      FForm.MonitorUnreadMsgs:=FMonitorUnreadMsgs;
    end;
  end;
end;

procedure TWBot.InternalError(const ASender: TObject; const AError: string;
  const AAdditionalInformation: string);
begin
  if (Assigned(FOnError)) then
  begin
    FOnError(ASender, AError, AAdditionalInformation);
  end;
end;

procedure TWBot.InternalNotification(const ASender: TObject;
  const AAction: TActionType; const AData: string);
var                              
  VResponseChat: TResponseChat;
  VResponseContact: TResponseContact;   
  VResponseGroups: TResponseGroups;
  VResponseBattery: TResponseBattery;
begin
  if (Assigned(FOnNotification)) then
  begin
    FOnNotification(ASender, AAction, AData);
  end;

  case AAction of
    atConnected:
    begin
      if (Assigned(FOnConnected)) then
      begin
        FOnConnected(Self);
      end;
    end;

    atDisconnected:
    begin
      if (Assigned(FOnDisconnected)) then
      begin
        FOnDisconnected(Self);
      end;
    end;

    atGetBatteryLevel:
    begin
      if (Assigned(FOnLowBatteryLevel)) then
      begin
        VResponseBattery := TResponseBattery.Create;
        try
          VResponseBattery.LoadJSON(AData);
          if (StrToIntDef(VResponseBattery.Result, 0) <= FLowBatteryLevel) then
          begin
            FOnLowBatteryLevel(Self);
          end;
        finally
          FreeAndNil(VResponseBattery);
        end;
      end;
    end;

    atGetAllContacts:
    begin
      if (Assigned(FOnRequestContact)) then
      begin
        VResponseContact := TResponseContact.Create;
        try
          VResponseContact.LoadJSON(AData);
          FOnRequestContact(Self, VResponseContact);
        finally
          FreeAndNil(VResponseContact);
        end;
      end;
    end;

    atGetAllGroups:
    begin   
      if (Assigned(FOnRequestGroups)) then
      begin
        VResponseGroups := TResponseGroups.Create;
        try
          VResponseGroups.LoadJSON(AData);
          FOnRequestGroups(Self, VResponseGroups);
        finally
          FreeAndNil(VResponseGroups);
        end;
      end;
    end;

    atGetAllChats,
    atGetUnreadMessages:
    begin
      if (Assigned(FOnRequestChat)) then
      begin
        VResponseChat := TResponseChat.Create;
        try
          VResponseChat.LoadJSON(AData);
          FOnRequestChat(Self, VResponseChat);
        finally
          FreeAndNil(VResponseChat);
        end;
      end;
    end;
  end;
end;

constructor TWBot.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);    
  if (not(csDesigning in ComponentState)) then
  begin
    FForm := TWBotForm.Create(Self);  
    FForm.OnError := @InternalError;
    FForm.OnNotification := @InternalNotification;
  end;
  FBrowser := True;
  FLowBatteryLevel := 10;
  FMonitorBattery := True;
  FMonitorUnreadMsgs:= True;
  FVersion := WBOT_VERSION;
end;

destructor TWBot.Destroy;
begin
  inherited Destroy;
end;

procedure TWBot.Connect;
begin
  FForm.Browser := FBrowser;
  FForm.MonitorBattery := FMonitorBattery;
  FForm.MonitorUnreadMsgs := FMonitorUnreadMsgs;
  FForm.Connect;
end;

procedure TWBot.Disconnect(const ALogout: boolean);
begin
  FForm.Disconnect(ALogout);
end;

procedure TWBot.GetAllContacts;
begin
  FForm.GetAllContacts;
end;

procedure TWBot.GetAllGroups;
begin                 
  FForm.GetAllGroups;
end;

procedure TWBot.GetBatteryLevel;
begin               
  FForm.GetBatteryLevel;
end;

procedure TWBot.GetUnreadMessages;
begin                  
  FForm.GetUnreadMessages;
end;      

procedure TWBot.ReadMsg(const APhone: String);
begin
  FForm.ReadMsg(APhone);
end;

procedure TWBot.SendContact(const APhone, AContact: string);
begin                      
  // TODO: Check phone structure
  FForm.SendContact(APhone, AContact);
end;

procedure TWBot.SendFile(const APhone, ACaption, AFileName: string);
var
  VStream: TStream;   
  VFileBase64: string;
  VFileName: string;
  VFileExt: string;
  VMsg: string;
begin     
  // TODO: Check phone structure
                             
  if (not(FileExists(AFileName))) then
  begin
    InternalError(Self, Format(EXCEPT_FILE_NOFOUND, [AFileName]), '');
    Exit;
  end;

  VStream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  try                                      
    VFileBase64 := StreamToBase64(VStream);
  finally
    FreeAndNil(VStream);
  end;
  VFileName := ExtractFileName(AFileName);
  VFileExt := ExtractFileExt(AFileName);
  VMsg := 'data:application/';
  if (AnsiIndexStr(VFileExt,
    ['.jpg', '.jpeg', '.tif', '.ico', '.bmp', '.png', '.raw']) > -1) then
  begin
    VMsg := 'data:image/';
  end;
  VMsg := VMsg + VFileExt + ';base64,' + VFileBase64;

  // Send
  FForm.SendMsgBase64(APhone, VMsg, VFileName, ACaption);
end;  

procedure TWBot.SendMsg(const APhone, AMsg: string);
begin
  // TODO: Check phone structure
  FForm.SendMsg(APhone, AMsg);
end;

end.

