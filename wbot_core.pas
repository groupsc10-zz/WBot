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
  Classes, SysUtils, LResources,
  //WBot
  WBot_Model, WBot_Form;

type      
  TGetAllContactsEvent = procedure(const ASender: TObject;
    const ACantacts: TResponseContact) of object;

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
    FOnGetAllContact: TGetAllContactsEvent;
    FOnNotification: TNotificationEvent; 
    FOnLowBatteryLevel: TNotifyEvent;
    FForm: TWBotForm;    
    FVersion: string;
    function GetAuthenticated: boolean;
    function GetConected: boolean;
  protected
    procedure InternalError(const ASender: TObject; const AError: string;
      const AAdditionalInformation: string);  
    procedure InternalNotification(const ASender: TObject;
      const AAction: TActionType; const AData: string);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Connect;
    procedure Disconnect;
    procedure GetAllContacts;
    procedure GetAllGroups;
    procedure GetBatteryLevel;
    procedure GetUnreadMessages;
    procedure SendArchive(const APhone, AArchive, AMsg: string);
    procedure SendContact(const APhone, AContact: string);   
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
      read FMonitorUnreadMsgs write FMonitorUnreadMsgs default True;
    property Version: string read FVersion;
    // Events
    property OnConnected: TNotifyEvent
      read FOnConnected write FOnConnected;
    property OnDisconnected: TNotifyEvent
      read FOnDisconnected write FOnDisconnected;
    property OnGetAllContact: TGetAllContactsEvent
      read FOnGetAllContact write FOnGetAllContact;
    property OnLowBatteryLevel: TNotifyEvent
      read FOnLowBatteryLevel write FOnLowBatteryLevel;
  end;

procedure Register;

implementation

uses
  WBot_Const;

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
  VResponseContact: TResponseContact;  
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
      if (Assigned(FOnGetAllContact)) then
      begin
        VResponseContact := TResponseContact.Create;
        try
          VResponseContact.LoadJSON(AData);
          FOnGetAllContact(Self, VResponseContact);
        finally
          FreeAndNil(VResponseContact);
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
  FMonitorBattery := False;
  FMonitorUnreadMsgs:= False;
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

procedure TWBot.Disconnect;
begin
  FForm.Disconnect;
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

procedure TWBot.SendArchive(const APhone, AArchive, AMsg: string);
begin

end;

procedure TWBot.SendContact(const APhone, AContact: string);
begin

end;

procedure TWBot.SendMsg(const APhone, AMsg: string);
begin
  FForm.SendMsg(APhone, AMsg);
end;

end.

