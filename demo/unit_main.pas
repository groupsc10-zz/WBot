unit unit_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  ExtCtrls, StrUtils, WBot_Core, WBot_Model, WBot_Utils;

type

  { TForm1 }

  TForm1 = class(TForm)
    ButtonEnviarMsgText: TButton;
    ButtonCarregaContatos: TButton;
    ButtonConecta: TButton;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    EditNumero: TEdit;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    LabelStatus: TLabel;
    LabelCtName: TLabel;
    LabelCtId: TLabel;
    ListBoxContatos: TListBox;
    MemoLog: TMemo;
    MemoRecebida: TMemo;
    MemoEnviada: TMemo;
    MemoMsgTxt: TMemo;
    PageControl1: TPageControl;
    Panel1: TPanel;
    TabSheet1: TTabSheet;
    TabSheet3: TTabSheet;
    WBot1: TWBot;
    procedure ButtonCarregaContatosClick(Sender: TObject);
    procedure ButtonConectaClick(Sender: TObject);
    procedure ButtonEnviarMsgTextClick(Sender: TObject);
    procedure CheckBox1Change(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ListBoxContatosDblClick(Sender: TObject);
    procedure WBot1Connected(Sender: TObject);
    procedure WBot1Disconnected(Sender: TObject);
    procedure WBot1LowBatteryLevel(Sender: TObject);
    procedure WBot1RequestChat(const Sender: TObject;
      const AChats: TResponseChat);
    procedure WBot1RequestContact(const Sender: TObject;
      const AContacts: TResponseContact);
  private
    procedure AddLog(const AStr: String);
    procedure Limpar;
    procedure RespostaAutomatica(const AFoneId, AMsgRecebida: String);
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

function ExtractBetween(const AValue, ADelimiterA, ADelimiterB: string): string;
var
  VPositonA: NativeInt;
  VPositonB: NativeInt;
begin
  Result := EmptyStr;
  VPositonA := Pos(ADelimiterA, AValue);
  if (VPositonA > 0) then
  begin
    VPositonA := VPositonA + Length(ADelimiterA);
    VPositonB := PosEx(ADelimiterB, AValue, VPositonA);
    if (VPositonB > 0) then
    begin
      Result := Copy(AValue, VPositonA, VPositonB-VPositonA);
    end;
  end;
end;

{ TForm1 }

procedure TForm1.ButtonConectaClick(Sender: TObject);
begin
  if (WBot1.Conected) then
  begin
    WBot1.Disconnect;
    Limpar;
  end
  else
  begin
    WBot1.MonitorBattery := CheckBox3.Checked;
    WBot1.Browser := CheckBox2.Checked;
    WBot1.Connect;
  end;
end;

procedure TForm1.ButtonEnviarMsgTextClick(Sender: TObject);
begin
  if WBot1.Conected then
  begin
    WBot1.SendMsg(EditNumero.Text, NormalizeString(MemoMsgTxt.Text));
  end;
end;

procedure TForm1.CheckBox1Change(Sender: TObject);
begin
  WBot1.MonitorUnreadMsgs:=CheckBox1.Checked;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  Limpar;
end;

procedure TForm1.ListBoxContatosDblClick(Sender: TObject);
begin
  EditNumero.Text:= ExtractBetween(ListBoxContatos.GetSelectedText, '(', ')');
end;

procedure TForm1.ButtonCarregaContatosClick(Sender: TObject);
begin
  if WBot1.Conected then
  begin
    WBot1.GetAllContacts;
  end;
end;

procedure TForm1.WBot1Connected(Sender: TObject);
begin
  LabelStatus.Color:=clGreen;
  LabelStatus.Caption:='Conectado'; 
  ButtonConecta.Caption:='Disconectar';
  AddLog('Conectado');
end;

procedure TForm1.WBot1Disconnected(Sender: TObject);
begin                       
  LabelStatus.Color:=clRed;
  LabelStatus.Caption:='Disconectado';
  ButtonConecta.Caption:='Connectar';
  AddLog('Desconectado');
end;


procedure TForm1.WBot1LowBatteryLevel(Sender: TObject);
begin
  AddLog('Bateria baixa...');
end;

procedure TForm1.WBot1RequestChat(const Sender: TObject;
  const AChats: TResponseChat);
var
  VChat: TChat;
  VMsg: TMessage;
begin
  for VChat in AChats.Result do
  begin
    if (Assigned(VChat)) and (not(VChat.IsGroup)) then
    begin
      for VMsg in VChat.Messages do
      begin
        if (Assigned(VMsg)) and (not(VMsg.Sender.IsMe)) then
        begin
          LabelCtId.Caption:=VChat.Contact.Id;
          LabelCtName.Caption:=VChat.Contact.Name;
          MemoRecebida.Text:=VMsg.Content;
          RespostaAutomatica(VChat.Contact.Id, VMsg.Content);
        end;
      end;
    end;
  end;
end;

procedure TForm1.WBot1RequestContact(const Sender: TObject;
  const AContacts: TResponseContact);
Var
  VContact: TContact;
begin
  for VContact in AContacts.Result do
  begin
    if (Assigned(VContact)) then
    begin
      ListBoxContatos.Items.Add(VContact.Name + ' (' + VContact.Id +')');
    end;
  end;
end;

procedure TForm1.AddLog(const AStr: String);
begin
  MemoLog.Lines.Add(AStr);
  MemoLog.SelLength:=Length(MemoLog.Text);
end;

procedure TForm1.Limpar;
begin  
  PageControl1.ActivePage := TabSheet1;
  LabelStatus.Color := clRed;
  LabelStatus.Font.Color := clWhite;
  LabelStatus.Font.Style := LabelStatus.Font.Style + [fsBold];
  ListBoxContatos.Clear;
  EditNumero.Text:= '';
  MemoMsgTxt.Text:= 'Olá';
end;

procedure TForm1.RespostaAutomatica(const AFoneId, AMsgRecebida: String);
const
  CMSGDEF = 'Você esta recebendo uma mensagem automatica ao escolher a opção 1!';
  CMSGPER ='Olá, este é um teste de mensagem automatica feito com WBot 1.0'+'\n'+
           'Digite a opção *1*.';
begin
  //Ler mensagem
  WBot1.ReadMsg(AFoneId);
  if AMsgRecebida <> '1' then
  begin
    MemoEnviada.Text:=CMSGPER;
    WBot1.SendMsg(AFoneId, CMSGPER);
  end
  else
  begin
    MemoEnviada.Text:=CMSGDEF;
    WBot1.SendMsg(AFoneId, CMSGDEF);
  end;
end;

end.

