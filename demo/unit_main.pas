unit unit_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  ExtCtrls, WBot_Core, WBot_Model;

type

  { TForm1 }

  TForm1 = class(TForm)
    ButtonEnviarMsgText: TButton;
    ButtonCarregaContatos: TButton;
    ButtonConecta: TButton;
    CheckBox1: TCheckBox;
    EditNumero: TEdit;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    LabelStatus: TLabel;
    LabelCtName: TLabel;
    LabelCtId: TLabel;
    ListBoxContatos: TListBox;
    MemoRecebida: TMemo;
    MemoEnviada: TMemo;
    MemoMsgTxt: TMemo;
    MemoLog: TMemo;
    PageControl1: TPageControl;
    Panel1: TPanel;
    StatusBar1: TStatusBar;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    WBot1: TWBot;
    procedure ButtonCarregaContatosClick(Sender: TObject);
    procedure ButtonConectaClick(Sender: TObject);
    procedure ButtonEnviarMsgTextClick(Sender: TObject);
    procedure CheckBox1Change(Sender: TObject);
    procedure ListBoxContatosDblClick(Sender: TObject);
    procedure WBot1Connected(Sender: TObject);
    procedure WBot1Disconnected(Sender: TObject);
    procedure WBot1LowBatteryLevel(Sender: TObject);
    procedure WBot1RequestChat(const ASender: TObject;
      const AChats: TResponseChat);
    procedure WBot1RequestContact(const ASender: TObject;
      const ACantacts: TResponseContact);
  private
    procedure AddLog(const AStr: String);
    procedure RespostaAutomatica(const AFoneId, AMsgRecebida: String);
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.ButtonConectaClick(Sender: TObject);
begin
  WBot1.Connect;
end;

procedure TForm1.ButtonEnviarMsgTextClick(Sender: TObject);
begin
  if WBot1.Conected then
  begin
    WBot1.SendMsg(EditNumero.Text, String(MemoMsgTxt.Text).Replace(LineEnding, '\n'));
  end;
end;

procedure TForm1.CheckBox1Change(Sender: TObject);
begin
  WBot1.MonitorUnreadMsgs:=CheckBox1.Checked;
end;

procedure TForm1.ListBoxContatosDblClick(Sender: TObject);
begin
  EditNumero.Text:=ListBoxContatos.GetSelectedText;
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
  LabelStatus.Font.Color:=clGreen;
  LabelStatus.Caption:='Conectado';
  AddLog('Conectado');
end;

procedure TForm1.WBot1Disconnected(Sender: TObject);
begin
  AddLog('Desconectado');
end;


procedure TForm1.WBot1LowBatteryLevel(Sender: TObject);
begin
  AddLog('Bateria baixa...');
end;

procedure TForm1.WBot1RequestChat(const ASender: TObject;
  const AChats: TResponseChat);
var
  I, II: Integer;
  VChat: TChat;
  VMsg: TMessage;
begin
  for I := 0 to Pred(AChats.Result.Count) do
  begin
    VChat:=AChats.Result.Items[I];
    if VChat.IsGroup = False then
    begin
      for II := 0 to Pred(VChat.Messages.Count) do
      begin
        VMsg:=VChat.Messages.Items[II];
        if VMsg.Sender.IsMe = False then
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

procedure TForm1.WBot1RequestContact(const ASender: TObject;
  const ACantacts: TResponseContact);
Var
  I: Integer;
begin
  for I := 0 to Pred(ACantacts.Result.Count) do
  begin
    ListBoxContatos.Items.Add(ACantacts.Result.Items[I].Id);
  end;
end;


procedure TForm1.AddLog(const AStr: String);
begin
  MemoLog.Lines.Add(AStr);
  MemoLog.SelLength:=Length(MemoLog.Text);
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
    WBot1.SendMsg(
      AFoneId,
      CMSGPER
    );
  end
  else
  begin
    MemoEnviada.Text:=CMSGDEF;
    WBot1.SendMsg(AFoneId, CMSGDEF);
  end;
end;

end.

