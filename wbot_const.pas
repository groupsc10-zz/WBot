{
 _    _  _               _____
| |  | || |             |_   _|   /|
| |  | || |___  ___   ___ | |    / |__
| |/\| ||  _  |/ _ \ / _ \| |   /_   /
| /  \ || (_) | (_) | (_) | |     | /
|__/\__||_____|\___/ \___/|_|     |/

}
unit WBot_Const;

{$i wbot.inc}

interface

uses
  Classes, SysUtils;

const
  WBOT_NAME = 'WBOT';  
  WBOT_VERSION = '0.1.1.1';   
  WBOT_WHATSAPP = 'https://web.whatsapp.com/';
  WBOT_JS = 'js.bot';
  WBOT_INI = 'wbot.ini';

  CEF_VERSION_MAJOR = 85;
  CEF_VERSION_MINOR = 2;
  CEF_VERSION_REVIEW = 11;
  CEF_VERSION_BUILD = 1;


  CMD_SEND_CHAT_STATE =
    'Store.WapQuery.sendChatstateComposing("<#MSG_PHONE#>");';
  CMD_SEND_MSG =
    'window.WAPI.sendMessageToID("<#MSG_PHONE#>","<#MSG_BODY#>")';


  CMD_GET_QRCODE =
    'var _qrCode = document.getElementsByTagName("canvas")[0].toDataURL("image/png");' + LineEnding +
    'console.log(JSON.stringify({"name":"getQrCode","result":{_qrCode}}));';
  CMD_GET_BATTERY_LEVEL =
    'window.WAPI.getBatteryLevel();';
  CMD_GET_UNREAD_MESSAGES =
    'window.WAPI.getUnreadMessages(includeMe="True", includeNotifications="True", use_unread_count="True");';
  CMD_GET_ALL_GROUPS =
    'window.WAPI.getAllGroups();';
  CMD_GET_ALL_CONTACTS =
    'window.WAPI.getAllContacts();';
  CMD_GET_ALL_CHATS =
    'window.WAPI.getAllChats();';

resourcestring
  EXCEPT_CEF_BROWSER =
    'Error creating browser in CEF';
  EXCEPT_CEF_CONNECT =
    'You are not connected to the service server';
  EXCEPT_CEF_VERSION =
    'Your CEF4 version is not compatible, please update your library at https://github.com/salvadordf/CEF4Delphi'
    + LineEnding + 'Required version: %s' + LineEnding + 'Identified version: %s';

  EXCEPT_JS_UNKNOWN = 'Unknown js.bot return';


  QRCODE_LOADING =  'Loading QRCode...';
  QRCODE_SUCCESS = 'Point your phone now!';

implementation

end.

