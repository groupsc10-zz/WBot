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
  Classes, SysUtils, LResources;

type

  { TWBot }

  TWBot = class(TComponent)
  private
    FVersion: string;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Version: string read FVersion;
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

constructor TWBot.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FVersion := WBOT_VERSION;
end;

end.

