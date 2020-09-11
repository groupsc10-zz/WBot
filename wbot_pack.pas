{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit WBot_Pack;

{$warn 5023 off : no warning about unused units}
interface

uses
  WBot_Const, WBot_Model, WBot_Utils, WBot_Config, WBot_Form, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('WBot_Pack', @Register);
end.
