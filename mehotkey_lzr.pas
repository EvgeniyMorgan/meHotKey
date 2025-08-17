{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit meHotKey_lzr;

{$warn 5023 off : no warning about unused units}
interface

uses
  meHotKey, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('meHotKey', @meHotKey.Register);
end;

initialization
  RegisterPackage('meHotKey_lzr', @Register);
end.
