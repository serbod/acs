{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit laz_acs_visual;

interface

uses
  acs_properties, acs_reg, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('acs_reg', @acs_reg.Register);
end;

initialization
  RegisterPackage('laz_acs_visual', @Register);
end.
