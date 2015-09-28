{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit laz_acs;

interface

uses
  acs_audio, acs_audiomix, acs_cdrom, acs_classes, acs_converters, acs_file, 
  acs_filters, acs_indicator, acs_misc, acs_mixer, acs_multimix, acs_procs, 
  acs_streams, acs_strings, acs_types, acs_dsfiles, acs_flac, acs_lame, 
  acs_mac, acs_vorbis, acs_wave, acs_allformats, acs_dxaudio, acs_stdaudio, 
  acs_alsaaudio, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('laz_acs', @Register);
end.
