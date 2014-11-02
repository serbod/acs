unit acs_allformats;

interface

uses
  ACS_Vorbis,
  {$IFDEF MSWINDOWS}
  ACS_DSFiles,
  ACS_MAC,
  {$ELSE}
  acs_mpeg,
  {$ENDIF}
  ACS_FLAC,
  ACS_LAME,
  ACS_Wave;

implementation

end.

