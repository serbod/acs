unit acs_allformats;

interface

{$I ../../acs_defines.inc}

uses
  acs_classes

  {$IFDEF ACS_VORBIS_EXT}
  , ACS_Vorbis
  {$ENDIF}

  {$IFDEF ACS_MP3IN_L3_BUILTIN}
  , acs_mp3
  {$ENDIF}

  {$IFDEF MSWINDOWS}
    {$IFDEF DIRECTX_ENABLED}
    , ACS_DSFiles
    {$ENDIF}
    , ACS_MAC
  {$ELSE}
    {$IFDEF ACS_MP3IN_L12_EXT}
    , acs_mpeg
    {$ENDIF}
  {$ENDIF}

  {$IFDEF ACS_FLAC_EXT}
  , ACS_FLAC
  {$ENDIF}

  {$IFDEF ACS_LAME_EXT}
  , ACS_LAME
  {$ENDIF}

  {$IFDEF ACS_WAVE_BUILTIN}
  , ACS_Wave
  {$ENDIF}

  ;

implementation

end.

