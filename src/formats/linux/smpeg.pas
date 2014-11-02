(*
  this file is a part of audio components suite v 2.3.
  copyright (c) 2002-2005 andrei borovsky. all rights reserved.
  see the license file for more details.
  you can contact me at mail@z0m3ie.de
*)

{
$Log: smpeg.pas,v $
Revision 1.5  2006/09/04 14:40:16  z0m3ie
*** empty log message ***

Revision 1.4  2006/08/30 18:59:51  z0m3ie
*** empty log message ***

Revision 1.3  2005/12/19 18:37:03  z0m3ie
*** empty log message ***

Revision 1.1  2005/09/12 22:04:53  z0m3ie
modified structure again, fileformats are now in an sperat folder.
all File In/Out classes are capsulated from TFileIn and TFileOut

Revision 1.1  2005/08/25 20:18:00  z0m3ie
Version 2.4 restructure
TCDPlayer removed (fits not in component structure)
TMP3ToWavConverter removed (fits not in component structure)

Revision 1.4  2005/08/22 20:17:01  z0m3ie
changed Headers to log
changed mail adress

}


{$weakpackageunit on}

// Linux and Windows C-Compilers use different byte-allignment
{$IFDEF Linux}
  {$align 4} // linux uses dword alignment
{$endif}
{$ifdef win32}
  {$ifdef ver140}
    {$align 8} // windows uses quad-word alignment
  {$endif}
{$endif}

{$IFDEF FPC}
{$PACKRECORDS 4}
{$ENDIF FPC}

unit smpeg;

{$ifdef fpc}
{$mode delphi}
{$endif}

interface

uses
  SysUtils,ACS_Procs,Dialogs
  {$ifdef LINUX}
  , baseunix,dl
  {$else}
  , Windows
  {$endif}
  ;
  
const

  SMPEG_ERROR = -1;
  SMPEG_STOPPED = 0;
  SMPEG_PLAYING = 1;

type

  _SMPEG = record
    //obj: PMPEG;
  end;
  TSMPEG = _SMPEG;
  PSMPEG = ^_SMPEG;

  SDL_AudioSpec = record
    freq: Integer;
    format: Word;
    channels: Byte;
    silence: Byte;
    samples: Word;
    padding: Word;
    size: LongWord;
    callback: Pointer;
    userdata: Pointer;
  end;

  SMPEG_Info ={ packed} record
    has_audio : Integer;
    has_video : Integer;
    width : Integer;
    height: Integer;
    current_frame: Integer;
    current_fps: Double;
    audio_string : array[0..79] of Char;
    audio_current_frame : Integer;
    current_offset: LongWord;
    total_size: LongWord;
    current_time: Double;
    total_time: Double;
  end;

const

  {$ifdef LINUX}
  LibsmpegPath = 'libsmpeg*.so*';
  LibSDLPath = 'libSDL-*.so*';
  {$ELSE}
  LibsmpegPath = 'smpeg.dll';
  LibSDLPath = 'SDL.dll';
  {$ENDIF}

var
  LibsmpegLoaded : Boolean = False;

type

  SMPEG_new_t = function(const filename : PChar; var info : SMPEG_Info; sdl_audio : Integer): PSMPEG; cdecl;

  SMPEG_delete_t = procedure(mpeg: Pointer); cdecl;

  SMPEG_wantedSpec_t = function(mpeg: Pointer; var spec: SDL_AudioSpec): Integer; cdecl;

  SMPEG_play_t = procedure(mpeg: Pointer); cdecl;

  SMPEG_status_t = function(mpeg: Pointer) : Integer; cdecl;

  SMPEG_stop_t = procedure(mpeg: Pointer); cdecl;

  SMPEG_playAudio_t = function(mpeg: Pointer; stream: Pointer; len: Integer): Integer; cdecl;

  SMPEG_skip_t = procedure(mpeg : Pointer; Pos : Single); cdecl;

  SMPEG_rewind_t = procedure(mpeg : Pointer); cdecl;

var

  SMPEG_new : SMPEG_new_t;

  SMPEG_delete : SMPEG_delete_t;

  SMPEG_wantedSpec : SMPEG_wantedSpec_t;

  SMPEG_play : SMPEG_play_t;

  SMPEG_status : SMPEG_status_t;

  SMPEG_stop : SMPEG_stop_t;

  SMPEG_playAudio : SMPEG_playAudio_t;

  SMPEG_skip : SMPEG_skip_t;

  SMPEG_rewind : SMPEG_rewind_t;

  procedure LoadMPEGLibrary;
  procedure UnloadMPEGLibrary;


implementation

type
  SDL_Init_t = function(Flags : LongWord) : Integer; cdecl;
  SDL_Quit_t = procedure; cdecl;

const
  SDL_INIT_AUDIO = $00000010;

var
  {$ifdef LINUX}
  Libhandle : Pointer;
  SDLhandle : Pointer;
  {$else}
  Libhandle : Cardinal;
  SDLhandle : Cardinal;
  {$endif}
  SDL_Init : SDL_Init_t;
  SDL_Quit : SDL_Quit_t;

procedure LoadMPEGLibrary;
var
  Path : string;
begin
  {$ifdef LINUX}
  Path := FindLibs(LibSDLPath);
  if Path <> '' then SDLhandle := dlopen(@Path, RTLD_NOW or RTLD_GLOBAL);
  if SDLhandle = nil then exit;
  SDL_Init := dlsym(SDLhandle, 'SDL_Init');
  SDL_Quit := dlsym(SDLhandle, 'SDL_Quit');
  SDL_Init(SDL_INIT_AUDIO);

  Path := FindLibs(LibsmpegPath);
  if Path <> '' then Libhandle := dlopen(@Path, RTLD_NOW or RTLD_GLOBAL);
  if Libhandle = nil then exit;
  if Libhandle <> nil then
  begin
    LibsmpegLoaded := True;
    SMPEG_new := dlsym(Libhandle, 'SMPEG_new');
    SMPEG_delete := dlsym(Libhandle, 'SMPEG_delete');
    SMPEG_wantedSpec := dlsym(Libhandle, 'SMPEG_wantedSpec');
    SMPEG_play := dlsym(Libhandle, 'SMPEG_play');
    SMPEG_status := dlsym(Libhandle, 'SMPEG_status');
    SMPEG_stop := dlsym(Libhandle, 'SMPEG_stop');
    SMPEG_playAudio := dlsym(Libhandle, 'SMPEG_playAudio');
    SMPEG_skip := dlsym(Libhandle, 'SMPEG_skip');
    SMPEG_rewind := dlsym(Libhandle, 'SMPEG_rewind');
  end;
  {$ELSE}
  SDLhandle := LoadLibrary(LibSDLPath);
  if SDLhandle = 0 then exit;
  SDL_Init := GetProcAddress(SDLhandle, 'SDL_Init');
  SDL_Quit := GetProcAddress(SDLhandle, 'SDL_Quit');
  SDL_Init(SDL_INIT_AUDIO);
  Libhandle := LoadLibrary(LibsmpegPath);
  if Libhandle = 0 then exit;
  if Libhandle <> 0 then
  begin
    LibsmpegLoaded := True;
    SMPEG_new := GetProcAddress(Libhandle, 'SMPEG_new');
    SMPEG_delete := GetProcAddress(Libhandle, 'SMPEG_delete');
    SMPEG_wantedSpec := GetProcAddress(Libhandle, 'SMPEG_wantedSpec');
    SMPEG_play := GetProcAddress(Libhandle, 'SMPEG_play');
    SMPEG_status := GetProcAddress(Libhandle, 'SMPEG_status');
    SMPEG_stop := GetProcAddress(Libhandle, 'SMPEG_stop');
    SMPEG_playAudio := GetProcAddress(Libhandle, 'SMPEG_playAudio');
    SMPEG_skip := GetProcAddress(Libhandle, 'SMPEG_skip');
    SMPEG_rewind := GetProcAddress(Libhandle, 'SMPEG_rewind');
  end;
  {$ENDIF}
end;

procedure UnloadMPEGLibrary;

begin
  SDL_Quit;
  {$ifdef LINUX}
  if Libhandle <> nil then dlclose(Libhandle);
  if SDLhandle <> nil then dlclose(SDLhandle);
  {$else}
  if Libhandle <> 0 then FreeLibrary(Libhandle);
  if SDLhandle <> 0 then FreeLibrary(SDLhandle);
  {$endif}
end;

end.
