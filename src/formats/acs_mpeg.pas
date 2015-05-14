(*
SDL SMPEG library (MPEG-1) components

This file is a part of Audio Components Suite.
Copyright (C) 2002-2005 Andrei Borovsky. All rights reserved.
See the license file for more details.
This is the ACS for Linux and Windows version of the unit.
*)

unit acs_mpeg;

interface

uses
  Classes, SysUtils, ACS_file, ACS_classes, smpeg;

type
  TMPEGIn = class(TAcsCustomFileIn)
  private
    _M: Pointer;
    FTime: Integer;
  protected
    procedure OpenFile(); override;
    procedure CloseFile(); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy(); override;
    function GetData(Buffer: Pointer; BufferSize: Integer): Integer; override;
    procedure Init(); override;
  end;


implementation

constructor TMPEGIn.Create();
begin
  inherited Create(AOwner);
  BufferSize:=$2000;
end;

destructor TMPEGIn.Destroy();
begin
  inherited Destroy;
end;

procedure TMPEGIn.Init();
begin
  inherited Init();
  SMPEG_play(_M);
end;

procedure TMPEGIn.OpenFile();
var
  info: SMPEG_info;
  spec: SDL_AudioSpec;
begin
  if FOpened then
  begin
    (* the next call is needed just to make sure
      the SDL library is loaded *)
    _M:=SMPEG_new(PChar(FFileName), info, 1);
    SMPEG_delete(_M);
    FValid:=True;
    _M:=SMPEG_new(PChar(FFileName), info, 0);
    if info.has_audio <> 1 then
    begin
      FValid:=False;
      Exit;
    end;
    FTime:=Round(info.total_time);
    SMPEG_wantedSpec(_M, spec);
    FSR:=spec.freq;
    FBPS:=16;
    FChan:=spec.channels;
    FSize:=FTime * 2 * FChan * FSR;
    FOpened:=True;
  end;
end;

procedure TMPEGIn.CloseFile();
begin
  if FOpened then
  begin
    if SMPEG_status(_M) = SMPEG_PLAYING then
    SMPEG_stop(_M);
    SMPEG_delete(_M);
    FOpened:=False;
  end;
end;

function TMPEGIn.GetData(Buffer: Pointer; BufferSize: Integer): Integer;
var
  l, offs: Integer;
  tmp: Single;
begin
  if not Busy then
    raise EACSException.Create('The Stream is not opened');
  if BufStart > BufEnd then
  begin
    if FOffset <> 0 then
    begin
      offs:=Round((FOffset / 100) * FSize);
      FPosition:=FPosition + offs;
      if FPosition < 0 then
        FPosition:=0
      else
        if FPosition > FSize then FPosition:=FSize;
      if FOffset < 0 then
      begin
        SMPEG_rewind(_M);
        SMPEG_play(_M);
        tmp:=(FPosition / FSize) * FTime;
        SMPEG_skip(_M, tmp);
      end;
      tmp:=(FOffset / 100) * FTime;
      SMPEG_skip(_M, tmp);
      FOffset:=0;
    end;
    BufStart:=1;
    FillChar(FBuffer, BufferSize, 0);
    l:=Buffersize;
    l:=SMPEG_playAudio(_M, @FBuffer[BufEnd+1], BufferSize - BufEnd);
    if l = 0 then
    begin
      if FLoop then
      begin
        Done();
        Init();
        SMPEG_rewind(_M);
        SMPEG_play(_M);
        FPosition:=0;
        l:=SMPEG_playAudio(_M, @FBuffer[BufEnd+1], BufferSize - BufEnd);
      end
      else
      begin
        Result:=0;
        Exit;
      end;
    end;
    BufEnd:=l;
  end;
  if BufferSize < (BufEnd - BufStart + 1) then
    Result:=BufferSize
  else
    Result:=BufEnd - BufStart + 1;
  Move(FBuffer[BufStart], Buffer^, Result);
  Inc(BufStart, Result);
  Inc(FPosition, Result);
end;

initialization
  if LoadMPEGLibrary() then
  begin
    FileFormats.Add('mp3', 'Mpeg Audio Layer 3', TMPEGIn);
    FileFormats.Add('mp2', 'Mpeg Audio Layer 2', TMPEGIn);
    FileFormats.Add('mpeg', 'Mpeg Audio', TMPEGIn);
  end;

finalization
  UnLoadMPEGLibrary();

end.

