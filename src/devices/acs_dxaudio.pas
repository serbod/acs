(*
  this file is a part of audio components suite v 2.3.
  copyright (c) 2002-2005 andrei borovsky. all rights reserved.
  see the license file for more details.
  you can contact me at mail@z0m3ie.de
*)

{
$Log: acs_dxaudio.pas,v $
Revision 1.7  2006/08/31 20:10:56  z0m3ie
*** empty log message ***

Revision 1.6  2006/07/09 16:40:35  z0m3ie
*** empty log message ***

Revision 1.5  2006/07/04 17:12:45  z0m3ie
ACS 2.4 alt wiederhergestellt (unterschiedliche Sampleformate ...)

Revision 1.3  2006/01/01 18:46:40  z0m3ie
*** empty log message ***

Revision 1.2  2005/12/26 17:31:39  z0m3ie
fixed some problems in acs_dsfiles
fixed some problems in acs_vorbis
reworked all buffers

Revision 1.1  2005/12/19 18:36:05  z0m3ie
*** empty log message ***

Revision 1.9  2005/12/18 17:01:54  z0m3ie
delphi compatibility

Revision 1.8  2005/12/04 16:54:34  z0m3ie
All classes are renamed, Style TACS... than T... to avoid conflicts with other components (eg TMixer is TACSMixer now)

Revision 1.7  2005/10/02 16:51:31  z0m3ie
*** empty log message ***

Revision 1.6  2005/09/23 14:04:58  z0m3ie
*** empty log message ***

Revision 1.5  2005/09/18 19:28:59  z0m3ie
more progress on driver handling

Revision 1.4  2005/09/16 17:34:29  z0m3ie
*** empty log message ***

Revision 1.3  2005/09/15 20:59:38  z0m3ie
start translate the documentation in the source for pasdoc

Revision 1.2  2005/09/14 21:19:37  z0m3ie
*** empty log message ***

Revision 1.1  2005/09/13 21:53:45  z0m3ie
maked seperat driver (not complete jet)

Revision 1.1  2005/09/12 22:04:52  z0m3ie
modified structure again, fileformats are now in an sperat folder.
all File In/Out classes are capsulated from TFileIn and TFileOut

Revision 1.1  2005/08/25 20:18:00  z0m3ie
Version 2.4 restructure
TCDPlayer removed (fits not in component structure)
TMP3ToWavConverter removed (fits not in component structure)

Revision 1.3  2005/08/22 20:17:02  z0m3ie
changed Headers to log
changed mail adress

}

{$ifdef linux}{$message error 'unit not supported'}{$endif linux}

unit acs_dxaudio;

interface

uses
  ACS_Audio,SysUtils, Classes, Forms, ACS_Types, ACS_Classes, Windows,ACS_Strings;

const
  LATENCY = 25;
  DS_POLLING_INTERVAL = 400; //milliseconds

type

  TDSoundWrapper = record
    dsw_pDirectSound : Pointer;
    dsw_OutputBuffer : Pointer;
    dsw_WriteOffset : LongWord;
    dsw_OutputSize : Integer;
    dsw_BytesPerFrame : Integer;
    dsw_CounterTicksPerBuffer : Int64;
    dsw_LastPlayTime : Int64;
    dsw_LastPlayCursor : Int64;
    dsw_OutputUnderflows : Int64;
    dsw_OutputRunning : LongBool;
    dsw_FramesWritten : Double;
    dsw_FramesPlayed : Double;
    dsw_pDirectSoundCapture : Pointer;
    dsw_InputBuffer : Pointer;
    dsw_ReadOffset : LongWord;
    dsw_InputSize : LongWord;
  end;

  PDSoundWrapper = ^TDSoundWrapper;

  TDSW_DeviceInfo = record
    guid : TGUID;
    name : array[0..127] of char;
  end;

  TDSW_Devices = record
    devcount : Integer;
    dinfo : array [0..15] of TDSW_DeviceInfo;
  end;

  PDSW_Devices = ^TDSW_Devices;

  { TDXAudioOut }

  TDXAudioOut = class(TACSBaseAudioOut)
  private
    DSW : TDSoundWrapper;
    Devices : TDSW_Devices;
    Chan, SR, BPS : Integer;
    EndOfInput, StartInput : Boolean;
    FDeviceNumber : Integer;
    FDeviceCount : Integer;
  protected
    procedure SetDevice(Ch : Integer);override;
    function GetDeviceInfo : TACSDeviceInfo;override;
    function GetDeviceCount : Integer;override;
    procedure Done; override;
    function DoOutput(Abort : Boolean):Boolean; override;
    procedure Prepare; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Pause;override;
    procedure Resume;override;
  end;

  TDXAudioIn = class(TACSBaseAudioIn)
  private
    DSW : TDSoundWrapper;
    Devices : TDSW_Devices;
    FDeviceNumber : Integer;
    FDeviceCount : Integer;
    FBPS, FChan, FFreq : Integer;
    FOpened : Integer;
    FBytesToRead : Integer;
    FRecTime : Integer;
    function GetDeviceName(Number : Integer) : String;
    procedure OpenAudio;
    procedure CloseAudio;
    procedure SetRecTime(aRecTime : Integer);
  protected
    procedure SetDevice(i : Integer);override;
    function GetBPS : Integer; override;
    function GetCh : Integer; override;
    function GetSR : Integer; override;
    function GetTotalTime : real; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetData(Buffer : Pointer; BufferSize : Integer): Integer; override;
    procedure Init; override;
    procedure Flush; override;
    property DeviceCount : Integer read FDeviceCount;
    property DeviceName[Number : Integer] : String read GetDeviceName;
  end;

implementation

type

  DSW_Init_t = function(dsw: PDSoundWrapper) : HRESULT; cdecl;
  DSW_Term_t =  procedure(dsw: PDSoundWrapper); cdecl;
  DSW_InitOutputDevice_t = function (dsw: PDSoundWrapper; const guid : TGUID) : HRESULT; cdecl;
  DSW_InitOutputBuffer_t = function (dsw: PDSoundWrapper; Wnd : HWND; bps, nFrameRate : LongWord; nChannels, bufSize : Integer): HRESULT; cdecl;
  DSW_StartOutput_t = function (dsw: PDSoundWrapper) : HRESULT; cdecl;
  DSW_StopOutput_t = function(dsw: PDSoundWrapper) : HRESULT; cdecl;
  DSW_RestartOutput_t = function(dsw: PDSoundWrapper) : HRESULT; cdecl;
  DSW_GetOutputStatus_t = function(dsw: PDSoundWrapper) : DWORD; cdecl;
  DSW_WriteBlock_t = function(dsw: PDSoundWrapper; buf : PByte; numBytes : Integer) : HRESULT; cdecl;
  DSW_ZeroEmptySpace_t = function(dsw: PDSoundWrapper) : HRESULT; cdecl;
  DSW_QueryOutputSpace_t = function(dsw: PDSoundWrapper; var bytesEmpty ) : HRESULT; cdecl;
  DSW_Enumerate_t = function(var devices : TDSW_Devices) : HRESULT; cdecl;

  DSW_InitInputBuffer_t = function(dsw: PDSoundWrapper; bps, nFrameRate, nChannels, bufSize : Integer) : HRESULT; cdecl;
  DSW_InitInputDevice_t = function(dsw: PDSoundWrapper; const GUID : TGUID) : HRESULT; cdecl;
  DSW_StartInput_t = function(dsw: PDSoundWrapper) : HRESULT; cdecl;
  DSW_StopInput_t = function(dsw: PDSoundWrapper) : HRESULT; cdecl;
  DSW_ReadBlock_t = function(dsw: PDSoundWrapper; buf : PByte; numBytes : Integer) : HRESULT; cdecl;
  DSW_QueryInputFilled_t = function(dsw: PDSoundWrapper; var bytesFilled : Integer) : HRESULT; cdecl;

var

  LibdswLoaded : Boolean = False;

  DSW_Init : DSW_Init_t;
  DSW_Term : DSW_Term_t;
  DSW_InitOutputDevice : DSW_InitOutputDevice_t;
  DSW_InitOutputBuffer : DSW_InitOutputBuffer_t;
  DSW_StartOutput : DSW_StartOutput_t;
  DSW_StopOutput : DSW_StopOutput_t;
  DSW_RestartOutput : DSW_RestartOutput_t;
  DSW_GetOutputStatus : DSW_GetOutputStatus_t;
  DSW_WriteBlock : DSW_WriteBlock_t;
  DSW_ZeroEmptySpace : DSW_ZeroEmptySpace_t;
  DSW_QueryOutputSpace : DSW_QueryOutputSpace_t;
  DSW_Enumerate : DSW_Enumerate_t;

  DSW_InitInputBuffer : DSW_InitInputBuffer_t;
  DSW_InitInputDevice : DSW_InitInputDevice_t;
  DSW_StartInput : DSW_StartInput_t;
  DSW_StopInput : DSW_StopInput_t;
  DSW_ReadBlock : DSW_ReadBlock_t;
  DSW_QueryInputFilled : DSW_QueryInputFilled_t;

  Libhandle : HMODULE;


procedure TDXAudioOut.Prepare;
var
  Res : HResult;
  Wnd : HWND;
  Form : TForm;
begin
  if (FDeviceNumber >= FDeviceCount) then raise EACSException.Create(Format(strChannelnotavailable,[FDeviceNumber]));
  FInput.Init;
  FBuffer := AllocMem(FBufferSize);
  Chan := FInput.Channels;
  SR := FInput.SampleRate;
  BPS := FInput.BitsPerSample;
  DSW_Init(@DSW);
  Res := DSW_InitOutputDevice(@DSW, Devices.dinfo[FDeviceNumber].guid);
  if Res <> 0 then raise EACSException.Create(strFailedtoCreateDSdev);
{  if Owner is TForm then
  begin
    Form := Owner as TForm;
    Wnd := Form.Handle;
  end else }
  Wnd := 0;
  Res := DSW_InitOutputBuffer(@DSW, Wnd, BPS, SR, Chan, FBufferSize);
  if Res <> 0 then raise EACSException.Create(strFailedtoCreateDSbuf);
  StartInput := True;
  EndOfInput := False;
end;

procedure TDXAudioOut.Done;
begin
  Finput.Flush;
  DSW_Term(@DSW);
  FreeMem(FBuffer);
end;

function TDXAudioOut.DoOutput(Abort : Boolean):Boolean;
var
  Len, offs, lb : Integer;
  Stat : LongWord;
  Res : HRESULT;
  PlayTime, CTime : LongWord;
begin
  Result := True;
  if not Busy then Exit;
  if not CanOutput then
  begin
    Result := False;
    Exit;
  end;
  if Abort then
  begin
    DSW_StopOutput(@DSW);
    CanOutput := False;
    Result := False;
    Exit;
  end;
  if StartInput then
  begin
    Len := 0;
    while Len < FBufferSize do
    begin
      offs := FInput.GetData(@FBuffer^[Len], FBufferSize-Len);
      if offs = 0 then
      begin
        EndOfInput := True;
        Break;
      end;
      Inc(Len, offs);
    end;
    DSW_WriteBlock(@DSW, @FBuffer^, Len);
    DSW_StartOutput(@DSW);
    StartInput := False;
  end;
  if EndOfInput then
  begin
    CanOutput := False;
    PlayTime := Round(FBufferSize/(Chan*(BPS div 8)*SR))*1000;
    CTime := 0;
    while CTime < PlayTime do
    begin
      Sleep(100);
      DSW_ZeroEmptySpace(@DSW);
      Inc(CTime, 100);
    end;
    DSW_StopOutput(@DSW);
    Result := False;
    Exit;
  end;
  Sleep(DS_POLLING_INTERVAL);
  DSW_QueryOutputSpace(@DSW, lb);
  lb := lb - (lb mod 1024);
  Len := 0;
  while Len < lb do
  begin
    if FInput.Busy then
      begin
        try
          offs := Finput.GetData(@FBuffer^[Len], lb-Len);
        except
          DSW_StopOutput(@DSW);
          CanOutput := False;
          Result := False;
          Exit;
        end;
      end;
    if offs = 0 then Break;
    Inc(Len, offs);
  end;
  DSW_WriteBlock(@DSW, @Fbuffer^, Len);
  if offs = 0 then
  begin
    DSW_ZeroEmptySpace(@DSW);
    EndOfInput := True;
  end;
end;

constructor TDXAudioOut.Create;
begin
  inherited Create(AOwner);
  FBufferSize := $40000;
  if not (csDesigning	in ComponentState) then
  begin
    if not LibdswLoaded then
    raise EACSException.Create(Format(strCoudntloadlib,['dswrapper.dll']));
  end;
  if LibdswLoaded then DSW_Enumerate(Devices);
  FDeviceCount := Devices.devcount;
end;

destructor TDXAudioOut.Destroy;
begin
  if LibdswLoaded then DSW_Term(@DSW);
end;

procedure TDXAudioOut.Pause;
begin
  if EndOfInput then Exit;
  DSW_StopOutput(@DSW);
end;

procedure TDXAudioOut.Resume;
begin
  if EndOfInput then Exit;
  DSW_RestartOutput(@DSW);
end;

procedure TDXAudioOut.SetDevice(Ch: Integer);
begin
  FBaseChannel := Ch;
end;

function TDXAudioOut.GetDeviceInfo: TACSDeviceInfo;
begin
  if (FBaseChannel >= FDeviceCount) then
    exit;
  Result.DeviceName := PChar(@(Devices.dinfo[FBaseChannel].Name[0]));
end;

function TDXAudioOut.GetDeviceCount: Integer;
begin
  Result := FDeviceCount;
end;

constructor TDXAudioIn.Create;
begin
  inherited Create(AOwner);
  FBPS := 8;
  FChan := 1;
  FFreq := 8000;
  FSize := -1;
  BufferSize := $2000;
  if not (csDesigning	in ComponentState) then
  begin
    if not LibdswLoaded then
    raise EACSException.Create(Format(strCoudntloadlib,['dswrapper.dll']));
  end;
  if LibdswLoaded then DSW_Enumerate(Devices);
  FDeviceCount := Devices.devcount;
end;

destructor TDXAudioIn.Destroy;
begin
  if LibdswLoaded then DSW_Term(@DSW);
  inherited Destroy;
end;

procedure TDXAudioIn.OpenAudio;
var
  Res : HResult;
  BufSize : Integer;
begin
  BufSize := BufferSize;
  if FOpened = 0 then
  begin
    DSW_Init(@DSW);
    if not Assigned(DSW_InitInputDevice) then raise EACSException.Create(Format(strChannelNotAvailable,[FDeviceNumber]));
    Res := DSW_InitInputDevice(@DSW, Devices.dinfo[FDeviceNumber].guid);
    if Res <> 0 then raise EACSException.Create(strFailedtoCreateDSdev);
    Res := DSW_InitInputBuffer(@DSW, FBPS, FFreq, FChan, BufSize);
    if Res <> 0 then raise EACSException.Create(strFailedtoCreateDSbuf);
  end;
  Inc(FOpened);
end;

procedure TDXAudioIn.CloseAudio;
begin
  if FOpened = 1 then DSW_Term(@DSW);
  if FOpened > 0 then Dec(FOpened);
end;

function TDXAudioIn.GetBPS : Integer;
begin
  Result := FBPS;
end;

function TDXAudioIn.GetCh : Integer;
begin
  Result := FChan;
end;

function TDXAudioIn.GetSR : Integer;
begin
  Result := FFreq;
end;

procedure TDXAudioIn.Init;
begin
  if Busy then raise EACSException.Create(strBusy);
  if (FDeviceNumber >= FDeviceCount) then raise EACSException.Create(Format(strChannelnotavailable,[FDeviceNumber]));
  if FRecTime > 0 then FBytesToRead := FRecTime*FFreq*FChan*(FBPS div 8);
  BufEnd := 0;
  BufStart := 1;
  FPosition := 0;
  FBusy := True;
  FSize := FBytesToRead;
  OpenAudio;
  DSW_StartInput(@DSW);
end;

procedure TDXAudioIn.Flush;
begin
  DSW_StopInput(@DSW);
  CloseAudio;
  FBusy := False;
end;

function TDXAudioIn.GetData(Buffer : Pointer; BufferSize : Integer): Integer;
var
  l : Integer;
begin
  if not Busy then  raise EACSException.Create(strStreamnotopen);
  if  (FBytesToRead >=0) and (FPosition >= FBytesToRead) then
  begin
    Result := 0;
    Exit;
  end;
  if BufStart >= BufEnd then
  begin
    BufStart := 0;
    Sleep(DS_POLLING_INTERVAL);
    DSW_QueryInputFilled(@DSW, l);
    if l > BufferSize then
      l := BufferSize; (* We have lost some data.
                          Generally this shouldn't happen. *)
    l := l - (l mod 1024);
    DSW_ReadBlock(@DSW, @FBuffer, l);
    BufEnd := l;
  end;
  if BufferSize < (BufEnd - BufStart)
  then Result := BufferSize
  else Result := BufEnd - BufStart;
  Move(FBuffer[BufStart], Buffer^, Result);
  Inc(BufStart, Result);
  Inc(FPosition, Result);
end;

procedure TDXAudioIn.SetRecTime;
begin
  FRecTime := aRecTime;
  if FRecTime > 0 then
    FBytesToRead := FRecTime*FFreq*FChan*(FBPS div 8)
  else
    FBytesToRead := -1;
end;

procedure TDXAudioIn.SetDevice(i : Integer);
begin
  FDeviceNumber := i
end;

function TDXAudioIn.GetDeviceName(Number : Integer) : String;
begin
  if (Number < FDeviceCount) then Result := PChar(@(Devices.dinfo[Number].Name[0]))
  else Result := '';
end;

function TDXAudioIn.GetTotalTime : real;
var
  BytesPerSec : Integer;
begin
  BytesPerSec := FFreq*FChan*(FBPS div 8);
  Result := FBytesToRead/BytesPerSec;
end;

initialization

  Libhandle := LoadLibraryEx('dswrapper.dll', 0, 0);

  if Libhandle <> 0 then
  begin
    LibdswLoaded := True;

    DSW_Init := GetProcAddress(Libhandle, 'DSW_Init');
    DSW_Term := GetProcAddress(Libhandle, 'DSW_Term');
    DSW_InitOutputDevice := GetProcAddress(Libhandle, 'DSW_InitOutputDevice');
    DSW_InitOutputBuffer := GetProcAddress(Libhandle, 'DSW_InitOutputBuffer');
    DSW_StartOutput := GetProcAddress(Libhandle, 'DSW_StartOutput');
    DSW_StopOutput := GetProcAddress(Libhandle, 'DSW_StopOutput');
    DSW_RestartOutput := GetProcAddress(Libhandle, 'DSW_RestartOutput');
    DSW_GetOutputStatus := GetProcAddress(Libhandle, 'DSW_GetOutputStatus');
    DSW_WriteBlock := GetProcAddress(Libhandle, 'DSW_WriteBlock');
    DSW_ZeroEmptySpace := GetProcAddress(Libhandle, 'DSW_ZeroEmptySpace');
    DSW_QueryOutputSpace := GetProcAddress(Libhandle, 'DSW_QueryOutputSpace');
    DSW_Enumerate := GetProcAddress(Libhandle, 'DSW_Enumerate');
    DSW_InitInputDevice := GetProcAddress(Libhandle, 'DSW_InitInputDevice');
    DSW_InitInputBuffer := GetProcAddress(Libhandle, 'DSW_InitInputBuffer');
    DSW_StartInput := GetProcAddress(Libhandle, 'DSW_StartInput');
    DSW_StopInput := GetProcAddress(Libhandle, 'DSW_StopInput');
    DSW_ReadBlock := GetProcAddress(Libhandle, 'DSW_ReadBlock');
    DSW_QueryInputFilled := GetProcAddress(Libhandle, 'DSW_QueryInputFilled');

  end;
  
  RegisterAudioOut('DirectSound',TDXAudioOut,LATENCY);
  RegisterAudioIn('DirectSound',TDXAudioIn,LATENCY);

finalization

  if Libhandle <> 0 then FreeLibrary(Libhandle);

end.
