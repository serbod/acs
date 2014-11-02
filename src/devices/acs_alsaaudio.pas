(*
  this file is a part of audio components suite v 2.3.
  copyright (c) 2002-2005 andrei borovsky. all rights reserved.
  see the license file for more details.
  you can contact me at mail@z0m3ie.de
*)

{
$Log: acs_alsaaudio.pas,v $
Revision 1.6  2006/08/30 18:59:51  z0m3ie
*** empty log message ***

Revision 1.5  2006/07/04 17:12:45  z0m3ie
ACS 2.4 alt wiederhergestellt (unterschiedliche Sampleformate ...)

Revision 1.2  2006/01/01 18:46:40  z0m3ie
*** empty log message ***

Revision 1.1  2005/12/19 18:36:05  z0m3ie
*** empty log message ***

Revision 1.1  2005/09/13 21:53:45  z0m3ie
maked seperat driver (not complete jet)

Revision 1.1  2005/09/12 22:04:52  z0m3ie
modified structure again, fileformats are now in an sperat folder.
all File In/Out classes are capsulated from TFileIn and TFileOut

Revision 1.3  2005/08/28 20:31:17  z0m3ie
linux restructuring for 2.4

Revision 1.2  2005/08/26 17:03:20  z0m3ie
begon to make acs resourcestring aware
more advanced tmixer for windows
restructured tmixer its better handleable now

Revision 1.1  2005/08/25 20:18:00  z0m3ie
Version 2.4 restructure
TCDPlayer removed (fits not in component structure)
TMP3ToWavConverter removed (fits not in component structure)

Revision 1.2  2005/08/22 20:17:01  z0m3ie
changed Headers to log
changed mail adress

}

{$ifdef mswindows}{$message error 'unit not supported'}{$endif}

unit acs_alsaaudio;

interface

uses
  Classes, SysUtils, ACS_Types, ACS_Classes, baseunix, alsa, ACS_Strings,ACS_Audio;

const
  BUF_SIZE = $4000;
  ALSAStateIdle = $ffffffff;  // additional DriverState value;
  LATENCY = 60;


type

  EALSABufferUnderrun = class(EACSException);
  EALSABufferOverrun = class(EACSException);

  { TALSAAudioIn }

  TALSAAudioIn = class(TACSBaseAudioIn)
  private
    FDevice : String;
    FPeriodSize, FPeriodNum : Integer;
    _audio_handle : Psnd_pcm_t;
    _hw_params : Psnd_pcm_hw_params_t;
    Busy : Boolean;
    FBPS, FChan, FFreq : Integer;
    BufStart, BufEnd : Integer;
    FOpened : Integer;
    FRecTime : Integer;
    FRecBytes : Integer;
    FLatency : Double;
    FSilentOnOverrun : Boolean;
    function GetDriverState: Integer;
    procedure OpenAudio;
    procedure CloseAudio;
//    function GetDriverState : Integer;
  protected
    function GetBPS : Integer; override;
    function GetCh : Integer; override;
    function GetSR : Integer; override;

    procedure SetDevice(Ch : Integer);override;
    function GetDeviceInfo : TACSDeviceInfo;override;
    function GetTotalTime : real; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetData(Buffer : Pointer; BufferSize : Integer): Integer; override;
    procedure Init; override;
    procedure Flush; override;
    property DriverState : Integer read GetDriverState;
  published
    property PeriodSize : Integer read FPeriodSize write FPeriodSize;
    property PeriodNum : Integer read FPeriodNum write FPeriodNum;
    property SilentOnOverrun : Boolean read FSilentOnOverrun write FSilentOnOverrun;
  end;



  { TALSAAudioOut }

  TALSAAudioOut = class(TACSBaseAudioOut)

  private
    FDevice : String;
    FPeriodSize, FPeriodNum : Integer;
    _audio_handle : Psnd_pcm_t;
    _hw_params : Psnd_pcm_hw_params_t;
    FBufferSize : Integer;
    FVolume : Byte;
    _audio_fd : Integer;
    Buffer : array [0..BUF_SIZE-1] of Byte;
    FLatency : Double;
    FSilentOnUnderrun : Boolean;
    function GetDriverState : Integer;
  protected
    procedure Done; override;
    function DoOutput(Abort : Boolean):Boolean; override;
    procedure Prepare; override;
    procedure SetDevice(Ch : Integer);override;
    function GetDeviceInfo : TACSDeviceInfo;override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property DriverState : Integer read GetDriverState;
    property Latency : Double read FLatency;
  published
    property BufferSize : Integer read FBufferSize write FBufferSize;
    property Device : String read FDevice write FDevice stored True;
    property PeriodSize : Integer read FPeriodSize write FPeriodSize;
    property PeriodNum : Integer read FPeriodNum write FPeriodNum;
    property SilentOnUnderrun : Boolean read FSilentOnUnderrun write FSilentOnUnderrun;
    property Volume : Byte read FVolume write FVolume stored True;
  end;

implementation

  constructor TALSAAudioIn.Create;
  begin
    inherited Create(AOwner);
    if not (csDesigning	in ComponentState) then
    if not AsoundlibLoaded then
    raise EACSException.Create(Format(strcoudntloadLib,[asoundlib_path]));
    FBPS := 8;
    FChan := 1;
    FFreq := 8000;
    FSize := -1;
    FRecTime := 600;
    FDevice := 'default';
    BufferSize := 32768;
    FSilentOnOverrun := True;
  end;

  destructor TALSAAudioIn.Destroy;
  begin
    inherited Destroy;
    CloseAudio;
  end;

  procedure TALSAAudioIn.OpenAudio;
  var
    Res : Integer;
  begin
    if FOpened = 0 then
    begin
      Res := snd_pcm_open(_audio_handle, @FDevice[1], SND_PCM_STREAM_CAPTURE, 0);
      if Res < 0 then
        raise EACSException.Create(Format(strcoudntopendevice,[FDevice]));
   //   snd_pcm_reset(_audio_handle);
    end;
    Inc(FOpened);
  end;

  procedure TALSAAudioIn.CloseAudio;
  begin
    if FOpened = 1 then
    begin
      snd_pcm_drop(_audio_handle);
      snd_pcm_close(_audio_handle);
    end;
    if FOpened > 0 then Dec(FOpened);
  end;

(* Note on the following three methods.
   These methods simply return the values passed by the user.
   As the actual input process begins, ALSAAudioIn may return a bit different
   value of the samplerate that is actually set by the ALSA drivers.*)

function TALSAAudioIn.GetBPS : Integer;
begin
  Result := FBPS;
end;

function TALSAAudioIn.GetCh : Integer;
begin
  Result := FChan;
end;

function TALSAAudioIn.GetSR : Integer;
begin
  Result := FFreq;
end;

procedure TALSAAudioIn.SetDevice(Ch: Integer);
begin
  FDevice := IntToStr(ch);
end;

function TALSAAudioIn.GetDeviceInfo: TACSDeviceInfo;
begin
end;

procedure TALSAAudioIn.Init;
var
  aBufSize : Integer;
begin
  if Busy then raise EACSException.Create(strBusy);
  BufEnd := 0;
  BufStart := 1;
  FPosition := 0;
  OpenAudio;

  snd_pcm_hw_params_malloc(_hw_params);
  snd_pcm_hw_params_any(_audio_handle, _hw_params);
  snd_pcm_hw_params_set_access(_audio_handle, _hw_params, SND_PCM_ACCESS_RW_INTERLEAVED);
  if FBPS = 8 then snd_pcm_hw_params_set_format(_audio_handle, _hw_params, SND_PCM_FORMAT_U8)
  else snd_pcm_hw_params_set_format(_audio_handle, _hw_params, SND_PCM_FORMAT_S16_LE);
  Self.FFreq := snd_pcm_hw_params_set_rate_near(_audio_handle, _hw_params, FFreq, 0);
  snd_pcm_hw_params_set_channels(_audio_handle, _hw_params, FChan);
  if (FPeriodSize <> 0) and (FPeriodNum <> 0) then
  begin
    snd_pcm_hw_params_set_period_size_near(_audio_handle, _hw_params, FPeriodSize, 0);
    snd_pcm_hw_params_set_periods_near(_audio_handle, _hw_params, FPeriodNum, 0);
    aBufSize := (FPeriodSize * FPeriodNum) div (FChan * (FBPS shr 3));
  end
  else aBufSize := Self.BufferSize div (FChan * (FBPS shr 3));
  snd_pcm_hw_params_set_buffer_size_near(_audio_handle, _hw_params, aBufSize);
  snd_pcm_hw_params(_audio_handle, _hw_params);
  snd_pcm_hw_params_free(_hw_params);
  if snd_pcm_prepare(_audio_handle) < 0 then
  begin
    CloseAudio;
    raise EACSException.Create(strInputstartfailed);
  end;
  try
  FLatency := snd_pcm_hw_params_get_period_size(_audio_handle, 0) *
              snd_pcm_hw_params_get_periods(_audio_handle, 0)/(FFreq * FChan * (FBPS shr 3));
  except
  end;
  FRecBytes := FRecTime * (GetBPS div 8) * GetCh * GetSR;
  Busy := True;
  FSize := FRecBytes;
end;

procedure TALSAAudioIn.Flush;
begin
  snd_pcm_drain(_audio_handle);
  CloseAudio;
  Busy := False;
end;

function TALSAAudioIn.GetData(Buffer : Pointer; BufferSize : Integer): Integer;
var
  l : Integer;
begin
  if not Busy then  raise EACSException.Create(strStreamnotopen);
  if FPosition >= FRecBytes then
  begin
    Result := 0;
    Exit;
  end;
  if BufStart > BufEnd then
  begin
    BufStart := 1;
    l := snd_pcm_readi(_audio_handle, @FBuffer[1], (BUF_SIZE div FChan) div (FBPS shr 3));
    while l < 0 do
    begin
      snd_pcm_prepare(_audio_handle);
      if not FSilentOnOverrun then
         raise EALSABufferOverrun.Create(strBufferoverrun);
      l := snd_pcm_readi(_audio_handle, @FBuffer[1], (BUF_SIZE div FChan) div (FBPS shr 3));
    end;
    if l <> (BUF_SIZE div FChan) div (FBPS shr 3) then
    begin
      Result := 0;
      Exit;
    end
    else BufEnd := l*FChan*(FBPS shr 3);
  end;
  if BufferSize < (BufEnd - BufStart + 1)
  then Result := BufferSize
  else Result := BufEnd - BufStart + 1;
  Move(FBuffer[BufStart-1], Buffer^, Result);
  Inc(BufStart, Result);
  Inc(FPosition, Result);
end;


function TALSAAudioIn.GetTotalTime : real;
begin
  Result := FRecTime;
end;

function TALSAAudioIn.GetDriverState : Integer;

begin

  if FOpened = 0 then Result := ALSAStateIdle

  else Result := snd_pcm_state(_audio_handle);

end;


constructor TALSAAudioOut.Create;

begin
  inherited Create(AOwner);
  if not (csDesigning	in ComponentState) then
  if not AsoundlibLoaded then
  raise EACSException.Create(Format(strCoudntloadLib,[asoundlib_path]));
  FVolume := 255;
  FDevice := 'default';
  FBufferSize := 32768;
  FSilentOnUnderrun := True;
end;


destructor TALSAAudioOut.Destroy;

begin
  if _audio_handle <> nil then snd_pcm_close(_audio_handle);
  inherited Destroy;
end;


procedure TALSAAudioOut.Prepare;

var
  Res, aBufSize : Integer;
begin
  FInput.Init;
  Res := snd_pcm_open(_audio_handle, @FDevice[1], SND_PCM_STREAM_PLAYBACK, 0);
  if Res < 0 then
     raise EACSException.Create(Format(strCoudntopendeviceOut,[FDevice]));
  //snd_pcm_reset(_audio_handle);
  snd_pcm_hw_params_malloc(_hw_params);
  snd_pcm_hw_params_any(_audio_handle, _hw_params);
  snd_pcm_hw_params_set_access(_audio_handle, _hw_params, SND_PCM_ACCESS_RW_INTERLEAVED);
  if FInput.BitsPerSample = 8 then
    snd_pcm_hw_params_set_format(_audio_handle, _hw_params, SND_PCM_FORMAT_U8)
  else
    snd_pcm_hw_params_set_format(_audio_handle, _hw_params, SND_PCM_FORMAT_S16_LE);
  snd_pcm_hw_params_set_rate_near(_audio_handle, _hw_params, FInput.SampleRate, 0);
  snd_pcm_hw_params_set_channels(_audio_handle, _hw_params, FInput.Channels);
  if (FPeriodSize <> 0) and (FPeriodNum <> 0) then
  begin
    snd_pcm_hw_params_set_period_size_near(_audio_handle, _hw_params, FPeriodSize, 0);
    snd_pcm_hw_params_set_periods_near(_audio_handle, _hw_params, FPeriodNum, 0);
    aBufSize := (FPeriodSize * FPeriodNum) div (Finput.Channels * (Finput.BitsPerSample shr 3));
  end
  else aBufSize := Self.FBufferSize div (Finput.Channels * (Finput.BitsPerSample shr 3));
  snd_pcm_hw_params_set_buffer_size_near(_audio_handle, _hw_params, aBufSize);
  snd_pcm_hw_params(_audio_handle, _hw_params);
  snd_pcm_hw_params_free(_hw_params);
  if snd_pcm_prepare(_audio_handle) < 0 then
  begin
    raise EACSException.Create(strFailedtostartoutput);
  end;
  try
  FLatency := snd_pcm_hw_params_get_period_size(_audio_handle, 0) *
              snd_pcm_hw_params_get_periods(_audio_handle, 0)/(Finput.Channels * (Finput.BitsPerSample shr 3));
  except
  end;
end;

procedure TALSAAudioOut.SetDevice(Ch: Integer);
begin
//  FDevice := IntToStr(ch);
end;

function TALSAAudioOut.GetDeviceInfo: TACSDeviceInfo;
begin
end;

procedure TALSAAudioOut.Done;
begin
  snd_pcm_drain(_audio_handle);
  snd_pcm_close(_audio_handle);
  _audio_handle := 0;
  FInput.Flush;
end;

function TALSAAudioOut.DoOutput(Abort : Boolean):Boolean;
var
  Len, i, VCoef, l : Integer;
  P : Pointer;
  P1 : PACSBuffer8;
  P2 : PACSBuffer16;
begin
  // No exceptions Here
  Result := True;
  if not CanOutput then Exit;
  Len := 0;
  if Abort then
  begin
    snd_pcm_drain(_audio_handle);
    snd_pcm_close(_audio_handle);
    _audio_handle := 0;
    Result := False;
    Exit;
  end;
  try
    P := @Buffer[0];
    while InputLock do;
    InputLock := True;
    Len := Finput.GetData(P, BUF_SIZE);
    InputLock := False;
    if Len = 0 then
    begin
      Result := False;
      Exit;
    end;
    if FVolume < 255 then
    begin
      VCoef := Round(FVolume/255);
      if FInput.BitsPerSample = 16 then
      begin
        P2 := @Buffer[0];
        for i := 0 to (Len shr 1) -1 do
        P2[i] := P2[i]*VCoef;
      end else
      begin
        P1 := @Buffer[0];
        for i := 0 to Len - 1 do
        P1[i] := P1[i]*VCoef;
      end;
    end;
    l := snd_pcm_writei(_audio_handle, P, (Len div Finput.Channels) div (FInput.BitsPerSample shr 3));
    while l < 0 do
    begin
      snd_pcm_prepare(_audio_handle);
      if not FSilentOnUnderrun then
         raise EALSABufferUnderrun.Create(strBufferunderrun);
      l := snd_pcm_writei(_audio_handle, P, (Len div Finput.Channels) div (FInput.BitsPerSample shr 3));
    end;
    if l = (Len div Finput.Channels) div (FInput.BitsPerSample shr 3)
    then Result := True
    else Result := False;
  except
  end;
end;

function TALSAAudioOut.GetDriverState : Integer;
begin

  if not Busy then Result := ALSAStateIdle

  else Result := snd_pcm_state(_audio_handle);

end;

initialization
  if AsoundlibLoaded then
    begin
      RegisterAudioOut('Alsa',TAlsaAudioOut,LATENCY);
      RegisterAudioIn('Alsa',TAlsaAudioIn,LATENCY);
    end;

end.
