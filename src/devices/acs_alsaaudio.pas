(*
Linux ALSA audio

This file is a part of Audio Components Suite.
Copyright (C) 2002-2005 Andrei Borovsky. All rights reserved.
See the license file for more details.
This is the ACS for Linux version of the unit.
*)

{
Status: 24 bit PCM works wrong
}

unit acs_alsaaudio;

interface

{$IFDEF FPC}
{$MODE delphi}
{$ENDIF}

{$ifdef LINUX}
{$ifdef mswindows}{$message error 'unit not supported'}{$endif}

uses
  Classes, SysUtils, ACS_Types, ACS_Classes, alsa, ACS_Strings,
  ACS_Audio, BaseUnix{, dbugintf};

const
  BUF_SIZE = $4000;
  ALSAStateIdle = $ffffffff;  // additional DriverState value;
  LATENCY = 60;

type

  EALSABufferUnderrun = class(EACSException);
  EALSABufferOverrun = class(EACSException);

  { TALSAAudioIn }

  TALSAAudioIn = class(TAcsAudioInDriver)
  private
    FDeviceName: string;
    FPeriodSize: Integer;
    FPeriodNum: Integer;
    _audio_handle: Psnd_pcm_t;
    _hw_params: Psnd_pcm_hw_params_t;
    BufStart: Integer;
    BufEnd:Integer;
    FOpened: Integer;
    FRecTime: Integer;
    FRecBytes: Integer;
    FLatency: Double;
    FSilentOnOverrun: Boolean;
    function GetDriverState(): Integer;
    procedure OpenAudio();
    procedure CloseAudio();
    //function GetDriverState(): Integer;
  protected
    procedure SetDevice(Ch: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy(); override;
    function GetData(ABuffer: Pointer; ABufferSize: Integer): Integer; override;
    (* As the actual input process begins, ALSAAudioIn may return a bit different
       value of the samplerate that is actually set by the ALSA drivers.*)
    procedure Init(); override;
    procedure Done(); override;
    property DriverState: Integer read GetDriverState;
  published
    property PeriodSize: Integer read FPeriodSize write FPeriodSize;
    property PeriodNum: Integer read FPeriodNum write FPeriodNum;
    property SilentOnOverrun: Boolean read FSilentOnOverrun write FSilentOnOverrun;
  end;



  { TALSAAudioOut }
  { ALSA use ring buffer, can play async with callback or polling
    Polling is tricky }

  TALSAAudioOut = class(TAcsAudioOutDriver)
  private
    FDeviceName: AnsiString;
    FPeriodSize: Integer;
    FPeriodNum: Integer;
    _audio_handle: Psnd_pcm_t;
    _hw_params: Psnd_pcm_hw_params_t;
    //_audio_fd: Integer;
    FLatency: Double;
    FSilentOnUnderrun: Boolean;
    function GetDriverState(): Integer;
    function PollFreeBufferSize(): Integer;
    function snd_err(const tag: string; r: Integer): Integer;
  protected
    procedure SetDevice(Ch: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy(); override;
    procedure Init(); override;
    function DoOutput(Abort: Boolean):Boolean; override;
    procedure Done(); override;
    property DriverState: Integer read GetDriverState;
    property Latency: Double read FLatency;
  published
    property Device: string read FDeviceName write FDeviceName stored True;
    property PeriodSize: Integer read FPeriodSize write FPeriodSize;
    property PeriodNum: Integer read FPeriodNum write FPeriodNum;
    property SilentOnUnderrun: Boolean read FSilentOnUnderrun write FSilentOnUnderrun;
    property Volume;
  end;

{$endif LINUX}

implementation
uses Math;

{$ifdef LINUX}

{
procedure alsa_callback(ahandler: Psnd_async_handler_t); cdecl;
var
  audio_handle: Psnd_pcm_t;
  AlsaAudioOut: TALSAAudioOut;
begin
  audio_handle:=snd_async_handler_get_pcm(ahandler);
  AlsaAudioOut:=snd_async_handler_get_callback_private(ahandler);
end;
}

constructor TALSAAudioIn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  if not (csDesigning in ComponentState) then
    if not AsoundlibLoaded then
      raise EACSException.Create(Format(strcoudntloadLib, [asoundlib_path]));
  FBPS:=8;
  FChan:=1;
  FSampleRate:=8000;
  //FSize:=-1;
  FRecTime:=600;
  FDeviceName:='default';
  BufferSize:=32768;
  FSilentOnOverrun:=True;
end;

destructor TALSAAudioIn.Destroy();
begin
  inherited Destroy();
  CloseAudio();
end;

procedure TALSAAudioIn.OpenAudio();
var
  Res: Integer;
begin
  if FOpened = 0 then
  begin
    Res:=snd_pcm_open(_audio_handle, @FDeviceName[1], SND_PCM_STREAM_CAPTURE, 0);
    if Res < 0 then
      raise EACSException.Create(Format(strcoudntopendevice, [FDeviceName]));
    //snd_pcm_reset(_audio_handle);
  end;
  Inc(FOpened);
end;

procedure TALSAAudioIn.CloseAudio();
begin
  if FOpened = 1 then
  begin
    snd_pcm_drop(_audio_handle);
    snd_pcm_close(_audio_handle);
  end;
  if FOpened > 0 then Dec(FOpened);
end;

procedure TALSAAudioIn.SetDevice(Ch: Integer);
begin
  FDeviceName:=IntToStr(ch);
end;

procedure TALSAAudioIn.Init();
var
  iBufSize, iNearDir, iVal: Integer;
begin
  inherited Init();
  iNearDir:=0; // target/chosen exact value is <,=,> val following dir (-1,0,1)
  OpenAudio();

  snd_pcm_hw_params_malloc(_hw_params);
  snd_pcm_hw_params_any(_audio_handle, _hw_params);
  snd_pcm_hw_params_set_access(_audio_handle, _hw_params, SND_PCM_ACCESS_RW_INTERLEAVED);
  if FBPS = 8 then
    snd_pcm_hw_params_set_format(_audio_handle, _hw_params, SND_PCM_FORMAT_U8)
  else
    snd_pcm_hw_params_set_format(_audio_handle, _hw_params, SND_PCM_FORMAT_S16_LE);
  Self.FSampleRate:=snd_pcm_hw_params_set_rate_near(_audio_handle, _hw_params, @FSampleRate, @iNearDir);
  snd_pcm_hw_params_set_channels(_audio_handle, _hw_params, FChan);
  if (FPeriodSize <> 0) and (FPeriodNum <> 0) then
  begin
    snd_pcm_hw_params_set_period_size_near(_audio_handle, _hw_params, @FPeriodSize, @iNearDir);
    snd_pcm_hw_params_set_periods_near(_audio_handle, _hw_params, @FPeriodNum, @iNearDir);
    iBufSize:=(FPeriodSize * FPeriodNum) div (FChan * (FBPS div 8));
  end
  else
    iBufSize:=Self.BufferSize div (FChan * (FBPS div 8));
  // approximate target buffer size in frames / returned chosen approximate target buffer size in frames
  // Returns 0 otherwise a negative error code if configuration space is empty
  snd_pcm_hw_params_set_buffer_size_near(_audio_handle, _hw_params, @iBufSize);
  // set new buffer size
  Self.BufferSize:=iBufSize * (FChan * (FBPS div 8));
  snd_pcm_hw_params(_audio_handle, _hw_params);
  snd_pcm_hw_params_free(_hw_params);
  if snd_pcm_prepare(_audio_handle) < 0 then
  begin
    CloseAudio();
    raise EACSException.Create(strInputstartfailed);
  end;
  // compute latency
  try
    iVal:=0;
    // val	Returned approximate period size in frames
    snd_pcm_hw_params_get_period_size(_hw_params, @iVal, @iNearDir);
    FLatency:=iVal;
    iVal:=0;
    // val	approximate periods per buffer
    snd_pcm_hw_params_get_periods(_hw_params, @iVal, @iNearDir);
    FLatency:=FLatency * iVal / (FSampleRate * FChan * (FBPS div 8));
  except
  end;
  FRecBytes:=FRecTime * (GetBPS div 8) * GetCh * GetSR;
end;

procedure TALSAAudioIn.Done();
begin
  snd_pcm_drain(_audio_handle);
  CloseAudio();
  inherited Done();
end;

function TALSAAudioIn.GetData(ABuffer: Pointer; ABufferSize: Integer): Integer;
var
  l: Integer;
begin
  if not Active then
    raise EACSException.Create(strStreamnotopen);
  if FPosition >= FRecBytes then
  begin
    Result:=0;
    Exit;
  end;
  if BufStart > BufEnd then
  begin
    BufStart:=1;
    l:=snd_pcm_readi(_audio_handle, @FBuffer[1], (BUF_SIZE div FChan) div (FBPS div 8));
    while l < 0 do
    begin
      snd_pcm_prepare(_audio_handle);
      if not FSilentOnOverrun then
         raise EALSABufferOverrun.Create(strBufferoverrun);
      l:=snd_pcm_readi(_audio_handle, @FBuffer[1], (BUF_SIZE div FChan) div (FBPS div 8));
    end;
    if l <> (BUF_SIZE div FChan) div (FBPS div 8) then
    begin
      Result:=0;
      Exit;
    end
    else
      BufEnd:=l * FChan * (FBPS div 8);
  end;
  if ABufferSize < (BufEnd - BufStart + 1) then
    Result:=ABufferSize
  else
    Result:=BufEnd - BufStart + 1;
  Move(FBuffer[BufStart-1], ABuffer^, Result);
  Inc(BufStart, Result);
  Inc(FPosition, Result);
end;

function TALSAAudioIn.GetDriverState(): Integer;
begin
  if FOpened = 0 then
    Result:=Integer(ALSAStateIdle)
  else
    Result:=snd_pcm_state(_audio_handle);
end;

{ TALSAAudioOut }

constructor TALSAAudioOut.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  if not (csDesigning in ComponentState) then
    if not AsoundlibLoaded then
      raise EACSException.Create(Format(strCoudntloadLib, [asoundlib_path]));
  FVolume:=255;
  //FDeviceName:='plughw:0,0';
  FDeviceName:='default';
  //FBufferSize:=32768;
  //FBufferSize:=$40000;
  FBufferSize:=$4000;
  FFetchDelay:=10;
  FSilentOnUnderrun:=True;
end;


destructor TALSAAudioOut.Destroy();
begin
  if _audio_handle <> nil then snd_pcm_close(_audio_handle);
  inherited Destroy();
end;


procedure TALSAAudioOut.Init();
var
  Res, iBufSize, iVal, iNearDir: Integer;
begin
  inherited Init();

  FSampleSize:=(FInput.Channels * (FInput.BitsPerSample div 8));
  iNearDir:=0; // target/chosen exact value is <,=,> val following dir (-1,0,1)

  Res:=snd_pcm_open(_audio_handle, PChar(FDeviceName), SND_PCM_STREAM_PLAYBACK, SND_PCM_NONBLOCK);
  if Res < 0 then
     raise EACSException.Create(Format(strCoudntopendeviceOut, [FDeviceName]));
  // set audio parameters
  //snd_pcm_reset(_audio_handle);
  Res:=snd_pcm_hw_params_malloc(_hw_params);
  if Res < 0 then
     raise EACSException.Create('cannot allocate hardware parameter structure');
  Res:=snd_pcm_hw_params_any(_audio_handle, _hw_params);
  if Res < 0 then
     raise EACSException.Create('cannot initialize hardware parameter structure');
  Res:=snd_pcm_hw_params_set_access(_audio_handle, _hw_params, SND_PCM_ACCESS_RW_INTERLEAVED);
  if Res < 0 then
     raise EACSException.Create('cannot set access type');

  // set sample format
  Res:=-1;
  if FInput.BitsPerSample = 8 then
    Res:=snd_pcm_hw_params_set_format(_audio_handle, _hw_params, SND_PCM_FORMAT_U8)
  else if FInput.BitsPerSample = 16 then
    Res:=snd_pcm_hw_params_set_format(_audio_handle, _hw_params, SND_PCM_FORMAT_S16)
  else if FInput.BitsPerSample = 24 then
    Res:=snd_pcm_hw_params_set_format(_audio_handle, _hw_params, SND_PCM_FORMAT_S24);
  if Res < 0 then
     raise EACSException.Create('cannot set sample format');

  // set sample rate
  iVal:=FInput.SampleRate;
  Res:=snd_pcm_hw_params_set_rate_near(_audio_handle, _hw_params, @iVal, @iNearDir);
  if Res < 0 then
     raise EACSException.Create('cannot set sample rate');

  // set channel count
  Res:=snd_pcm_hw_params_set_channels(_audio_handle, _hw_params, FInput.Channels);
  if Res < 0 then
     raise EACSException.Create('cannot set channel count');

  // buffer size in samples
  //FPeriodNum:=4;
  //FPeriodSize:=(FBuffer.Size div FSampleSize) div FPeriodNum;
  if (FPeriodSize <> 0) and (FPeriodNum <> 0) then
  begin
    // approximate target period size in frames / returned chosen approximate target period size
    Res:=snd_pcm_hw_params_set_period_size_near(_audio_handle, _hw_params, @FPeriodSize, @iNearDir);
    // approximate target periods per buffer / returned chosen approximate target periods per buffer
    Res:=snd_pcm_hw_params_set_periods_near(_audio_handle, _hw_params, @FPeriodNum, @iNearDir);
    iBufSize:=(FPeriodSize * FPeriodNum) * FSampleSize;
  end
  else
    iBufSize := FBuffer.Size div FSampleSize;
  // approximate target buffer size in frames / returned chosen approximate target buffer size in frames
  // Returns 0 otherwise a negative error code if configuration space is empty
  if snd_pcm_hw_params_set_buffer_size_near(_audio_handle, _hw_params, @iBufSize) < 0 then
    raise EACSException.Create('cannot set buffer');
  // set new buffer size
  FBuffer.Size:=iBufSize * FSampleSize;
  FBuffer.Reset();

  // set parameters
  Res:=snd_pcm_hw_params(_audio_handle, _hw_params);
  if Res < 0 then
     raise EACSException.Create('cannot set parameters');
  // free parameters structure
  snd_pcm_hw_params_free(_hw_params);

  {
  if snd_pcm_prepare(_audio_handle) < 0 then
    raise EACSException.Create('cannot prepare audio interface for use');
  }
  {
  // compute latency
  try
    iVal:=0;
    // val	Returned approximate period size in frames
    snd_pcm_hw_params_get_period_size(_hw_params, @iVal, @iNearDir);
    FLatency:=iVal;
    iVal:=0;
    // val	approximate periods per buffer
    snd_pcm_hw_params_get_periods(_hw_params, @iVal, @iNearDir);
    FLatency:=FLatency * iVal / (FInput.Channels * (FInput.BitsPerSample div 8));
  except
  end;
  }
end;


procedure TALSAAudioOut.SetDevice(Ch: Integer);
begin
  //FDeviceName := IntToStr(ch);
end;

procedure TALSAAudioOut.Done();
begin
  snd_pcm_drop(_audio_handle);
  //snd_pcm_drain(_audio_handle);
  snd_pcm_close(_audio_handle);
  _audio_handle := nil;
  inherited Done();
end;

function TALSAAudioOut.snd_err(const tag: string; r: Integer): Integer;
begin
  Result:=r;
  if r < 0 then
  begin
    Result:=snd_pcm_recover(_audio_handle, r, 1);
    {
    begin
      WriteLn(tag, ' err ', r, ' -> ', Result);
    end;
    }
  end;
end;

function snd_ok(r: Integer): boolean;
begin
  // checks snd_pcm_* results
  Result:=(r >= 0) or ((r <> -ESysEAGAIN) and (r <> -ESysEINTR));
end;

function TALSAAudioOut.PollFreeBufferSize(): Integer;
begin
  repeat
    // Try to get allowed output data size
    // wait till the interface is ready for data, or 50 msecond has elapsed.
    Result:=snd_pcm_wait(_audio_handle, 50);
  until snd_ok(Result);
  if Result = 0 then
  begin
    // timeout, need to wait a bit
    Exit;
  end;
  if Result < 0 then
  begin
    //if Assigned(OnThreadException) then
    //  OnThreadException(Self, Exception.Create('poll failed'));
    Exit;
  end;

  // find out how much space is available for playback data
  repeat
    Result:=snd_err('PollFreeBufferSize.snd_pcm_avail_update', snd_pcm_avail_update(_audio_handle) );
  until snd_ok(Result);
  if Result < 0 then
  begin
    //if Assigned(OnThreadException) then
    //  OnThreadException(Self, Exception.Create('unknown ALSA avail update return value'));
    Exit;
  end;

  Result:=Result * FSampleSize;
end;

function TALSAAudioOut.DoOutput(Abort: Boolean): Boolean;
var
  Len, iSamplesCount, iSamplesWritten, BytesAllowed, BytesWritten: Integer;
  Res: Integer;
begin
  // No exceptions Here
  Result:=False;
  if not CanOutput then Exit;
  Len:=0;
  if Abort then Exit;

  BytesAllowed:=PollFreeBufferSize();
  if BytesAllowed < 0 then Exit;
  // returned BytesAllowed not accurate, and can be greater, than actual

  Result:=True;
  if BytesAllowed = 0 then
    Exit;

  if FBuffer.UnreadSize = 0 then
    FBuffer.Reset();

  Len:=FBuffer.GetBufWriteSize(BytesAllowed);
  if Len > 0 then
  begin
    Len:=FInput.GetData(FBuffer.Memory + FBuffer.WritePosition, Len);
    FBuffer.WritePosition:=FBuffer.WritePosition + Len;
  end;

  Result:=FBuffer.UnreadSize > 0; // current buffer is not empty

  while FBuffer.UnreadSize > 0 do
  begin
    BytesAllowed:=PollFreeBufferSize();
    if BytesAllowed < 0 then Exit(False);
    if BytesAllowed = 0 then Exit(True);

    iSamplesCount:=min(BytesAllowed, FBuffer.UnreadSize) div FSampleSize;
    //WriteLn('TALSAAudioOut.DoOutput. PollFreeBufferSize ', BytesAllowed, ' Unread ', FBuffer.UnreadSize, ' -> Samples ', BytesAllowed div FSampleSize);

    iSamplesWritten:=0;
    repeat
      // Write interleaved frames to a PCM.
      // size	- frames to be written
      // Returns a positive number of frames actually written, otherwise a negative error code
      repeat
        Res:=snd_err('TALSAAudioOut.DoOutput1', snd_pcm_writei(_audio_handle, FBuffer.Memory + FBuffer.ReadPosition, iSamplesCount) );
      until snd_ok(Res);

      if Res < 0 then
      begin
        Result:=False;
        Exit;
      end;
      Inc(iSamplesWritten, Res)
    until Res > 0;

    BytesWritten:=(iSamplesWritten * FSampleSize);
    FBuffer.ReadPosition:=FBuffer.ReadPosition + BytesWritten;
  end;
end;

function TALSAAudioOut.GetDriverState(): Integer;
begin
  if not Active then
    Result:=Integer(ALSAStateIdle)
  else
    Result:=snd_pcm_state(_audio_handle);
end;


initialization
  if LoadAlsaLibrary() then
  begin
    RegisterAudioOut('Alsa', TAlsaAudioOut, LATENCY);
    RegisterAudioIn('Alsa', TAlsaAudioIn, LATENCY);
  end;


finalization
  UnloadAlsaLibrary();

{$endif LINUX}
end.
