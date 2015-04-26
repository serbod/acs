{
$Log
}

const
  MAX_CHANNELS = 16; // Maximum number of audio channels/devices
  O_RDONLY = 0;
  O_WRONLY = 1;
  libname = 'libc.so.6';

var
  AudioChannels : array[0..MAX_CHANNELS-1] of String;

(* We import libc functions directly to avoid Kylix
  Libc unit limitations *)

function __write(fd : Integer; data : Pointer; size : Integer): Integer; cdecl; external libname;
function __read(Handle: Integer; var Buffer; Count: Integer): Integer; cdecl; external libname;
function ioctl(fd : Integer; command : Integer): Integer; varargs; cdecl; external libname;
function open(PathName: PChar; Flags: Integer): Integer; varargs; cdecl; external libname;
function __close(Handle: Integer): Integer; cdecl; external libname;


function GetAudioDeviceInfo(DevID : Integer; OutputDev : Boolean) : TAcsDeviceInfo;
begin
  Result.DeviceName := '/dev/dsp'+IntToStr(DevID);
  Result.DrvVersion := 0;
  Result.Formats := [];
  Result.Stereo := True;
end;

procedure TStdAudioOut.SetDevice;
begin
  if Busy then raise EACSException.Create(strBusy);
  if Ch < OutputChannelsCount then FBaseChannel := Ch
  else raise EACSException.Create(Format(strChannelnotavailable,[ch]));
end;

procedure TStdAudioOut.Prepare;
var
  parm : Integer;
begin
  GetMem(FBuffer,FBufferSize);
  // No exceptions here!
  FInput.Init;
  case FInput.BitsPerSample of
    8 : parm := AFMT_U8;
    16 : parm := AFMT_S16_LE;
  end;
  _audio_fd := open(PChar(AudioChannels[FBaseChannel]), O_WRONLY);
  ioctl(_audio_fd, SNDCTL_DSP_SETFMT, @parm);
  parm := FInput.Channels;
  ioctl(_audio_fd, SNDCTL_DSP_CHANNELS, @parm);
  parm := FInput.SampleRate;
  ioctl(_audio_fd, SNDCTL_DSP_SPEED, @parm);
end;

procedure TStdAudioOut.Done;
begin
//TODO:why this raises an exception ??  FreeMem(FBuffer);
  __close(_audio_fd);
  _audio_fd := -1;
  FInput.Flush;
end;

function TStdAudioOut.DoOutput;
var
  Len, i, VCoef : Integer;
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
    __close(_audio_fd);
    Result := False;
    Exit;
  end;
  try
    P := @FBuffer[0];
    while InputLock do;
    InputLock := True;
    Len := Finput.GetData(P, FBufferSize);
    InputLock := False;
    if FVolume < 255 then
    begin
      VCoef := Round(FVolume/255);
      if FInput.BitsPerSample = 16 then
      begin
        P2 := @FBuffer[0];
        for i := 0 to (Len shr 1) -1 do
        P2[i] := P2[i]*VCoef;
      end else
      begin
        P1 := @FBuffer[0];
        for i := 0 to Len - 1 do
        P1[i] := P1[i]*VCoef;
      end;
    end;
    __write(_audio_fd, P, Len);
  except
  end;
  if Len > 0 then Result := True
  else Result := False;
end;

constructor TStdAudioOut.Create;
begin
  inherited Create(AOwner);
  FVolume := 255;
  FBufferSize := $8000;
  _audio_fd := -1;
end;

destructor TStdAudioOut.Destroy;
begin
  if _audio_fd > 0 then __close(_audio_fd);
  inherited Destroy;
end;

destructor TStdAudioIn.Destroy;
begin
  inherited Destroy;
  __close(_audio_fd);
end;

procedure TStdAudioIn.OpenAudio;
begin
  if FOpened = 0 then
  _audio_fd := open(PChar(AudioChannels[FBaseChannel]), O_RDONLY);
  Inc(FOpened);
end;

procedure TStdAudioIn.CloseAudio;
begin
  if FOpened = 1 then __close(_audio_fd);
  if FOpened > 0 then Dec(FOpened);
end;

function TStdAudioIn.GetBPS;
var
  BPS : Integer;
begin
  OpenAudio;
  BPS := FBPS;
  if (BPS in [8, 16]) = False then BPS := 16;
  ioctl(_audio_fd, SNDCTL_DSP_SETFMT, @BPS);
  FBPS := BPS;
  Result := BPS;
  CloseAudio;
end;

function TStdAudioIn.GetCh;
var
  Ch : Integer;
begin
  OpenAudio;
  Ch := FChan;
  ioctl(_audio_fd, SNDCTL_DSP_CHANNELS, @Ch);
  FChan := Ch;
  Result := Ch;
  CloseAudio;
end;

function TStdAudioIn.GetSR;
var
  SR : Integer;
begin
  OpenAudio;
  SR := FSampleRate;
  ioctl(_audio_fd, SNDCTL_DSP_SPEED, @SR);
  FSampleRate := SR;
  Result := SR;
  CloseAudio;
end;

procedure TStdAudioIn.Init;
begin
  if Busy then raise EACSException.Create(strBusy);
  BufEnd := 0;
  BufStart := 1;
  FPosition := 0;
  BufferSize := $8000;
  OpenAudio;
  FRecBytes := FRecTime * (GetBPS div 8) * GetCh * GetSR;
  FBusy := True;
  FSize := FRecBytes;
end;

procedure TStdAudioIn.Flush;
begin
  CloseAudio;
  FBusy := False;
end;

procedure TStdAudioIn.SetDevice;
begin
  if Ch > (OutputChannelsCount - 1) then
  if not (csDesigning in ComponentState) then
  raise EACSException.Create(Format(strChannelnotavailable,[Ch]));
  FBaseChannel := Ch;
end;

function TStdAudioIn.GetData;
var
  l : Integer;
begin
  if not Busy then  raise EACSException.Create(strStreamnotOpen);
  if FRecBytes >= 0 then
  if FPosition >= FRecBytes then
  begin
    Result := 0;
    Exit;
  end;
  if BufStart > BufEnd then
  begin
    BufStart := 1;
    l := __read(_audio_fd, FBuffer[BufStart], BufferSize);
    if l < 1 then
    begin
      Result := 0;
      Exit;
    end
    else BufEnd := l;
  end;
  if BufferSize < (BufEnd - BufStart + 1)
  then Result := BufferSize
  else Result := BufEnd - BufStart + 1;
  Move(FBuffer[BufStart], Buffer^, Result);
  Inc(BufStart, Result);
  Inc(FPosition, Result);
end;

procedure CountChannels;
var
  i, fd : Integer;
  fname : String;
begin
  OutputChannelsCount := 0;
  fname := '/dev/dsp0';
  fd := open(PChar(fname), O_RDONLY);
  if fd < 0 then
  begin
    //  Under ALSA there is no /dev/dsp0 device
    fname := '/dev/dsp';
    fd := open(PChar(fname), O_RDONLY);
    if fd < 0 then Exit;
  end;
  AudioChannels[OutputChannelsCount] := fname;
  __close(fd);
  Inc(OutputChannelsCount);
  for i := 1 to MAX_CHANNELS - 2 do
  begin
    fname := '/dev/dsp' + IntToStr(i);
    fd := open(PChar(fname), O_RDONLY);
    if fd < 0 then Break;
    __close(fd);
    AudioChannels[OutputChannelsCount] := fname;
    Inc(OutputChannelsCount);
  end;
end;

