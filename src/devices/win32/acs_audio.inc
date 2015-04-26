(*
  Windows standard audio

  this file is a part of audio components suite v 2.3 (delphi version).
  copyright (c) 2002-2005 andrei borovsky. all rights reserved.
  see the license file for more details.
  you can contact me at acs@compiler4.net
  this is the acs for delphi (windows) version of the unit.
*)

function CountChannels(): Integer;
begin
  Result:=0;
  OutputChannelsCount:=waveOutGetNumDevs;
  InputChannelsCount:=waveInGetNumDevs;
end;

function GetAudioDeviceInfo(DevID: Integer; OutputDev: Boolean): TAcsDeviceInfo;
var
  WIC: TWaveInCaps;
  i: Integer;
begin
  if OutputDev then
  begin
    if DevID >= OutputChannelsCount then
      raise EAcsException.Create(Format(strChannelnotavailable, [DevId]));
  end
  else
  begin
    if DevID >= InputChannelsCount then
      raise EAcsException.Create(Format(strChannelnotavailable, [DevId]));
  end;
  if OutputDev then
    waveOutGetDevCaps(DevID, @WIC, SizeOf(WIC))
  else
    waveInGetDevCaps(DevID, @WIC, SizeOf(WIC));
  i:=0;
  while WIC.szPname[i] <> #0 do Inc(i);
  SetLength(Result.DeviceName, i);
  Move(WIC.szPname[0], Result.DeviceName[1], i);
  Result.Formats:=[];
  if (WIC.dwFormats and WAVE_FORMAT_1M08) <> 0 then Result.Formats := Result.Formats + [af1M08];
  if (WIC.dwFormats and WAVE_FORMAT_1M16) <> 0 then Result.Formats := Result.Formats + [af1M16];
  if (WIC.dwFormats and WAVE_FORMAT_1S08) <> 0 then Result.Formats := Result.Formats + [af1S08];
  if (WIC.dwFormats and WAVE_FORMAT_1S16) <> 0 then Result.Formats := Result.Formats + [af1S16];
  if (WIC.dwFormats and WAVE_FORMAT_2M08) <> 0 then Result.Formats := Result.Formats + [af2M08];
  if (WIC.dwFormats and WAVE_FORMAT_2M16) <> 0 then Result.Formats := Result.Formats + [af2M16];
  if (WIC.dwFormats and WAVE_FORMAT_2S08) <> 0 then Result.Formats := Result.Formats + [af2S08];
  if (WIC.dwFormats and WAVE_FORMAT_2S16) <> 0 then Result.Formats := Result.Formats + [af2S16];
  if (WIC.dwFormats and WAVE_FORMAT_4M08) <> 0 then Result.Formats := Result.Formats + [af4M08];
  if (WIC.dwFormats and WAVE_FORMAT_4M16) <> 0 then Result.Formats := Result.Formats + [af4M16];
  if (WIC.dwFormats and WAVE_FORMAT_4S08) <> 0 then Result.Formats := Result.Formats + [af4S08];
  if (WIC.dwFormats and WAVE_FORMAT_4S16) <> 0 then Result.Formats := Result.Formats + [af4S16];
  Result.DrvVersion:=WIC.vDriverVersion;
  if WIC.wChannels = 1 then Result.Stereo:=False else Result.Stereo:=True;
end;

procedure WaveOutProc(hwo, Msg: LongWord; Instance: Pointer; Param1, Param2: LongWord); stdcall;
var
  Audio: TStdAudioOut;
begin
  EnterCriticalSection(CrSecO);
  if Msg = WOM_DONE then
  begin
    Audio:=TStdAudioOut(Instance);
    Audio.AddBlockToChain(PWaveHdr(Param1));
  end;
  LeaveCriticalSection(CrSecO);
end;

procedure WaveInProc(hwi, Msg: LongWord; Instance: Pointer; Param1, Param2: LongWord); stdcall;
var
  Audio: TStdAudioIn;
begin
  EnterCriticalSection(CrSecI);
  if Msg = WIM_DATA then
  begin
    Audio:=TStdAudioIn(Instance);
    Audio.AddBlockToChain(PWaveHdr(Param1));
  end;
  LeaveCriticalSection(CrSecI);
end;

constructor TStdAudioOut.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBaseChannel:=0;
  FVolume:=255;
  _audio_fd:=-1;
  Delay:=6;
  FReadChunks:=8;
  FBufferSize:=$8000; // default buffer size
end;

destructor TStdAudioOut.Destroy();
begin
  if _audio_fd <> -1 then WaveOutClose(_audio_fd);
  inherited Destroy;
end;

procedure TStdAudioOut.AddBlockToChain(WH: PWaveHdr);
begin
  WH.lpNext:=nil;
  EOC^:=WH;
  EOC:=@WH.lpNext;
  Dec(aBlock);
end;

procedure TStdAudioOut.SetDevice(Ch: Integer);
begin
  if Busy then
    raise EAcsException.Create(strBusy);
  if OutputChannelsCount = 0 then
    FBaseChannel:=0
  else
    if Ch < OutputChannelsCount then
      FBaseChannel:=Ch
    else
      raise EAcsException.Create(Format(strChannelnotavailable, [Ch]));
end;

procedure TStdAudioOut.Prepare();
var
  WF: TPCMWaveFormat;
begin
  inherited Prepare();
  // No exceptions here!

  WF.wf.wFormatTag:=WAVE_FORMAT_PCM;
  WF.wf.nChannels:=FInput.Channels;
  WF.wf.nSamplesPerSec:=FInput.SampleRate;
  WF.wBitsPerSample:=FInput.BitsPerSample;
  WF.wf.nAvgBytesPerSec:=WF.wf.nSamplesPerSec*(WF.wBitsPerSample div 8);
  WF.wf.nBlockAlign:=WF.wf.nChannels*(WF.wBitsPerSample div 8);

  waveOutOpen(@_audio_fd, FBaseChannel, @WF, DWORD(@WaveOutProc), DWORD(Self), CALLBACK_FUNCTION or WAVE_MAPPED);
  aBlock:=0;
  //FBuffer:=AllocMem(FBufferSize);
  EOC:=@BlockChain;
end;

procedure TStdAudioOut.Done();
var
  Tmp: PWaveHdr;
begin
  if _audio_fd <> -1 then
  begin
    while aBlock > 0 do;
    Tmp:=BlockChain;
    while Tmp <> nil do
    begin
      BlockChain:=Tmp.lpNext;
      waveOutUnprepareHeader(_audio_fd, Tmp, SizeOf(TWaveHdr));
      FreeMem(Tmp.lpData);
      Dispose(Tmp);
      Tmp:=BlockChain;
    end;
    EOC:=@BlockChain;
    waveOutClose(_audio_fd);
    //FreeMem(FBuffer);
    _audio_fd:=-1;
  end;
  inherited Done();
end;

procedure TStdAudioOut.WriteBlock(P: Pointer; Len: Integer);
var
  WH: PWaveHdr;
begin
  Inc(aBlock);
  New(WH);
  WH.lpData:=P;
  WH.dwBufferLength:=Len;
  WH.dwLoops:=0;
  WH.dwFlags:=0;
  waveOutPrepareHeader(_audio_fd, WH, SizeOf(TWaveHdr));
  waveOutWrite(_audio_fd, WH, SizeOf(TWaveHdr));
end;

function TStdAudioOut.DoOutput(Abort: Boolean): Boolean;
var
  Len, i, k, vCoef: Integer;
  P8: PAcsBuffer8;
  P16: PAcsBuffer16;
  Tmp: PWaveHdr;
begin
  // No exceptions Here
  Result:=False;
  if not Assigned(FInput) then Exit;
  if Abort or (not CanOutput) then Exit;
  if not Busy then Exit;
  Tmp:=BlockChain; // clear pending data blocks
  while Tmp <> nil do
  begin
    BlockChain:=Tmp.lpNext;
    waveOutUnprepareHeader(_audio_fd, Tmp, SizeOf(TWaveHdr));
    FreeMem(Tmp.lpData);
    Dispose(Tmp);
    Tmp:=BlockChain;
  end;
  EOC:=@BlockChain;
  (* Write more than one block. This is needed for audio sources like
     Vorbis codec that return data in small chunks. *)
  for k:=aBlock to FReadChunks do
  begin
    while InputLock do;
    InputLock:=True;
    Len:=FInput.GetData(FBuffer.Memory, (FBuffer.Size div FReadChunks));
    InputLock:=False;
    if Len <= 0 then Break;
    if FVolume < 255 then
    begin
      vCoef:=Round(FVolume/255);
      if FInput.BitsPerSample = 16 then
      begin
        P16:=FBuffer.Memory;
        for i:=0 to (Len shr 1)-1 do P16[i]:=P16[i]*vCoef;
      end
      else if FInput.BitsPerSample = 8 then
      begin
        P8:=FBuffer.Memory;
        for i:=0 to Len-1 do P8[i]:=P8[i]*vCoef;
      end;
    end;
    WriteBlock(FBuffer.Memory, Len);
  end;
  Result:=True;
end;

destructor TStdAudioIn.Destroy();
begin
  waveInClose(_audio_fd);
  inherited Destroy;
end;

procedure TStdAudioIn.OpenAudio();
var
  WF: TPCMWaveFormat;
begin
  WF.wf.wFormatTag:=WAVE_FORMAT_PCM;
  WF.wf.nChannels:=FChan;
  WF.wf.nSamplesPerSec:=FSampleRate;
  WF.wBitsPerSample:=FBPS;
  WF.wf.nAvgBytesPerSec:=WF.wf.nSamplesPerSec*(WF.wBitsPerSample div 8);
  WF.wf.nBlockAlign:=WF.wf.nChannels*(WF.wBitsPerSample div 8);
  if FOpened = 0 then
  begin
    waveInOpen(@_audio_fd, FBaseChannel, @WF, DWORD(@WaveInProc), DWORD(Self), CALLBACK_FUNCTION or WAVE_MAPPED);
  end;
  Inc(FOpened);
end;

procedure TStdAudioIn.CloseAudio();
begin
  if FOpened = 1 then
  begin
    waveInClose(_audio_fd);
    FreeMem(FBuffer);
  end;
  if FOpened > 0 then Dec(FOpened);
end;

function TStdAudioIn.GetBPS(): Integer;
begin
  Result:=FBPS;
end;

function TStdAudioIn.GetCh(): Integer;
begin
  Result:=FChan;
end;

function TStdAudioIn.GetSR(): Integer;
begin
  Result:=FSampleRate;
end;

procedure TStdAudioIn.Init();
begin
  if Busy then
    raise EAcsException.Create(strBusy);
  BufEnd:=0;
  BufStart:=1;
  FPosition:=0;
  FRecBytes:=FRecTime*(GetBPS div 8)*GetCh*GetSR;
  FBusy:=True;
  OpenAudio;
  waveInStart(_audio_fd);
  BlockChain:=nil;
  FSize:=FRecBytes;
  aBlock:=0;
  EOC:=@BlockChain;
end;

procedure TStdAudioIn.Flush();
var
  Tmp: PWaveHdr;
begin
  while aBlock > 0 do;  // wait until pending data blocks are put to the chain
  waveInReset(_audio_fd);    // return all pending data blocks
  sleep(10);
  Tmp:=BlockChain;         // clear pending data blocks
  while Tmp <> nil do
  begin
    BlockChain:=Tmp.lpNext;
    waveInUnprepareHeader(_audio_fd, Tmp, SizeOf(TWaveHdr));
    FreeMem(Tmp.lpData);
    Dispose(Tmp);
    Tmp:=BlockChain;
  end;
  CloseAudio;
  FBusy:=False;
end;

procedure TStdAudioIn.SetDevice(Ch: Integer);
begin
  if Busy then
    raise EAcsException.Create(strBusy);
  if Ch < InputChannelsCount then FBaseChannel:=Ch
  else
    raise EAcsException.Create(Format(strChannelnotavailable, [Ch]));
end;

function TStdAudioIn.GetData(Buffer: Pointer; oBufferSize: Integer): Integer;
var
  Tmp: PWaveHdr;
begin
  Result:=0;
  if not Busy then
    raise EAcsException.Create(strStreamnotopen);
  if FRecBytes >= 0 then
    if (FPosition >= FRecBytes) then Exit;

  while aBlock < FBlocksCount do NewBlock;
  if BufStart > BufEnd then
  begin
    BufStart:=1;
    while BlockChain = nil do sleep(10);
    TMP:=BlockChain;
    BlockChain:=BlockChain.lpNext;
    if BlockChain = nil then EOC:=@BlockChain;
    Move(Tmp.lpData[0], FBuffer[1], Tmp.dwBytesRecorded);
    BufEnd:=Tmp.dwBytesRecorded;
    waveInUnprepareHeader(_audio_fd, Tmp, SizeOf(TWaveHdr));
    FreeMem(Tmp.lpData);
    Dispose(Tmp);
  end;
  if BufferSize < (BufEnd-BufStart+1) then
    Result:=BufferSize
  else
    Result:=BufEnd-BufStart+1;
  Move(FBuffer[BufStart], Buffer^, Result);
  Inc(BufStart, Result);
  Inc(FPosition, Result);
end;

procedure TStdAudioIn.NewBlock();
var
  WH: PWaveHdr;
begin
  New(WH);
  GetMem(WH.lpData, (BufferSize div FBlocksCount));
  WH.dwBufferLength:=(BufferSize div FBlocksCount);
  WH.dwFlags:=0;
  waveInPrepareHeader(_audio_fd, WH, SizeOf(TWaveHdr));
  waveInAddBuffer(_audio_fd, WH, SizeOf(TWaveHdr));
  Inc(aBlock);
end;

procedure TStdAudioIn.AddBlockToChain(WH: PWaveHdr);
begin
  WH.lpNext:=nil;
  EOC^:=WH;
  EOC:=@WH.lpNext;
  Dec(aBlock);
end;

