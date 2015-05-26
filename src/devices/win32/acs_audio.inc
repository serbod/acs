(*
  Windows standard audio for acs_audio.pas

  This file is a part of Audio Components Suite.
  All rights reserved. See the license file for more details.

  Copyright (c) 2002-2009, Andrei Borovsky, anb@symmetrica.net
  Copyright (c) 2005-2006  Christian Ulrich, mail@z0m3ie.de
  Copyright (c) 2014-2015  Sergey Bodrov, serbod@gmail.com
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
  {
  EnterCriticalSection(CrSecO);
  if Msg = WOM_DONE then
  begin
    Audio:=TStdAudioOut(Instance);
    Audio.DelBlockFromChain(PWaveHdr(Param1));
  end;
  LeaveCriticalSection(CrSecO);
  }
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

function MMResultStr(Res: MMRESULT): string;
begin
  Result:='';
  if Res = MMSYSERR_NOERROR then
    // OK
  else if Res = MMSYSERR_INVALHANDLE then
    Result:='Specified device handle is invalid'
  else if Res = MMSYSERR_NODRIVER then
    Result:='No device driver is present'
  else if Res = MMSYSERR_NOMEM then
    Result:='Unable to allocate or lock memory'
  else if Res = WAVERR_STILLPLAYING then
    Result:='The data block pointed to by the pwh parameter is still in the queue'
  else if Res = WAVERR_UNPREPARED then
    Result:='The data block pointed to by the pwh parameter hasn''t been prepared';
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

procedure TStdAudioOut.DelBlockFromChain(WH: PWaveHdr);
begin
  WH.lpNext:=nil;
  EOC^:=WH;
  EOC:=@WH.lpNext;
  Dec(FBlockCount);
end;

procedure TStdAudioOut.SetDevice(Ch: Integer);
begin
  if Active then
    raise EAcsException.Create(strBusy);
  if OutputChannelsCount = 0 then
    FBaseChannel:=0
  else
    if Ch < OutputChannelsCount then
      FBaseChannel:=Ch
    else
      raise EAcsException.Create(Format(strChannelnotavailable, [Ch]));
end;

procedure TStdAudioOut.Init();
var
  WF: TPCMWaveFormat;
begin
  inherited Init();
  // No exceptions here!

  WF.wf.wFormatTag:=WAVE_FORMAT_PCM;
  WF.wf.nChannels:=FInput.Channels;
  WF.wf.nSamplesPerSec:=FInput.SampleRate;
  WF.wBitsPerSample:=FInput.BitsPerSample;
  WF.wf.nAvgBytesPerSec:=WF.wf.nSamplesPerSec*(WF.wBitsPerSample div 8);
  WF.wf.nBlockAlign:=WF.wf.nChannels*(WF.wBitsPerSample div 8);

  waveOutOpen(@_audio_fd, FBaseChannel, @WF, PtrUInt(@WaveOutProc), DWORD(Self), CALLBACK_FUNCTION or WAVE_MAPPED);
  FBlockCount:=0;
  FBlockSize:=(FBuffer.Size div FReadChunks);
  FBlockSize:=FBlockSize - (FBlockSize mod WF.wf.nBlockAlign); // aligned block size
  BlockChain:=nil;
  //EOC:=@BlockChain;
  FBuffer.Reset();
end;

procedure TStdAudioOut.Done();
var
  WH: PWaveHdr;
begin
  if _audio_fd <> -1 then
  begin
    //while FBlockCount > 0 do Sleep(1);
    WH:=BlockChain;
    while WH <> nil do
    begin
      BlockChain:=WH.lpNext;
      waveOutUnprepareHeader(_audio_fd, WH, SizeOf(TWaveHdr));
      //FreeMem(WH.lpData);
      Dispose(WH);
      WH:=BlockChain;
    end;
    //EOC:=@BlockChain;
    waveOutClose(_audio_fd);
    //FreeMem(FBuffer);
    _audio_fd:=-1;
  end;
  inherited Done();
end;

procedure TStdAudioOut.WriteBlock(P: Pointer; Len: Integer);
var
  WH: PWaveHdr;
  Res: string;
begin
  Inc(FBlockCount);
  New(WH);
  WH.lpNext:=nil;
  WH.lpData:=P;
  WH.dwBufferLength:=Len;
  WH.dwLoops:=0;
  WH.dwFlags:=0;
  if not Assigned(BlockChain) then BlockChain:=WH;
  waveOutPrepareHeader(_audio_fd, WH, SizeOf(TWaveHdr));
  Res:=MMResultStr(waveOutWrite(_audio_fd, WH, SizeOf(TWaveHdr)));
  if (Res <> '') then
  begin
    if Assigned(OnThreadException) then
      OnThreadException(Self, Exception.Create(Res))
    else
      raise EAcsException.Create(Res);
  end;
end;

function TStdAudioOut.DoOutput(Abort: Boolean): Boolean;
var
  Len, i, k, vCoef, BytesAllowed, AlignedSize: Integer;
  P8: PAcsBuffer8;
  P16: PAcsBuffer16;
  WH: PWaveHdr;
begin
  // No exceptions Here
  Result:=False;
  if not Assigned(FInput) then Exit;
  if Abort or (not CanOutput) then Exit;
  if not Active then Exit;

  // Get allowed output data size
  BytesAllowed:=FBlockSize*FReadChunks;
  WH:=BlockChain;
  while WH <> nil do
  begin
    if (WH.dwFlags AND WHDR_DONE) <> 0 then
    begin
      // Remove consumed block from chain
      BlockChain:=WH.lpNext;
      waveOutUnprepareHeader(_audio_fd, WH, SizeOf(TWaveHdr));
      //FreeMem(WH.lpData);
      Dispose(WH);
      WH:=BlockChain;
      Dec(FBlockCount);
    end
    else
    begin
      WH:=WH.lpNext;
      Dec(BytesAllowed, FBlockSize);
    end;
  end;
  //EOC:=@BlockChain;

  (* Write more than one block. This is needed for audio sources like
     Vorbis codec that return data in small chunks. *)
  //AlignedSize:=FBlockSize*FReadChunks;
  //for k:=FBlockCount to FReadChunks do
  if BytesAllowed >= FPrefetchSize then
  begin
    while InputLock do Sleep(1);
    InputLock:=True;
    Len:=FBuffer.GetBufWriteSize(BytesAllowed);
    if Len > FBlockSize then Len:=FBlockSize;
    Len:=FInput.GetData(FBuffer.Memory + FBuffer.WritePosition, Len);
    InputLock:=False;
    if Len <= 0 then Exit;
    // manually move writing position
    FBuffer.WritePosition:=FBuffer.WritePosition + Len;
    if FBuffer.WritePosition = FBuffer.Size then FBuffer.WritePosition:=0;

    // change volume
    if FVolume < 255 then
    begin
      vCoef:=Round(FVolume/255);
      if FInput.BitsPerSample = 16 then
      begin
        P16:=FBuffer.Memory;
        for i:=0 to (Len div 2)-1 do P16[i]:=P16[i] * vCoef;
      end
      else if FInput.BitsPerSample = 8 then
      begin
        P8:=FBuffer.Memory;
        for i:=0 to Len-1 do P8[i]:=P8[i] * vCoef;
      end;
    end;
    // create output block, that points to data, and add it to playing chain
    WriteBlock(FBuffer.Memory+FBuffer.ReadPosition, Len);
    // manually move read position
    FBuffer.ReadPosition:=FBuffer.ReadPosition + Len;
    if FBuffer.ReadPosition = FBuffer.Size then FBuffer.ReadPosition:=0;
  end;
  Result:=True;
end;

{ TStdAudioIn }

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
    waveInOpen(@_audio_fd, FBaseChannel, @WF, PtrUInt(@WaveInProc), DWORD(Self), CALLBACK_FUNCTION or WAVE_MAPPED);
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
  inherited Init();
  FRecBytes:=Trunc(FRecTime * (GetBPS div 8) * GetCh * GetSR);
  OpenAudio;
  waveInStart(_audio_fd);
  BlockChain:=nil;
  //FSize:=FRecBytes;
  aBlock:=0;
  EOC:=@BlockChain;
end;

procedure TStdAudioIn.Done();
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
  inherited Done();
end;

procedure TStdAudioIn.SetDevice(Ch: Integer);
begin
  if Active then
    raise EAcsException.Create(strBusy);
  if Ch < InputChannelsCount then FBaseChannel:=Ch
  else
    raise EAcsException.Create(Format(strChannelnotavailable, [Ch]));
end;

function TStdAudioIn.GetData(ABuffer: Pointer; ABufferSize: Integer): Integer;
var
  Tmp: PWaveHdr;
begin
  Result:=0;
  if not Active then
    raise EAcsException.Create(strStreamnotopen);
  if FRecBytes >= 0 then
    if (FPosition >= FRecBytes) then Exit;

  while aBlock < FBlocksCount do NewBlock();
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
  if ABufferSize < (BufEnd-BufStart+1) then
    Result:=ABufferSize
  else
    Result:=BufEnd-BufStart+1;
  Move(FBuffer[BufStart], ABuffer^, Result);
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

