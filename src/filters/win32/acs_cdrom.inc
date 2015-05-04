{
CD-ROM for Windows

This file is a part of Audio Components Suite.
All rights reserved. See the license file for more details.

Copyright (c) 2002-2009, Andrei Borovsky, anb@symmetrica.net
Copyright (c) 2005-2006  Christian Ulrich, mail@z0m3ie.de
Copyright (c) 2014-2015  Sergey Bodrov, serbod@gmail.com
}

type
  MSFAddr = array[0..3] of Byte;

constructor TAcsCDIn.Create();
begin
  inherited Create(AOwner);
  FStreamDisabled:=True;
  AppPath := ExtractFilePath(ParamStr(0));
  if AppPath[length(AppPath)] <> '\' then AppPath := AppPath + '\';
  LoadCDRip();
  if not (csDesigning in ComponentState) then
    if not CDRipLoaded then
      raise EAcsException.Create(akriplib + ' could not be loaded.');
  FillChar(FCDList, SizeOf(FCDList), 0);
  FCDList.max := MAXCDLIST;
  GetCDList(FCDList);
end;

destructor TAcsCDIn.Destroy();
begin
  if Busy then Done();
  CloseCD();
  UnloadCDRip();
  inherited Destroy;
end;

function Toc2MSF(t: MSFAddr): TAcsCDMSF;
begin
  Result.Minute := t[1];
  Result.Second := t[2];
  Result.Frame := t[3];
end;

procedure TAcsCDIn.OpenCD();
var
  GetCD: GETCDHAND;
begin
  with GetCD do
    begin
      Size := SizeOf(GETCDHAND);
      Ver := 1;
      ha := 0;
      tgt := FCurrentDrive;
      lun := 0;
      readType := CDR_ANY;
      jitterCorr := false;
      numJitter := 0;
      numOverlap := 0;
    end;
  FCDHandle := GetCDHandle(GetCD);
end;

procedure TAcsCDIn.CloseCD();
begin
  if FCDHandle <> 0 then
    CloseCDHandle(FCDHandle);
  FCDHandle := 0;
end;

function TAcsCDIn.GetInfo(): TAcsCDInfo;
var
  i: Integer;
begin
  OpenCD;
  Result := cdiUnknown;
  ModifyCDParms(FCDHandle, CDP_MSF, DWORD(true));
  FillChar(FToc, SizeOf(FToc),0);
  if ReadTOC(FCDHandle, FTOC) <> 1 then
  begin
    Result := cdiNoDisc;
    Exit;
  end;
  for i:=0 to FToc.lastTrack-1 do
  begin
    if (FToc.Tracks[i].adr and $04) = 0 then
    begin
      case Result of
        cdiUnknown: Result := cdiDiscAudio;
        cdiDiscData: Result := cdiDiscMixed;
      end;
    end else
    begin
      case Result of
        cdiUnknown: Result := cdiDiscData;
        cdiDiscAudio: Result := cdiDiscMixed;
      end;
    end;
  end;
  CloseCD;
end;

function TAcsCDIn.GetStatus(): TAcsCDStatus;
begin
  if FPlaying then
    Result := cdsPlaying
  else if (GetInfo <> cdiNoDisc) and (GetInfo <> cdiUnknown) then
    Result := cdsReady
  else
    Result := cdsNotReady;
end;

function TAcsCDIn.GetNumTracks(): Integer;
begin
  OpenCD();
  if FToc.lastTrack = 0 then
    begin
      ModifyCDParms(FCDHandle, CDP_MSF, DWORD(true));
      if ReadTOC(FCDHandle, FTOC) <> 1 then
        begin
          Result := -1;
          Exit;
        end;
    end;
  Result := FToc.lastTrack;
  CloseCD();
end;

FUNCTION TAcsCDIn.GetTrackInfo(const vIndex : Integer) : TAcsCDTrackInfo;
VAR
  Frames: Integer;
BEGIN
  IF Busy THEN
    RAISE EAcsException.Create(strBusy);
  IF (vIndex IN [0..GetNumTracks-1]) = False THEN
    RAISE EAcsException.Create(strTrackoutofrange);
  IF (FToc.Tracks[vIndex].adr and $04) = 0 THEN
    Result.TrackType := ttAudio
  ELSE
    Result.TrackType := ttData;
  Result.TrackStart.Minute := FToc.Tracks[vIndex].addr[1];
  Result.TrackStart.Second := FToc.Tracks[vIndex].addr[2];
  Result.TrackStart.Frame := FToc.Tracks[vIndex].addr[3];
  Frames := MSF2Frames(Toc2MSF(MSFAddr(FToc.Tracks[vIndex+1].addr)))-MSF2Frames(Toc2MSF(MSFAddr(FToc.Tracks[vIndex].addr)));
  Frames2MSF(Frames, Result.TrackLength);
end;

procedure TAcsCDIn.SetST();
begin
  if Self.Busy then raise EAcsException.Create(strBusy);
  FStartTrack := Track;
  FStartPos.Track := FStartTrack;
  FillChar(FStartPos.MSF, SizeOf(FStartPos.MSF), 0);
end;

procedure TAcsCDIn.SetET();
begin
  if Self.Busy then raise EAcsException.Create(strBusy);
  FEndTrack := Track;
  FEndPos.Track := FEndTrack+1;
  FillChar(FEndPos.MSF, SizeOf(FEndPos.MSF), 0);
end;

procedure TAcsCDIn.SetSP();
begin
  if Self.Busy then raise EAcsException.Create(strBusy);
  FStartPos := Pos;
end;

procedure TAcsCDIn.SetEP();
begin
  if Self.Busy then raise EAcsException.Create(strBusy);
  FEndPos := Pos;
  if Pos.Track = EndOfDisc.Track then FEndPos.Track := TracksCount + 1;
end;

function TAcsCDIn.GetSize(): Integer;
var
  Sect1, Sect2: Integer;
begin
  if Busy then
  begin
    Result := FRipEnd-FRipStart*CD_FRAMESIZE_RAW;
    Exit;
  end;
  Sect1 := MSF2Frames(Toc2MSF(MSFAddr(FToc.tracks[FStartPos.Track].addr)));
  Sect1 := Sect1 + MSF2Frames(FStartPos.MSF);
  Sect2 := MSF2Frames(Toc2MSF(MSFAddr(FToc.tracks[FEndPos.Track].addr)));
  Sect2 := Sect2 + MSF2Frames(FEndPos.MSF);
  Result := (Sect2 - Sect1) * CD_FRAMESIZE_RAW;
end;

procedure TAcsCDIn.Init();
var
  Sect1, Sect2: Integer;
begin
  if Status = cdsNotReady then
    raise EAcsException.Create(strDrivenotready);
  if (FStartPos.Track in [0..GetNumTracks-1]) = False then
    raise EAcsException.Create(strTrackoutofRange);
  if Tracks[FStartPos.Track].TrackType = ttData then
    raise EAcsException.Create(strnoAudioTreck);
  inherited Init();
  OpenCD();
  FSize := GetSize();
  Sect1 := MSF2Frames(Toc2MSF(MSFAddr(FToc.tracks[FStartPos.Track].addr)));
  Sect1 := Sect1 + MSF2Frames(FStartPos.MSF);
  Sect2 := MSF2Frames(Toc2MSF(MSFAddr(FToc.tracks[FEndPos.Track].addr)));
  Sect2 := Sect2 + MSF2Frames(FEndPos.MSF);
  FRipEnd := Sect2;
  FRipStart := Sect1;
  SetLength(FBuffer, (BUF_SIZE * CD_FRAMESIZE_RAW) + TRACKBUFEXTRA);
end;

procedure TAcsCDIn.Done();
begin
  CloseCD();
  SetLength(FBuffer,0);
  FBuffer := nil;
  FSize := 0;
  inherited Done();
end;

function TAcsCDIn.GetData(Buffer: Pointer; BufferSize: Integer): Integer;
var
  Abort: LongBool;
  fnum: Integer;
begin
  if not Busy then
    raise EAcsException.Create(strStreamnotOpen);
  if BufStart > BufEnd then //Buffer clear try to fill it
  begin
    BufStart := 1;
    Abort := False;
    if FRipEnd-FRipStart > BUF_SIZE then
      fNum := BUF_SIZE
    else
      fNum := FRipEnd-FRipStart;
    FiBuffer^.startFrame := FRipStart;
    FiBuffer^.numFrames := fNum;
    FiBuffer^.maxLen := FiBuffer^.numFrames * CD_FRAMESIZE_RAW;
    FiBuffer^.len := 0;
    FiBuffer^.status := 0;
    FiBuffer^.startOffset := 0;
    if ReadCDAudioLBA(FCDHandle,FiBuffer) = 1 then
      begin
        Inc(FRipStart,FiBuffer^.numFrames);
        BufEnd := FiBuffer^.len;
      end;
  end;
  if BufferSize < (BufEnd - BufStart + 1) then
    Result := BufferSize
  else
    Result := BufEnd - BufStart + 1;
  Move(FiBuffer^.buf[BufStart-1],Buffer^, Result);
  Inc(BufStart, Result);
  Inc(FPosition, Result);
end;

procedure TAcsCDIn.SetCurrentDrive();
begin
  if Value in [0..FCDList.num-1] then
    FCurrentDrive := Value;
  OpenCD();
  FillChar(FToc, SizeOf(FToc),0);
  ModifyCDParms(FCDHandle, CDP_MSF, DWORD(true));
  ReadTOC(FCDHandle, FTOC);
  CloseCD();
end;

function TAcsCDIn.GetDrivesCount(): Integer;
begin
  Result := FCDList.num;
end;

function TAcsCDIn.GetDriveName(): string;
begin
  Result := FCDList.Cd[FCurrentDrive].id;
end;

procedure TAcsCDIn.Eject();
begin
  if Busy then
    raise EAcsException.Create(strBusy);
end;

procedure TAcsCDIn.CloseTray();
begin
end;


