{
$Log: acs_cdrom.inc,v $
Revision 1.12  2006/08/31 20:10:56  z0m3ie
*** empty log message ***

Revision 1.11  2006/07/04 17:12:45  z0m3ie
ACS 2.4 alt wiederhergestellt (unterschiedliche Sampleformate ...)

Revision 1.3  2006/01/03 15:37:51  z0m3ie
*** empty log message ***

Revision 1.2  2005/12/26 17:31:39  z0m3ie
fixed some problems in acs_dsfiles
fixed some problems in acs_vorbis
reworked all buffers

Revision 1.1  2005/12/19 18:35:16  z0m3ie
*** empty log message ***

Revision 1.4  2005/12/04 16:54:34  z0m3ie
All classes are renamed, Style TACS... than T... to avoid conflicts with other components (eg TMixer is TACSMixer now)

Revision 1.3  2005/10/02 16:51:46  z0m3ie
*** empty log message ***

Revision 1.2  2005/09/13 21:54:11  z0m3ie
acs is localizeable now (ACS_Strings)

Revision 1.1  2005/09/12 22:04:52  z0m3ie
modified structure again, fileformats are now in an sperat folder.
all File In/Out classes are capsulated from TFileIn and TFileOut

Revision 1.5  2005/09/09 21:33:43  z0m3ie
linux corrections

Revision 1.4  2005/09/08 22:19:00  z0m3ie
completed akrip based CDIn

Revision 1.3  2005/09/07 20:53:22  z0m3ie
begon to add MPEG and WMA support using DirectX

Revision 1.2  2005/09/04 17:59:38  z0m3ie
moving CDIn support to AKRip mostly
begon to add mpegin support for Win with mpg123

Revision 1.1  2005/08/25 20:18:00  z0m3ie
Version 2.4 restructure
TCDPlayer removed (fits not in component structure)
TMP3ToWavConverter removed (fits not in component structure)

}

type
  MSFAddr = array[0..3] of Byte;

  function Toc2MSF(t : MSFAddr) : TACSCDMSF;
  begin
    Result.Minute := t[1];
    Result.Second := t[2];
    Result.Frame := t[3];
  end;

  procedure TACSCDIn.OpenCD;
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

  procedure TACSCDIn.CloseCD;
  begin
    if FCDHandle <> 0 then
      CloseCDHandle(FCDHandle);
    FCDHandle := 0;
  end;

  function TACSCDIn.GetInfo : TACSCDInfo;
  var
    i : Integer;
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
    for i := 0 to FToc.lastTrack-1 do
    begin
      if (FToc.Tracks[i].adr and $04) = 0 then
      begin
        case Result of
          cdiUnknown : Result := cdiDiscAudio;
          cdiDiscData : Result := cdiDiscMixed;
        end;
      end else
      begin
        case Result of
          cdiUnknown : Result := cdiDiscData;
          cdiDiscAudio : Result := cdiDiscMixed;
        end;
      end;
    end;
    CloseCD;
  end;

  function TACSCDIn.GetStatus : TACSCDStatus;
  var
    ms : Integer;
    AP : LongBool;
  begin
    if FPlaying then
      Result := cdsPlaying
    else if (GetInfo <> cdiNoDisc) and (GetInfo <> cdiUnknown) then
      Result := cdsReady
    else
      Result := cdsNotReady;
  end;

  function TACSCDIn.GetNumTracks : Integer;
  begin
    OpenCD;
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
    CloseCD;
  end;

  FUNCTION TACSCDIn.GetTrackInfo(const vIndex : Integer) : TACSCDTrackInfo;
  VAR
    Frames: Integer;
    tmpmsf : TACSCDMSF;
  BEGIN
    IF Busy THEN
      RAISE EACSException.Create(strBusy);
    IF (vIndex IN [0..GetNumTracks-1]) = False THEN
      RAISE EACSException.Create(strTrackoutofrange);
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

  procedure TACSCDIn.SetST;
  begin
    if Self.Busy then raise EACSException.Create(strBusy);
    FStartTrack := Track;
    FStartPos.Track := FStartTrack;
    FillChar(FStartPos.MSF, SizeOf(FStartPos.MSF), 0);
  end;

  procedure TACSCDIn.SetET;
  begin
    if Self.Busy then raise EACSException.Create(strBusy);
    FEndTrack := Track;
    FEndPos.Track := FEndTrack+1;
    FillChar(FEndPos.MSF, SizeOf(FEndPos.MSF), 0);
  end;

  procedure TACSCDIn.SetSP;
  begin
    if Self.Busy then raise EACSException.Create(strBusy);
    FStartPos := Pos;
  end;

  procedure TACSCDIn.SetEP;
  begin
    if Self.Busy then raise EACSException.Create(strBusy);
    FEndPos := Pos;
    if Pos.Track = EndOfDisc.Track then FEndPos.Track := TracksCount + 1;
  end;

  function TACSCDIn.GetSize : Integer;
  var
    Sect1, Sect2 : Integer;
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
    Result := (Sect2 - Sect1)*CD_FRAMESIZE_RAW;
  end;

  procedure TACSCDIn.Init;
  var
    Sect1, Sect2 : Integer;
  begin
    if Busy then raise EACSException.Create(strBusy);
    if Status = cdsNotReady then
      raise EACSException.Create(strDrivenotready);
    if (FStartPos.Track in [0..GetNumTracks-1]) = False then
      raise EACSException.Create(strTrackoutofRange);
    if Tracks[FStartPos.Track].TrackType = ttData then
      raise EACSException.Create(strnoAudioTreck);
    OpenCD;
    FSize := GetSize;
    FBusy := True;
    BufStart := 1;
    BufEnd := 0;
    FPosition := 0;
    Sect1 := MSF2Frames(Toc2MSF(MSFAddr(FToc.tracks[FStartPos.Track].addr)));
    Sect1 := Sect1 + MSF2Frames(FStartPos.MSF);
    Sect2 := MSF2Frames(Toc2MSF(MSFAddr(FToc.tracks[FEndPos.Track].addr)));
    Sect2 := Sect2 + MSF2Frames(FEndPos.MSF);
    FRipEnd := Sect2;
    FRipStart := Sect1;
    SetLength(FBuffer,(BUF_SIZE * CD_FRAMESIZE_RAW)+TRACKBUFEXTRA);
  end;

  procedure TACSCDIn.Flush;
  begin
    CloseCD;
    FBusy := False;
    Setlength(FBuffer,0);
    FBuffer := nil;
    FSize := 0;
  end;

  function TACSCDIn.GetData(Buffer : Pointer; BufferSize : Integer): Integer;
  var
    Abort : LongBool;
    fnum : Integer;
  begin
    if not Busy then  raise EACSException.Create(strStreamnotOpen);
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

  procedure TACSCDIn.SetCurrentDrive;
  begin
    if Value in [0..FCDList.num-1] then
      FCurrentDrive := Value;
    OpenCD;
    FillChar(FToc, SizeOf(FToc),0);
    ModifyCDParms(FCDHandle, CDP_MSF, DWORD(true));
    ReadTOC(FCDHandle, FTOC);
    CloseCD;
  end;

  function TACSCDIn.GetDrivesCount : Integer;
  begin
    Result := FCDList.num;
  end;

  function TACSCDIn.GetDriveName : string;
  begin
    Result := FCDList.Cd[FCurrentDrive].id;
  end;

  procedure TACSCDIn.Eject;
  begin
    if Busy then raise EACSException.Create(strBusy);
  end;

  procedure TACSCDIn.CloseTray;
  begin
  end;

  constructor TACSCDIn.Create;
  begin
    inherited Create(AOwner);
    AppPath := ExtractFilePath(ParamStr(0));
    if AppPath[length(AppPath)] <> '\' then AppPath := AppPath + '\';
    CDRIPInit(AppPath);
    if not (csDesigning in ComponentState) then
    if not CDRipLoaded then
      raise EACSException.Create(akriplib + ' could not be loaded.');
    FillChar(FCDList, SizeOf(FCDList),0);
    FCDList.max := MAXCDLIST;
    GetCDList(FCDList);
  end;

  destructor TACSCDIn.Destroy;
  begin
    if Busy then
      Flush;
    CloseCD;
    inherited Destroy;
  end;

