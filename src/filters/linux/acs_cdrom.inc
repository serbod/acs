{
$Log: acs_cdrom.inc,v $
Revision 1.7  2006/08/31 20:10:56  z0m3ie
*** empty log message ***

Revision 1.6  2006/07/04 17:12:45  z0m3ie
ACS 2.4 alt wiederhergestellt (unterschiedliche Sampleformate ...)

Revision 1.2  2005/12/26 17:31:39  z0m3ie
fixed some problems in acs_dsfiles
fixed some problems in acs_vorbis
reworked all buffers

}

  function GetTocEntry(cd_fd, Track : Integer): TACSCDTrackInfo;
  var
    Entry : cdrom_tocentry;
    toc : cdrom_tochdr;
    frames1, frames2 : Integer;
  begin
    fpioctl(cd_fd, CDROMREADTOCHDR, @toc);
    Entry.cdte_format := CDROM_MSF;
    Entry.cdte_track := Track+toc.cdth_trk0-1;
    fpioctl(cd_fd, CDROMREADTOCENTRY, @Entry);
    frames1 := MSF2Frames(TACSCDMSF(Entry.cdte_addr.msf));
    if (Entry.cdte_adr_ctrl and CDROM_DATA_TRACK) <> 0 then
    Result.TrackType := ttData
    else Result.TrackType := ttAudio;
    if Entry.cdte_track < toc.cdth_trk1 then Inc(Entry.cdte_track)
    else Entry.cdte_track := CDROM_LEADOUT;
    fpioctl(cd_fd, CDROMREADTOCENTRY, @Entry);
    frames2 := MSF2Frames(TACSCDMSF(Entry.cdte_addr.msf));
    Frames2MSF(frames2-frames1, Result.TrackLength);
  end;

  function GetCDStatus(cd_fd : Integer) : TACSCDStatus;
  (* not all drivers support the CDROM_DRIVE_STATUS ioctl
   we use this ioctl first and then some other tecnique
   if it is not supported. *)
  var
    sci : cdrom_subchnl;
    res :Integer;
    Data: Integer;
  begin
    Data := CDSL_CURRENT;
    res := fpioctl(cd_fd, CDROM_DRIVE_STATUS, @Data);
    case res of
      CDS_TRAY_OPEN, CDS_NO_DISC, CDS_DRIVE_NOT_READY:
      begin
        Result := cdsNotReady;
        Exit;
      end;
    end;
    (* Either the disc is ok or no information
     from the driver. Trying CDROMSUBCHNL.*)
    sci.cdsc_format := CDROM_MSF;
    if fpioctl(cd_fd, CDROMSUBCHNL, @sci) < 0 then
    begin
      Result := cdsNotReady;
      Exit;
    end;
    case sci.cdsc_audiostatus of
      CDROM_AUDIO_PLAY : Result := cdsPlaying;
      CDROM_AUDIO_PAUSED : Result := cdsPaused;
      CDROM_AUDIO_ERROR  : Result := cdsNotReady;
      else Result := cdsReady;
    end;
  end;

  function GetCDInfo(cd_fd : Integer) : TACSCDInfo;
  var
    res : Integer;
    Data: Integer;
  begin
    Result := cdiUnknown;
    Data := CDSL_CURRENT;
    res := fpioctl(cd_fd, CDROM_DRIVE_STATUS, @Data);
    case res of
      CDS_TRAY_OPEN, CDS_NO_DISC: Result := cdiNoDisc;
      CDS_DISC_OK :
      begin
        res := fpioctl(cd_fd, CDROM_DISC_STATUS, @Data);
        case res of
          CDS_AUDIO : Result := cdiDiscAudio;
          CDS_MIXED : Result := cdiDiscMixed;
          else Result :=  cdiDiscData;
        end;
      end;
    end;
  end;

  procedure TACSCDIn.OpenCD;
  begin
    if FCurrentDrive >= length(DrivesPaths) then
      exit;
    if FOpened = 0 then
    begin
      _cd_fd := fpopen(PChar(DrivesPaths[FCurrentDrive]), O_RDONLY or O_NONBLOCK);
      if _cd_fd < 0 then
      raise EACSException.Create(IntToStr(errno));
    end;
    Inc(FOpened);
  end;


  procedure TACSCDIn.CloseCD;
  begin
    if FOpened = 1 then fpclose(_cd_fd);
    if FOpened > 0 then Dec(FOpened);
  end;

  function TACSCDIn.GetInfo;
  begin
    if Busy then raise EACSException.Create(strBusy);
    OpenCD;
    Result := GetCDInfo(_cd_fd);
    CloseCD;
  end;

  function TACSCDIn.GetStatus;
  begin
    if FCurrentDrive >= length(DrivesPaths) then
      exit;
    if Busy then raise EACSException.Create(strBusy);
    if Fopened = 0 then
    _cd_fd := fpopen(PChar(DrivesPaths[FCurrentDrive]), O_RDONLY or O_NONBLOCK);
    if _cd_fd < 0 then
    begin
      Result := cdsNotReady;
      Exit;
    end;
    Inc(FOpened);
    Result := GetCDStatus(_cd_fd);
    CloseCD;
  end;

  function TACSCDIn.GetNumTracks;
  var
    toc : cdrom_tochdr;
  begin
    if Busy then raise EACSException.Create(strBusy);
    OpenCD;
    if GetStatus <> cdsNotReady then
    begin
      fpioctl(_cd_fd, CDROMREADTOCHDR, @toc);
      Result := toc.cdth_trk1 - toc.cdth_trk0 + 1;
    end else Result := 0;
    CloseCD;
  end;

  function TACSCDIn.GetTrackInfo;
  begin
    if Busy then raise EACSException.Create(strBusy);
    OpenCD;
    if (vIndex in [1..GetNumTracks]) = False then
    begin
      fpclose(_cd_fd);
      FOpened := 0;
      raise EACSException.Create(strTrackOutofRange);
    end;
    Result := GetTocEntry(_cd_fd, vIndex);
    CloseCD;
  end;

  function GetTrackMSF(cd_fd, Track : Integer) : TACSCDMSF;
  var
    entry : cdrom_tocentry;
    hdr : cdrom_tochdr;
  begin
    fpioctl(cd_fd, CDROMREADTOCHDR, @hdr);
    entry.cdte_format := CDROM_MSF;
    entry.cdte_track := Track + hdr.cdth_trk0 - 1;
    if entry.cdte_track > hdr.cdth_trk1 then
    entry.cdte_track := CDROM_LEADOUT;
    fpioctl(cd_fd, CDROMREADTOCENTRY, @entry);
    Result := TACSCDMSF(entry.cdte_addr.msf);
  end;

  function GetPosMSF(cd_fd : Integer; Pos : TACSCDPosition) : TACSCDMSF;
  var
    msf1 : TACSCDMSF;
    frames : Integer;
  begin
    msf1 := TACSCDMSF(GetTrackMSF(cd_fd, Pos.Track));
    frames := MSF2Frames(msf1);
    frames := frames + MSF2Frames(Pos.MSF);
    Frames2MSF(frames, msf1);
    Result := msf1;
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
    OpenCD;
    FEndPos.Track := FEndTrack + 1;
    FillChar(FEndPos.MSF, SizeOf(FEndPos.MSF), 0);
    CloseCD;
  end;

  procedure TACSCDIn.SetSP;
  begin
    if Self.Busy then raise EACSException.Create(strBusy);
    Self.FStartPos := Pos;
  end;

  procedure TACSCDIn.SetEP;
  begin
    if Self.Busy then raise EACSException.Create(strBusy);
    Self.FEndPos := Pos;
  end;

  constructor TACSCDIn.Create;
  begin
    inherited Create(AOwner);
    FCurrentDrive := 0;
  end;

  function TACSCDIn.GetSize;
  var
    msf1, msf2 : TACSCDMSF;
  begin
    if Busy then raise EACSException.Create(strBusy);
    OpenCD;
    msf1 := GetPosMSF(_cd_fd, FStartPos);
    msf2 := GetPosMSF(_cd_fd, FEndPos);
    CloseCD;
    Result := ((msf2.minute*60 + msf2.second)*75 + msf2.frame -
      ((msf1.minute*60 + msf1.second)*75 + msf1.frame))*CD_FRAMESIZE_RAW;
  end;

  procedure TACSCDIn.Init;
  begin
    if Busy then raise EACSException.Create(strBusy);
    if not (DiscInfo in [cdiDiscAudio, cdiDiscMixed]) then
    raise EACSException.Create(strNoAudioCD);
    FSize := GetSize;
    FBusy := True;
    BufStart := 1;
    BufEnd := 0;
    FPosition := 0;
    OpenCD;
    FCurPos := GetPosMSF(_cd_fd, FStartPos);
    FEndMSF := GetPosMSF(_cd_fd, FEndPos);
//    GetMem(FBuffer,BUF_SIZE * CD_FRAMESIZE_RAW);
  end;

  procedure TACSCDIn.Flush;
  begin
    CloseCD;
    FBusy := False;
//    FreeMem(FBuffer);
    FSize := 0;
  end;

  function TACSCDIn.GetData;
  var
    StartFrame, EndFrame : Integer;
    cdaudio : cdrom_read_audio;
  begin
    if not Busy then  raise EACSException.Create(strStreamnotopen);
    StartFrame := MSF2Frames(FCurPos);
    EndFrame := MSF2Frames(FEndMSF);
    if BufStart > BufEnd then
    begin
      if EndFrame = StartFrame then
      begin
        Result := 0;
        Exit;
      end;
      BufStart := 1;
      if (EndFrame - StartFrame) > (BUF_SIZE) then
        cdaudio.nframes := BUF_SIZE
      else
        cdaudio.nframes := EndFrame - StartFrame;
      cdaudio.addr_format := CDROM_MSF;
      cdaudio.addr.msf := cdrom_msf0(FCurPos);
      cdaudio.buf := Pointer(FBuffer);
      fpioctl(_cd_fd, CDROMREADAUDIO, @cdaudio);
      BufEnd := cdaudio.nframes * CD_FRAMESIZE_RAW;
      StartFrame := MSF2Frames(FCurPos) + cdaudio.nframes;
      Frames2MSF(StartFrame, FCurPos);
    end;
    if BufferSize < (BufEnd - BufStart + 1)
    then Result := BufferSize
    else Result := BufEnd - BufStart + 1;
    Move(FBuffer[BufStart], Buffer^, Result);
    Inc(BufStart, Result);
    Inc(FPosition, Result);
  end;

  procedure TACSCDIn.Eject;
  var
    Data: Integer;
  begin
    if Busy then raise EACSException.Create(strBusy);
    OpenCD;
    Data := 0;
    fpioctl(_cd_fd, CDROMEJECT,@Data);
    CloseCD;
  end;

  procedure TACSCDIn.CloseTray;
  var
    Data: Integer;
  begin
    OpenCD;
    Data := 0;
    fpioctl(_cd_fd, CDROMCLOSETRAY,@Data);
    CloseCD;
  end;
  
  procedure CountDrives;
  var
    _cd_fd, i : Integer;
    fname : String;
    sci : cdrom_subchnl;
    res :Integer;
    Data: Integer;
  begin
    DrivesCount := 0;
    Data := CDSL_CURRENT;
    for i := 0 to 3 do
    begin
      fname := '/dev/hd'+chr(ord('a')+i);
      _cd_fd := fpopen(PChar(fname), O_RDONLY or O_NONBLOCK);
      if _cd_fd >= 0 then
        begin
          res := fpioctl(_cd_fd, CDROM_DRIVE_STATUS, @Data);
          case res of
          CDS_TRAY_OPEN, CDS_NO_DISC, CDS_DRIVE_NOT_READY:
             begin
               inc(DrivesCount);
               setlength(DrivesPaths,DrivesCount);
               DrivesPaths[DrivesCount-1] := fname;
               fpclose(_cd_fd);
               continue;
             end;
          end;
          (* Either the disc is ok or no information
          from the driver. Trying CDROMSUBCHNL.*)
          sci.cdsc_format := CDROM_MSF;
          if fpioctl(_cd_fd, CDROMSUBCHNL, @sci) >= 0 then
            begin
               inc(DrivesCount);
               setlength(DrivesPaths,DrivesCount);
               DrivesPaths[DrivesCount-1] := fname;
            end;
          fpclose(_cd_fd);
        end;
    end;
  end;
  
  function TACSCDIn.GetDrivesCount : Integer;
  begin
    Result := DrivesCount;
  end;
  
  procedure TACSCDIn.SetCurrentDrive(Value : Integer);
  begin
    if Busy then raise EACSException.Create(strBusy);
    FCurrentDrive := Value;
  end;
  
  function TACSCDIn.GetDriveName : String;
  begin
    Result := '';
  end;

  destructor TACSCDIn.Destroy;
  begin
    inherited Destroy;
  end;


