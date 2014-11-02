{
$Log: acs_mixer.inc,v $
Revision 1.2  2005/12/30 12:54:42  z0m3ie
some error checks

Revision 1.1  2005/12/19 18:35:03  z0m3ie
*** empty log message ***

Revision 1.3  2005/12/04 16:54:34  z0m3ie
All classes are renamed, Style TACS... than T... to avoid conflicts with other components (eg TMixer is TACSMixer now)

Revision 1.2  2005/09/13 21:54:11  z0m3ie
acs is localizeable now (ACS_Strings)

Revision 1.1  2005/09/12 22:04:52  z0m3ie
modified structure again, fileformats are now in an sperat folder.
all File In/Out classes are capsulated from TFileIn and TFileOut

Revision 1.4  2005/09/09 21:33:43  z0m3ie
linux corrections

Revision 1.3  2005/08/31 20:30:40  z0m3ie
Mixer Channelname work now
minior corrections for Converters

Revision 1.2  2005/08/28 20:31:18  z0m3ie
linux restructuring for 2.4

Revision 1.1  2005/08/25 20:18:00  z0m3ie
Version 2.4 restructure
TCDPlayer removed (fits not in component structure)
TMP3ToWavConverter removed (fits not in component structure)

}


type
  TMixerInfo = record
    Path : String;
    Name : String;
  end;

const
  MAX_MIXERS = 5; (* There shouldn't be more than
                     5 valid mixers in the system.
                     Right? *)

var
  Mixers : array[0..MAX_MIXERS] of TMixerInfo; // one extra slot for /dev/mixer device

  function GetChannelMask(Ch : TACSMixerChannel; Request : Integer): LongWord;
  begin
    Result := 0;
    case Request of
      0:
      case Ch of
        mcVolume:  Result := SOUND_MIXER_VOLUME;
        mcTreble:  Result := SOUND_MIXER_TREBLE;
        mcBass:    Result := SOUND_MIXER_BASS;
        mcSynth:   Result := SOUND_MIXER_SYNTH;
        mcPCM:     Result := SOUND_MIXER_PCM;
        mcSpeaker: Result := SOUND_MIXER_SPEAKER;
        mcLine:    Result := SOUND_MIXER_LINE;
        mcMic:     Result := SOUND_MIXER_MIC;
        mcCD:      Result := SOUND_MIXER_CD;
        mcIMix:    Result := SOUND_MIXER_IMIX;
        mcAltPCM:  Result := SOUND_MIXER_ALTPCM;
        mcRecLev:  Result := SOUND_MIXER_RECLEV;
        mcUnknown: Result := 0;
      end;
      1:
      case Ch of
        mcVolume:  Result := SOUND_MIXER_WRITE_VOLUME;
        mcTreble:  Result := SOUND_MIXER_WRITE_TREBLE;
        mcBass:    Result := SOUND_MIXER_WRITE_BASS;
        mcSynth:   Result := SOUND_MIXER_WRITE_SYNTH;
        mcPCM:     Result := SOUND_MIXER_WRITE_PCM;
        mcSpeaker: Result := SOUND_MIXER_WRITE_SPEAKER;
        mcLine:    Result := SOUND_MIXER_WRITE_LINE;
        mcMic:     Result := SOUND_MIXER_WRITE_MIC;
        mcCD:      Result := SOUND_MIXER_WRITE_CD;
        mcIMix:    Result := SOUND_MIXER_WRITE_IMIX;
        mcAltPCM:  Result := SOUND_MIXER_WRITE_ALTPCM;
        mcRecLev:  Result := SOUND_MIXER_WRITE_RECLEV;
        mcUnknown: Result := 0;
      end;
      2:
      case Ch of
        mcVolume:  Result := SOUND_MIXER_READ_VOLUME;
        mcTreble:  Result := SOUND_MIXER_READ_TREBLE;
        mcBass:    Result := SOUND_MIXER_READ_BASS;
        mcSynth:   Result := SOUND_MIXER_READ_SYNTH;
        mcPCM:     Result := SOUND_MIXER_READ_PCM;
        mcSpeaker: Result := SOUND_MIXER_READ_SPEAKER;
        mcLine:    Result := SOUND_MIXER_READ_LINE;
        mcMic:     Result := SOUND_MIXER_READ_MIC;
        mcCD:      Result := SOUND_MIXER_READ_CD;
        mcIMix:    Result := SOUND_MIXER_READ_IMIX;
        mcAltPCM:  Result := SOUND_MIXER_READ_ALTPCM;
        mcRecLev:  Result := SOUND_MIXER_READ_RECLEV;
        mcUnknown: Result := 0;
      end;
    end;
  end;

  function GetChannelType(Mask : Integer) : TACSMixerChannel;
  begin
    case Mask of
      SOUND_MIXER_VOLUME:  Result := mcVolume;
      SOUND_MIXER_TREBLE:  Result := mcTreble;
      SOUND_MIXER_BASS:    Result := mcBass;
      SOUND_MIXER_SYNTH:   Result := mcSynth;
      SOUND_MIXER_PCM:     Result := mcPCM;
      SOUND_MIXER_SPEAKER: Result := mcSpeaker;
      SOUND_MIXER_LINE:    Result := mcLine;
      SOUND_MIXER_MIC:     Result := mcMic;
      SOUND_MIXER_CD:      Result := mcCD;
      SOUND_MIXER_IMIX:    Result := mcIMix;
      SOUND_MIXER_ALTPCM:  Result := mcAltPCM;
      SOUND_MIXER_RECLEV:  Result := mcRecLev;
      else                 Result := mcUnknown;
    end;
  end;

  procedure TACSMixer.SetDevNum(Num : Integer);
  var
    DevMask, i : Integer;
    Channel : TACSMixerChannel;
  begin
    if Num in [0..MixersCount - 1] then // check [0..0] [0..-1]
    begin
      FFileName := Mixers[Num].Path;
      FMixerName := Mixers[Num].Name;
      setlength(FChannels,0);
      _mix_fd := fpopen(PChar(FFileName), O_RDONLY);
      fpioctl(_mix_fd, SOUND_MIXER_READ_DEVMASK, @DevMask);
      fpclose(_mix_fd);
      for i:=0 to 31 do
      begin
        if (DevMask and (1 shl i)) <> 0 then
        begin
          Channel := GetChannelType(i);
          if Channel <> mcUnknown then
            begin
              setlength(FChannels,length(FChannels)+1);
              FChannels[length(FChannels)-1] := Channel;
            end;
        end;
      end;
    end;
  end;

  function TACSMixer.GetRecSource;
  var
    rs, pow,i : Integer;
  begin
    Result := -1;
    _mix_fd := fpopen(PChar(FFileName), O_RDONLY);
    fpioctl(_mix_fd, SOUND_MIXER_READ_RECSRC, @rs);
    fpclose(_mix_fd);
    pow := 0;
    while rs <> 1 do
    begin
      rs := rs shr 1;
      Inc(pow);
    end;
    for i := 0 to length(FChannels)-1 do
      if FChannels[i] = GetChannel(pow) then
        Result := i;
  end;

  function TACSMixer.GetVolume;
  var
    vol, chan : Integer;
  begin
    _mix_fd := fpopen(PChar(FFileName), O_RDONLY);
    chan := GetChannelMask(FChannels[vChannel], 2);
    fpioctl(_mix_fd, chan, @vol);
    fpclose(_mix_fd);
    if vol > 255 then
    begin
      Result.Left := Lo(vol);
      Result.Right := Lo(vol shr 8);
    end else Result.Main := vol;
  end;

  function TACSMixer.IsStereo;
  var
    mask, chan : Integer;
  begin
    _mix_fd := fpopen(PChar(FFileName), O_RDONLY);
    fpioctl(_mix_fd, SOUND_MIXER_READ_STEREODEVS, @mask);
    chan := GetChannelMask(FChannels[vChannel], 0);
    fpclose(_mix_fd);
    Result := (mask and (1 shl chan))<>0;
  end;

  function TACSMixer.IsRecordable;
  var
    mask, chan : Integer;
  begin
    _mix_fd := fpopen(PChar(FFileName), O_RDONLY);
    fpioctl(_mix_fd, SOUND_MIXER_READ_RECMASK, @mask);
    chan := GetChannelMask(FChannels[vChannel], 0);
    fpclose(_mix_fd);
    Result := (mask and (1 shl chan))<>0;
  end;

  procedure TACSMixer.SetRecSource;
  var
    chan : Integer;
  begin
    chan := 1 shl GetChannelMask(FChannels[vChannel], 0);
    _mix_fd := fpopen(PChar(FFileName), O_WRONLY);
    fpioctl(_mix_fd, SOUND_MIXER_WRITE_RECSRC, @chan);
    fpclose(_mix_fd);
    if chan <> (1 shl GetChannelMask(FChannels[vChannel], 0)) then
    raise EACSException.Create(Format(strChannelnotRecordable,[vChannel]));
  end;

  function TACSMixer.GetMute(vChannel : integer) : Boolean;
  begin
    Result := False;
  end;

  procedure TACSMixer.SetMute(vChannel : integer;  Mute : Boolean);
  begin
  end;

  procedure TACSMixer.SetVolume;
  var
    vol, chan : Integer;
  begin
    chan := GetChannelMask(FChannels[vChannel], 1);
    if IsStereo(vChannel) then
    vol := vLevel.Left + (vLevel.Right shl 8)
    else vol := vLevel.Main;
    _mix_fd := fpopen(PChar(FFileName), O_WRONLY);
    fpioctl(_mix_fd, chan, @vol);
    fpclose(_mix_fd);
  end;

  destructor TACSMixer.Destroy;
  begin
    Setlength(FChannels,0);
    inherited Destroy;
  end;

  function CountMixers : Byte;
  var
    fd, i, DevMask : Integer;
    fname : String;
    mi : mixer_info;
  begin
    Result := 0;
    for i := 0 to MAX_MIXERS-1 do
    begin
      fname := '/dev/mixer'+IntToStr(i-1);
      try
        fd := fpopen(PChar(fname), O_RDONLY);
      except
        Break;
      end;
      if fd = -1 then Break;
      DevMask := 0;
      fpioctl(fd, SOUND_MIXER_READ_DEVMASK, @DevMask);
      if DevMask <> 0 then
      begin
        Mixers[Result].Path := fname;
        fpioctl(fd, SOUND_MIXER_INFO, @mi);
        Mixers[Result].Name := String(mi.name);
        Inc(Result);
      end;
      fpclose(fd);
    end;
    fname := '/dev/mixer';
    try
      fd := fpopen(PChar(fname), O_RDONLY);
    except
      Exit;
    end;
    if fd = -1 then Exit;
    fpioctl(fd, SOUND_MIXER_READ_DEVMASK, @DevMask);
    if DevMask <> 0 then
    begin
      Mixers[Result].Path := fname;
      fpioctl(fd, SOUND_MIXER_INFO, @mi);
      Mixers[Result].Name := String(mi.name);
    end;
    fpclose(fd);
    Inc(Result);
  end;

