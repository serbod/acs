(*
  this file is a part of audio components suite v 2.3.
  copyright (c) 2002-2005 andrei borovsky. all rights reserved.
  see the license file for more details.
  you can contact me at mail@z0m3ie.de
*)

{
$Log: acs_vorbis.pas,v $
Revision 1.8  2006/07/04 17:12:45  z0m3ie
ACS 2.4 alt wiederhergestellt (unterschiedliche Sampleformate ...)

Revision 1.4  2006/01/01 18:46:40  z0m3ie
*** empty log message ***

Revision 1.3  2005/12/29 20:45:59  z0m3ie
fixed some problems with vorbis in lazarus

Revision 1.2  2005/12/26 17:31:39  z0m3ie
fixed some problems in acs_dsfiles
fixed some problems in acs_vorbis
reworked all buffers

Revision 1.1  2005/12/19 18:36:38  z0m3ie
*** empty log message ***

Revision 1.5  2005/12/04 16:54:34  z0m3ie
All classes are renamed, Style TAcs... than T... to avoid conflicts with other components (eg TMixer is TAcsMixer now)

Revision 1.4  2005/11/28 21:57:24  z0m3ie
mostly FileOut fixes
moved PBuffer to PBuffer8
set all to dynamically Buffering

Revision 1.3  2005/10/02 16:51:01  z0m3ie
*** empty log message ***

Revision 1.2  2005/09/13 04:04:50  z0m3ie
First release without Components for Fileformats
only TFileIn and TFileOut are Visible

Revision 1.1  2005/09/12 22:04:52  z0m3ie
modified structure again, fileformats are now in an sperat folder.
all File In/Out classes are capsulated from TFileIn and TFileOut

Revision 1.3  2005/09/10 08:25:40  z0m3ie
*** empty log message ***

Revision 1.2  2005/08/22 20:17:01  z0m3ie
changed Headers to log
changed mail adress

}

unit acs_vorbis;

{$ifdef fpc}
{$mode delphi}
{$endif}

{$DEFINE USE_VORBIS_11}

interface

uses
  ACS_File,Classes, SysUtils, ACS_Classes, ogg, vorbiscodec, VorbisFile, VorbisEnc,ACS_Strings
{$IFDEF LINUX}
  ,baseunix;
{$ENDIF}

{$IFDEF WIN32}
  ,Windows,Dialogs;
{$ENDIF}

type

  TVorbisBitRate = (brAutoSelect, br45, br48, br56, br64, br80, br96,
                 br112, br128, br144, br160, br192, br224, br256, br320, br499);

  TVorbisOut = class(TAcsCustomFileOut)
  private
    FComments : TStringList;
    FSerial : Integer;
    FDesiredNominalBitrate : TVorbisBitRate;
    FDesiredMaximumBitrate : TVorbisBitRate;
    FMinimumBitrate : TVorbisBitRate;
    OggSS : ogg_stream_state;
    OggPg : ogg_page;
    OggPk : ogg_packet;
    VInfo : vorbis_info;
    VComm : vorbis_comment;
    Vdsp : vorbis_dsp_state;
    VBlock : vorbis_block;
    header, header_comm, header_code : ogg_packet;
    FCompression : Single;
    EndOfStream : Boolean;
    procedure SetComments(vComments : TStringList);
    procedure SetDesiredNominalBitrate(Value : TVorbisBitRate);
    procedure SetDesiredMaximumBitrate(Value : TVorbisBitRate);
    procedure SetMinimumBitrate(Value : TVorbisBitRate);
  protected
    procedure Done; override;
    function DoOutput(Abort : Boolean):Boolean; override;
    procedure Prepare; override;
    procedure SetFileMode(aMode : TAcsFileOutputMode); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Compression : Single read FCompression write FCompression stored True;
    property Comments : TStringList read FComments write SetComments stored True;
    property DesiredMaximumBitrate : TVorbisBitRate read FDesiredMaximumBitrate write SetDesiredMaximumBitrate;
    property DesiredNominalBitrate : TVorbisBitRate read FDesiredNominalBitrate write SetDesiredNominalBitrate;
    property MinimumBitrate : TVorbisBitRate read FMinimumBitrate write SetMinimumBitrate;
    property Serial : Integer read FSerial write FSerial;
    //property Vendor : String read FVendor write FVendor;
  end;

  TVorbisIn = class(TAcsCustomFileIn)
  private
    FComments : TStringList;
//    FVendor : String;
    VFile : OggVorbis_File;
    cursec : Integer;
    FMaxBitrate: Integer;
    FNominalBitrate: Integer;
    FMinBitrate : Integer;
    EndOfStream : Boolean;
    function GetMaxBitrate: Integer;
    function GetNominalBitrate: Integer;
    function GetMinBitrate : Integer;
    function GetComments : TStringList;
    function GetBitStreams : Integer;
    function GetInstantBitRate : Integer;
    function GetCurrentBitStream : Integer;
    procedure SetCurrentBitStream(BS : Integer);
  protected
    procedure OpenFile; override;
    procedure CloseFile; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetData(Buffer : Pointer; BufferSize : Integer): Integer; override;
    function Seek(SampleNum : Integer) : Boolean; override;
    property BitStreams : Integer read GetBitStreams;
    property Comments : TStringList read GetComments;
    property CurrentBitStream : Integer read GetCurrentBitStream write SetCurrentBitStream;
    property InstantBitRate : Integer read GetInstantBitRate;
    //property Vendor : String read FVendor;
    property MaxBitrate: Integer read GetMaxBitrate;
    property MinBitrate: Integer read GetMinBitrate;
    property NominalBitrate: Integer read GetNominalBitrate;
  end;

implementation

  function cbRead(ptr : Pointer; size, nmemb : Cardinal;const datasource : Pointer) : Cardinal; cdecl;
  var
    VI : TVorbisIn;
    Buffer : array of Byte;
  begin
    VI := TVorbisIn(datasource);
    SetLength(Buffer, size*nmemb);
    Result :=  VI.FStream.Read(Buffer[0], size*nmemb);
    Move(Buffer[0], ptr^, Result);
    Setlength(Buffer,0);
    Buffer := nil;
  end;

  function cbSeek(const datasource : Pointer; offset : ogg_int64_t; whence : Integer) : Integer; cdecl;
  var
    VI : TVorbisIn;
    Origin : TSeekOrigin;
  begin
    VI := TVorbisIn(datasource);
    if not VI.Seekable then
    begin
      Result := -1;
      Exit;
    end;  
    case whence of
      SEEK_SET : Origin := TSeekOrigin(soFromBeginning);
      SEEK_CUR : Origin := TSeekOrigin(soFromCurrent);
      SEEK_END : Origin := TSeekOrigin(soFromEnd);
    end;
    Result := VI.FStream.Seek(offset, Origin);
  end;

  function cbClose(const datasource : Pointer) : Integer; cdecl;
  var
    VI : TVorbisIn;
  begin
    VI := TVorbisIn(datasource);
    if not VI.FStreamAssigned then VI.FStream.Free
    else VI.FStream.Seek(0, soFromBeginning);
    Result := 0;
  end;

  function cbTell(const datasource : Pointer) : Integer; cdecl;
  var
    VI : TVorbisIn;
  begin
    VI := TVorbisIn(datasource);
    Result := VI.FStream.Position
  end;

  function VorbisBitrateToInt(Bitrate : TVorbisBitrate) : Integer;
  begin
    case Bitrate of
      br45 : Result := 45000;
      br48 : Result := 48000;
      br56 : Result := 46000;
      br64 : Result := 64000;
      br80 : Result := 80000;
      br96 : Result := 96000;
      br112 : Result := 112000;
      br128 : Result := 128000;
      br144 : Result := 144000;
      br160 : Result := 160000;
      br192 : Result := 192000;
      br224 : Result := 224000;
      br256 : Result := 256000;
      br320 : Result := 320000;
      br499 : Result := 499000;
      else Result := -1;
    end;
  end;

  constructor TVorbisOut.Create;
  begin
    inherited Create(AOwner);
    FBufferSize:=$10000; // default buffer size
    VORBISLoadLibrary();
    FCompression := 0.2;
    FComments := TStringList.Create;
    FDesiredNominalBitrate := br64;
    FDesiredMaximumBitrate := br112;
    FMinimumBitrate := br48;
    if not (csDesigning	in ComponentState) then
    begin
      VORBISLoadLibrary();
      if not LiboggLoaded then
      raise EAcsException.Create(Format(strCoudntloadLib,[Liboggpath]));
      if not LibvorbisLoaded then
      raise EAcsException.Create(Format(strCoudntloadLib,[LibvorbisPath]));
      if not LibvorbisfileLoaded then
      raise EAcsException.Create(Format(strCoudntloadLib,[LibvorbisfilePath]));
      if not LibvorbisencLoaded then
      raise EAcsException.Create(Format(strCoudntloadLib,[LibvorbisencPath]));
    end;
  end;

  destructor TVorbisOut.Destroy;
  begin
    FComments.Free;
    inherited Destroy;
  end;

  procedure TVorbisOut.SetComments;
  begin
    FComments.Assign(vComments);
  end;

procedure TVorbisOut.Prepare();
var
  i, maxbr, minbr, nombr: Integer;
  Name, Value: String;
  rm: ovectl_ratemanage2_arg;
begin
  inherited Prepare();

  if FFileMode = foAppend then
    FStream.Seek(0, soFromEnd);
  EndOfStream:=False;
  vorbis_info_init(VInfo);
  if DesiredNominalBitrate = brAutoSelect then
  begin
    {$IFNDEF USE_VORBIS_11}
    vorbis_encode_init_vbr(VInfo, FInput.Channels, FInput.SampleRate, FCompression);
    {$ENDIF}
    {$IFDEF USE_VORBIS_11}
    vorbis_encode_setup_vbr(VInfo, FInput.Channels, FInput.SampleRate, FCompression);
    vorbis_encode_setup_init(VInfo);
    {$ENDIF}
  end
  else
  begin
    nombr:=VorbisBitrateToInt(FDesiredNominalBitrate);
    maxbr:=VorbisBitrateToInt(FDesiredMaximumBitrate);
    if maxbr < nombr then maxbr:=nombr;
    minbr:=VorbisBitrateToInt(Self.FMinimumBitrate);
    if minbr < 0 then minbr:=nombr;
    vorbis_encode_init(VInfo, FInput.Channels, FInput.SampleRate, maxbr, nombr, minbr);
  end;
  vorbis_comment_init(VComm);
  for i:=0 to FComments.Count-1 do
  begin
    Name:=FComments.Names[i];
    Value:=FComments.Values[Name];
    vorbis_comment_add_tag(VComm, PChar(Name), PChar(Value));
  end;
  vorbis_analysis_init(Vdsp, VInfo);
  vorbis_block_init(Vdsp, VBlock);
  ogg_stream_init(OggSS, FSerial);
  vorbis_analysis_headerout(Vdsp, VComm, header, header_comm, header_code);
  ogg_stream_packetin(OggSS, header);
  ogg_stream_packetin(OggSS, header_comm);
  ogg_stream_packetin(OggSS, header_code);
  while ogg_stream_flush(OggSS, OggPg) <> 0 do
  begin
    FStream.Write(OggPg.header^, OggPg.header_len);
    FStream.Write(OggPg.body^, OggPg.body_len);
  end;
end;

procedure TVorbisOut.Done();
begin
  FComments.Clear();
  ogg_stream_clear(OggSS);
  vorbis_block_clear(VBlock);
  vorbis_dsp_clear(Vdsp);
  vorbis_comment_clear(VComm);
  vorbis_info_clear(VInfo);
  inherited Done();
end;

  function TVorbisOut.DoOutput(Abort : Boolean):Boolean;
  var
    Len, i, chc : Integer;
    out_buf : PPFloat;
    tmpBuf1, tmpBuf2 :  PFloat;
  begin
    // No exceptions Here
    Result := True;
    if not CanOutput then Exit;
    if Abort or EndOfStream then
    begin
      (* We don't close file here to avoide exceptions
        if output componenet's Stop method is called *)
      Result := False;
      Exit;
    end;

    Len:=FillBufferFromInput();
    chc:=FInput.Channels;
    if Len <> 0 then
    begin
      if chc = 2 then
      begin
        out_buf := vorbis_analysis_buffer(Vdsp, (FBuffer.Size div 4));
       (* A bit of pointer arithmetics. What is easy in C
          is not so easy in Pascal. *)
        tmpBuf1 := out_buf^;
        Inc(out_buf);
        tmpBuf2 := out_buf^;
        for i:=0 to (Len div 4)-1 do
        begin
          { // old
          tmpBuf1[i] := FBuffer[i*2]/$8000;
          tmpBuf2[i] := FBuffer[i*2+1]/$8000; }
          { // slow but true
          FBuffer.Position:=0;
          tmpBuf1[i] := FBuffer.ReadByte()/$8000;
          tmpBuf2[i] := FBuffer.ReadByte()/$8000; }
          // fast
          tmpBuf1[i] := PByte(FBuffer.Memory)[i*2]/$8000;
          tmpBuf2[i] := PByte(FBuffer.Memory)[i*2+1]/$8000;
        end;
        vorbis_analysis_wrote(Vdsp, Len shr 2);
      end
      else
      begin
        out_buf := vorbis_analysis_buffer(Vdsp, (FBuffer.Size div 2));
        for i:=0 to (Len div 2)-1 do
        begin
          //out_buf^[i] := FBuffer[i]/$8000;
          out_buf^[i] := PByte(FBuffer.Memory)[i]/$8000;
        end;
        vorbis_analysis_wrote(Vdsp, Len shr 1);
      end;
    end else
    vorbis_analysis_wrote(Vdsp, 0);
    while vorbis_analysis_blockout(Vdsp, VBlock) = 1 do
    begin
      vorbis_analysis(VBlock, nil);
      vorbis_bitrate_addblock(VBlock);
      while vorbis_bitrate_flushpacket(Vdsp, OggPk) = 1 do
      begin
        ogg_stream_packetin(OggSS, OggPk);
        while not EndOfStream do
        begin
          if ogg_stream_pageout(OggSS, OggPg) = 0 then Break;
          FStream.Write(OggPg.header^, OggPg.header_len);
          FStream.Write(OggPg.body^, OggPg.body_len);
          if ogg_page_eos(OggPg) <> 0 then EndOfStream := True;
        end;
      end;
    end;
  end;

  constructor TVorbisIn.Create;
  begin
    inherited Create(AOwner);
    BufferSize := $2000;
    FComments := TStringList.Create;
    if not (csDesigning	in ComponentState) then
    begin
      VORBISLoadLibrary;
      if not LiboggLoaded then
      raise EAcsException.Create(Format(strCoudntloadLib,[LiboggPath]));
      if not LibvorbisLoaded then
      raise EAcsException.Create(Format(strCoudntloadLib,[LibvorbisPath]));
      if not LibvorbisfileLoaded then
      raise EAcsException.Create(Format(strCoudntloadLib,[LibvorbisfilePath]));
      if not LibvorbisencLoaded then
      raise EAcsException.Create(Format(strCoudntloadLib,[LibvorbisencPath]));
    end;
  end;

  destructor TVorbisIn.Destroy;
  begin
    FComments.Free;
    inherited Destroy;
  end;

  procedure TVorbisIn.OpenFile;
  var
    PVComm : PVORBIS_COMMENT;
    PVInfo : PVORBIS_INFO;
    PComment : PPChar;
    Comment : PChar;
    Callbacks : OV_CALLBACKS;
  begin
    if not FOpened then
    begin
      FValid := False;
      EndOfStream := False;
      if not FStreamAssigned then
      try
        Stream := TFileStream.Create(FileName, fmOpenRead) as TFileStream;
      except
        Exit;
      end;
      Callbacks.read_func := cbRead;
      Callbacks.close_func := cbClose;
      Callbacks.seek_func := cbSeek;
      Callbacks.tell_func := cbTell;
      ov_open_callbacks(Self, VFile, nil, 0, Callbacks);
      FComments.Clear;
{      PVComm := ov_comment(VFile, -1);
      PComment := PVComm.user_comments;
      Comment := PComment^;
      while Comment <> nil do
      begin
        FComments.Add(String(Comment));
        Inc(LongWord(PComment), 4);
        Comment := PComment^;
      end;}
//      FVendor := PVComm.vendor;
      PVInfo := ov_info(VFile, -1);
      FChan := PVInfo.channels;
      FSR := PVInfo.rate;
      FBPS := 16;
      FMaxBitrate := PVInfo.bitrate_upper;
      FNominalBitrate := PVInfo.bitrate_nominal;
      FMinBitrate := PVInfo.bitrate_lower;
      FTotalSamples := ov_pcm_total(VFile, -1);
      FSize := (FTotalSamples shl 1) * PVInfo.channels;
      cursec := -1;
      FTime := Round(ov_time_total(VFile, -1));
  //    ov_pcm_seek(VFile, FOffset);
      FValid := True;
      FOpened := True;
    end;
  end;

  procedure TVorbisIn.CloseFile;
  begin
    if FOpened then
    begin
      if ov_seekable(VFile) <> 0 then
        ov_pcm_seek(VFile, 0);
      ov_clear(VFile);
      FOpened:=False;
    end;
  end;

  function TVorbisIn.GetData(Buffer : Pointer; BufferSize : Integer): Integer;
  var
    l, offs : Integer;
  begin
    if not Busy then  raise EAcsException.Create('The Stream is not opened');
    if BufStart > BufEnd then
    begin
      if FOffset <> 0 then
      begin
        offs := Round((FOffset/100)*FSize);
        FPosition := FPosition + offs;
        if FPosition < 0 then FPosition := 0
        else if FPosition > FSize then FPosition := FSize;
//        tmp := (FPosition/FSize)*FTime;
        if ov_seekable(VFile) <> 0 then
        ov_pcm_seek(VFile, (FPosition shr 1) div FChan);
        FOffset := 0;
      end;
      BufStart := 1;
      BufEnd := 0;
      if not EndOfStream then
      begin
        (* The ov_read function can return data in quite small chunks
          (of about 512 bytes). We keep reading data until the buffer is filled
          or there is no more data to read. *)
        while BufEnd < BufferSize do
        begin
          l := ov_read(VFile, @FBuffer[BufEnd + 1], BufferSize - BufEnd, 0, 2, 1, @cursec);
          if l <= 0 then
          begin
            EndOfStream := True;
            Break;
          end;
          Inc(BufEnd, l);
          if (FPosition + BufEnd) >= FSize then
          begin
            BufEnd := FSize - FPosition;
            if BufEnd <= 0 then EndOfStream := True;
            Break;
          end;
        end;
      end;
      if EndOfStream and FLoop then
      begin
        Flush;
        Init;
        EndOfStream := False;
        while BufEnd < BufferSize do
        begin
          l := ov_read(VFile, @FBuffer[BufEnd + 1], BufferSize - BufEnd, 0, 2, 1, @cursec);
          if l <= 0 then
          begin
            EndOfStream := True;
            Break;
          end;
          Inc(BufEnd, l);
        end;
      end;
    end;
    if BufferSize < (BufEnd - BufStart + 1)
    then Result := BufferSize
    else Result := BufEnd - BufStart + 1;
    Move(FBuffer[BufStart], Buffer^, Result);
    Inc(BufStart, Result);
    Inc(FPosition, Result);
  end;

  function TVorbisIn.GetMaxBitrate : Integer;
  begin
    OpenFile;
    Result := FMaxBitrate;
    CloseFile;
  end;

  function TVorbisIn.GetNominalBitrate : Integer;
  begin
    OpenFile;
    Result := FNominalBitrate;
    CloseFile;
  end;

  function TVorbisIn.GetComments : TStringList;
  begin
    OpenFile;
    Result := FComments;
    CloseFile;
  end;

  function TVorbisIn.GetMinBitrate : Integer;
  begin
    OpenFile;
    Result := FMinBitrate;
    CloseFile;
  end;

  procedure TVorbisOut.SetFileMode;
  begin
    FFileMode := aMode;
  end;

  function TVorbisIn.GetBitStreams : Integer;
  begin
    Result := 0;
    if Busy then
    begin
      if ov_seekable(VFile)<>0 then
      Result := ov_streams(VFile);
    end;
  end;

  function TVorbisIn.GetInstantBitRate : Integer;
  begin
    Result := 0;
    if Busy then
    begin
      Result := ov_bitrate_instant(VFile);
    end;
  end;

  function TVorbisIn.GetCurrentBitStream : Integer;
  begin
    Result := -1;
    if Busy then
    begin
      if ov_seekable(VFile)<>0 then
      Result := VFile.current_link;
    end;
  end;

  procedure TVorbisIn.SetCurrentBitStream;
  var
    Offset : POGG_INT64_T;
  begin
    if Busy then
    begin
      if ov_seekable(VFile)<>0 then
      if (BS >= 0) and (BS < ov_streams(VFile)) then
      begin
        Offset := VFile.offsets;
        Inc(Offset, BS);
        FStream.Seek(Offset^, soFromBeginning);
      end;
    end;
  end;

  procedure TVorbisOut.SetDesiredNominalBitrate;
  begin
    FDesiredNominalBitrate := Value;
    if FMinimumBitrate > FDesiredNominalBitrate then
    FMinimumBitrate := FDesiredNominalBitrate;
    if FDesiredMaximumBitrate < FDesiredNominalBitrate then
    FDesiredMaximumBitrate := FDesiredNominalBitrate;
    if FDesiredNominalBitrate = brAutoSelect then
    FDesiredMaximumBitrate := brAutoSelect;
  end;

  procedure TVorbisOut.SetDesiredMaximumBitrate;
  begin
    if FDesiredNominalBitrate = brAutoSelect then Exit;
    if (Value = brAutoSelect) or (Value >= FDesiredNominalBitrate) then
    FDesiredMaximumBitrate := Value;
  end;

  procedure TVorbisOut.SetMinimumBitrate;
  begin
    if Value <= FDesiredNominalBitrate then
    FMinimumBitrate := Value;
  end;

  function TVorbisIn.Seek(SampleNum : Integer) : Boolean;
  begin
    Result := False;
    if not FSeekable then Exit;
    Result := True;
    OpenFile;
    ov_pcm_seek(VFile, SampleNum);
    CloseFile;
  end;
  
initialization

  FileFormats.Add('ogg','Ogg Vorbis',TVorbisOut);
  FileFormats.Add('ogg','Ogg Vorbis',TVorbisIn);

end.
