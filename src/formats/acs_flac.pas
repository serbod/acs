(*
FLAC (Free Lossless Audio Codec) components

This file is a part of Audio Components Suite.
All rights reserved. See the license file for more details.

Copyright (c) 2002-2010, Andrei Borovsky, anb@symmetrica.net
Copyright (c) 2005-2006  Christian Ulrich, mail@z0m3ie.de
Copyright (c) 2014-2015  Sergey Bodrov, serbod@gmail.com
*)

{
Status:
TLibFLACOut - not tested
TLibFLACIn - not tested

Some problems in record packing/alignment for FLAC__StreamMetadata and it's members
tested for Win32

TFLACIn - Read OK, Seek not implemented
}

unit acs_flac;

(* Title: ACS_FLAC
    NewAC interface to libFLAC.dll *)

interface

{$I ../../acs_defines.inc}

{$IFDEF FPC}
{$MODE delphi}
{$ENDIF}

uses
  Classes, SysUtils, Math, {FastMove,} ACS_Types, ACS_Classes, ACS_Tags
  , ACS_File, acs_strings
{$ifdef ACS_FLAC_EXT}
  , libflac
{$endif}
{$ifdef ACS_FLAC}
  , flac
{$endif}
{$IFDEF LINUX}
  , baseunix
{$ENDIF}

{$IFDEF WIN32}
  , Windows
{$ENDIF}
  ;

type

{$ifdef ACS_FLAC_EXT}
(* Class: TFLACOut
    FLAC encoder component.
    Descends from <TAcsCustomFileOut>.
    Requires libFLAC.dll
    More information about FLAC can be found at http://flac.sourceforge.com. *)

  TLibFLACOut = class(TAcsCustomFileOut)
  private
    Buffer: PAcsBuffer8;
    FBufSize: Integer;
    _encoder: P_FLAC__StreamEncoder;
    FVerify: Boolean;
    FBlockSize: Word;
    FBestModelSearch: Boolean;
    FEnableMidSideStereo: Boolean;
    FMaxLPCOrder: Word;
    EndOfInput: Boolean;
    FEnableLooseMidSideStereo: Boolean;
    FQLPCoeffPrecision: Word;
    FQLPCoeffPrecisionSearch: Boolean;
    FMaxResidualPartitionOrder: Word;
    FMinResidualPartitionOrder: Word;
    FCompressionLevel: Integer;
    BolckInserted: Boolean;
    FTags: TVorbisTags;
    procedure SetEnableLooseMidSideStereo(val: Boolean);
    procedure SetBestModelSearch(val: Boolean);
    procedure SetCompressionLevel(val: Integer);
    procedure SetTags(AValue: TVorbisTags);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Init(); override;
    procedure Done(); override;
    function DoOutput(Abort: Boolean): Boolean; override;
  published
  (* Property: BestModelSearch
      Similar to America's Next Top Model, except for algorithms. *)
    property BestModelSearch: Boolean read FBestModelSearch write SetBestModelSearch;
  (* Property: Blocksize
      The size you want some blocks to be. Has nothing to do with <BestModelSearch> *)
    property BlockSize: Word read FBlockSize write FBlockSize;
  (* Property: CompressionLevel
      What level you want your compression at. *)
    property CompressionLevel: Integer read FCompressionLevel write SetCompressionLevel;
  (* Property: EnableMidSideStereo
      Set this property to True to get a bit more compression. *)
    property EnableMidSideStereo: Boolean read FEnableMidSideStereo
      write FEnableMidSideStereo;
    property EnableLooseMidSideStereo: Boolean
      read FEnableLooseMidSideStereo write SetEnableLooseMidSideStereo;
    property MaxLPCOrder: Word read FMaxLPCOrder write FMaxLPCOrder;
    property MaxResidualPartitionOrder: Word
      read FMaxResidualPartitionOrder write FMaxResidualPartitionOrder;
    property MinResidualPartitionOrder: Word
      read FMinResidualPartitionOrder write FMinResidualPartitionOrder;
    property QLPCoeffPrecision: Word read FQLPCoeffPrecision write FQLPCoeffPrecision;
    property QLPCoeffPrecisionSearch: Boolean
      read FQLPCoeffPrecisionSearch write FQLPCoeffPrecisionSearch;
  (* Property: Tags
      Use this property to add a set of Vorbis-style comments (artist, title, etc.) to the output file. *)
    property Tags: TVorbisTags read FTags write SetTags;
  (* Property: Verify
      Setting Verify to True forces the FLAC encoder to verify its own output. It slows down encoding process and usually unnecessary. *)
    property Verify: Boolean read FVerify write FVerify;
  end;


(* Class: TLibFLACIn
    FLAC decoder component.
    Descends from <TAuFileIn>.
    Requires libFLAC.dll
    More information about FLAC can be found at http://flac.sourceforge.com. *)

  { TLibFLACIn }

  TLibFLACIn = class(TAcsCustomFileIn)
  private
    EndOfMetadata: Boolean;
    FComments: TVorbisTags;
    Residue: Integer;
    //Buff: PAcsBuffer8;
    //BuffSize: LongWord;
    _decoder: P_FLAC__StreamDecoder;
    FBlockSize: LongWord;
    BytesPerBlock: LongWord;
    MinFrameSize: LongWord;
    FCheckMD5Signature: Boolean;
    FSignatureValid: Boolean;
    FSampleSize: Integer;
    function GetComments: TVorbisTags;
  protected
    procedure OpenFile(); override;
    procedure CloseFile(); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy(); override;
    function GetData(ABuffer: Pointer; ABufferSize: Integer): Integer; override;
    function Seek(SampleNum: Integer): Boolean; override;
  (* Property: IsMD5SignatureValid
      If MD5 signature checking is turned on, this property returns True if the signature is correct (i.e. file contents is not broken).
      This property value becomes meaningful only after the file has finished playing.
      Note that if FLAC codec cannot check the signature for some internal reason, this property still returns True.*)
    property IsMD5SignatureValid: Boolean read FSignatureValid;
  (* Property: VorbisComments
      Read this property to get tags (artist, title, etc.) that may be attached to the file.*)
    property VorbisComments: TVorbisTags read GetComments;
  published
  (* Property:  CheckMD5Signature
      This property specifies whether the input file's MD5 signature should be checked.
      The MD5 signature checking should be turned on before the file starts playing.
      If you set this property to True, you can use <IsMD5SignatureValid> value to check the signature after file has finished playing.
      Note, that seeking in the input file turns the signature checking off (the value of CheckMD5Signature becomes False).
      In this case <IsMD5SignatureValid> will aalways return True.*)
    property CheckMD5Signature: Boolean read FCheckMD5Signature write FCheckMD5Signature;
    property EndSample;
    property StartSample;
  end;
{$endif}

{$ifdef ACS_FLAC}
  { TFLACIn }

  TFLACIn = class(TAcsCustomFileIn)
  private
    FBitReader: TBitReader;
    //Residue: Integer;
    //Buff: PAcsBuffer8;
    //BuffSize: LongWord;
    //FBlockSize: LongWord;
    //BytesPerBlock: LongWord;
    //MinFrameSize: LongWord;
    //FCheckMD5Signature: Boolean;
    //FSignatureValid: Boolean;
    FSampleSize: Integer;
    FFlacStatus: Integer;
    FSamplesRead: Int64; // from file begining
    FSeekSampleNum: Int64; // for OpenFile()
    FStreamInfoBlock: TFlacStreamInfoBlock;
    FChannelBuffers: array of TFlacSampleBuffer;
  protected
    procedure OpenFile(); override;
    procedure CloseFile(); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy(); override;
    function GetData(ABuffer: Pointer; ABufferSize: Integer): Integer; override;
    function Seek(SampleNum: Integer): Boolean; override;
  published
    property EndSample;
    property StartSample;
  end;

{$endif}

implementation

{$ifdef ACS_FLAC_EXT}
type

  FLACBuf = array[0..0] of FLAC__int32;
  PFLACBuf = ^FLACBuf;
  FLACUBuf = array[0..0] of FLAC__uint32;
  PFLACUBuf = ^FLACBuf;

type
  TBlockInfo = record
    BlockType: Word;
    BlockLength: LongWord;
    HasNext: Boolean;
  end;

  TVComments = record
    Vendor: UTF8String;
    Title: UTF8String;
    Artist: UTF8String;
    Album: UTF8String;
    Date: UTF8String;
    Genre: UTF8String;
    Track: UTF8String;
    Disc: UTF8String;
    Reference: UTF8String;
    TrackGain: UTF8String;
    TrackPeak: UTF8String;
    AlbumGain: UTF8String;
    AlbumPeak: UTF8String;
  end;


function BuildCommentsBlock(var Comments: TVComments; var Block: Pointer;
  HasNext: Boolean): Integer;
var
  BS: Integer;
  Header: array[0..4] of byte;
  t: byte;
  P: PAnsiChar;
  i, StrCount: Integer;

  procedure AddString(const Str: UTF8String; AddEmpty: Boolean = False);
  var
    l: Integer;
  begin
    l := Length(Str);
    if (l = 0) and (not AddEmpty) then Exit;
    Move(l, P[i], 4);
    Inc(i, 4);
    Move(Str[1], P[i], l);
    Inc(i, l);
  end;

begin
  BS := 4;
  StrCount := 0;
  Comments.Vendor := Utf8Encode('VENDOR=Hacked');
  Inc(BS, Length(Comments.Vendor) + 4);
  if Comments.Title <> '' then
  begin
    Inc(StrCount);
    Inc(BS, Length(Comments.Title) + 4);
  end;
  if Comments.Artist <> '' then
  begin
    Inc(StrCount);
    Inc(BS, Length(Comments.Artist) + 4);
  end;
  if Comments.Album <> '' then
  begin
    Inc(StrCount);
    Inc(BS, Length(Comments.Album) + 4);
  end;
  if Comments.Date <> '' then
  begin
    Inc(StrCount);
    Inc(BS, Length(Comments.Date) + 4);
  end;
  if Comments.Genre <> '' then
  begin
    Inc(StrCount);
    Inc(BS, Length(Comments.Genre) + 4);
  end;
  if Comments.Track <> '' then
  begin
    Inc(StrCount);
    Inc(BS, Length(Comments.Track) + 4);
  end;
  if Comments.Disc <> '' then
  begin
    Inc(StrCount);
    Inc(BS, Length(Comments.Disc) + 4);
  end;
  if Comments.Reference <> '' then
  begin
    Inc(StrCount);
    Inc(BS, Length(Comments.Reference) + 4);
  end;
  if Comments.TrackGain <> '' then
  begin
    Inc(StrCount);
    Inc(BS, Length(Comments.TrackGain) + 4);
  end;
  if Comments.TrackPeak <> '' then
  begin
    Inc(StrCount);
    Inc(BS, Length(Comments.TrackPeak) + 4);
  end;
  if Comments.AlbumGain <> '' then
  begin
    Inc(StrCount);
    Inc(BS, Length(Comments.AlbumGain) + 4);
  end;
  if Comments.AlbumPeak <> '' then
  begin
    Inc(StrCount);
    Inc(BS, Length(Comments.AlbumPeak) + 4);
  end;
  Header[0] := Ord(FLAC__METADATA_TYPE_VORBIS_COMMENT);
  if not HasNext then
    Inc(Header[0], 128);
  Move(BS, Header[1], 3);
  t := Header[3];
  Header[3] := Header[1];
  Header[1] := t;
  Result := BS + 4;
  GetMem(Block, Result);
  P := PAnsiChar(Block);
  Move(Header[0], P[0], 4);
  i := 4;
  AddString(Comments.Vendor, True);
  Move(StrCount, P[i], 4);
  Inc(i, 4);
  AddString(Comments.Title);
  AddString(Comments.Artist);
  AddString(Comments.Album);
  AddString(Comments.Date);
  AddString(Comments.Genre);
  AddString(Comments.Track);
  AddString(Comments.Disc);
  AddString(Comments.Reference);
  AddString(Comments.TrackGain);
  AddString(Comments.TrackPeak);
  AddString(Comments.AlbumGain);
  AddString(Comments.AlbumPeak);
end;

function EncWriteCBFunc(encoder: P_FLAC__StreamEncoder;
  buffer: PFLAC__byte;
  bytes, samples, current_frame: LongWord;
  client_data: Pointer): Integer; cdecl;
var
  FLACOut: TLibFLACOut;
  BI: TBlockInfo;
  Header: array[0..3] of Byte;
  Comm: TVComments;
  Block: Pointer;
  bresult: LongInt;
begin
  FLACOut := TLibFLACOut(client_data);
  Result := FLAC__STREAM_ENCODER_WRITE_STATUS_OK;
  try
    if not FLACOut.BolckInserted then
    begin
      Move(buffer^, Header, 4);
      BI.HasNext := (Header[0] shr 7) = 0;
      BI.BlockType := Header[0] mod 128;
      if (BI.BlockType = Ord(FLAC__METADATA_TYPE_VORBIS_COMMENT)) then
      begin
        FLACOut.BolckInserted := True;
        if FlacOut.FTags.Artist <> '' then
          Comm.Artist := Utf8Encode(WideString(_vorbis_Artist + '=') +
            FlacOut.FTags.Artist);
        if FlacOut.FTags.Album <> '' then
          Comm.Album := Utf8Encode(WideString(_vorbis_Album + '=') + FlacOut.FTags.Album);
        if FlacOut.FTags.Title <> '' then
          Comm.Title := Utf8Encode(WideString(_vorbis_Title + '=') + FlacOut.FTags.Title);
        if FlacOut.FTags.Date <> '' then
          Comm.Date := Utf8Encode(WideString(_vorbis_Date + '=') + FlacOut.FTags.Date);
        if FlacOut.FTags.Genre <> '' then
          Comm.Genre := Utf8Encode(WideString(_vorbis_Genre + '=') + FlacOut.FTags.Genre);
        if FlacOut.FTags.Track <> '' then
          Comm.Track := Utf8Encode(WideString(_vorbis_Track + '=') + FlacOut.FTags.Track);
        if FlacOut.FTags.Disc <> '' then
          Comm.Disc := Utf8Encode(WideString(_vorbis_Disc + '=') + FlacOut.FTags.Disc);
        if FlacOut.FTags.Reference <> '' then
          Comm.Reference := Utf8Encode(WideString(_vorbis_Reference + '=') +
            FlacOut.FTags.Reference);
        if FlacOut.FTags.TrackGain <> '' then
          Comm.TrackGain := Utf8Encode(WideString(_vorbis_TrackGain + '=') +
            FlacOut.FTags.TrackGain);
        if FlacOut.FTags.TrackPeak <> '' then
          Comm.TrackPeak := Utf8Encode(WideString(_vorbis_TrackPeak + '=') +
            FlacOut.FTags.TrackPeak);
        if FlacOut.FTags.AlbumGain <> '' then
          Comm.AlbumGain := Utf8Encode(WideString(_vorbis_AlbumGain + '=') +
            FlacOut.FTags.AlbumGain);
        if FlacOut.FTags.AlbumPeak <> '' then
          Comm.AlbumPeak := Utf8Encode(WideString(_vorbis_AlbumPeak + '=') +
            FlacOut.FTags.AlbumPeak);
        bytes := BuildCommentsBlock(Comm, Block, BI.HasNext);
        bresult := FLACOut.FStream.Write(Block^, bytes);
        FreeMem(Block);
      end
      else
        bresult := FLACOut.FStream.Write(buffer^, bytes);
    end
    else
      bresult := FLACOut.FStream.Write(buffer^, bytes);
    if bresult <> Integer(bytes) then
      Result := FLAC__STREAM_ENCODER_WRITE_STATUS_FATAL_ERROR;
  except
    Result := FLAC__STREAM_ENCODER_WRITE_STATUS_FATAL_ERROR;
  end;
end;

function EncSeekCBFunc(encoder: P_FLAC__StreamEncoder;
  absolute_byte_offset: FLAC__uInt64;
  client_data: Pointer): Integer; cdecl;
var
  FLACOut: TLibFLACOut;
begin
  FLACOut := TLibFLACOut(client_data);
  Result := FLAC__STREAM_ENCODER_SEEK_STATUS_OK;
  try
    FLACOut.FStream.Seek(absolute_byte_offset, soFromBeginning);
  except
    Result := FLAC__STREAM_ENCODER_SEEK_STATUS_ERROR;
  end;
end;

function EncTellCBFunc(decoder: P_FLAC__StreamDecoder;
  var absolute_byte_offset: FLAC__uInt64;
  client_data: Pointer): Integer; cdecl;
var
  FLACOut: TLibFLACOut;
begin
  FLACOut := TLibFLACOut(client_data);
  absolute_byte_offset := FLACOut.Stream.Position;
  Result := FLAC__STREAM_ENCODER_TELL_STATUS_OK;
end;


procedure EncMetadataCBFunc(decoder: P_FLAC__StreamDecoder;
  metadata: Pointer;
  client_data: Pointer); cdecl;
begin
  // Nothing to do here
end;


function DecReadCBFunc(decoder: P_FLAC__StreamDecoder;
  buffer: PFLAC__byte; var bytes: LongWord;
  client_data: Pointer): Integer; cdecl;
var
  FLACIn: TLibFLACIn;
begin
  FLACIn := TLibFLACIn(client_data);
  Result := FLAC__STREAM_DECODER_READ_STATUS_CONTINUE;
  if FLACIn.FStream.Position >= FLACIn.FStream.Size then
  begin
    Result := FLAC__STREAM_DECODER_READ_STATUS_END_OF_STREAM;
    Exit;
  end;
  try
    bytes := FLACIn.FStream.Read(buffer^, bytes);
    if bytes = 0 then
      Result := FLAC__STREAM_DECODER_READ_STATUS_END_OF_STREAM;
  except
    Result := FLAC__STREAM_DECODER_READ_STATUS_ABORT;
  end;
end;

function DecSeekCBFunc(decoder: P_FLAC__StreamDecoder;
  absolute_byte_offset: FLAC__uInt64;
  client_data: Pointer): Integer; cdecl;
var
  FLACIn: TLibFLACIn;
begin
  FLACIn := TLibFLACIn(client_data);
  if not FLACIn.FSeekable then
  begin
    Result := FLAC__STREAM_DECODER_SEEK_STATUS_UNSUPPORTED;
    Exit;
  end;
  Result := FLAC__STREAM_DECODER_SEEK_STATUS_OK;
  try
    FLACIn.FStream.Seek(absolute_byte_offset, soFromBeginning);
    if absolute_byte_offset > FlacIn.FSize then
      Result := FLAC__STREAM_DECODER_READ_STATUS_END_OF_STREAM;
  except
    Result := FLAC__STREAM_DECODER_SEEK_STATUS_ERROR;
  end;
end;

function DecTellCBFunc(decoder: P_FLAC__StreamDecoder;
  var absolute_byte_offset: FLAC__uInt64;
  client_data: Pointer): Integer; cdecl;
var
  FLACIn: TLibFLACIn;
begin
  FLACIn := TLibFLACIn(client_data);
  if FLACIn.FSize = 0 then
  begin
    Result := FLAC__STREAM_DECODER_TELL_STATUS_UNSUPPORTED;
    Exit;
  end;
  Result := FLAC__STREAM_DECODER_TELL_STATUS_OK;
  try
    absolute_byte_offset := FLACIn.FStream.Position;
  except
    Result := FLAC__STREAM_DECODER_TELL_STATUS_ERROR;
  end;
end;

function DecLengthCBFunc(decoder: P_FLAC__StreamDecoder;
  var stream_length: FLAC__uInt64;
  client_data: Pointer): Integer; cdecl;
var
  FLACIn: TLibFLACIn;
begin
  FLACIn := TLibFLACIn(client_data);
  Result := FLAC__STREAM_DECODER_LENGTH_STATUS_OK;
  try
    stream_length := FLACIn.FStream.Size;
  except
    Result := FLAC__STREAM_DECODER_LENGTH_STATUS_ERROR;
  end;
end;

function DecEOFCBFunc(decoder: P_FLAC__StreamDecoder;
  client_data: Pointer): longbool; cdecl;
var
  FLACIn: TLibFLACIn;
begin
  FLACIn := TLibFLACIn(client_data);
  Result := (FLACIn.FStream.Position >= FLACIn.FStream.Size);
end;

function DecWriteCBFunc(decoder: P_FLAC__StreamDecoder;
  frame: PFLAC__Frame;
  buffer: PFLACInt32BufArray;
  client_data: Pointer): Integer; cdecl;
var
  FLACIn: TLibFLACIn;
  Header: PFLAC__FrameHeader;
  i, ii, j: LongWord;
begin
  FLACIn := TLibFLACIn(client_data);
  Header := PFLAC__FrameHeader(frame);
  FLACIn.FBlockSize := Header.blocksize;
  FLACIn.BytesPerBlock := FLACIn.FBlockSize * (FLACIn.FBPS div 8) * FLACIn.FChan;
  if FLACIn.FAudioBuffer.Size < FLACIn.BytesPerBlock then
    FLACIn.FAudioBuffer.Size := FLACIn.BytesPerBlock;

  if FLACIn.FAudioBuffer.Interleaved then
  begin
    {$R-}
    for i := 0 to FLACIn.FBlockSize - 1 do
    begin
      for j := 0 to FLACIn.FChan - 1 do
      begin
        ii := ((i * FLACIn.FChan) + j) * (FLACIn.FBPS div 8);

        if FLACIn.FBPS = 8 then
          pUInt8(FLACIn.FAudioBuffer.Memory + ii)^ := uint8(buffer[j][i])

        else if FLACIn.FBPS = 16 then
          pUInt16(FLACIn.FAudioBuffer.Memory + ii)^ := uint16(buffer[j][i])

        else if FLACIn.FBPS = 24 then
        begin
          pUInt8(FLACIn.FAudioBuffer.Memory + ii + 0)^ :=
            uint8(uint32(buffer[j][i]) and $000000FF);
          pUInt8(FLACIn.FAudioBuffer.Memory + ii + 1)^ :=
            uint8((uint32(buffer[j][i]) and $0000FF00) shr 8);
          pUInt8(FLACIn.FAudioBuffer.Memory + ii + 2)^ :=
            uint8((uint32(buffer[j][i]) and $00FF0000) shr 16);
        end;
      end;
    end;
    {$R-}
    // manually move WritePosition
    FLACIn.FAudioBuffer.WritePosition := FLACIn.BytesPerBlock;
  end
  else // if not FLACIn.FAudioBuffer.Interleaved then
  begin
  end;
  Result := FLAC__STREAM_ENCODER_OK;
end;

function Utf8ToWideStr(s: ansistring): WideString;
begin
  {$ifdef FPC}
  Result:=UTF8Decode(s);
  {$else}
  {$WARNINGS OFF}
  {$IF CompilerVersion < 20}
  Result := Utf8Decode(s);
  {$IFEND}
  {$IF CompilerVersion >= 20}
  Result := Utf8ToString(s);
  {$IFEND}
  {$WARNINGS ON}
  {$endif FPC}
end;

procedure DecMetadataCBProc(decoder: P_FLAC__StreamDecoder;
  metadata: PFLAC__StreamMetadata;
  client_data: Pointer); cdecl;
var
  FLACIn: TLibFLACIn;
  FI: FLAC__StreamMetadata_StreamInfo;
  i: Integer;
  S: ansistring;
  Entry: PFLAC__StreamMetadata_VorbisComment_Entry;
  SL: TStringList;
begin
  FLACIn := TLibFLACIn(client_data);
  if metadata._type = FLAC__METADATA_TYPE_STREAMINFO then
  begin
    //LongWord(metadata):=LongWord(metadata) + 4;
    FI := metadata.stream_info;
    FLACIn.FSR := FI.sample_rate;
    FLACIn.FChan := FI.channels;
    //if FLACIn.FChan > 2 then FLACIn.FValid:=False;
    FLACIn.FBPS := FI.bits_per_sample;
    FLACIn.FTotalSamples := FI.total_samples;
    FLACIn.FSize := FLACIn.FTotalSamples * (FLACIn.FBPS div 8) * FLACIn.FChan;
    FLACIn.MinFrameSize := FI.min_framesize;
    if FLACIn.FSR <> 0 then
      FLACIn.FTotalTime := FLACIn.FTotalSamples / FLACIn.FSR;
  end;
  if metadata._type = FLAC__METADATA_TYPE_VORBIS_COMMENT then
  begin
    SL := TStringList.Create();
    try
      Entry := metadata.vorbis_comment.comments;
      for i := 0 to metadata.vorbis_comment.num_comments - 1 do
      begin
        SetLength(S, Entry.length);
        Move(Entry.entry^, S[1], Length(S));
        SL.Add(string(S));
        Inc(PtrUInt(Entry), SizeOf(FLAC__StreamMetadata_VorbisComment_Entry));
      end;

      FLACIn.FComments.Title := Utf8ToWideStr(SL.Values[AnsiUpperCase(_vorbis_Title)]);
      FLACIn.FComments.Artist := Utf8ToWideStr(SL.Values[AnsiUpperCase(_vorbis_Artist)]);
      FLACIn.FComments.Album := Utf8ToWideStr(SL.Values[AnsiUpperCase(_vorbis_Album)]);
      FLACIn.FComments.Date := Utf8ToWideStr(SL.Values[AnsiUpperCase(_vorbis_Date)]);
      FLACIn.FComments.Genre := Utf8ToWideStr(SL.Values[AnsiUpperCase(_vorbis_Genre)]);
      FLACIn.FComments.Track := Utf8ToWideStr(SL.Values[AnsiUpperCase(_vorbis_Track)]);
      FLACIn.FComments.Disc := Utf8ToWideStr(SL.Values[AnsiUpperCase(_vorbis_Disc)]);
      FLACIn.FComments.Reference := Utf8ToWideStr(SL.Values[AnsiUpperCase(_vorbis_Reference)]);
      FLACIn.FComments.TrackGain := Utf8ToWideStr(SL.Values[AnsiUpperCase(_vorbis_TrackGain)]);
      FLACIn.FComments.TrackPeak := Utf8ToWideStr(SL.Values[AnsiUpperCase(_vorbis_TrackPeak)]);
      FLACIn.FComments.AlbumGain := Utf8ToWideStr(SL.Values[AnsiUpperCase(_vorbis_AlbumGain)]);
      FLACIn.FComments.AlbumPeak := Utf8ToWideStr(SL.Values[AnsiUpperCase(_vorbis_AlbumPeak)]);
      FLACIn.FComments.Artist := Utf8ToWideStr(SL.Values[AnsiUpperCase(_vorbis_Title)]);
    finally
      SL.Free;
    end;
  end;
  FLacIn.EndOfMetadata := metadata.is_last;
end;

procedure DecErrorCBProc(decoder: P_FLAC__StreamDecoder;
  status: Integer;
  client_data: Pointer); cdecl;
var
  FLACIn: TLibFLACIn;
begin
  FLACIn := TLibFLACIn(client_data);
  FLACIn.FValid := False;
end;

{ TLibFLACOut }

constructor TLibFLACOut.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FVerify := False;
  FBlockSize := 4608;
  FBestModelSearch := False;
  FEnableMidSideStereo := True;
  FCompressionLevel := -1;
  FTags := TVorbisTags.Create;
end;

destructor TLibFLACOut.Destroy();
begin
  FTags.Free;
  inherited Destroy;
end;

procedure TLibFLACOut.Init();
begin
  if not LibFLACLoaded then
    raise EAcsException.Create(LibFLACPath + ' library could not be loaded.');
  if not Assigned(FStream) then
  begin
    if FFileName = '' then
      raise EAcsException.Create('File name is not assigned.');
    if (not FileExists(FFileName)) or (FFileMode = foRewrite) then
      FStream := TFileStream.Create(FFileName, fmCreate, FAccessMask)
    else
      FStream := TFileStream.Create(FFileName, fmOpenReadWrite, FAccessMask);
  end;
  EndOfInput := False;
  BolckInserted := False;
  _encoder := FLAC__stream_encoder_new();
  if not Assigned(_encoder) then
    raise EAcsException.Create('Failed to initialize FLAC encoder.');
  FInput.Init();
  FLAC__stream_encoder_set_verify(_encoder, FVerify);
  FLAC__stream_encoder_set_channels(_encoder, FInput.Channels);
  FLAC__stream_encoder_set_bits_per_sample(_encoder, FInput.BitsPerSample);
  FLAC__stream_encoder_set_sample_rate(_encoder, FInput.SampleRate);
  if FInput.Channels = 2 then
  begin
    FLAC__stream_encoder_set_do_mid_side_stereo(_encoder, FEnableMidSideStereo);
    FLAC__stream_encoder_set_loose_mid_side_stereo(_encoder, FEnableLooseMidSideStereo);
  end;
  FLAC__stream_encoder_set_blocksize(_encoder, FBlockSize);
  if FCompressionLevel >= 0 then
    FLAC__stream_encoder_set_compression_level(_encoder, FCompressionLevel)
  else
  begin
    FLAC__stream_encoder_set_max_lpc_order(_encoder, FMaxLPCOrder);
    if FQLPCoeffPrecision + FInput.BitsPerSample > 31 then
      FQLPCoeffPrecision := 31 - FInput.BitsPerSample;
    FLAC__stream_encoder_set_qlp_coeff_precision(_encoder, FQLPCoeffPrecision);
    FLAC__stream_encoder_set_do_qlp_coeff_prec_search(_encoder,
      FQLPCoeffPrecisionSearch);
    FLAC__stream_encoder_set_min_residual_partition_order(_encoder,
      FMinResidualPartitionOrder);
    FLAC__stream_encoder_set_max_residual_partition_order(_encoder,
      FMaxResidualPartitionOrder);
    FLAC__stream_encoder_set_do_exhaustive_model_search(_encoder, FBestModelSearch);
  end;
  //if FInput.Size > 0 then
  //  FLAC__stream_encoder_set_total_samples_estimate(_encoder, Round(FInput.Size/(FInput.BitsPerSample shr 3)/FInput.Channels));
  //FLAC__stream_encoder_set_seek_callback(_encoder, EncSeekCBFunc);
  //FLAC__stream_encoder_set_write_callback(_encoder, EncWriteCBFunc);
  //FLAC__seekable_stream_encoder_set_client_data(_encoder, Self);
  if FLAC__stream_encoder_init_stream(_encoder, EncWriteCBFunc,
    EncSeekCBFunc, EncTellCBFunc, EncMetadataCBFunc, Self) <>
    FLAC__STREAM_ENCODER_OK then
  begin
    FInput.Done();
    raise EAcsException.Create('Failed to initialize FLAC encoder.');
  end;
  FBufSize := FBlockSize * (FInput.BitsPerSample shr 3) * FInput.Channels;
  GetMem(Buffer, FBufSize);
end;

procedure TLibFLACOut.Done();
begin
  if Assigned(FStream) then
    FLAC__stream_encoder_finish(_encoder);
  FLAC__stream_encoder_delete(_encoder);
  if Buffer <> nil then
    FreeMem(Buffer);
  Buffer := nil;
  FreeAndNil(FStream);
  FInput.Done();
end;

function TLibFLACOut.DoOutput(Abort: Boolean): Boolean;
var
  Len, i, samples: LongWord;
  FB: PFLACBuf;
  FBU: PFLACUBuf;
  B16: PAcsBuffer16;
  B32: PAcsBuffer32;
begin
  Result := True;
  if not CanOutput then Exit;
  if Abort or EndOfInput then
  begin
    Result := False;
    Exit;
  end;
  //Len:=FInput.FillBuffer(Buffer, FBufSize, EndOfInput);
  Len := Self.FillBufferFromInput(EndOfInput);
  (*while Len < FBufSize do
  begin
    l:=Finput.CopyData(@Buffer[Len], FBufSize-Len);
    Inc(Len, l);
    if l = 0 then
    begin
      EndOfInput:=True;
      Break;
    end;
  end; *)
  if Len = 0 then
  begin
    Result := False;
    Exit;
  end;
  samples := (Len shl 3) div Finput.BitsPerSample;
  GetMem(FB, samples * SizeOF(FLAC__int32));
  if FInput.BitsPerSample = 16 then
  begin
    B16 := @Buffer[0];
    for i := 0 to samples - 1 do FB[i] := B16[i];
  end
  else
  if FInput.BitsPerSample = 8 then
  begin
    for i := 0 to samples - 1 do FB[i] := Buffer[i];
  end
  else
  if FInput.BitsPerSample = 24 then
  begin
    FBU := PFLACUBuf(FB);
    for i := 0 to samples - 1 do
      FBU[i] := (shortint(Buffer[i * 3 + 2]) shl 16) + (Buffer[i * 3 + 1] shl 8) + (Buffer[i * 3]);
  end
  else
  if FInput.BitsPerSample = 32 then
  begin
    B32 := @Buffer[0];
    for i := 0 to samples - 1 do FB[i] := B32[i];
  end;
  if not FLAC__stream_encoder_process_interleaved(_encoder, @FB[0],
    samples div FInput.Channels) then
    raise EAcsException.Create('Failed to encode data.');
  FreeMem(FB);
end;

procedure TLibFLACOut.SetEnableLooseMidSideStereo(val: Boolean);
begin
  if Val then FEnableMidSideStereo := True;
  FEnableLooseMidSideStereo := Val;
end;

procedure TLibFLACOut.SetBestModelSearch(val: Boolean);
begin
  if Val then
  begin
    FEnableMidSideStereo := True;
    FEnableLooseMidSideStereo := False;
  end;
  FBestModelSearch := Val;
end;

{ TLibFLACIn }

constructor TLibFLACIn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FComments := TVorbisTags.Create();
  FBufferSize := $8000;
  BytesPerBlock := 0;
end;

destructor TLibFLACIn.Destroy();
begin
  CloseFile();
  FreeAndNil(FComments);
  inherited Destroy;
end;

procedure TLibFLACIn.OpenFile();
begin
  if FOpened then Exit;
  if not LibFLACLoaded then
    raise EAcsException.Create(LibFLACPath + ' library could not be loaded.');
  inherited OpenFile();
  if FOpened then
  begin
    Residue := 0;
    FValid := False;
    _decoder := FLAC__stream_decoder_new();
    if not Assigned(_decoder) then
      raise EAcsException.Create('Failed to initialize the FLAC decoder.');
    if not FLAC__stream_decoder_set_md5_checking(_decoder,
      longbool(FCheckMD5Signature)) then
      raise EAcsException.Create(
        'Internal error 113, please report to NewAC developers');
    FLAC__stream_decoder_set_metadata_respond_all(_decoder);
    if FLAC__stream_decoder_init_stream(_decoder, DecReadCBFunc,
      DecSeekCBFunc, DecTellCBFunc,
      DecLengthCBFunc, DecEOFCBFunc, DecWriteCBFunc,
      DecMetadataCBProc, DecErrorCBProc, Self) <>
      FLAC__STREAM_DECODER_INIT_STATUS_OK then
      raise EAcsException.Create('Failed to set up the FLAC decoder.');
    FValid := True;
    //BuffSize:=0;
    //Buff:=nil;
    // read metadata
    FComments.Clear();
    EndOfMetadata := False;
    while (not EndOfMetadata) and (FValid) do
    begin
      if not FLAC__stream_decoder_process_single(_decoder) then
      begin
        FValid := False;
        Break;
      end;
    end;
    if FValid then
    begin
      //_CommonTags.Clear();
      //_CommonTags.Artist:=FComments.Artist;
      //_CommonTags.Album:=FComments.Album;
      //_CommonTags.Title:=FComments.Title;
      ////{$WARNINGS OFF}
      //_CommonTags.Year:=FComments.Date;
      //_CommonTags.Track:=FComments.Track;
      ////{$WARNINGS ON}
      //_CommonTags.Genre:=FComments.Genre;
    end;
  end;
end;

procedure TLibFLACIn.CloseFile();
begin
  if FOpened then
  begin
    if Assigned(_decoder) then
    begin
      FSignatureValid := FLAC__stream_decoder_finish(_decoder);
      FLAC__stream_decoder_delete(_decoder);
      _decoder := nil;
    end;
    //if Buff <> nil then FreeMem(Buff);
    //Buff:=nil;
  end;
  inherited CloseFile();
end;

function TLibFLACIn.GetData(ABuffer: Pointer; ABufferSize: Integer): Integer;
var
  dec_state: Integer;
begin
  Result := 0;
  if not Active then
    raise EAcsException.Create(strStreamnotopen);

  if FAudioBuffer.UnreadSize = 0 then
  begin
    BufStart := 0;
    //BufEnd:=0;
    FAudioBuffer.Reset();
    //while FAudioBuffer.WritePosition < (FAudioBuffer.Size-Self.BytesPerBlock) do
    begin
      if not FLAC__stream_decoder_process_single(_decoder) then
      begin
        dec_state := FLAC__stream_decoder_get_state(_decoder);
        if (dec_state = FLAC__STREAM_DECODER_END_OF_STREAM) or (not FValid) then
        else
          raise EAcsException.Create('Error reading FLAC file');
      end;
    end;
  end;
  FSampleSize := FChan * (FBPS div 8);
  if Residue <> 0 then
  begin
    BufStart := (Residue - 1) * FSampleSize;
    if BufStart >= Self.BytesPerBlock then
      raise EAcsException.Create('Seek failed');
    Residue := 0;
    Inc(FPosition, BufStart - FSampleSize);
  end;

  Result := ABufferSize - (ABufferSize mod FSampleSize);
  if Result > FAudioBuffer.UnreadSize then Result := FAudioBuffer.UnreadSize;
  FAudioBuffer.Read(ABuffer^, Result);
  Inc(BufStart, Result);
  Inc(FPosition, Result);
end;

function TLibFLACIn.Seek(SampleNum: Integer): Boolean;
var
  Aligned: Int64;
begin
  FCheckMD5Signature := False;
  if FBlockSize <> 0 then
  begin
    Residue := SampleNum mod FBlockSize;
    Aligned := SampleNum - Residue;
  end
  else
    Aligned := SampleNum;
  Result := FLAC__stream_decoder_seek_absolute(_decoder, Aligned);
  if not Result then FLAC__stream_decoder_reset(_decoder);
  SampleNum := Aligned;
  BufStart := 0;
  BufEnd := 0;
end;

procedure TLibFLACOut.SetCompressionLevel(val: Integer);
begin
  if Val > 8 then
    FCompressionLevel := 8
  else
    FCompressionLevel := Val;
end;

procedure TLibFLACOut.SetTags(AValue: TVorbisTags);
begin
  FTags.Assign(AValue);
end;


procedure GetBlockInfo(FS: TStream; var BI: TBlockInfo);
var
  Header: array [0..3] of byte;
  t: byte;
begin
  FS.Read(Header, 4);
  BI.HasNext := (Header[0] shr 7) = 0;
  BI.BlockType := Header[0] mod 128;
  BI.BlockLength := 0;
  t := Header[1];
  Header[1] := Header[3];
  Header[3] := t;
  Move(Header[1], BI.BlockLength, 3);
end;

function TLibFLACIn.GetComments(): TVorbisTags;
begin
  OpenFile();
  Result := FComments;
end;
{$endif ACS_FLAC_EXT}

{$ifdef ACS_FLAC}
{ TFLACIn }

procedure TFLACIn.OpenFile;
var
  marker: array[0..3] of AnsiChar;
  metaDataBlockHeader: TFlacMetaDataBlockHeader;
  //frameHeader: TFlacFrameHeader;
begin
  inherited OpenFile;
  FFlacStatus := FLAC_STATUS_SUCCESS;
  FSamplesRead := 0;

  if not Assigned(FBitReader) then
    FBitReader := TBitReader.Create(FStream);

  { Check FLAC marker }
  FStream.ReadBuffer(marker, 4);
  if marker <> FLAC_MARKER then
  begin
    FFlacStatus := FLAC_STATUS_INVALID_MARKER;
    Exit;
  end;

  { Read Meta Data Blocks}
  repeat
    FLACReadMetaDataBlockHeader(FStream, metaDataBlockHeader);
    //if LOG_FLAC then LogMetaDataBlockHeader(metaDataBlockHeader);

    case metaDataBlockHeader.blockType of
      FLAC_BLOCK_TYPE_STREAMINFO:
      begin
        FLACReadStreamInfoBlock(FStream, FStreamInfoBlock);
        //if LOG_FLAC then LogStreamInfoBlock(FStreamInfoBlock);
        // fill info
        FSR := FStreamInfoBlock.sampleRate;
        FBPS := FStreamInfoBlock.bitsPerSample;
        FChan := FStreamInfoBlock.numberOfChannels;
        FSampleSize := FStreamInfoBlock.bytesPerSample;
        FTotalSamples := FStreamInfoBlock.totalSamplesInStream;
        if FSR <> 0 then
          FTotalTime := FTotalSamples / FSR;
        FSize := Integer(FStream.Size);
        FValid := True;
      end;
      FLAC_BLOCK_TYPE_PADDING,
      FLAC_BLOCK_TYPE_APPLICATION,
      FLAC_BLOCK_TYPE_SEEKTABLE,
      FLAC_BLOCK_TYPE_VORBIS_COMMENT,
      FLAC_BLOCK_TYPE_CUESHEET,
      FLAC_BLOCK_TYPE_PICTURE:
      begin
        FStream.Seek(metaDataBlockHeader.length, soCurrent);
      end;
    end;
  until metaDataBlockHeader.lastMetaDataBlock;

  { Allocate channel buffers }
  SetLength(FChannelBuffers, FStreamInfoBlock.numberOfChannels,
    FStreamInfoBlock.maxBlockSize);
  BufferSize := FStreamInfoBlock.numberOfChannels * FStreamInfoBlock.maxBlockSize *
    FStreamInfoBlock.bytesPerSample;

  // skip samples if needed
  // todo: use SEEKTABLE block
  {while FSamplesRead < FSeekSampleNum do
  begin
    FLACReadFrameHeader(FStream, frameHeader);
    FStream.Seek(frameHeader.blockSize, soCurrent);
  end; }
end;

procedure TFLACIn.CloseFile;
begin
  { De-Allocate channel buffers }
  SetLength(FChannelBuffers, 0);

  FreeAndNil(FBitReader);
  inherited CloseFile;
end;

constructor TFLACIn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TFLACIn.Destroy;
begin
  FreeAndNil(FBitReader);
  inherited Destroy;
end;

function TFLACIn.GetData(ABuffer: Pointer; ABufferSize: Integer): Integer;
var
  samplesLeft: Int64;
  frameHeader: TFlacFrameHeader;
  subframeHeader: TFlacSubframeHeader;
  channel: Integer;
  i, nBPS, sampleIndex: Integer;
  midSample, sideSample: LongInt;
  BufSize, OutBufFreeSize: Integer;
begin
  //Result:=inherited GetData(ABuffer, ABufferSize);
  Result := 0;
  BufSize := Min(ABufferSize, BufferSize);
  if BufSize = 0 then
    Exit;

  OutBufFreeSize := ABufferSize;
  // feed rest of previously decoded data to output
  BufSize := Min(BufEnd - BufStart, OutBufFreeSize);
  if BufSize > 0 then
  begin
    Move(FBuffer[BufStart], ABuffer^, BufSize);
    Inc(ABuffer, BufSize); // buf pointer to end of data
    Inc(Result, BufSize);
    Dec(OutBufFreeSize, BufSize);
    Inc(BufStart, BufSize);
    Inc(FPosition, BufSize);
  end;

  if not Assigned(FBitReader) then
    FBitReader := TBitReader.Create(FStream);
  { Read Frames }
  samplesLeft := FStreamInfoBlock.totalSamplesInStream - FSamplesRead;
  while (OutBufFreeSize > 0) and (samplesLeft > 0) do
  begin
    FLACReadFrameHeader(FStream, frameHeader);
    if LOG_FLAC then LogFrameHeader(frameHeader);

    if frameHeader.sampleSizeInBits = 0 then
      frameHeader.sampleSizeInBits := FStreamInfoBlock.bitsPerSample;

    //FStreamInfoBlock.bytesPerSample := Ceil(frameHeader.sampleSizeInBits / 8);
    FStreamInfoBlock.bytesPerSample :=
      (frameHeader.sampleSizeInBits div 8) + Ord((frameHeader.sampleSizeInBits mod 8) > 0);

    FBitReader.Reset;

    { Read subframes, one per channel }
    for channel := 0 to FStreamInfoBlock.numberOfChannels - 1 do
    begin
      if LOG_FLAC then WriteLn('Reading subframe for channel: ' + IntToStr(channel));
      FLACReadSubframeHeader(FBitReader, subframeHeader);

      { Side channels have an extra bit per sample }
      subframeHeader.sampleSizeInBits := frameHeader.sampleSizeInBits;
      case frameHeader.channelAssignment of
        FLAC_CHANNEL_LEFT_SIDE: if channel = 1 then
            Inc(subframeHeader.sampleSizeInBits);
        FLAC_CHANNEL_SIDE_RIGHT: if channel = 0 then
            Inc(subframeHeader.sampleSizeInBits);
        FLAC_CHANNEL_MID_SIDE: if channel = 1 then
            Inc(subframeHeader.sampleSizeInBits);
      end;

      if LOG_FLAC then LogSubframeHeader(subframeHeader);

      case subframeHeader.subframeType of
        FLAC_SUBFRAME_TYPE_CONSTANT: FLACReadSubframeConstant(frameHeader,
            subframeHeader, FBitReader, @(FChannelBuffers[channel]));
        FLAC_SUBFRAME_TYPE_VERBATIM: FLACReadSubframeVerbatim(frameHeader,
            subframeHeader, FBitReader, @(FChannelBuffers[channel]));
        FLAC_SUBFRAME_TYPE_FIXED: FLACReadSubframeFixed(frameHeader,
            subframeHeader, FBitReader, @(FChannelBuffers[channel]));
        FLAC_SUBFRAME_TYPE_LPC: FLACReadSubframeLPC(frameHeader,
            subframeHeader, FBitReader, @(FChannelBuffers[channel]));
        else
        begin
          Result := FLAC_STATUS_INVALID_FORMAT;
          WriteLn('Error: Unsupported subframetype: ' +
            IntToStr(subframeHeader.subframeType));
          Exit;
        end;
      end;

      if subframeHeader.wastedBitsPerSample > 0 then
        for sampleIndex := 0 to frameHeader.blockSize do
          FChannelBuffers[channel][sampleIndex] :=
            FChannelBuffers[channel][sampleIndex] shl subframeHeader.wastedBitsPerSample;
    end;

    frameHeader.crc16Footer := FStream.ReadWord(); { Frame Footer CRC-16 }

    { Channel decorrelation }
    case frameHeader.channelAssignment of
      FLAC_CHANNEL_LEFT_SIDE:
        for sampleIndex := 0 to frameHeader.blockSize-1 do
          FChannelBuffers[1][sampleIndex] :=
            FChannelBuffers[0][sampleIndex] - FChannelBuffers[1][sampleIndex];

      FLAC_CHANNEL_SIDE_RIGHT:
        for sampleIndex := 0 to frameHeader.blockSize-1 do
          Inc(FChannelBuffers[0][sampleIndex], FChannelBuffers[1][sampleIndex]);

      FLAC_CHANNEL_MID_SIDE:
      begin
        for sampleIndex := 0 to frameHeader.blockSize-1 do
        begin
          sideSample := FChannelBuffers[1][sampleIndex];
          midSample := (FChannelBuffers[0][sampleIndex] shl 1) or (sideSample and 1);

          FChannelBuffers[0][sampleIndex] := LongInt(QWord(midSample + sideSample) shr 1);
          FChannelBuffers[1][sampleIndex] := LongInt(QWord(midSample - sideSample) shr 1);
        end;
      end;
    end;

    { Convert to PCM }
    BufSize := frameHeader.blockSize * FStreamInfoBlock.numberOfChannels *
      FStreamInfoBlock.bytesPerSample;
    BufStart := 0;
    BufEnd := 0;

    nBPS := FStreamInfoBlock.bytesPerSample;
    for sampleIndex := 0 to frameHeader.blockSize - 1 do
    begin
      for channel := 0 to FStreamInfoBlock.numberOfChannels - 1 do
      begin
        for i := 0 to nBPS - 1 do
        begin
          {$ifdef ENDIAN_BIG}
          FBuffer[BufEnd] := ((QWord(FChannelBuffers[channel][sampleIndex]) shr ((nBPS - i - 1) * 8)) and $FF);
          {$endif}
          {$ifdef ENDIAN_LITTLE}
          FBuffer[BufEnd] := ((QWord(FChannelBuffers[channel][sampleIndex]) shr (i * 8)) and $FF);
          {$endif}
          Inc(BufEnd);
        end;
      end;
    end;

    // copy PCM data to ABuffer
    BufSize := Min(BufSize, OutBufFreeSize);
    if BufSize > 0 then
    begin
      Move(FBuffer[BufStart], ABuffer^, BufSize);
      Inc(ABuffer, BufSize);
      Inc(Result, BufSize);
      Dec(OutBufFreeSize, BufSize);
      Inc(BufStart, BufSize);
      Inc(FPosition, BufSize);
    end;

    Inc(FSamplesRead, frameHeader.blockSize);
    Dec(samplesLeft, frameHeader.blockSize);
  end;
end;

function TFLACIn.Seek(SampleNum: Integer): Boolean;
begin
  Result := False;
  if not Seekable then Exit;
  // clear buffered data
  BufStart := 0;
  BufEnd := 0;
  FPosition := 0;
  if not Assigned(FStream) then Exit;
  FStream.Position := 0;
  FSeekSampleNum := SampleNum;
  OpenFile();
  Result := True;
end;

{$endif ACS_FLAC}


initialization

{$ifdef ACS_FLAC}
  FileFormats.Add('flac', 'Free Lossless Audio Codec', TFLACIn);
{$endif}
{$ifdef ACS_FLAC_EXT}
  if LoadFlacLibrary() then
  begin
    FileFormats.Add('flac', 'Free Lossless Audio Codec (lib)', TLibFLACIn);
    FileFormats.Add('flac', 'Free Lossless Audio Codec (lib)', TLibFLACOut);
  end;
{$endif}

finalization

{$ifdef ACS_FLAC_EXT}
  UnloadFlacLibrary();
{$endif}

end.