unit FLAC;

{
  Author: Jolon Faichney 2021
  License: Public Domain

  See the FLAC specification: https://xiph.org/flac/format.html

  The specification doesn't include some important details for the decoder.
  These are detailed here: https://github.com/wader/flac.tcl
}

{$mode delphi}

interface

uses
  Classes,
  SysUtils,
  Math;

const
  FLAC_MARKER: AnsiString = 'fLaC';

  FLAC_LAST_METADATA_BLOCK_FLAG  = 1 shl 7;

  FLAC_BLOCK_TYPE_MASK           = $7F;
  FLAC_BLOCK_TYPE_STREAMINFO     = 0;
  FLAC_BLOCK_TYPE_PADDING        = 1;
  FLAC_BLOCK_TYPE_APPLICATION    = 2;
  FLAC_BLOCK_TYPE_SEEKTABLE      = 3;
  FLAC_BLOCK_TYPE_VORBIS_COMMENT = 4;
  FLAC_BLOCK_TYPE_CUESHEET       = 5;
  FLAC_BLOCK_TYPE_PICTURE        = 6;

  FLAC_BLOCK_TYPE_STRINGS: array[0..6] of AnsiString = (
    'FLAC_BLOCK_TYPE_STREAMINFO    ',
    'FLAC_BLOCK_TYPE_PADDING       ',
    'FLAC_BLOCK_TYPE_APPLICATION   ',
    'FLAC_BLOCK_TYPE_SEEKTABLE     ',
    'FLAC_BLOCK_TYPE_VORBIS_COMMENT',
    'FLAC_BLOCK_TYPE_CUESHEET      ',
    'FLAC_BLOCK_TYPE_PICTURE       '
  );

  FLAC_BLOCKING_STRATEGY_FIXED    = 0;
  FLAC_BLOCKING_STRATEGY_VARIABLE = 1;

  FLAC_CHANNEL_LEFT_SIDE  = 8;
  FLAC_CHANNEL_SIDE_RIGHT = 9;
  FLAC_CHANNEL_MID_SIDE   = 10;

  FLAC_SUBFRAME_TYPE_CONSTANT = 0;
  FLAC_SUBFRAME_TYPE_VERBATIM = 1;
  FLAC_SUBFRAME_TYPE_FIXED    = 8;
  FLAC_SUBFRAME_TYPE_LPC      = 32;

  FLAC_FIXED_COEFFICIENTS: array[0..4] of array[0..3] of SmallInt = (
    (0, 0, 0, 0),  { 0 }
    (1, 0, 0, 0),  { x[n-1] }
    (2, -1, 0, 0), { 2x[n-1] - x[n-2] }
    (3, -3, 1, 0), { 3x[n-1] - 3x[n-2] + x[n-3] }
    (4, -6, 4, -1) { 4x[n-1] - 6x[n-2] + 4x[n-3] - x[n-4] }
  );

  FLAC_RESIDUAL_CODING_METHOD_PARTITIONED_RICE  = 0;
  FLAC_RESIDUAL_CODING_METHOD_PARTITIONED_RICE2 = 1;

  FLAC_STATUS_SUCCESS = 0;
  FLAC_STATUS_INVALID_MARKER = 1;
  FLAC_STATUS_INVALID_FORMAT = 2;

type

  PFlacMetaDataBlockHeader = ^TFlacMetaDataBlockHeader;
  TFlacMetaDataBlockHeader = record
    lastMetaDataBlock: Boolean; { Last-metadata-block flag: '1' if this block is the last metadata block before the audio blocks, '0' otherwise. }
    blockType: Byte;            { FLAC_LAST_METADATA_BLOCK_FLAG? | FLAC_BLOCK_TYPE_* }
    length: DWord;              { 24 bits - length of meta data not including this header }
  end;

  PFlacStreamInfoBlock = ^TFlacStreamInfoBlock;
  TFlacStreamInfoBlock = record
    minBlockSize:         Word;  { 16 bits - The minimum block size (in samples) used in the stream. }
    maxBlockSize:         Word;  { 16 bits - The maximum block size (in samples) used in the stream. (Minimum blocksize == maximum blocksize) implies a fixed-blocksize stream. }
    minFrameSize:         DWord; { 24 bits - The minimum frame size (in bytes) used in the stream. May be 0 to imply the value is not known. }
    maxFrameSize:         DWord; { 24 bits - The maximum frame size (in bytes) used in the stream. May be 0 to imply the value is not known. }
    sampleRate:           DWord; { 20 bits - Sample rate in Hz. Though 20 bits are available, the maximum sample rate is limited by the structure of frame headers to 655350Hz. Also, a value of 0 is invalid. }
    numberOfChannels:     Word;  { 3 bits -	(number of channels)-1. FLAC supports from 1 to 8 channels }
    bitsPerSample:        Word;  { 5 bits - (bits per sample)-1. FLAC supports from 4 to 32 bits per sample. Currently the reference encoder and decoders only support up to 24 bits per sample. }
    totalSamplesInStream: Int64; { 36 bits - Total samples in stream. 'Samples' means inter-channel sample, i.e. one second of 44.1Khz audio will have 44100 samples regardless of the number of channels. A value of zero here means the number of total samples is unknown. }
    md5Signature:         array[0..15] of Byte;  { 128 bits - MD5 signature of the unencoded audio data. This allows the decoder to determine if an error exists in the audio data even when the error does not result in an invalid bitstream. }
    bytesPerSample:       Integer; { This is not in the STREAMINFO block, instead it is computed for our outputed PCM (it will be bitsPerSample rounded up to next whole byte, e.g. 20 bits will be 3 bytes) }
  end;

  TFlacApplicationBlock = record
    appId: array[0..3] of Char;          { Registered application ID. (Visit the registration page to register an ID with FLAC.) }
    { Followed by application data }
  end;

  TFlacPaddingBlock = record
    { <n> 0 bytes }
  end;

  TFlacSeekTableBlock = record
    { One or more TSeekPoints }
  end;

  TFlacSeekPoint = record
    sampleNumber: Int64;         { Sample number of first sample in the target frame, or 0xFFFFFFFFFFFFFFFF for a placeholder point. }
    offset: Int64;               { Offset (in bytes) from the first byte of the first frame header to the first byte of the target frame's header. }
    numberOfSamples: Word;       { Number of samples in the target frame. }
  end;

  TFlacVorbisCommentBlock = record
    { See https://www.xiph.org/vorbis/doc/v-comment.html }
  end;

  TFlacCueSheetBlock = record
    mediaCatalogNumber: array[0..127] of Char; { Media catalog number, in ASCII printable characters 0x20-0x7e. In general, the media catalog number may be 0 to 128 bytes long; any unused characters should be right-padded with NUL characters. For CD-DA, this is a thirteen digit number, followed by 115 NUL bytes. }
    numberOfLeadInSamples: Int64;              { The number of lead-in samples. This field has meaning only for CD-DA cuesheets; for other uses it should be 0. For CD-DA, the lead-in is the TRACK 00 area where the table of contents is stored; more precisely, it is the number of samples from the first sample of the media to the first sample of the first index point of the first track. According to the Red Book, the lead-in must be silence and CD grabbing software does not usually store it; additionally, the lead-in must be at least two seconds but may be longer. For these reasons the lead-in length is stored here so that the absolute position of the first track can be computed. Note that the lead-in stored here is the number of samples up to the first index point of the first track, not necessarily to INDEX 01 of the first track; even the first track may have INDEX 00 data. }
    compactDisc: 0.. $1;                       { 1 if the CUESHEET corresponds to a Compact Disc, else 0. }
    reserved0:   0..$3F;                       { Reserved }
    reserved1:   array[0..258] of Char;        { Reserved }
    numberOfTracks: Byte;                      { The number of tracks. Must be at least 1 (because of the requisite lead-out track). For CD-DA, this number must be no more than 100 (99 regular tracks and one lead-out track). }
    { Followed by multiple CUESHEET_TRACKs }
  end;

  TFlacPictureBlock = record
    pictureType: LongWord;   { The picture type according to the ID3v2 APIC frame }
    mimeTypeLength: LongWord; { The length of the MIME type string in bytes. }
    { Remainder of block follows }
  end;

  PFlacFrameHeader = ^TFlacFrameHeader;
  TFlacFrameHeader = record
    syncCode:         Integer; { 14 bits - Sync code '11111111111110' }
    blockingStrategy: Integer; { 1 bit - Blocking strategy: 0 : fixed-blocksize stream; frame header encodes the frame number; 1 : variable-blocksize stream; frame header encodes the sample number }
    blockSize:        DWord;   { Block size in inter-channel samples }
    sampleRate:       DWord; 
    channelAssignment: Word;   { 4 bits - Channel assignment:
                                        0000-0111 : (number of independent channels)-1. Where defined, the channel order follows SMPTE/ITU-R recommendations. The assignments are as follows:
                                          1 channel: mono
                                          2 channels: left, right
                                          3 channels: left, right, center
                                          4 channels: front left, front right, back left, back right
                                          5 channels: front left, front right, front center, back/surround left, back/surround right
                                          6 channels: front left, front right, front center, LFE, back/surround left, back/surround right
                                          7 channels: front left, front right, front center, LFE, back center, side left, side right
                                          8 channels: front left, front right, front center, LFE, back left, back right, side left, side right
                                        1000 : left/side stereo: channel 0 is the left channel, channel 1 is the side(difference) channel
                                        1001 : right/side stereo: channel 0 is the side(difference) channel, channel 1 is the right channel
                                        1010 : mid/side stereo: channel 0 is the mid(average) channel, channel 1 is the side(difference) channel
                                        1011-1111 : reserved }
    sampleSizeInBits: Word;
    number: Int64;             { Sample number if variable block number, otherwise frame number }
    crc8: Byte;
    crc16Footer: Word;
  end;

  PFlacSubframeHeader = ^TFlacSubframeHeader;
  TFlacSubframeHeader = record
    subframeType: Word;        { 6 bits - Subframe type }
    order: Word;
    wastedBitsPerSample: Word; { 1 bit - 'Wasted bits-per-sample' flag:
                                  0 : no wasted bits-per-sample in source subblock, k=0
                                  1 : k wasted bits-per-sample in source subblock, k-1 follows, unary coded; e.g. k=3 => 001 follows, k=7 => 0000001 follows. }
    sampleSizeInBits: Word;    { Side channels are one bit wider than specified in the frame header so set a custom value here if a side channel }
  end;

  TFlacFrameFooter = packed record
    crc16: Word; { CRC-16 (polynomial = x^16 + x^15 + x^2 + x^0, initialized with 0) of everything before the crc, back to and including the frame header sync code }
  end;

  PFlacSampleBuffer = ^TFlacSampleBuffer;  { We need this to pass the 2nd dimension of channelBuffers by reference }
  TFlacSampleBuffer = array of LongInt;

  TBitReader = class
    stream: TStream;        { Source stream }
    buffer: Byte;           { We read a byte in and keep it }
    bitsRemaining: Integer; { How many unread bits remaining in the buffer byte }

    constructor Create(inStream: TStream);
    function ReadBits(bitCount: Integer): Int64;
    function ReadSignedBits(bitCount: Integer): Int64;
    procedure Reset; { Sets bitsRemaining to zero - will force a new byte to be read from stream on next read }
  end;

var
  LOG_FLAC: Boolean;

{ Convert entire FLAC file to PCM }
function FLAC2PCM(flac: TStream; pcm: TStream; var streamInfoBlock: TFlacStreamInfoBlock): LongWord;

{ Read Headers }
function FLACReadMetaDataBlockHeader(flac: TStream; out metaDataBlockHeader: TFlacMetaDataBlockHeader): DWord;
function FLACReadStreamInfoBlock(flac: TStream; out streamInfoBlock: TFlacStreamInfoBlock): DWord;
function FLACReadFrameHeader(flac: TStream; out frameHeader: TFlacFrameHeader): DWord;
function FLACReadSubframeHeader(bitReader: TBitReader; out subframeHeader: TFlacSubframeHeader): DWord;

{ Read Subframe }
function FLACReadSubframeConstant(const frameHeader: TFlacFrameHeader; const subframeHeader: TFlacSubframeHeader; bitReader: TBitReader; sampleHandle: PFlacSampleBuffer): DWord;
function FLACReadSubframeVerbatim(const frameHeader: TFlacFrameHeader; const subframeHeader: TFlacSubframeHeader; bitReader: TBitReader; sampleHandle: PFlacSampleBuffer): DWord;
function FLACReadSubframeFixed(const frameHeader: TFlacFrameHeader; const subframeHeader: TFlacSubframeHeader; bitReader: TBitReader; sampleHandle: PFlacSampleBuffer): DWord;
function FLACReadSubframeLPC(const frameHeader: TFlacFrameHeader; const subframeHeader: TFlacSubframeHeader; bitReader: TBitReader; sampleHandle: PFlacSampleBuffer): DWord;
function FLACReadLPCResidual(const frameHeader: TFlacFrameHeader; const subframeHeader: TFlacSubframeHeader; bitReader: TBitReader; sampleBuffer: TFlacSampleBuffer; sampleIndex: Integer; coefficients: array of SmallInt; qlpShift: Integer): DWord;
function FLACReadRiceCode(bitReader: TBitReader; riceParameter: Integer): Int64;

{ Byte Array Utilities }
function FLACReadBytesAsInt64(flac: TStream; byteCount: Integer): Int64;
procedure FLACWriteLongIntAsBytes(output: TStream; value: Int32; byteCount: Integer);
function FLACReadUTF8Integer(flac: TStream): Int64;

{ Logging functions }
procedure LogMetaDataBlockHeader(const metaDataBlockHeader: TFlacMetaDataBlockHeader);
procedure LogStreamInfoBlock(const streamInfoBlock: TFlacStreamInfoBlock);
procedure LogFrameHeader(const frameHeader: TFlacFrameHeader);
procedure LogSubframeHeader(const subframeHeader: TFlacSubframeHeader);

implementation

function FLAC2PCM(flac: TStream; pcm: TStream; var streamInfoBlock: TFlacStreamInfoBlock): LongWord;
{
  Converts a stream containing a FLAC file into a raw PCM stream.
  flac: TStream containing a complete FLAC file.
  pcm: PCM will be output to this stream. The PCM format is channel interleaved. 
        Bytes per sample is rounded up to the next whole byte, e.g. a 20 bit FLAC
        file will produce a 24 bit PCM stream.
  streamInfoBlock: Contains information from the stream info block from the FLAC file
        which can be useful for the caller to interpret the PCM stream, e.g. number
        of channels, original bits per sample.
  returns: FLAC_STATUS_SUCCESS if successful.
}
var
  marker: array[0..3] of Char;
  metaDataBlockHeader: TFlacMetaDataBlockHeader;
  frameHeader: TFlacFrameHeader;
  subframeHeader: TFlacSubframeHeader;
  channel: Integer;
  bitReader: TBitReader;
  channelBuffers: array of TFlacSampleBuffer;
  sampleIndex: Integer;
  midSample, sideSample: LongInt;
  samplesRead: Int64;
begin
  FLAC2PCM := FLAC_STATUS_SUCCESS;

  bitReader := TBitReader.Create(flac);

  { Check FLAC marker }
  flac.ReadBuffer(marker, 4);
  if marker <> FLAC_MARKER then 
  begin 
    FLAC2PCM := FLAC_STATUS_INVALID_MARKER;
    Exit;
  end;

  { Read Meta Data Blocks}
  repeat
    FLACReadMetaDataBlockHeader(flac, metaDataBlockHeader);
    if LOG_FLAC then LogMetaDataBlockHeader(metaDataBlockHeader);

    case metaDataBlockHeader.blockType of
      FLAC_BLOCK_TYPE_STREAMINFO    :
      begin
        FLACReadStreamInfoBlock(flac, streamInfoBlock);
        if LOG_FLAC then LogStreamInfoBlock(streamInfoBlock);
      end;
      FLAC_BLOCK_TYPE_PADDING       ,
      FLAC_BLOCK_TYPE_APPLICATION   ,
      FLAC_BLOCK_TYPE_SEEKTABLE     ,
      FLAC_BLOCK_TYPE_VORBIS_COMMENT,
      FLAC_BLOCK_TYPE_CUESHEET      ,
      FLAC_BLOCK_TYPE_PICTURE       :
      begin
        flac.Seek(metaDataBlockHeader.length, soCurrent);
      end;
    end;
  until metaDataBlockHeader.lastMetaDataBlock;
  
  { Allocate channel buffers }
  SetLength(channelBuffers, streamInfoBlock.numberOfChannels, streamInfoBlock.maxBlockSize);

  { Read Frames }
  samplesRead := 0;
  repeat
    FLACReadFrameHeader(flac, frameHeader);
    if LOG_FLAC then LogFrameHeader(frameHeader);

    if frameHeader.sampleSizeInBits = 0 then frameHeader.sampleSizeInBits := streamInfoBlock.bitsPerSample;

    //streamInfoBlock.bytesPerSample := Ceil(frameHeader.sampleSizeInBits / 8);
    streamInfoBlock.bytesPerSample := (frameHeader.sampleSizeInBits div 8) + Ord((frameHeader.sampleSizeInBits mod 8) > 0);

    bitReader.Reset;

    { Read subframes, one per channel }
    for channel := 0 to streamInfoBlock.numberOfChannels - 1 do
    begin
      if LOG_FLAC then WriteLn('Reading subframe for channel: ' + IntToStr(channel));
      FLACReadSubframeHeader(bitReader, subframeHeader);

      { Side channels have an extra bit per sample }
      subframeHeader.sampleSizeInBits := frameHeader.sampleSizeInBits;
      case frameHeader.channelAssignment of
        FLAC_CHANNEL_LEFT_SIDE:   if channel = 1 then Inc(subframeHeader.sampleSizeInBits);
        FLAC_CHANNEL_SIDE_RIGHT:  if channel = 0 then Inc(subframeHeader.sampleSizeInBits);
        FLAC_CHANNEL_MID_SIDE:    if channel = 1 then Inc(subframeHeader.sampleSizeInBits);
      end;

      if LOG_FLAC then LogSubframeHeader(subframeHeader);

      case subframeHeader.subframeType of
        FLAC_SUBFRAME_TYPE_CONSTANT:  FLACReadSubframeConstant(frameHeader, subframeHeader, bitReader, @(channelBuffers[channel]));
        FLAC_SUBFRAME_TYPE_VERBATIM:  FLACReadSubframeVerbatim(frameHeader, subframeHeader, bitReader, @(channelBuffers[channel]));
        FLAC_SUBFRAME_TYPE_FIXED:     FLACReadSubframeFixed(frameHeader, subframeHeader, bitReader, @(channelBuffers[channel]));
        FLAC_SUBFRAME_TYPE_LPC:       FLACReadSubframeLPC(frameHeader, subframeHeader, bitReader, @(channelBuffers[channel]));
        else
        begin
          Result := FLAC_STATUS_INVALID_FORMAT;
          WriteLn('Error: Unsupported subframetype: ' + IntToStr(subframeHeader.subframeType));
          Exit;
        end;
      end;

      if subframeHeader.wastedBitsPerSample > 0 then
        for sampleIndex := 0 to frameHeader.blockSize do
          channelBuffers[channel][sampleIndex] := channelBuffers[channel][sampleIndex] shl subframeHeader.wastedBitsPerSample;
    end;
  
    frameHeader.crc16Footer := flac.ReadWord(); { Frame Footer CRC-16 }

    { Channel decorrelation }
    case frameHeader.channelAssignment of
      FLAC_CHANNEL_LEFT_SIDE:   
        for sampleIndex := 0 to frameHeader.blockSize do
          channelBuffers[1][sampleIndex] := channelBuffers[0][sampleIndex] - channelBuffers[1][sampleIndex];

      FLAC_CHANNEL_SIDE_RIGHT:  
        for sampleIndex := 0 to frameHeader.blockSize do
          Inc(channelBuffers[0][sampleIndex], channelBuffers[1][sampleIndex]);

      FLAC_CHANNEL_MID_SIDE:    
      begin
        for sampleIndex := 0 to frameHeader.blockSize do
        begin
          sideSample := channelBuffers[1][sampleIndex];
          midSample := (channelBuffers[0][sampleIndex] shl 1) or (sideSample and 1);

          channelBuffers[0][sampleIndex] := QWord(midSample + sideSample) shr 1;
          channelBuffers[1][sampleIndex] := QWord(midSample - sideSample) shr 1;
        end;
      end;
    end;

    { Convert to PCM }
    for sampleIndex := 0 to frameHeader.blockSize - 1 do
      for channel := 0 to streamInfoBlock.numberOfChannels - 1 do
        FLACWriteLongIntAsBytes(pcm, channelBuffers[channel][sampleIndex], streamInfoBlock.bytesPerSample);

    Inc(samplesRead, frameHeader.blockSize);
  until samplesRead >= streamInfoBlock.totalSamplesInStream;

  bitReader.Free;
end;

function FLACReadMetaDataBlockHeader(flac: TStream; out metaDataBlockHeader: TFlacMetaDataBlockHeader): DWord;
var
  buffer: Byte;
begin
  buffer := flac.ReadByte();
  metaDataBlockHeader.lastMetaDataBlock := (buffer and FLAC_LAST_METADATA_BLOCK_FLAG) <> 0;
  metaDataBlockHeader.blockType := buffer and FLAC_BLOCK_TYPE_MASK;
  metaDataBlockHeader.length := FLACReadBytesAsInt64(flac, 3);
  Result := FLAC_STATUS_SUCCESS;
end;

function FLACReadStreamInfoBlock(flac: TStream; out streamInfoBlock: TFlacStreamInfoBlock): DWord;
var
  buffer: Int64;
begin
  streamInfoBlock.minBlockSize := FLACReadBytesAsInt64(flac, 2);
  streamInfoBlock.maxBlockSize := FLACReadBytesAsInt64(flac, 2);
  streamInfoBlock.minFrameSize := FLACReadBytesAsInt64(flac, 3);
  streamInfoBlock.maxFrameSize := FLACReadBytesAsInt64(flac, 3);

  buffer := FLACReadBytesAsInt64(flac, 8);
 
  streamInfoBlock.sampleRate := (buffer shr 44) and $FFFFF;
  streamInfoBlock.numberOfChannels := ((buffer shr 41) and $7) + 1;
  streamInfoBlock.bitsPerSample := ((buffer shr 36) and $1F) + 1;
  streamInfoBlock.totalSamplesInStream := buffer and $3FFFFFFFF;
  streamInfoBlock.bytesPerSample := (streamInfoBlock.bitsPerSample div 8) + Ord((streamInfoBlock.bitsPerSample mod 8) > 0);

  flac.Read(streamInfoBlock.md5Signature, SizeOf(TFlacStreamInfoBlock.md5Signature));

  Result := FLAC_STATUS_SUCCESS;
end;

function FLACReadFrameHeader(flac: TStream; out frameHeader: TFlacFrameHeader): DWord;
var
  buffer: Byte;
  blockSize: Integer;
  sampleRate: Integer;
  sampleSizeInBits: Integer;
begin
  frameHeader.syncCode := FLACReadBytesAsInt64(flac, 2);
  frameHeader.blockingStrategy := frameHeader.syncCode and 1;
  buffer := flac.ReadByte();
  blockSize := (buffer shr 4) and $F;
  sampleRate := buffer and $F;
  buffer := flac.ReadByte();
  frameHeader.channelAssignment := (buffer shr 4) and $F;
  sampleSizeInBits := (buffer shr 1) and $7;

  frameHeader.number := FLACReadUTF8Integer(flac);

  case blockSize of 
    0:     WriteLn('Error block size 0');
    1:     frameHeader.blockSize := 192;
    2..5:  frameHeader.blockSize := 576 * (1 shl (blockSize - 2));
    6:     frameHeader.blockSize := flac.ReadByte() + 1;
    7:     frameHeader.blockSize := FLACReadBytesAsInt64(flac, 2) + 1;
    8..15: frameHeader.blockSize := 256 * (1 shl (blockSize - 8));
  end;

  case sampleRate of
    0:  frameHeader.sampleRate := 0; { Get from STREAMINFO block }
    1:  frameHeader.sampleRate := 88200;
    2:  frameHeader.sampleRate := 176400;
    3:  frameHeader.sampleRate := 192000;
    4:  frameHeader.sampleRate := 8000;
    5:  frameHeader.sampleRate := 16000;
    6:  frameHeader.sampleRate := 22050;
    7:  frameHeader.sampleRate := 24000;
    8:  frameHeader.sampleRate := 32000;
    9:  frameHeader.sampleRate := 44100;
    10: frameHeader.sampleRate := 48000;
    11: frameHeader.sampleRate := 96000;
    12: frameHeader.sampleRate := flac.ReadByte();
    13: frameHeader.sampleRate := FLACReadBytesAsInt64(flac, 2);
    14: frameHeader.sampleRate := 10 * FLACReadBytesAsInt64(flac, 2);
    else
    begin
      Result := FLAC_STATUS_INVALID_FORMAT;
      WriteLn('Error: Invalid sampleRate = ' + IntToStr(sampleRate));
      Exit;
    end;
  end;

  case sampleSizeInBits of
    0: frameHeader.sampleSizeInBits := 0; { Get from STREAMINFO block }
    1: frameHeader.sampleSizeInBits := 8;
    2: frameHeader.sampleSizeInBits := 12;
    4: frameHeader.sampleSizeInBits := 16;
    5: frameHeader.sampleSizeInBits := 20;
    6: frameHeader.sampleSizeInBits := 24;
    else
    begin
      Result := FLAC_STATUS_INVALID_FORMAT;
      WriteLn('Error: Unkown sampleSizeInBits = ' + IntToStr(sampleSizeInBits));
      Exit;
    end;
  end;

  frameHeader.crc8 := flac.ReadByte();

  Result := FLAC_STATUS_SUCCESS;
end;

function FLACReadSubframeHeader(bitReader: TBitReader; out subframeHeader: TFlacSubframeHeader): DWord;
var
  padding: Byte;
  wastedBit: Byte;
begin
  padding := bitReader.ReadBits(1);
  if padding <> 0 then 
  begin
    Result := FLAC_STATUS_INVALID_FORMAT;
    WriteLn('Error: Invalid padding in subframe header');
    Exit;
  end;
  subframeHeader.subframeType := bitReader.ReadBits(6);

  { Check if SUBFRAME_FIXED}
  if (subframeHeader.subframeType and 8) = 8 then
  begin
    subframeHeader.order := subframeHeader.subframeType and 7;
    subframeHeader.subframeType := FLAC_SUBFRAME_TYPE_FIXED;
  end
  { Check if SUBFRAME_LPC }
  else if (subframeHeader.subframeType and 32) = 32 then
  begin
    subframeHeader.order := (subframeHeader.subframeType and 31) + 1;
    subframeHeader.subframeType := FLAC_SUBFRAME_TYPE_LPC;
  end;

  { Check for wasted bits per sample }
  wastedBit := bitReader.ReadBits(1);
  subframeHeader.wastedBitsPerSample := 0;
  if wastedBit <> 0 then 
  begin
    repeat
      Inc(subframeHeader.wastedBitsPerSample);
      wastedBit := bitReader.ReadBits(1);
    until wastedBit = 1;
    Dec(subframeHeader.sampleSizeInBits, subframeHeader.wastedBitsPerSample);
  end;

  Result := FLAC_STATUS_SUCCESS;
end;

function FLACReadSubframeConstant(const frameHeader: TFlacFrameHeader;
                              const subframeHeader: TFlacSubframeHeader;
                              bitReader: TBitReader;
                              sampleHandle: PFlacSampleBuffer): DWord;
var
  sample: Integer;
  sampleIndex: Integer;
  sampleBuffer: TFlacSampleBuffer;
begin
  sampleBuffer := sampleHandle^;

  { Reads in a single sample and repeats it for the entire subframe }
  sample := bitReader.ReadBits(subframeHeader.sampleSizeInBits);

  for sampleIndex := 0 to frameHeader.blockSize - 1 do
  begin
    sampleBuffer[sampleIndex] := sample;
  end;

  Result := FLAC_STATUS_SUCCESS;
end;

function FLACReadSubframeVerbatim(const frameHeader: TFlacFrameHeader;
  const subframeHeader: TFlacSubframeHeader;
  bitReader: TBitReader;
  sampleHandle: PFlacSampleBuffer): DWord;
var
  sampleIndex: Integer;
  sampleBuffer: TFlacSampleBuffer;
begin
  sampleBuffer := sampleHandle^;

  { Samples are read in uncompressed and written verbatim }
  for sampleIndex := 0 to frameHeader.blockSize - 1 do
  begin
    sampleBuffer[sampleIndex] := bitReader.ReadBits(subframeHeader.sampleSizeInBits);
  end;

  Result := FLAC_STATUS_SUCCESS;
end;

function FLACReadSubframeFixed(const frameHeader: TFlacFrameHeader;
  const subframeHeader: TFlacSubframeHeader;
  bitReader: TBitReader;
  sampleHandle: PFlacSampleBuffer): DWord;
var
  sampleIndex: Integer;
  sampleBuffer: TFlacSampleBuffer;
begin
  sampleBuffer := sampleHandle^;
  sampleIndex := 0;

  { Read n warm up samples, where n is LPC order }
  while sampleIndex < subframeHeader.order do
  begin
    sampleBuffer[sampleIndex] := bitReader.ReadSignedBits(subframeHeader.sampleSizeInBits);
    //WriteLn('Warm up sample ' + IntToStr(sampleIndex) + ': ' + IntToStr(sampleBuffer[sampleIndex]));
    Inc(sampleIndex);
  end;

  Result := FLACReadLPCResidual(frameHeader, subframeHeader, bitReader, sampleBuffer, sampleIndex, FLAC_FIXED_COEFFICIENTS[subframeHeader.order], 0);
end;

function FLACReadSubframeLPC(const frameHeader: TFlacFrameHeader;
  const subframeHeader: TFlacSubframeHeader;
  bitReader: TBitReader;
  sampleHandle: PFlacSampleBuffer): DWord;
var
  i: Integer;
  qlpPrecision: Integer;
  qlpShift: ShortInt;
  coefficients: array[0..32] of SmallInt;
  sampleIndex: Integer;
  sampleBuffer: TFlacSampleBuffer;
begin
  sampleBuffer := sampleHandle^;
  sampleIndex := 0;

  { Read n warm up samples, where n is LPC order }
  while sampleIndex < subframeHeader.order do
  begin
    sampleBuffer[sampleIndex] := bitReader.ReadSignedBits(subframeHeader.sampleSizeInBits);
    if LOG_FLAC then WriteLn('    Warm up sample ' + IntToStr(sampleIndex) + ': ' + IntToStr(sampleBuffer[sampleIndex]));
    Inc(sampleIndex);
  end;

  qlpPrecision := bitReader.ReadBits(4) + 1;
  if qlpPrecision >= 16 then WriteLn('Error: qlpPrecision too large: ' + IntToStr(qlpPrecision));

  qlpShift := bitReader.ReadSignedBits(5);

  if LOG_FLAC then
  begin
    WriteLn('  qlpPrecision: ' + IntToStr(qlpPrecision));
    WriteLn('  qlpShift: ' + IntToStr(qlpShift));
  end;

  { Read unencoded predictor coefficients }
  for i := 0 to subframeHeader.order - 1 do
  begin
    coefficients[i] := bitReader.ReadSignedBits(qlpPrecision);
    if LOG_FLAC then WriteLn('   coefficients[' + IntToStr(i) + ']: ' + IntToStr(coefficients[i]));
  end;
  
  Result := FLACReadLPCResidual(frameHeader, subframeHeader, bitReader, sampleBuffer, sampleIndex, coefficients, qlpShift);
end;

function FLACReadLPCResidual(const frameHeader: TFlacFrameHeader;
  const subframeHeader: TFlacSubframeHeader;
  bitReader: TBitReader;
  sampleBuffer: TFlacSampleBuffer;
  sampleIndex: Integer;
  coefficients: array of SmallInt;
  qlpShift: Integer): DWord;
var
  sample: LongInt;
  residual: Integer;
  partitionOrder: Integer;
  riceParameterBits: Integer;
  riceParameter: Integer;
  unencoded: Boolean;
  unencodedBitsPerSample: Integer;
  numberOfSamples: Integer;
  numberOfPartitions: Integer;
  partitionIndex: Integer;
  c: Integer;
  i, n: Integer;
begin

  residual := bitReader.ReadBits(2);
  partitionOrder := bitReader.ReadBits(4);
  numberOfPartitions := 1 shl partitionOrder;
  riceParameterBits := 4;

  if residual = FLAC_RESIDUAL_CODING_METHOD_PARTITIONED_RICE2 then
  begin
    riceParameterBits := 5;
  end;

  if LOG_FLAC then
  begin
    WriteLn('  residual: ' + IntToStr(residual));
    WriteLn('  partitionOrder: ' + IntToStr(partitionOrder));
    WriteLn('  numberOfPartitions: ' + IntToStr(numberOfPartitions));
    WriteLn('  Rice parameter bits: ' + IntToStr(riceParameterBits));
  end;

  for partitionIndex := 0 to numberOfPartitions - 1 do
  begin

    if      partitionOrder = 0 then numberOfSamples := frameHeader.blockSize - subframeHeader.order
    else if partitionIndex > 0 then numberOfSamples := frameHeader.blockSize div numberOfPartitions
    else                            numberOfSamples := (frameHeader.blockSize div numberOfPartitions) - subframeHeader.order;

    riceParameter := bitReader.ReadBits(riceParameterBits);

    if LOG_FLAC then
    begin
      WriteLn('   partitionIndex: ' + IntToStr(partitionIndex));
      WriteLn('   numberOfSamples: ' + IntToStr(numberOfSamples));
      WriteLn('   riceParameter: ' + IntToStr(riceParameter));
    end;

    unencoded := False;
    unencodedBitsPerSample := 0;
    if riceParameter = ((1 shl riceParameterBits) - 1) then
    begin
      unencoded := True;
      unencodedBitsPerSample := bitReader.ReadBits(5);
      if LOG_FLAC then WriteLn('   unencodedBitsPerSample: ' + IntToStr(unencodedBitsPerSample));
    end;

    for i := 1 to numberOfSamples do
    begin
      if unencoded then
        sample := bitReader.ReadSignedBits(unencodedBitsPerSample)
      else
        sample := FLACReadRiceCode(bitReader, riceParameter);

      sampleBuffer[sampleIndex] := sample;
      Inc(sampleIndex);
    end;
  end;

  { Convert residuals to original values in-place using the LPC coefficients }
  for sampleIndex := subframeHeader.order to frameHeader.blockSize - 1 do
  begin
    sample := 0;

    for c := 0 to subframeHeader.order - 1 do
    begin
      Inc(sample, coefficients[c] * sampleBuffer[sampleIndex - c - 1]);
    end;

    Inc(sampleBuffer[sampleIndex], Integer(QWord(sample) shr qlpShift));
  end;

  Result := FLAC_STATUS_SUCCESS;
end;

function FLACReadRiceCode(bitReader: TBitReader; riceParameter: Integer): Int64;
var
  leadingZeros: Integer;
begin
  { Count leading zeros }
  leadingZeros := 0;

  while bitReader.ReadBits(1) = 0 do
    Inc(leadingZeros);
  
  Result := (leadingZeros shl riceParameter) + bitReader.ReadBits(riceParameter);

  { ZigZag decode: https://github.com/mewkiz/flac/blob/69bef32f4513a3409058e7f9008e787e7c52118b/internal/bits/zigzag.go#L17
      0 =>  0
      1 => -1
      2 =>  1
      3 => -2
      4 =>  2
      5 => -3
      6 =>  3
  }
  { If Result is even then simply halve and the result is positive }
  if (Result and 1) = 0 then 
    Result := Result shr 1
  else
    Result := -((Result shr 1) + 1); { For odd: halve, then add one, then negate}
end;

function FLACReadBytesAsInt64(flac: TStream; byteCount: Integer): Int64;
var
  buffer: array[0..7] of Byte;
  i: Integer;
  number: Int64;
begin
  flac.Read(buffer, byteCount);

  number := 0;
  for i := 0 to byteCount - 1 do
  begin
    number := number or (Int64(buffer[i]) shl ((byteCount - i - 1) * 8));
  end;
  Result := number;
end;

procedure FLACWriteLongIntAsBytes(output: TStream; value: LongInt; byteCount: Integer);
var
  i: Integer;
begin
  for i := 0 to byteCount - 1 do
    output.WriteByte((QWord(value) shr ((byteCount - i - 1) * 8)) and $FF);
end;

function FLACReadUTF8Integer(flac: TStream): Int64;
var
  number: Int64;
  buffer: Int64;
  trailingByteCount: Int64;
  selectBit: Int64;
  bufferArray: array[0..6] of Byte;
  i: integer;
  firstByteShift: Int64;
  firstByteMask: Int64;
begin
  buffer := flac.ReadByte();

  if (buffer and (1 shl 7)) = 0 then
  begin
    number := buffer and $7F;
    Result := number;
    Exit;
  end;

  trailingByteCount := 0;
  selectBit := 1 shl 6;
  
  while (buffer and selectBit) <> 0 do
  begin
    selectBit := selectBit shr 1;
    Inc(trailingByteCount);
  end;

  flac.Read(bufferArray, trailingByteCount);

  Result := 0;
  for i := 0 to trailingByteCount - 1 do
  begin
    Result := Result or (Int64(bufferArray[i] and $3F) shl ((trailingByteCount - i - 1) * 6));
  end;

  firstByteShift := 6 - trailingByteCount;
  firstByteMask := (1 shl firstByteShift) - 1;

  Result := Result or ((buffer and firstByteMask) shl (trailingByteCount * 6));

end;

procedure LogMetaDataBlockHeader(const metaDataBlockHeader: TFlacMetaDataBlockHeader);
begin
  WriteLn('MetaDataBlockHeader: ' + FLAC_BLOCK_TYPE_STRINGS[metaDataBlockHeader.blockType]);
  WriteLn(' lastMetaDataBlock: ' + BoolToStr(metaDataBlockHeader.lastMetaDataBlock, 'True', 'False'));
  WriteLn(' blockType: ' + IntToStr(metaDataBlockHeader.blockType));
  WriteLn(' length: ' + IntToStr(metaDataBlockHeader.length));
end;

procedure LogStreamInfoBlock(const streamInfoBlock: TFlacStreamInfoBlock);
var
  md5: Integer;
begin
  WriteLn('StreamInfoBlock: ');
  WriteLn(' minBlockSize: ' + IntToStr(streamInfoBlock.minBlockSize));
  WriteLn(' maxBlockSize: ' + IntToStr(streamInfoBlock.maxBlockSize));
  WriteLn(' minFrameSize: ' + IntToStr(streamInfoBlock.minFrameSize));
  WriteLn(' maxFrameSize: ' + IntToStr(streamInfoBlock.maxFrameSize));
  WriteLn(' sampleRate: ' + IntToStr(streamInfoBlock.sampleRate));
  WriteLn(' numberOfChannels: ' + IntToStr(streamInfoBlock.numberOfChannels));
  WriteLn(' bitsPerSample: ' + IntToStr(streamInfoBlock.bitsPerSample));
  WriteLn(' totalSamplesInStream: ' + IntToStr(streamInfoBlock.totalSamplesInStream));
  Write(' md5: ');
  for md5 in streamInfoBlock.md5Signature do Write(' ' + IntToHex(md5, 2));
  WriteLn('');
end;

procedure LogFrameHeader(const frameHeader: TFlacFrameHeader);
begin
  WriteLn('FrameHeader:');
  WriteLn(' syncCode: ' + IntToHex(frameHeader.syncCode, 4));
  WriteLn(' blockingStrategy: ' + IntToStr(frameHeader.blockingStrategy));
  WriteLn(' blockSize: ' + IntToStr(frameHeader.blockSize));
  WriteLn(' sampleRate: ' + IntToStr(frameHeader.sampleRate));
  WriteLn(' channelAssignment: ' + IntToStr(frameHeader.channelAssignment));
  WriteLn(' sampleSizeInBits: ' + IntToStr(frameHeader.sampleSizeInBits));
  WriteLn(' number: ' + IntToStr(frameHeader.number));
  WriteLn(' crc8: ' + IntToHex(frameHeader.crc8, 2));
end;

procedure LogSubframeHeader(const subframeHeader: TFlacSubframeHeader);
begin
  WriteLn('SubframeHeader: ');
  WriteLn(' subframeType: ' + IntToStr(subframeHeader.subframeType));
  WriteLn(' order: ' + IntToStr(subframeHeader.order));
  WriteLn(' wastedBitsPerSample: ' + IntToStr(subframeHeader.wastedBitsPerSample));
end;

constructor TBitReader.Create(inStream: TStream);
begin
  stream := inStream;
end;

function TBitReader.ReadBits(bitCount: Integer): Int64;
var
  bitsAvailable: Integer;
begin
  Result := 0;
  while bitCount > 0 do
  begin
    if bitsRemaining = 0 then
    begin
      buffer := stream.ReadByte();
      bitsRemaining := 8;
    end;
    bitsAvailable := Min(bitsRemaining, bitCount);
    Result := Result shl bitsAvailable;
    Result := Result or ((buffer shr (bitsRemaining - bitsAvailable)) and ((1 shl bitsAvailable) - 1));
    Dec(bitsRemaining, bitsAvailable);
    Dec(bitCount, bitsAvailable);
  end;
end;

{ Preserves the sign bit and 2's complement encoding }
function TBitReader.ReadSignedBits(bitCount: Integer): Int64;
var
  twos: Int64;
begin
  Result := ReadBits(bitCount);
  { If signed bit (top bit) is set then fill bits above it with 1s }
  if (Result and (1 shl (bitCount - 1))) <> 0 then
  begin
    twos := (((Int64(1) shl (64 - bitCount)) - 1) shl bitCount);
    Result := Result or twos;
  end
end;

procedure TBitReader.Reset;
begin
  bitsRemaining := 0;
end;

initialization
  LOG_FLAC := False;
end.