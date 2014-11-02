(*
  the original version of this file is written by thomas la cour,
  http://www.top-house.dk/~nr161/delphi/

  This file is a part of Audio Components Suite v 2.4
  Copyright (c) 2002, 2003 Andrei Borovsky. All rights reserved.
  See the LICENSE file for more details.
  You can contact me at acs@compiler4.net
*)

{
$Log: acs_mac.pas,v $
Revision 1.7  2006/07/04 17:12:45  z0m3ie
ACS 2.4 alt wiederhergestellt (unterschiedliche Sampleformate ...)

Revision 1.3  2005/12/30 11:10:57  z0m3ie
some corrections to lazarus-linux depending things

Revision 1.2  2005/12/26 17:31:39  z0m3ie
fixed some problems in acs_dsfiles
fixed some problems in acs_vorbis
reworked all buffers

Revision 1.1  2005/12/19 18:36:38  z0m3ie
*** empty log message ***

Revision 1.6  2005/12/18 17:01:54  z0m3ie
delphi compatibility

Revision 1.5  2005/12/04 16:54:34  z0m3ie
All classes are renamed, Style TACS... than T... to avoid conflicts with other components (eg TMixer is TACSMixer now)

Revision 1.4  2005/11/29 18:32:51  z0m3ie
bugfixes for win32 version

Revision 1.3  2005/11/28 21:57:24  z0m3ie
mostly FileOut fixes
moved PBuffer to PBuffer8
set all to dynamically Buffering

Revision 1.2  2005/09/13 04:04:50  z0m3ie
First release without Components for Fileformats
only TFileIn and TFileOut are Visible

Revision 1.1  2005/09/12 22:04:52  z0m3ie
modified structure again, fileformats are now in an sperat folder.
all File In/Out classes are capsulated from TFileIn and TFileOut

Revision 1.1  2005/08/25 20:18:00  z0m3ie
Version 2.4 restructure
TCDPlayer removed (fits not in component structure)
TMP3ToWavConverter removed (fits not in component structure)

}

unit acs_mac;

{$ifdef linux}{$message error 'unit not supported'}{$endif linux}

interface

uses
  ACS_File,Classes, SysUtils, Windows, ACS_Classes, MACDll;

type

  // Note by A.B.: It seems that APE compressor supports file output only.

  TMACOut = class(TACSCustomFileOut)
  private
    APECompress: TAPECompress;
    WaveFormatEx: TWaveFormatEx;
    EndOfStream: Boolean;
    FCompressionLevel: Integer;
    FMaxAudioBytes: Integer;
    procedure SetCompressionLevel(Value: Integer);
  protected
    procedure Done; override;
    function DoOutput(Abort: Boolean): Boolean; override;
    procedure Prepare; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property CompressionLevel: LongInt read FCompressionLevel write SetCompressionLevel stored True;
    property MaxAudioBytes: Integer read FMaxAudioBytes write FMaxAudioBytes;
  end;

  (* Note by A.B.: Due to the reasons described above this component
     ignores streamed input *)

  TMACIn = class(TACSCustomFileIn)
  private
    APEDecompress: TAPEDecompress;
    EndOfStream: Boolean;
    function GetAverageBitrate: Integer;
    function GetCurrentBitrate: Integer;
    function GetCurrentBlock: Integer;
    function GetCurrentMS: Integer;
    function GetLengthMS: Integer;
    function GetTotalBlocks: Integer;
  protected
    function GetBPS: Integer; override;
    function GetCh: Integer; override;
    function GetSR: Integer; override;
    function GetTotalTime: real; override;
    procedure OpenFile; override;
    procedure CloseFile; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetData(Buffer: Pointer; BufferSize: Integer): Integer; override;

    function Seek(Sample : Integer) : Boolean; override;

    procedure Flush; override;
    procedure Init; override;

    property AverageBitrate: Integer read GetAverageBitrate;
    property CurrentBitrate: Integer read GetCurrentBitrate;
    property CurrentBlock: Integer read GetCurrentBlock;
    property CurrentMS: Integer read GetCurrentMS;
    property LengthMS: Integer read GetLengthMS;
    property TotalBlocks: Integer read GetTotalBlocks;
  end;

implementation

constructor TMACOut.Create;
begin
  inherited Create(AOwner);
  FBufferSize := $10000;
  FCompressionLevel := COMPRESSION_LEVEL_NORMAL;
  FMaxAudioBytes := MAX_AUDIO_BYTES_UNKNOWN;
  if not (csDesigning in ComponentState) then
  begin
    if not MACLoaded then
      raise EACSException.Create(MACPath + ' library could not be loaded.');
  end;
end;

destructor TMACOut.Destroy;
begin
  if Assigned(APECompress) then
    APECompress.Free;
  inherited Destroy;
end;

procedure TMACOut.Prepare;
var
  r: Integer;
begin
  GetMem(FBuffer,FBufferSize);
  if FFileName = '' then raise EACSException.Create('File name is not assigned.');
  FInput.Init;
  EndOfStream := False;

  APECompress := TAPECompress.Create;

  macFillWaveFormatEx(WaveFormatEx, FInput.SampleRate, FInput.BitsPerSample, FInput.Channels);

  r := APECompress.Start(
    PChar(FFileName),
    @WaveFormatEx,
    FMaxAudioBytes,
    FCompressionLevel,
    nil,
    CREATE_WAV_HEADER_ON_DECOMPRESSION);

  CanOutput := (r = 0);

  if r <> 0 then
  raise EACSException.Create('Error starting APECompress.' + #13#10 +
      macErrorExplanation(r));
end;

procedure TMACOut.Done;
begin
  APECompress.Finish(nil, 0, 0);
  APECompress.Free;
  APECompress := nil;
  FInput.Flush;
  FreeMem(FBuffer);
end;

function TMACOut.DoOutput(Abort: Boolean): Boolean;
var
  Len, i, x, z: Integer;
  pBuffer: PByteArray;
  nAudioBytesLeft, nBufferBytesAvailable, nNoiseBytes, nRetVal: Integer;
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
  Len := Finput.GetData(@FBuffer[0], FBufferSize);
  x := 0;
  if Len <> 0 then
  begin
    nAudioBytesLeft := Len;
    while (nAudioBytesLeft > 0) do
    begin
      nBufferBytesAvailable := 0;
      pBuffer := APECompress.LockBuffer(nBufferBytesAvailable);

      nNoiseBytes := nBufferBytesAvailable;
      if nNoiseBytes > nAudioBytesLeft then
        nNoiseBytes := nAudioBytesLeft;

      //whats this ? schoult System.Move not be faster ?
      for z := 0 to nNoiseBytes - 1 do
      begin
        pBuffer[z] := FBuffer[x];
        inc(x);
      end;

      nRetVal := APECompress.UnlockBuffer(nNoiseBytes, TRUE);
      if (nRetVal <> 0) then
        raise EACSException.Create('APECompress.UnlockBuffer Error: ' + inttostr(nRetVal));

      dec(nAudioBytesLeft, nNoiseBytes);
    end
  end
  else
    EndOfStream := True;
end;


constructor TMACIn.Create;
begin
  inherited Create(AOwner);
  BufferSize := $2000;
  if not (csDesigning in ComponentState) then
  begin
    if not MACLoaded then
      raise EACSException.Create(MACPath + ' library could not be loaded.');
  end;
end;

destructor TMACIn.Destroy;
begin
  if Assigned(APEDecompress) then
    APEDecompress.Free;
  inherited Destroy;
end;

procedure TMACIn.OpenFile;
begin
  FValid := True;
  if FOpened = 0 then
  begin
    EndOfStream := False;

    APEDecompress := TAPEDecompress.Create(FileName);
    if APEDecompress.Handle <> 0 then
    begin
      FSize := APEDecompress.InfoWavTotalBytes;
      FSR := APEDecompress.InfoSampleRate;
      FBPS := APEDecompress.InfoBitsPerSample;
      FChan := APEDecompress.InfoChannels;
      FTime := APEDecompress.InfoLengthMS div 1000; // Round(ov_time_total(VFile, 0));
      FTotalSamples := (FSize div (FBPS shr 3)) div FChan;
    end
    else
    begin
      FValid := False;
      FOpened := -1;
    end;
  end;
  Inc(FOpened);
end;

procedure TMACIn.CloseFile;
begin
  if FOpened = 1 then
  begin
    if Assigned(APEDecompress) then
      APEDecompress.Free;
    APEDecompress := nil;
  end;
  if FOpened > 0 then Dec(FOpened);
end;

function TMACIn.GetData(Buffer: Pointer; BufferSize: Integer): Integer;
var
  l, csize, offs: Integer;
  blocks: Integer;
  tmp: Double;
begin
  if not Busy then raise EACSException.Create('The Stream is not opened');
  if BufStart > BufEnd then
  begin
    if FOffset <> 0 then
    begin
      offs := Round((FOffset / 100) * FSize);
      FPosition := FPosition + offs;
      if FPosition < 0 then FPosition := 0
      else if FPosition > FSize then FPosition := FSize;
      APEDecompress.Seek(FPosition shr 2);
      FOffset := 0;
    end;
    BufStart := 1;
    BufEnd := 0;
    if not EndOfStream then
    begin
      while BufEnd < BufferSize do
      begin
        //l := ov_read(VFile, @buf[BufEnd + 1], BUF_SIZE - BufEnd, 0, 2, 1, @cursec);
        blocks := (BufferSize - BufEnd) div 4;
        APEDecompress.GetData(@FBuffer[BufEnd], blocks, l);
        l := l * 4;
        if l <= 0 then
        begin
          EndOfStream := True;
          Break;
        end;
        Inc(BufEnd, l);
        if (FEndSample <> -1) then
        begin
          csize := (FEndSample-FStartSample)*(FBPS shr 3)*FChan;
          if (csize - FPosition) <= 0 then
          begin
            EndOfStream := True;
            Break;
          end;
          if (csize - FPosition) < BufEnd then
          begin
            BufEnd := csize - FPosition;
            Break;
          end;
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
        //l := ov_read(VFile, @buf[BufEnd + 1], BUF_SIZE - BufEnd, 0, 2, 1, @cursec);
        blocks := (BufferSize - BufEnd) div 4;
        APEDecompress.GetData(@FBuffer[BufEnd], blocks, l);
        l := l * 4;
        if l <= 0 then
        begin
          EndOfStream := True;
          Break;
        end;
        Inc(BufEnd, l);
      end;
    end;
  end;
  if BufferSize < (BufEnd - BufStart + 1) then
    Result := BufferSize
  else
    Result := BufEnd - BufStart + 1;
  Move(FBuffer[BufStart - 1], Buffer^, Result);
  Inc(BufStart, Result);
  Inc(FPosition, Result);
end;

function TMACIn.GetTotalTime: real;
begin
  OpenFile;
  if Assigned(APEDecompress) then
    Result := APEDecompress.LengthMS / 1000;
  CloseFile;  
end;

function TMACIn.GetAverageBitrate: Integer;
begin
  if Assigned(APEDecompress) then
    Result := APEDecompress.AverageBitrate;
end;

function TMACIn.GetCurrentBitrate: Integer;
begin
  if Assigned(APEDecompress) then
    Result := APEDecompress.CurrentBitrate;
end;

function TMACIn.GetCurrentBlock: Integer;
begin
  if Assigned(APEDecompress) then
    Result := APEDecompress.CurrentBlock;
end;

function TMACIn.GetCurrentMS: Integer;
begin
  if Assigned(APEDecompress) then
    Result := APEDecompress.CurrentMS;
end;

function TMACIn.GetLengthMS: Integer;
begin
  if Assigned(APEDecompress) then
    Result := APEDecompress.LengthMS;
end;

function TMACIn.GetTotalBlocks: Integer;
begin
  if Assigned(APEDecompress) then
    Result := APEDecompress.TotalBlocks;
end;

function TMACIn.GetBPS: Integer;
begin
  OpenFile;
  Result := FBPS;
  CloseFile;
end;

function TMACIn.GetCh: Integer;
begin
  OpenFile;
  Result := FChan;
  CloseFile;
end;

function TMACIn.GetSR: Integer;
begin
  OpenFile;
  Result := FSR;
  CloseFile;
end;

procedure TMACOut.SetCompressionLevel(Value: Integer);
begin
  case Value of
    COMPRESSION_LEVEL_FAST,
      COMPRESSION_LEVEL_NORMAL,
      COMPRESSION_LEVEL_HIGH,
      COMPRESSION_LEVEL_EXTRA_HIGH: FCompressionLevel := Value;
  else
    FCompressionLevel := COMPRESSION_LEVEL_NORMAL;
  end;
end;

procedure TMACIn.Flush;
begin
  inherited Flush;
end;

procedure TMACIn.Init;
begin
  inherited Init;
  BufStart := 1;
  BufEnd := 0;
end;

function TMACIn.Seek(Sample : Integer) : Boolean;
begin
  Result := False;
  if not FSeekable then Exit;
  Result := True;
  OpenFile;
  APEDecompress.Seek(Sample);
  CloseFile;
end;

initialization

  FileFormats.Add('mac','Monkey Audio',TMACOut);
  FileFormats.Add('mac','Monkey Audio',TMACIn);


end.

