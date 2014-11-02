(*
  this file is a part of audio components suite v 2.3 (delphi version).
  copyright (c) 2002-2005 andrei borovsky. all rights reserved.
  see the license file for more details.
  you can contact me at acs@compiler4.net
  this is the acs for delphi (windows) version of the unit.
*)

{
$Log: waveconverter.pas,v $
Revision 1.1  2005/12/19 18:36:49  z0m3ie
*** empty log message ***

Revision 1.1  2005/09/12 22:04:53  z0m3ie
modified structure again, fileformats are now in an sperat folder.
all File In/Out classes are capsulated from TFileIn and TFileOut

Revision 1.1  2005/08/25 20:18:00  z0m3ie
Version 2.4 restructure
TCDPlayer removed (fits not in component structure)
TMP3ToWavConverter removed (fits not in component structure)

Revision 1.2  2005/08/22 20:17:02  z0m3ie
changed Headers to log
changed mail adress

}

unit waveconverter;

interface

uses
  SysUtils,
  Windows, Classes, MMSystem, MSAcm;

type
  {$IFDEF FPC}
  TWaveFormat = WAVEFORMAT;
  {$ENDIF}

  TRiffID = array[0..3] of char;
  TRiffHeader = packed record
    ID: TRiffID;
    BytesFollowing: DWord;
  end;


  TACMWaveFormat = packed record
    case integer of
      0 : (Format : TWaveFormatEx);
      1 : (RawData : Array[0..128] of byte);
  end;

  TWaveConverter = class(TMemoryStream)
  private
    FMaxFmtSize: DWord;
  public
    CurrentFormat: TACMWaveFormat;
    NewFormat: TACMWaveFormat;
    function LoadStream(Stream : TStream): integer;
    function Convert: integer;
    function SaveWavToStream(MS: TStream): Integer;
    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TWaveConverter }

function TWaveConverter.Convert: integer;
var
  FStreamHandle: HACMStream;
  OutputBufferSize: DWord;
  FStreamHeader: TACMStreamHeader;
  OutPut: Pointer;
begin
  FStreamHandle := nil;

  //
  // Open the stream we're going to use to convert from the current to the
  // new format
  //
  Result := acmStreamOpen(FStreamhandle, nil, CurrentFormat.Format,
   NewFormat.Format, nil, 0, 0, ACM_STREAMOPENF_NONREALTIME);
  if Result <> 0 then
  begin
    //SetError('acmStreamOpen', Result);
    Exit;
  end;

  //
  // Calculate the size of the converted data
  //
  Result := acmStreamSize(FStreamHandle, self. Size, OutputBufferSize,
   ACM_STREAMSIZEF_SOURCE);

  if Result <> 0 then
  begin
//    SetError('acmStreamSize', Result);
    Exit;
  end;

  //
  // Allocate memory for the converted data
  //
  GetMem(OutPut, OutputBufferSize);
  FillChar(OutPut^,OutputBufferSize,#0);

  Self.Seek(0,0);

  //
  // Initialize and prepare a header
  //
  with FStreamHeader do
  begin
    cbStruct := SizeOf(TACMStreamHeader);
    fdwStatus := 0;
    dwUser := 0;
    pbSrc := self.Memory;
    cbSrcLength := self.Size;
    cbSrcLengthUsed := 0;
    dwSrcUser := 0;
    pbDst := OutPut;
    cbDstLength := OutputBufferSize;
    cbDstLengthUsed := 0;
    dwDstUser := 0;
  end;
  Result := acmStreamPrepareHeader(FStreamHandle,FStreamHeader, 0);
  if Result <> 0 then
  begin
//    SetError('acmStreamPrepareHeader', Result);
    Exit;
  end;

  //
  // Tell acm to convert the stream
  //
  Result := acmStreamConvert(FStreamHandle,FStreamHeader,
    ACM_STREAMCONVERTF_BLOCKALIGN);
  if Result <> 0 then
  begin
//    SetError('acmStreamConvert', Result);
    Exit;
  end;

  //
  // Set the format eqaul to the newformat and copy the
  // data over to the streams memory
  //
  Move(NewFormat.RawData, CurrentFormat.RawData, FMaxFmtSize);
  Self.SetSize(OutputBufferSize);
  Self.Seek(0,0);
  Self.Write(Output^, OutputBufferSize);

  //
  // Unprepeare the header
  //
  Result := acmStreamUnprepareHeader(FStreamHandle,FStreamHeader, 0);
  if Result <> 0 then
  begin
//    SetError('acmStreamUnprepareHeader', Result);
    Exit;
  end;

  //
  // Close the stream
  //
  Result := acmStreamClose(FStreamHandle, 0);
  if Result <> 0 then
  begin
//    SetError('acmStreamClose', Result);
    Exit;
  end;

  FreeMem(OutPut);
end;

constructor TWaveConverter.Create;
begin
  inherited;

  acmMetrics(nil, ACM_METRIC_MAX_SIZE_FORMAT, FMaxFmtSize);

  FillChar(CurrentFormat.Format, FMaxFmtSize, 0);
  FillChar(NewFormat.Format, FMaxFmtSize, 0);
end;

destructor TWaveConverter.Destroy;
begin
  inherited;
end;

function TWaveConverter.LoadStream(Stream : TStream): integer;
var
  Header: TRiffHeader;
  ID: TRiffID;
  Mem: Pointer;
  Data: PByteArray;


  NumRead: Integer;
  Pos: Integer;

begin
  Result := 0;
  try
    //read the header
    NumRead := Stream.Read(Header, SizeOf(Header));
    Pos := NumRead;

    NumRead := Stream.Read(ID, SizeOf(ID));
    Pos := Pos + NumRead;

    if (Header.ID <> 'RIFF') or (ID <> 'WAVE') then
    begin
      Exit;
    end;

    while Pos < Stream.Size -1 do
    begin
      Dec(Pos,7);
      Stream.Seek(Pos, soFromBeginning);

      NumRead := Stream.Read(Header, SizeOf(Header));
      Pos := Pos + NumRead;


      if Header.ID = 'fmt ' then
      begin
        GetMem(Mem, Header.BytesFollowing);
        try
          NumRead := Stream.Read(Mem^, Header.BytesFollowing);
          Pos := Pos + NumRead;

          if Header.BytesFollowing < SizeOf(TWaveFormatEx) then
            Move(Mem^, CurrentFormat.Format , SizeOf(TWaveFormatEx))
          else
            Move(Mem^, CurrentFormat.Format, Header.BytesFollowing);
        finally
          FreeMem(Mem);
        end;
      end
      else
      if Header.ID = 'fact' then
      begin
        GetMem(Data, Header.BytesFollowing);
        try
          NumRead := Stream.Read(Data^, Header.BytesFollowing);
          Pos := Pos + NumRead;
        finally
          FreeMem(Data);
        end;
      end
      else
      if Header.ID = 'data' then
      begin
        if Header.BytesFollowing > 0 then
        begin
          GetMem(Data, Header.BytesFollowing);
          try
            NumRead := Stream.Read(Data^, Header.BytesFollowing);
            Pos := Pos + NumRead;

            Self.SetSize(Header.BytesFollowing);
            Self.Seek(0,0);
            Self.Write(Data^, (*Header.BytesFollowing*)self.Size);
          finally
            FreeMem(Data);
          end;
        end;
      end;
    end;

    Seek(0,0);
  finally
    //  FileStream.Free;
  end;
end;


function TWaveConverter.SaveWavToStream(MS: TStream): Integer;
var
  CurrentPos                  : Integer;
  H                           : TRiffHeader;
  ID                          : TRiffID;
begin
  Result := 0;
  try
    CurrentPos := Position;

    H.ID := 'RIFF';
    H.BytesFollowing := 0;
    MS.Write(H, SizeOf(H));

    ID := 'WAVE';
    MS.Write(ID, SizeOf(ID));

    H.ID := 'fmt ';
    H.BytesFollowing := SizeOf(TWaveFormat) + 2;
    MS.Write(H, SizeOf(H));
    MS.Write(CurrentFormat.Format, SizeOf(TWaveFormat) + 2);

    H.ID := 'data';
    H.BytesFollowing := Size;
    MS.Write(H, SizeOf(H));
    Seek(0,0);
//    MS.CopyFrom(Self, Size);
//    ms.Write( Self, Size);
    self.SaveToStream(MS);


    MS.Seek(0,0);
    H.ID := 'RIFF';
    H.BytesFollowing := MS.Size - SizeOf(H) +1;
    MS.Write(H,SizeOf(H));

    Position := CurrentPos;
//    MS.Free;
  except
    on E: Exception do
    begin
      Result := MCIERR_FILE_NOT_SAVED;
//      SetError('SaveFile', MCIERR_FILE_NOT_SAVED);
    end;
  end;
end;

end.




