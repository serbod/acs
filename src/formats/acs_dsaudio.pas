(*
  this file is a part of audio components suite v 2.4.
  see the license file for more details.
  you can contact me at mail@z0m3ie.de
*)

{
$Log: acs_dsaudio.pas,v $
Revision 1.1  2005/12/19 18:36:38  z0m3ie
*** empty log message ***

Revision 1.3  2005/11/28 21:57:24  z0m3ie
mostly FileOut fixes
moved PBuffer to PBuffer8
set all to dynamically Buffering

Revision 1.2  2005/09/13 04:04:50  z0m3ie
First release without Components for Fileformats
only TFileIn and TFileOut are Visible

Revision 1.1  2005/09/13 03:13:57  z0m3ie
*** empty log message ***

Revision 1.1  2005/09/12 22:04:52  z0m3ie
modified structure again, fileformats are now in an sperat folder.
all File In/Out classes are capsulated from TFileIn and TFileOut

Revision 1.4  2005/09/11 18:06:26  z0m3ie
first working Version

Revision 1.3  2005/09/10 08:25:40  z0m3ie
*** empty log message ***

Revision 1.2  2005/09/07 21:13:24  z0m3ie
*** empty log message ***

Revision 1.1  2005/09/07 20:53:22  z0m3ie
begon to add MPEG and WMA support using DirectX

}
unit acs_dsaudio;

{$ifdef linux}{$message error 'unit not supported'}{$endif linux}

{$DEFINE DYNAMIC_LINK_ALL}

interface

uses
  ACS_File,ACS_Classes,DirectShow9,Classes,ActiveX,MMSystem,Windows;

const
  BUF_SIZE = $8000; // 32k

type
  { TDSIn }

  TDSIn = class(TAcsFileIn)
  private
    buf : PBuffer8;  // ring buffer
    FxFormat: TWaveFormatEx;
    FxAMMultiMediaStream: IAMMultiMediaStream;
    FxGraphBuilder: IGraphBuilder;
    FxMediaSeeking: IMediaSeeking;
    FxMediaControl: IMediaControl;
    FxAudioMediaStream: IAudioMediaStream;
    FxMediaStream: IMediaStream;
    FxAudioStreamSample: IAudioStreamSample;
    FxAudioData: IAudioData;
    FxBuffer: Pointer;
    FxBufferSize: DWord;
    FDuration,
    FxPosition,
    FxSelStart,
    FxSelLength,
    FxLastReadingStartTime,
    FxLastReadingEndTime : Stream_Time;
    FSeekScale : Integer;
    function Read(const Buffer; xSize: DWord): DWord;
    procedure SetPosition( Value: STREAM_TIME );
  protected
    procedure OpenFile; override;
    procedure CloseFile; override;

    procedure Init; override;
    procedure Flush; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetData(Buffer : Pointer; BufferSize : Integer): Integer; override;
    function Seek(SampleNum : Integer) : Boolean; override;
  end;

const
  DSUSER_HRESULT = HResult($08000000);
  DSUSER_INVALIDSIZE = DSUSER_HRESULT + 1;

function ErrorCheck( Value: HRESULT ): HRESULT; { Check the result of a COM operation }

implementation

procedure TDSIn.OpenFile;
var
  v : WideString;
begin
  FValid := True;
  if FOpened = 0 then
  begin
    ErrorCheck( CoCreateInstance( CLSID_AMMultiMediaStream, nil, CLSCTX_INPROC_SERVER,IID_IAMMultiMediaStream, FxAMMultiMediaStream ) );
    ErrorCheck( FxAMMultiMediaStream.Initialize(STREAMTYPE_READ, AMMSF_NOGRAPHTHREAD, nil) );
    ErrorCheck( FxAMMultiMediaStream.AddMediaStream(nil, @MSPID_PrimaryAudio, 0, FxMediaStream) );
    ErrorCheck( FxAMMultiMediaStream.GetMediaStream(MSPID_PrimaryAudio, FxMediaStream) );
    v := FFileName;
    ErrorCheck( FxAMMultiMediaStream.OpenFile(PWideChar(v), 0) );
    ErrorCheck( FxAMMultiMediaStream.GetFilterGraph(FxGraphBuilder) );
    ErrorCheck( FxGraphBuilder.QueryInterface(IID_IMediaControl, FxMediaControl) );
    ErrorCheck( FxGraphBuilder.QueryInterface(IID_IMediaSeeking, FxMediaSeeking) );
    ErrorCheck( FxMediaStream.QueryInterface(IID_IAudioMediaStream, FxAudioMediaStream) );
    ErrorCheck( FxAudioMediaStream.GetFormat(FxFormat) );
    ErrorCheck( CoCreateInstance(CLSID_AMAudioData, nil, CLSCTX_INPROC_SERVER,IID_IAudioData, FxAudioData) );
    ErrorCheck( FxAudioData.SetFormat(FxFormat) );
    ErrorCheck( FxAudioMediaStream.CreateSample(FxAudioData, 0, FxAudioStreamSample) );
    ErrorCheck( FxAMMultiMediaStream.GetDuration(FDuration) );
    ErrorCheck( FxAMMultiMediaStream.SetState( STREAMSTATE_RUN ) );
    FxSelLength := 0;
    FSR   := FxFormat.nSamplesPerSec;
    FBPS  := FxFormat.wBitsPerSample;
    FChan := FxFormat.nChannels;
    FTotalSamples := FSize;
    FSeekable     := TRUE;
    FSize := (FDuration div 10000000) *
            FxFormat.nSamplesPerSec *
            FxFormat.nChannels *
            FxFormat.wBitsPerSample div 8;
    FSeekScale := FDuration div FSize;
  end;
  Inc(FOpened);
end;

procedure TDSIn.CloseFile;
begin
  if FOpened = 1 then
  begin
    if Assigned(FxAMMultiMediaStream) then
    ErrorCheck( FxAMMultiMediaStream.SetState( STREAMSTATE_STOP ) );
    FxAudioStreamSample  := nil;
    FxAudioData          := nil;
    FxAudioMediaStream   := nil;
    FxMediaStream        := nil;
    FxMediaSeeking       := nil;
    FxMediaControl       := nil;
    FxGraphBuilder       := nil;
    FxAMMultiMediaStream := nil;
  end;
  if FOpened > 0 then Dec(FOpened);
end;

function TDSIn.GetData(Buffer: Pointer; BufferSize: Integer): Integer;
var
  nDone : Integer;
  nOffs : Integer;
begin
  if not Busy then raise EACSException.Create('The Stream is not opened');
  if BufStart > BufEnd then
  begin
    if FOffset <> 0 then
    begin
      FPosition := FOffset;
      if FPosition < 0 then
        FPosition := 0
      else if FPosition > FSize then
        FPosition := FSize;
      SetPosition(Int64(FPosition) * FSeekScale);
      FOffset := 0;
    end;
    BufStart := 1;
    nDone := Read(Buf^, BUF_SIZE);
    if nDone = 0 then
    begin
      if FLoop then
      begin
        SetPosition(0); // just rewind
        nDone := Read(Buf^, BUF_SIZE);
      end else
      begin
        Result := 0;
        Exit;
      end;
    end;
    BufEnd := nDone;
  end;

  if BufferSize < (BufEnd - BufStart + 1) then
    Result := BufferSize
  else
    Result := BufEnd - BufStart + 1;

  Move(Buf^[BufStart], Buffer^, Result);
  Inc(BufStart, Result);
  Inc(FPosition, Result);
end;

constructor TDSIn.Create(AOwner: TComponent);
var
  AMovie: IGraphBuilder;
begin
  QzInitialize(nil);
  ErrorCheck(CoCreateInstance( CLSID_FilterGraph, nil, CLSCTX_INPROC_SERVER,IID_IGraphBuilder, AMovie));
  inherited Create(AOwner);
end;

destructor TDSIn.Destroy;
begin
  inherited Destroy;
end;

procedure TDSIn.Init;
begin
  inherited Init;
  GetMem(Buf, BUF_SIZE);
end;

procedure TDSIn.Flush;
begin
  FreeMem(Buf);
  inherited Flush;
end;

function TDSIn.Seek(SampleNum: Integer): Boolean;
begin
  FOffset := SampleNum;
end;

function TDSIn.Read(const Buffer; xSize: DWord): DWord;
var
  hr : DWord;
  Tmp: STREAM_TIME;
begin
  Result := 0;
  if (xSize <= 0) then ErrorCheck(DSUSER_INVALIDSIZE);
  if (FxSelLength <> 0) and (FxPosition >= FxSelStart + FxSelLength) then exit;
  if (@Buffer <> FxBuffer) or (xSize <> FxBufferSize) then
  begin
    FxBuffer := @Buffer;
    FxBufferSize := xSize;
    ErrorCheck( FxAudioData.SetBuffer( FxBufferSize, FxBuffer, 0 ) );
  end;
  hr := FxAudioStreamSample.Update(0, 0, nil, 0);
  if (hr = MS_S_ENDOFSTREAM) or (hr = MS_E_NOSTREAM) then exit;
  ErrorCheck(hr);
  ErrorCheck( FxAudioData.GetInfo(FxBufferSize, FxBuffer, Result) );
  ErrorCheck( FxAudioStreamSample.GetSampleTimes( FxLastReadingStartTime,FxLastReadingEndTime, Tmp ) );
  if FxLastReadingStartTime > FxLastReadingEndTime then
    FxLastReadingStartTime := FxLastReadingEndTime;
  if (FxSelLength <> 0) and (FxLastReadingEndTime > FxLastReadingStartTime) and
    (FxSelStart + FxSelLength < FxLastReadingEndTime) then
  begin
    Result := DWord(Trunc(((Result *
      (FxSelStart + FxSelLength - FxLastReadingStartTime)) /
      (FxLastReadingEndTime - FxLastReadingStartTime)))) and
      (not(FxFormat.nBlockAlign-1));
    FxLastReadingEndTime := FxSelStart + FxSelLength;
  end;
  FxPosition := FxLastReadingEndTime;
end;

procedure TDSIn.SetPosition( Value: STREAM_TIME );
var pfs: TFilterState;
begin
  if (Value <> FxPosition) then
  begin
    if (Value < FxSelStart) then
      Value := FxSelStart
    else
      if (Value > FDuration) then
        Value := FDuration
      else
        if (FxSelLength <> 0) and (Value > FxSelStart + FxSelLength) then
          Value := FxSelStart + FxSelLength;
    ErrorCheck(FxMediaControl.StopWhenReady );
    ErrorCheck(FxMediaSeeking.SetPositions(Value,AM_SEEKING_AbsolutePositioning, Value, AM_SEEKING_NoPositioning));
    ErrorCheck(FxMediaControl.Run );
    ErrorCheck(FxMediaControl.GetState(INFINITE, pfs) );
    FxPosition := Value;
  end;
end;

function ErrorCheck( Value: HRESULT ): HRESULT; { Check the result of a COM operation }
var
  S: String;
  S2: array [0..300] of Char;
begin
  Result := Value;
  if (Value <> S_OK) then
  begin
    Case DWord(Value) of
      DSUSER_INVALIDSIZE: S:='Invalid buffer size.';
      DWord(REGDB_E_CLASSNOTREG): S:='A specified class is not registered in the registration database.';
      DWord(CLASS_E_NOAGGREGATION): S:='This class cannot be created as part of an aggregate.';
      DWord(E_ABORT): S:='The update aborted.';
      DWOrd(E_INVALIDARG): S:='One of the parameters is invalid.';
      DWord(E_POINTER): S:='This method tried to access an invalid pointer.';
      DWord(E_NOINTERFACE): S:='No interface.';
      MS_S_PENDING: S:='The asynchronous update is pending.';
      MS_S_NOUPDATE: S:='Sample was not updated after forced completion.';
      MS_S_ENDOFSTREAM: S:='Reached the end of the stream; the sample wasn''t updated.';
      MS_E_SAMPLEALLOC: S:='An IMediaStream object could not be removed from an IMultiMediaStream object because it still contains at least one allocated sample.';
      MS_E_PURPOSEID: S:='The specified purpose ID can''t be used for the call.';
      MS_E_NOSTREAM: S:='No stream can be found with the specified attributes.';
      MS_E_NOSEEKING: S:='One or more media streams don''t support seeking.';
      MS_E_INCOMPATIBLE: S:='The stream formats are not compatible.';
      MS_E_BUSY: S:='This sample already has a pending update.';
      MS_E_NOTINIT: S:='The object can''t accept the call because its initialize function or equivalent has not been called.';
      MS_E_SOURCEALREADYDEFINED: S:='Source already defined.';
      MS_E_INVALIDSTREAMTYPE: S:='The stream type is not valid for this operation.';
      MS_E_NOTRUNNING: S:='The IMultiMediaStream object is not in running state.';
      Else
        begin
          if AMGetErrorText( Value, s2, High(s2) ) = 0 then
            S:='Unrecognized error value.'
          else
            S:=String( s2 );
        end;
    end;
    raise EACSException.Create(S);
  end;
end ;

initialization
  FileFormats.Add('mp3','Mpeg Audio Layer III',TDSIn);
  FileFormats.Add('mp2','Mpeg Audio Layer II',TDSIn);
  FileFormats.Add('mpeg','Mpeg Audio',TDSIn);
  FileFormats.Add('wma','Windows Media Audio',TDSIn);

end.

