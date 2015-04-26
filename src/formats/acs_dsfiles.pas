(*
  this file is a part of audio components suite v 2.4.
  see the license file for more details.
  you can contact me at mail@z0m3ie.de
*)

unit acs_dsfiles;

{$ifdef linux}{$message error 'unit not supported'}{$endif linux}

{$DEFINE DYNAMIC_LINK_ALL}

interface

uses
  ACS_File, ACS_Classes, DirectShow9, Classes, ActiveX, MMSystem, Windows, ACS_Types;

type
  { TDSIn }

  TDSIn = class(TAcsCustomFileIn)
  private
    {$ifdef fpc}
    FxFormat: _WAVEFORMATEX;
    {$else}
    FxFormat: TWaveFormatEx;
    {$endif}
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
    FDuration: STREAM_TIME;
    FxPosition: STREAM_TIME;
    FxSelStart: STREAM_TIME;
    FxSelLength: STREAM_TIME;
    FxLastReadingStartTime: STREAM_TIME;
    FxLastReadingEndTime: STREAM_TIME;
    FSeekScale: Integer;
    function Read(Buffer: Pointer; xSize: DWord): DWord;
    procedure SetPosition(Value: STREAM_TIME);
  protected
    procedure OpenFile; override;
    procedure CloseFile; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetData(Buffer: Pointer; BufferSize: Integer): Integer; override;
    function Seek(SampleNum: Integer): Boolean; override;
  end;

const
  DSUSER_HRESULT = HResult($08000000);
  DSUSER_INVALIDSIZE = DSUSER_HRESULT + 1;

function ErrorCheck(Value: HRESULT): HRESULT; { Check the result of a COM operation }

implementation

procedure TDSIn.OpenFile;
var
  v: WideString;
begin
  FValid:=True;
  if FOpened = 0 then
  begin
    QzInitialize(nil);
    ErrorCheck( CoCreateInstance( CLSID_AMMultiMediaStream, nil, CLSCTX_INPROC_SERVER,IID_IAMMultiMediaStream, FxAMMultiMediaStream ) );
    ErrorCheck( FxAMMultiMediaStream.Initialize(STREAMTYPE_READ, AMMSF_NOGRAPHTHREAD, nil) );
    ErrorCheck( FxAMMultiMediaStream.AddMediaStream(nil, @MSPID_PrimaryAudio, 0, FxMediaStream) );
    ErrorCheck( FxAMMultiMediaStream.GetMediaStream(MSPID_PrimaryAudio, FxMediaStream) );
    v:=FFileName;
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
    FxSelLength:=0;
    FSR   := FxFormat.nSamplesPerSec;
    FBPS  := FxFormat.wBitsPerSample;
    FChan := FxFormat.nChannels;
    FTotalSamples := FSize;
    FSeekable     := TRUE;
    if fDuration = 0 then exit;
    FSize :=(FDuration div 10000000) *
            FSR *
            FChan *
            FBPS div 8;
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
  nDone: Integer;
  nOffs: Integer;
begin
  Result:=0;
  if not Busy then raise EAcsException.Create('The Stream is not opened');
  if BufStart > BufEnd then
  begin
    if FOffset <> 0 then
    begin
      FPosition:=FPosition+Round((FOffset/100)*FSize);
      if FPosition < 0 then
        FPosition:=0
      else
        if FPosition > FSize then
          FPosition:=FSize;
      SetPosition(Int64(FPosition)*FSeekScale);
      FOffset:=0;
    end;
    BufStart:=1;
    nDone:=Read(FBuffer, BufferSize);
    if nDone = 0 then
    begin
      if FLoop then
      begin
        SetPosition(0); // just rewind
        nDone:=Read(FBuffer, BufferSize);
      end
      else Exit;
    end;
    BufEnd:=nDone;
  end;

  if BufferSize < (BufEnd-BufStart+1) then
    Result:=BufferSize
  else
    Result:=BufEnd-BufStart+1;

  Move(FBuffer[BufStart-1], Buffer^, Result);
  Inc(BufStart, Result);
  Inc(FPosition, Result);
end;

constructor TDSIn.Create(AOwner: TComponent);
var
  AMovie: IGraphBuilder;
begin
  inherited Create(AOwner);
  BufferSize:=$8000;
  QzInitialize(nil);
  ErrorCheck( CoCreateInstance(CLSID_FilterGraph, nil, CLSCTX_INPROC_SERVER, IID_IGraphBuilder, AMovie) );
end;

destructor TDSIn.Destroy;
begin
  QzUninitialize;
  inherited Destroy;
end;

function TDSIn.Seek(SampleNum: Integer): Boolean;
begin
  Result:=True;
  FPosition:=SampleNum;
end;

function TDSIn.Read(Buffer: Pointer; xSize: DWord): DWord;
var
  hr: DWord;
  Tmp: STREAM_TIME;
begin
  Result:=0;
  if (xSize <= 0) then ErrorCheck(DSUSER_INVALIDSIZE);
  if (FxSelLength <> 0) and (FxPosition >= FxSelStart+FxSelLength) then Exit;

  if (Buffer <> FxBuffer) or (xSize <> FxBufferSize) then
  begin
    FxBuffer:=Buffer;
    FxBufferSize:=xSize;
    ErrorCheck( FxAudioData.SetBuffer(FxBufferSize, FxBuffer, 0) );
  end;
  hr:=FxAudioStreamSample.Update(0, 0, nil, 0);
  if (hr = MS_S_ENDOFSTREAM) or (hr = MS_E_NOSTREAM) then Exit;
  ErrorCheck(hr);
  ErrorCheck( FxAudioData.GetInfo(FxBufferSize, FxBuffer, Result) );
  ErrorCheck( FxAudioStreamSample.GetSampleTimes(FxLastReadingStartTime, FxLastReadingEndTime, Tmp) );
  if FxLastReadingStartTime > FxLastReadingEndTime then
    FxLastReadingStartTime:=FxLastReadingEndTime;
  if (FxSelLength <> 0) and (FxLastReadingEndTime > FxLastReadingStartTime)
  and (FxSelStart+FxSelLength < FxLastReadingEndTime) then
  begin
    Result:=DWord(Trunc(((Result*(FxSelStart+FxSelLength-FxLastReadingStartTime))
      /(FxLastReadingEndTime-FxLastReadingStartTime))))
      AND (not(FxFormat.nBlockAlign-1));
    FxLastReadingEndTime:=FxSelStart+FxSelLength;
  end;
  FxPosition:=FxLastReadingEndTime;
end;

procedure TDSIn.SetPosition(Value: STREAM_TIME);
var
  pfs: TFilterState;
begin
  if (Value <> FxPosition) then
  begin
    if (Value < FxSelStart) then
      Value:=FxSelStart
    else
      if (Value > FDuration) then
        Value:=FDuration
      else
        if (FxSelLength <> 0) and (Value > FxSelStart+FxSelLength) then
          Value:=FxSelStart+FxSelLength;
    ErrorCheck( FxMediaControl.StopWhenReady );
    ErrorCheck( FxMediaSeeking.SetPositions(Value, AM_SEEKING_AbsolutePositioning, Value, AM_SEEKING_NoPositioning) );
    ErrorCheck( FxMediaControl.Run );
    ErrorCheck( FxMediaControl.GetState(INFINITE, pfs) );
    FxPosition:=Value;
  end;
end;

function ErrorCheck( Value: HRESULT ): HRESULT; { Check the result of a COM operation }
var
  s: String;
  s2: array [0..300] of Char;
begin
  Result := Value;
  if (Value <> S_OK) then
  begin
    case DWord(Value) of
      DSUSER_INVALIDSIZE: s:='Invalid buffer size.';
      DWord(REGDB_E_CLASSNOTREG): s:='A specified class is not registered in the registration database.';
      DWord(CLASS_E_NOAGGREGATION): s:='This class cannot be created as part of an aggregate.';
      DWord(E_ABORT): s:='The update aborted.';
      DWOrd(E_INVALIDARG): s:='One of the parameters is invalid.';
      DWord(E_POINTER): s:='This method tried to access an invalid pointer.';
      DWord(E_NOINTERFACE): s:='No interface.';
      MS_S_PENDING: s:='The asynchronous update is pending.';
      MS_S_NOUPDATE: s:='Sample was not updated after forced completion.';
      MS_S_ENDOFSTREAM: s:='Reached the end of the stream; the sample wasn''t updated.';
      MS_E_SAMPLEALLOC: s:='An IMediaStream object could not be removed from an IMultiMediaStream object because it still contains at least one allocated sample.';
      MS_E_PURPOSEID: s:='The specified purpose ID can''t be used for the call.';
      MS_E_NOSTREAM: s:='No stream can be found with the specified attributes.';
      MS_E_NOSEEKING: s:='One or more media streams don''t support seeking.';
      MS_E_INCOMPATIBLE: s:='The stream formats are not compatible.';
      MS_E_BUSY: s:='This sample already has a pending update.';
      MS_E_NOTINIT: s:='The object can''t accept the call because its initialize function or equivalent has not been called.';
      MS_E_SOURCEALREADYDEFINED: s:='Source already defined.';
      MS_E_INVALIDSTREAMTYPE: s:='The stream type is not valid for this operation.';
      MS_E_NOTRUNNING: s:='The IMultiMediaStream object is not in running state.';
      else
      begin
        if AMGetErrorText(Value, s2, High(s2)) = 0 then
          s:='Unrecognized error value.'
        else
          s:=String(s2);
      end;
    end;
//    raise EAcsException.Create(s);
  end;
end ;

initialization
  FileFormats.Add('mp3',  'Mpeg Audio Layer III', TDSIn);
  FileFormats.Add('mp2',  'Mpeg Audio Layer II', TDSIn);
  FileFormats.Add('mpeg', 'Mpeg Audio', TDSIn);
  FileFormats.Add('wma',  'Windows Media Audio', TDSIn);

end.

