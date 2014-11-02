(*
  this file is a part of audio components suite v 2.3.
  copyright (c) 2002-2005 andrei borovsky. all rights reserved.
  see the license file for more details.
  you can contact me at mail@z0m3ie.de
*)

unit acs_indicator;

interface

uses
   Classes, ACS_Types, ACS_Classes, ACS_Procs, ACS_Strings;

type

  { TACSSoundIndicator }

  TACSSoundIndicator = class(TACSCustomConverter)
  private
    FLocked: Boolean;
    Window: array of Double;
    FValuesCount: Integer;
    procedure CalculateSpectrum(PSampleWindow: Pointer; SamplesCount: integer;
      var AValues: array of Double);
  protected
    function GetBPS: Integer; override;
    function GetCh: Integer; override;
    function GetSR: Integer; override;
    procedure SetValuesCount(AValue: integer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetData(Buffer: Pointer; BufferSize: Integer): Integer; override;
    procedure GetValues(var Values: array of Double);
    procedure Init; override;
    procedure Flush; override;
  published
    { Spectrum slices count }
    property ValuesCount: integer read FValuesCount write SetValuesCount;
  end;

implementation

constructor TACSSoundIndicator.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLocked:=False;
  FValuesCount:=32;
  //HannWindow(@Window, Length(Window), True);
end;

destructor TACSSoundIndicator.Destroy;
begin
  inherited Destroy;
end;

procedure TACSSoundIndicator.CalculateSpectrum(PSampleWindow: Pointer;
  SamplesCount: integer; var AValues: array of Double);
var
  pSample: PACSDoubleArray;
  pWindow: PACSDoubleArray;
  pCA: PACSComplexArray;
  i, ii, n, nn: integer;
  sum: Double;
begin
  if Length(AValues)<>ValuesCount then Exit;
  pSample:=PSampleWindow;

  i:=SizeOf(Double)*SamplesCount;
  { TODO : Save window between calls }
  pWindow:=GetMem(i);
  // Hann window elements count must be equal to sample elements count
  HannWindow(pWindow, SamplesCount, True);
  // apply Hann window
  MultDoubleArrays(@pWindow[0], @pSample[0], SamplesCount);
  Freemem(pWindow);

  // create ComplexArr
  i:=SizeOf(TACSComplex)*SamplesCount;
  pCA:=GetMem(i);

  // apply FFT
  {$R-}
  for i:=0 to SamplesCount-1 do
  begin
    pCA[i].Re:=pSample[i];
    pCA[i].Im:=0;
  end;
  {$R+}

  ComplexFFT(pCA, SamplesCount, 1);
  LgMagnitude(@pCA[0], @pSample[0], SamplesCount, 0);

  // dispose ComplexArr
  Freemem(pCA);

  // use first half of result, second half is symmetric
  //SetLength(AValues, SamplesCount);
  {$R-}
  n:=(SamplesCount div (ValuesCount*2)); // total spectrum slices
  for i:=0 to ValuesCount-1 do
  begin
    sum:=0;
    for ii:=0 to n-1 do
    begin
      sum:=sum+pSample[i*n+ii];
    end;
    AValues[i]:=sum/n;
  end;
  {$R+}
end;

function TACSSoundIndicator.GetBPS: Integer;
begin
  if not Assigned(FInput) then
    raise EACSException.Create(strInputnotassigned);
  Result:=FInput.BitsPerSample;
end;

function TACSSoundIndicator.GetCh: Integer;
begin
  if not Assigned(FInput) then
    raise EACSException.Create(strInputnotassigned);
  Result:=FInput.Channels;
end;

function TACSSoundIndicator.GetSR: Integer;
begin
  if not Assigned(FInput) then
    raise EACSException.Create(strInputnotassigned);
  Result:=FInput.SampleRate;
end;

procedure TACSSoundIndicator.SetValuesCount(AValue: integer);
begin
  FValuesCount:=AValue;
end;

procedure TACSSoundIndicator.Init;
begin
  if not Assigned(FInput) then
    raise EACSException.Create(strInputnotassigned);
  FBusy:=True;
  FInput.Init;
  FSize:=FInput.Size;
  //FillChar(FValues[0], SizeOf(Double)*32, 0);
  FLocked:=False;
  FPosition:=0;
end;

procedure TACSSoundIndicator.Flush;
begin
  FInput.Flush;
  //FillChar(FValues[0], SizeOf(Double)*32, 0);
  FBusy:=False;
  FLocked:=False;
end;

function TACSSoundIndicator.GetData(Buffer: Pointer; BufferSize: Integer): Integer;
begin
  if not Busy then
    raise EACSException.Create(strStreamnotopen);
  while InputLock do;
  InputLock:=True;
  Result:=FInput.GetData(Buffer, BufferSize);
  FPosition:=Finput.Position;
  InputLock:=False;

  while FLocked do;
  FLocked:=True;
  // copy input buffer to local buffer
  if Length(FBuffer)<>Result then SetLength(FBuffer, Result);
  if Result > 0 then Move(Buffer^, FBuffer[0], Result);
  FLocked:=False;
 end;

procedure TACSSoundIndicator.GetValues(var Values: array of Double);
var
  i, NumSamples: Integer;
  pSample: PACSDoubleArray;
  P: Pointer;
begin
  while FLocked do;
  FLocked:=True;

  if Length(Values)<>ValuesCount then Exit;
  //SetLength(Values, ValuesCount);
  if BufferSize=0 then
  begin
    FillChar(Values[0], SizeOf(Double)*ValuesCount, 0);
    Exit;
  end;

  NumSamples:=((BufferSize div FInput.Channels) div (FInput.BitsPerSample div 8));

  i:=SizeOf(Double)*NumSamples;
  pSample:=GetMem(i);
  // convert raw sample to mono sample array
  {$R-}
  P:=@FBuffer[0];
  if FInput.BitsPerSample=8 then
  begin
    if FInput.Channels=1 then
      for i:=0 to NumSamples-1 do pSample[i]:=PACSBuffer8(P)[i]
    else
      for i:=0 to NumSamples-1 do pSample[i]:=(PACSStereoBuffer8(P)[i].Left + PACSStereoBuffer8(P)[i].Right)/2;
    end
  else
  begin
    if FInput.Channels=1 then
      for i:=0 to NumSamples-1 do pSample[i]:=PACSBuffer16(P)[i]
    else
      for i:=0 to NumSamples-1 do pSample[i]:=(PACSStereoBuffer16(P)[i].Left + PACSStereoBuffer16(P)[i].Right)/2;
  end;
  {$R+}

  CalculateSpectrum(pSample, NumSamples, Values);
  Freemem(pSample);

  for i:=0 to ValuesCount-1 do Values[i]:=Values[i]*0.4; //ValCount;
  FLocked:=False;
end;

end.
