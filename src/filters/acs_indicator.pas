(*
  this file is a part of audio components suite v 2.3.
  copyright (c) 2002-2005 andrei borovsky. all rights reserved.
  see the license file for more details.
  you can contact me at mail@z0m3ie.de
*)
{
Status: broken =(
}
{$define FFTREAL_}
unit acs_indicator;

interface

uses
   sysutils, Classes, ACS_Types, ACS_Classes, ACS_Procs, ACS_Strings
{$ifdef FFTREAL}
   , FFTReal
{$endif}
   ;

type

  { TAcsSoundIndicator }

  TAcsSoundIndicator = class(TAcsCustomConverter)
  private
    FLocked: Boolean;
    //Window: array of Double;
    FValuesCount: Integer;
    procedure CalculateSpectrum(PSampleWindow: Pointer; SamplesCount: integer;
      var AValues: array of Double);
  protected
    procedure SetValuesCount(AValue: integer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetData(ABuffer: Pointer; ABufferSize: Integer): Integer; override;
    procedure GetValues(var Values: array of Double);
    procedure Init(); override;
    procedure Done(); override;
  published
    { Spectrum slices count }
    property ValuesCount: integer read FValuesCount write SetValuesCount;
  end;

implementation

constructor TAcsSoundIndicator.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLocked:=False;
  FValuesCount:=32;
  FBufferSize:=$1000;
  //HannWindow(@Window, Length(Window), True);
end;

destructor TAcsSoundIndicator.Destroy;
begin
  inherited Destroy;
end;

{$ifdef FFTREAL}
procedure TAcsSoundIndicator.CalculateSpectrum(PSampleWindow: Pointer;
  SamplesCount: integer; var AValues: array of Double);
var
  pSample: PAcsDoubleArray;
  pFFTResult: pflt_array;
  i, ii, n, FreqCount: integer;
  k, sum: Double;
  fftr: TFFTReal;
begin
  if Length(AValues)<>ValuesCount then Exit;

  pSample:=PSampleWindow;
  pFFTResult:=GetMem(SamplesCount * SizeOf(flt_t));

  fftr:=TFFTReal.Create(SamplesCount);

  fftr.do_fft(pFFTResult, pflt_array(pSample));
  // pFFTResult[0..length(x)/2] = real values
  // pFFTResult[length(x)/2+1..length(x)-1] = imaginary values of coefficents 1..length(x)/2-1.

  FreeAndNil(fftr);

  // Using only real values, first half of result
  FreqCount:=SamplesCount div 2;
  // FreqCount to ValueCount ratio
  k:=FreqCount / ValuesCount;

  // use first half of result, second half is symmetric
  //SetLength(AValues, SamplesCount);
  {$R-}
  for i:=0 to ValuesCount-1 do
  begin
    n:=Trunc(i*k);
    AValues[i]:=pFFTResult[n];
  end;
  {$R+}
  Freemem(pFFTResult);
end;

{$else}
procedure TAcsSoundIndicator.CalculateSpectrum(PSampleWindow: Pointer;
  SamplesCount: integer; var AValues: array of Double);
var
  pSample: PAcsDoubleArray;
  pWindow: PAcsDoubleArray;
  pCA: PAcsComplexArray;
  i, ii, n: integer;
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
  i:=SizeOf(TAcsComplex)*SamplesCount;
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
  //LgMagnitude(pCA, pSample, SamplesCount, 0);

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
{$endif}

procedure TAcsSoundIndicator.SetValuesCount(AValue: integer);
begin
  FValuesCount:=AValue;
end;

procedure TAcsSoundIndicator.Init();
begin
  inherited Init();
  //FillChar(FValues[0], SizeOf(Double)*32, 0);
  FLocked:=False;
end;

procedure TAcsSoundIndicator.Done();
begin
  inherited Done();
  //FillChar(FValues[0], SizeOf(Double)*32, 0);
  FLocked:=False;
end;

function TAcsSoundIndicator.GetData(ABuffer: Pointer; ABufferSize: Integer): Integer;
var
  n: Integer;
begin
  if not Active then
    raise EAcsException.Create(strStreamnotopen);

  // copy input ABuffer to param ABuffer
  while InputLock do Sleep(1);
  InputLock:=True;
  Result:=FInput.GetData(ABuffer, ABufferSize);
  FPosition:=FInput.Position;
  InputLock:=False;

  // copy param ABuffer to local ABuffer
  while FLocked do Sleep(1);
  FLocked:=True;
  {
  if Length(FBuffer)<>Result then SetLength(FBuffer, Result);
  if Result > 0 then Move(ABuffer^, FBuffer[0], Result);
  }
  n:=Result;
  if n > FAudioBuffer.Size then n:=FAudioBuffer.Size;
  FAudioBuffer.Reset();
  FAudioBuffer.Write(ABuffer^, n);
  FLocked:=False;
 end;

procedure TAcsSoundIndicator.GetValues(var Values: array of Double);
var
  i, NumSamples: Integer;
  pSample: PAcsDoubleArray;
  P: Pointer;
begin
  if Length(Values)<>ValuesCount then Exit;
  //SetLength(Values, ValuesCount);
  //if BufferSize=0 then
  FillChar(Values[0], SizeOf(Double) * ValuesCount, 0);
  if FAudioBuffer.WritePosition = 0 then Exit;

  if (FInput.Channels = 0) or (FInput.BitsPerSample = 0) then Exit;
  NumSamples:=FAudioBuffer.WritePosition div (FInput.Channels * (FInput.BitsPerSample div 8));

  while FLocked do Sleep(1);
  FLocked:=True;

  i:=SizeOf(Double) * NumSamples;
  pSample:=GetMem(i);
  // convert raw sample to mono sample array
  {$R-}
  //P:=@FBuffer[0];
  P:=FAudioBuffer.Memory;
  if FInput.BitsPerSample=8 then
  begin
    if FInput.Channels=1 then
      for i:=0 to NumSamples-1 do pSample[i]:=PAcsBuffer8(P)[i]
    else
      for i:=0 to NumSamples-1 do pSample[i]:=(PAcsStereoBuffer8(P)[i].Left + PAcsStereoBuffer8(P)[i].Right)/2;
    end
  else
  begin
    if FInput.Channels=1 then
      for i:=0 to NumSamples-1 do pSample[i]:=PAcsBuffer16(P)[i]
    else
      for i:=0 to NumSamples-1 do pSample[i]:=(PAcsStereoBuffer16(P)[i].Left + PAcsStereoBuffer16(P)[i].Right)/2;
  end;
  {$R+}

  CalculateSpectrum(pSample, NumSamples, Values);
  Freemem(pSample);

  for i:=0 to ValuesCount-1 do Values[i]:=Values[i] * 0.4; //ValCount;
  FLocked:=False;
end;

end.
