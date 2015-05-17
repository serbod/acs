(*
  this file is a part of audio components suite v 2.3.
  copyright (c) 2002-2005 andrei borovsky. all rights reserved.
  see the license file for more details.
  you can contact me at mail@z0m3ie.de
*)

unit acs_procs;

interface

uses
  SysUtils, ACS_Types, Math;

type

  TAcsFilterWindowType = (fwHamming, fwHann, fwBlackman);

{$IFDEF LINUX}
  function FindLibs(const Pattern: String): String;
{$ENDIF}


  // Fast Fourier Transformation for Complex array
  // Direction = 1 - forward FFT, Direction = -1 - inverse FFT.
  procedure ComplexFFT(PData: PAcsComplexArray; DataSize, Direction: Integer);

  procedure HannWindow(OutData: PAcsDoubleArray; Width: Integer; Symmetric: Boolean);

  procedure HammingWindow(OutData: PAcsDoubleArray; Width: Integer; Symmetric: Boolean);

  procedure BlackmanWindow(OutData: PAcsDoubleArray; Width: Integer; Symmetric: Boolean);

  procedure CalculateSincKernel(OutData: PAcsDoubleArray; CutOff: Double; Width: Integer; WType: TAcsFilterWindowType);

  // not used
  procedure SmallIntArrayToDouble(InData: PSmallInt; OutData: PDouble; DataSize: Integer);

  // not used
  procedure SmallIntArrayToComplex(InData: PSmallInt; OutData: PAcsComplex; DataSize: Integer);


  // Computes Op2[i] = Op1[i]*Op2[i], i = [0..DataSize-1]
  procedure MultDoubleArrays(Op1, Op2: PDouble; DataSize: Integer);

  (*
    Performs calculation of
                   /
                  | Lg(Abs(InData[i])) + Shift, if Lg(Abs(InData[i])) + Shift >= 0
    OutData[i] = <  0, if Lg(Abs(InData[i])) + Shift < 0
                  | 0, if Abs(InData[i]) = 0.
                   \
    i = [0..DataSize-1]
  *)
  procedure LgMagnitude(InData: PAcsComplexArray; OutData: PAcsDoubleArray; DataSize, Shift: Integer);

implementation

{$IFDEF LINUX}
function FindLibs(const Pattern: String): String;

function FindInPath(sPath: string): string;
var
  SR: TSearchRec;
begin
  Result:='';
  if FindFirst(sPath+Pattern, faAnyFile, SR) = 0 then
  begin
    Result := sPath+SR.Name;
    FindClose(SR);
  end;
end;

begin
  Result:=FindInPath('/usr/lib/');
  if Result = '' then Result:=FindInPath('/usr/lib/i386-linux-gnu/');
  if Result = '' then Result:=FindInPath('/usr/lib/x86_64-linux-gnu/');
  if Result = '' then Result:=FindInPath('/usr/local/lib/');
end;
{$ENDIF}


(* This routine is converted from the original C code by P. Burke
 Direction = 1 - forward FFT, Direction = -1 - inverse FFT. *)
procedure ComplexFFT(PData: PAcsComplexArray; DataSize, Direction: Integer);
var
  i, ii, i1, j, m, n, l, l1, l2, Log2n: Integer;
  c1, c2, tr, ti, t1, t2, u1, u2, z: Double;
  Data: PAcsComplexArray;
begin
  Data:=PData;
  {$R-}
  Log2n:=Trunc(Log2(DataSize));
  // Do the bit reversal
  j:=1;
  for i:=0 to DataSize-1 do
  begin
    if i < j then
    begin
      tr:=Data[i].Re;
      ti:=Data[i].Im;
      Data[i].Re:=Data[j].Re;
      Data[i].Im:= Data[j].Im;
      Data[j].Re:=tr;
      Data[j].Im:=ti;
    end;
    m:=DataSize div 2;
    while (m >=2) and (j > m) do
    begin
      Dec(j, m);
      m:=(m div 2);
    end;
    Inc(j, m);
  end;
  // Compute the FFT
  c1:=-1.0;
  c2:=0.0;
  l2:=1;
  for l:=0 to Log2n-1 do
  begin
    l1:=l2;
    l2:=(l2 shl 1);
    u1:=1.0;
    u2:=0.0;
    for j:=0 to l1-1 do
    begin
      i:=j;
      while i < DataSize do
      begin
        i1:=i+l1;
        t1:=u1*Data[i1].Re - u2*Data[i1].Im;
        t2:=u1*Data[i1].Im + u2*Data[i1].Re;
        Data[i1].Re:=Data[i].Re-t1;
        Data[i1].Im:=Data[i].Im-t2;
        Data[i].Re:=Data[i].Re+t1;
        Data[i].Im:=Data[i].Im+t2;
        Inc(i, l2);
      end;
      z:=u1*c1 - u2*c2;
      u2:=u1*c2 + u2*c1;
      u1:=z;
    end;
    c2:=Sqrt((1.0-c1)/2.0);
    if Direction = 1 then c2:=-c2;
    c1:=Sqrt((1.0+c1)/2.0);
  end;

  // Scaling for forward transform
  if Direction = 1 then
  for i:=0 to DataSize-1 do
  begin
    Data[i].Re:=Data[i].Re/DataSize;
    Data[i].Im:=Data[i].Im/DataSize;
  end;
  {$R+}
end;

procedure HannWindow(OutData: PAcsDoubleArray; Width: Integer; Symmetric: Boolean);
var
  i, n: Integer;
begin
  if Symmetric then n:=Width-1 else n:=Width;
  {$R-}
  for i:=0 to Width-1 do OutData[i]:=(1-Cos(TwoPi*i/n))/2;
  {$R+}
end;

procedure HammingWindow(OutData: PAcsDoubleArray; Width: Integer; Symmetric: Boolean);
var
  i, n: Integer;
begin
  if Symmetric then n:=Width-1 else n:=Width;
  {$R-}
  for i:=0 to Width-1 do OutData[i]:=0.54-0.46*Cos(TwoPi*i/n);
  {$R+}
end;

procedure BlackmanWindow(OutData: PAcsDoubleArray; Width: Integer; Symmetric: Boolean);
var
  i, n: Integer;
begin
  if Symmetric then n:=Width-1 else n:=Width;
  {$R-}
  for i:=0 to Width-1 do OutData[i]:=0.42-0.5*Cos(TwoPi*i/n) + 0.08*Cos(2*TwoPi*i/n);
  {$R+}
end;

procedure CalculateSincKernel(OutData: PAcsDoubleArray; CutOff: Double; Width: Integer; WType: TAcsFilterWindowType);
var
  i: Integer;
  S: Double;
  Window: array of Double;
begin
//    SetLength(OutData, Width);
  SetLength(Window, Width);
  case WType of
    fwHamming: HammingWindow(@Window[0], Width, False);
    fwHann: HannWindow(@Window[0], Width, False);
    fwBlackman: BlackmanWindow(@Window[0], Width, False);
  end;
  S:=0;
  for i:=0 to Width-1 do
  begin
    if i-(Width shr 1) <> 0 then
      OutData[i]:=Sin(TwoPi*CutOff*(i-(Width shr 1)))/(i-(Width shr 1))*Window[i]
    else
      OutData[i]:=TwoPi*CutOff*Window[i];
    S:=S+OutData[i];
  end;
  for i:=0 to Width-1 do OutData[i]:=OutData[i]/S;
end;

procedure SmallIntArrayToDouble(InData: PSmallInt; OutData: PDouble; DataSize: Integer);
begin
  {$IFDEF CPU32}
  asm
              MOV EDX, DataSize;
              SHL EDX, 3;
              MOV ECX, OutData;
              ADD EDX, ECX;
              MOV EAX, InData;
    @test:    CMP EDX, ECX;
              JE @out;
              FILD WORD[EAX];
              ADD EAX, 2;
              FSTP QWORD[ECX];
              ADD ECX, 8;
              JMP @test;
    @out:     ;
  end;
  {$ENDIF}
end;

procedure SmallIntArrayToComplex(InData: PSmallInt; OutData: PAcsComplex; DataSize: Integer);
begin
  {$IFDEF CPU32}
  asm
              MOV EDX, DataSize;
              SHR EDX, 4;
              MOV ECX, OutData;
              ADD EDX, ECX;
              MOV EAX, InData;
    @test:    CMP EDX, ECX;
              JE @out;
              FILD WORD[EAX];
              ADD EAX, 2;
              FSTP QWORD[EAX];
              ADD ECX, 16;
              JMP @test;
    @out:     ;
  end;
  {$ENDIF}
end;

procedure MultDoubleArrays(Op1, Op2: PDouble; DataSize: Integer);
{$ifndef CPU386}
var
  i: integer;
  pd1, pd2: PDouble;
begin
  pd1:=Op1;
  pd2:=Op2;
  for i:=0 to DataSize-1 do
  begin
    pd2^:=pd1^ * pd2^;
    Inc(pd1);
    Inc(pd2);
  end;
end;
{$else}
begin
  asm
              MOV EDX, DataSize;
              SHL EDX, 3;        // DataSize * 8
              MOV ECX, Op1;
              ADD EDX, ECX;      // @End = @Op1 + (DataSize * 8)
              MOV EAX, Op2;
    @test:    CMP EDX, ECX;
              JE @out;           // if @Op1 = @End then Exit
              FLD QWORD[ECX];    // load double from Op1
              FLD QWORD[EAX];    // load double from Op2
              FMUL;              // multiply them
              FSTP QWORD[EAX];   // save result to Op2
              ADD ECX, 8;        // next Op1
              ADD EAX, 8;        // next Op2
              JMP @test;
    @out:     ;
  end;
end;
{$endif}

procedure LgMagnitude(InData: PAcsComplexArray; OutData: PAcsDoubleArray;
  DataSize, Shift: Integer);
{$ifdef CPU386}
var
  LogBase, num: Double;
  pIn: PACSComplex;
  pOut: PDouble;
  i: integer;
begin
  {$R-}
  //pIn:=@InData[0];
  //pOut:=@OutData[0];
  //LogBase:=1/log2(10); // 0.3010299956639812
  for i:=0 to DataSize-1 do
  begin
    OutData[i]:=0;
    num:=sqrt((InData[i].Re * 2) + (InData[i].Im * 2));
    //num:=InData[i]^.Re;
    if num > 0 then
    begin
      num:=log10(num)+Shift;
      if num >= 0 then OutData[i]:=num;
    end;
    //num:=num*log2(LogBase)+Shift;
    //num:=LogBase*log2(num)+Shift;
    //Inc(pIn);
    //Inc(pOut);
  end;
  {$R+}

{$else}

var
  LogBase: Double;
begin
  asm
              FLD1;               // st0 := 1.0
              FLDL2T;             // st1 <- st0, st0 := log2(10)
              FDIVP ST(1), ST(0); // st0 := st1 / st0
              FSTP LogBase;       // LogBase:=1/log2(10)
              MOV EDX, DataSize;
              SHL EDX, 3;
              MOV ECX, OutData;   // cx:=@OutData
              ADD EDX, ECX;       // dx:=@OutData+(DataSize*8)
              MOV EAX, InData;    // ax:=@InData.re
    @test:    CMP EDX, ECX;
              JE @out;
              FLD QWORD[EAX];     // st0 := InData.re
              FMUL ST(0), ST(0);  // st0 := st0 * st0
              ADD EAX, 8;         // ax := InData.im
              FLD QWORD[EAX];     // st1:=st0, st0 := @ax
              FMUL ST(0), ST(0);  // st0 := st0 * st0
              FADDP ST(1), ST(0); // st0 := st0 + st1
              FSQRT;              // st0 := sqrt(st0)
              FTST;               // test st0
              PUSH EAX;           // save ax
              FSTSW AX;           // status -> ax
              SAHF;               // ah -> flags
              JE @skip;           // if 0 goto @skip
              FLD LogBase;        // st1:=st0, st0 := LogBase
              FXCH;               // st0 <-> st1 (st1=LogBase)
              FYL2X;              // st0 := st1 * log2(st0)
              FIADD Shift;        // st0 := st0 + Shift
              FTST;               // test st0
              FSTSW AX;           // status -> ax
              SAHF;               // ah -> flags
              JAE @skip;          // >=0 -> @skip
              FSTP QWORD[ECX];    // st0 -> @cx
              FLDZ;               // st0 := 0
    @skip:    POP EAX;            // load ax
              ADD EAX, 8;         // next InData
              FSTP QWORD[ECX];    // st0 -> @cx
              ADD ECX, 8;         // @cx+8
              JMP @test;          // goto @test
    @out:     ;
  end;
{$endif}
end;


end.
