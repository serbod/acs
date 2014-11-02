(*
  this file is a part of audio components suite v 2.3.
  copyright (c) 2002-2005 andrei borovsky. all rights reserved.
  see the license file for more details.
  you can contact me at mail@z0m3ie.de
*)

unit acs_types;

interface

type

  TACSBuffer16 = array[0..0] of SmallInt;
  PACSBuffer16 = ^TACSBuffer16;

  TACSBuffer8 = array[0..0] of Byte;
  PACSBuffer8 = ^TACSBuffer8;

  TACSStereoSample16 = packed record
    Left, Right : SmallInt;
  end;

  TACSStereoBuffer16 = array[0..0] of TACSStereoSample16;
  PACSStereoBuffer16 = ^TACSStereoBuffer16;

  TACSStereoSample8 = packed record
    Left, Right: Byte;
  end;

  TACSStereoBuffer8 = array[0..0] of TACSStereoSample8;
  PACSStereoBuffer8 = ^TACSStereoBuffer8;

  TACSComplex = packed record
    Re, Im: Double;
  end;
  PACSComplex = ^TACSComplex;

  TACSComplexArray = array[0..0] of TACSComplex;
  PACSComplexArray = ^TACSComplexArray;

  TACSDoubleArray = array[0..0] of Double;
  PACSDoubleArray = ^TACSDoubleArray;

  TACSStereoSampleD = record
    Left: Double;
    Right: Double;
  end;

  TACSStereoBufferD = array[0..0] of TACSStereoSampleD;
  PACSStereoBufferD = ^TACSStereoBufferD;

const

  Pi = 3.14159265359;
  TwoPi = 6.28318530718;
  HalfPi = 1.57079632679;

implementation

end.
