(*
  this file is a part of audio components suite v 2.3.
  copyright (c) 2002-2005 andrei borovsky. all rights reserved.
  see the license file for more details.
  you can contact me at mail@z0m3ie.de
*)

unit acs_types;

interface

type

  // 32 bit
  TAcsBuffer32 = array[0..0] of Longint;
  PAcsBuffer32 = ^TAcsBuffer32;

  TAcsStereoSample32 = packed record
    Left: Longint;
    Right: Longint;
  end;

  TAcsStereoBuffer32 = array[0..0] of TAcsStereoSample32;
  PAcsStereoBuffer32 = ^TAcsStereoBuffer32;

  // 16 bit
  TAcsBuffer16 = array[0..0] of SmallInt;
  PAcsBuffer16 = ^TAcsBuffer16;

  TAcsStereoSample16 = packed record
    Left: SmallInt;
    Right: SmallInt;
  end;

  TAcsStereoBuffer16 = array[0..0] of TAcsStereoSample16;
  PAcsStereoBuffer16 = ^TAcsStereoBuffer16;

  // 8 bit
  TAcsBuffer8 = array[0..0] of Byte;
  PAcsBuffer8 = ^TAcsBuffer8;

  TAcsStereoSample8 = packed record
    Left: Byte;
    Right: Byte;
  end;

  TAcsStereoBuffer8 = array[0..0] of TAcsStereoSample8;
  PAcsStereoBuffer8 = ^TAcsStereoBuffer8;

  // complex
  TAcsComplex = packed record
    Re: Double;
    Im: Double;
  end;
  PAcsComplex = ^TAcsComplex;

  TAcsComplexArray = array[0..0] of TAcsComplex;
  PAcsComplexArray = ^TAcsComplexArray;

  // double
  TAcsDoubleArray = array[0..0] of Double;
  PAcsDoubleArray = ^TAcsDoubleArray;

  TAcsStereoSampleD = record
    Left: Double;
    Right: Double;
  end;

  TAcsStereoBufferD = array[0..0] of TAcsStereoSampleD;
  PAcsStereoBufferD = ^TAcsStereoBufferD;

const

  Pi = 3.14159265359;
  TwoPi = 6.28318530718;
  HalfPi = 1.57079632679;

implementation

end.
