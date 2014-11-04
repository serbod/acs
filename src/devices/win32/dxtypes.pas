{******************************************************************************}
{*                                                                            *}
{*  copyright (c) microsoft corporation.  all rights reserved.                *}
{*                                                                            *}
{*  files:      dxsdkver.h, extracts from various directx sdk include files   *}
{*  content:    directx 9.0 headers common types                              *}
{*                                                                            *}
{*  directx 9.0 delphi / freepascal adaptation by alexey barkovoy             *}
{*  e-mail: directx@clootie.ru                                                *}
{*                                                                            *}
{*  latest version can be downloaded from:                                    *}
{*    http://www.clootie.ru                                                   *}
{*    http://sourceforge.net/projects/delphi-dx9sdk                           *}
{*                                                                            *}
{*----------------------------------------------------------------------------*}
{*  $id: dxtypes.pas,v 1.1 2005/09/12 22:04:53 z0m3ie exp $  }
{******************************************************************************}
{                                                                              }
{ the contents of this file are used with permission, subject to the mozilla   }
{ public license version 1.1 (the "license"); you may not use this file except }
{ in compliance with the license. you may obtain a copy of the license at      }
{ http://www.mozilla.org/mpl/mpl-1.1.html                                      }
{                                                                              }
{ software distributed under the license is distributed on an "as is" basis,   }
{ without warranty of any kind, either express or implied. see the license for }
{ the specific language governing rights and limitations under the license.    }
{                                                                              }
{ alternatively, the contents of this file may be used under the terms of the  }
{ gnu lesser general public license (the  "lgpl license"), in which case the   }
{ provisions of the lgpl license are applicable instead of those above.        }
{ if you wish to allow use of your version of this file only under the terms   }
{ of the lgpl license and not to allow others to use your version of this file }
{ under the mpl, indicate your decision by deleting  the provisions above and  }
{ replace  them with the notice and other provisions required by the lgpl      }
{ license.  if you do not delete the provisions above, a recipient may use     }
{ your version of this file under either the mpl or the lgpl license.          }
{                                                                              }
{ for more information about the lgpl: http://www.gnu.org/copyleft/lesser.html }
{                                                                              }
{******************************************************************************}

{.$I DirectX.inc}

unit dxtypes;

interface


uses windows;

(*==========================================================================;
 *
 *  File:   dxsdkver.h
 *  Content:    DirectX SDK Version Include File
 *
 ****************************************************************************)
const
  _DXSDK_PRODUCT_MAJOR  = 9;
  _DXSDK_PRODUCT_MINOR  = 08;
  _DXSDK_BUILD_MAJOR    = 299;
  _DXSDK_BUILD_MINOR    = 0000;



(****************************************************************************
 *  Other files
 ****************************************************************************)
type
  // TD3DValue is the fundamental Direct3D fractional data type
  D3DVALUE = Single;
  TD3DValue = D3DVALUE;
  PD3DValue = ^TD3DValue;

  D3DCOLOR = DWord;
  TD3DColor = D3DCOLOR;
  PD3DColor = ^TD3DColor;

  _D3DVECTOR = packed record
    x: Single;
    y: Single;
    z: Single;
  end {_D3DVECTOR};
  D3DVECTOR = _D3DVECTOR;
  TD3DVector = _D3DVECTOR;
  PD3DVector = ^TD3DVector;

  REFERENCE_TIME = LONGLONG;
  TReferenceTime = REFERENCE_TIME;
  PReferenceTime = ^TReferenceTime;


// ==================================================================
// Here comes generic Windows types for Win32 / Win64 compatibility
//

  //
  // The INT_PTR is guaranteed to be the same size as a pointer.  Its
  // size with change with pointer size (32/64).  It should be used
  // anywhere that a pointer is cast to an integer type. UINT_PTR is
  // the unsigned variation.
  //
  {$IFDEF WIN64}
  INT_PTR = Int64;
  UINT_PTR = Int64;  // not yet: UInt64;
  LONG_PTR = Int64;
  ULONG_PTR = Int64; // not yet: UInt64;
  DWORD_PTR = Int64; // not yet: UInt64;
  {$ELSE}
  INT_PTR = Longint;
  UINT_PTR = Longword;
  LONG_PTR = Longint;
  ULONG_PTR = Longword;
  DWORD_PTR = Longword;
  {$ENDIF}
  PINT_PTR = ^INT_PTR;
  PUINT_PTR = ^UINT_PTR;
  PLONG_PTR = ^LONG_PTR;
  PULONG_PTR = ^ULONG_PTR;


  //
  // SIZE_T used for counts or ranges which need to span the range of
  // of a pointer.  SSIZE_T is the signed variation.
  //
  SIZE_T = ULONG_PTR;
  SSIZE_T = LONG_PTR;
  PSIZE_T = ^SIZE_T;
  PSSIZE_T = ^SSIZE_T;

implementation

end.

