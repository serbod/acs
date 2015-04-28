(*
Stream in/out

This file is a part of Audio Components Suite.
Copyright (C) 2002-2005 Andrei Borovsky. All rights reserved.
See the license file for more details.
*)

unit acs_streams;

interface

uses
  Classes, SysUtils, ACS_Classes, ACS_Strings;

const

  OUTBUF_SIZE = $4000;


type

  TAcsStreamOut = class(TAcsStreamedOutput)
  protected
    function GetBPS(): Integer;
    function GetCh(): Integer;
    function GetSR(): Integer;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy(); override;
    function DoOutput(Abort: Boolean): Boolean; override;
    property OutSampleRate: Integer read GetSR;
    property OutBitsPerSample: Integer read GetBPS;
    property OutChannles: Integer read GetCh;
  end;

  TAcsStreamIn = class(TAcsStreamedInput)
  private
    FBPS: Integer;
    FChan: Integer;
    FSampleRate: Integer;
  protected
    function GetBPS: Integer; override;
    function GetCh: Integer; override;
    function GetSR: Integer; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetData(Buffer: Pointer; BufferSize: Integer): Integer; override;
    procedure Init; override;
    procedure Flush; override;
  published
    property InBitsPerSample: Integer read FBPS write FBPS;
    property InChannels: Integer read FChan write FChan;
    property InSampleRate: Integer read FSampleRate write FSampleRate;
  end;


implementation

constructor TAcsStreamOut.Create();
begin
  inherited Create(AOwner);
  FBufferSize:=OUTBUF_SIZE;
end;

destructor TAcsStreamOut.Destroy();
begin
  inherited Destroy();
end;

function TAcsStreamOut.DoOutput(Abort: Boolean): Boolean;
var
  Len: Integer;
begin
  // No exceptions Here
  Result:=False;
  if not Active then Exit;
  if Abort or (not CanOutput) then Exit;
  if not Assigned(FStream) then Exit;

  // copy samples from input
  while InputLock do;
  InputLock:=True;
  //Len:=FInput.GetData(FBuffer.Memory, FBuffer.Size);
  Len:=FInput.GetData(FBuffer);
  InputLock:=False;

  // write samples to stream
  if Len > 0 then
  begin
    FBuffer.Position:=0;
    FStream.CopyFrom(FBuffer, Len);
    Result:=True;
  end;
end;

constructor TAcsStreamIn.Create;
begin
  inherited Create(AOwner);
  FBPS:=8;
  FChan:=1;
  FSampleRate:=8000;
  FSize:=-1;
end;

destructor TAcsStreamIn.Destroy;
begin
  inherited Destroy;
end;

procedure TAcsStreamIn.Init;
begin
  if Busy then
    raise EAcsException.Create(strBusy);
  if not Assigned(FStream) then
    raise EAcsException.Create(strStreamObjectnotassigned);
  FPosition:=FStream.Position;
  FBusy:=True;
  FSize:=FStream.Size;
end;

procedure TAcsStreamIn.Flush;
begin
//  FStream.Position := 0;
  FBusy:=False;
end;

function TAcsStreamIn.GetData(Buffer: Pointer; BufferSize: Integer): Integer;
begin
  Result:=FStream.Read(Buffer^, BufferSize);
  FPosition:=FStream.Position;
  //  Inc(FPosition, Result);
  if FPosition >= FSize then Result:=0;
end;

function TAcsStreamOut.GetSR: Integer;
begin
  if not Assigned(Input) then
    raise EAcsException.Create(strInputnotassigned);
  Result:=FInput.SampleRate;
end;

function TAcsStreamOut.GetBPS: Integer;
begin
  if not Assigned(Input) then
    raise EAcsException.Create(strInputnotassigned);
  Result:=FInput.BitsPerSample;
end;

function TAcsStreamOut.GetCh: Integer;
begin
  if not Assigned(Input) then
    raise EAcsException.Create(strInputnotassigned);
  Result:=FInput.Channels;
end;

function TAcsStreamIn.GetBPS: Integer;
begin
  Result:=Self.FBPS;
end;

function TAcsStreamIn.GetCh: Integer;
begin
  Result:=Self.FChan;
end;

function TAcsStreamIn.GetSR: Integer;
begin
  Result:=Self.FSampleRate;
end;


end.
