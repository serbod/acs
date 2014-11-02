(*
  this file is a part of audio components suite v 2.3.
  copyright (c) 2002-2005 andrei borovsky. all rights reserved.
  see the license file for more details.
  you can contact me at mail@z0m3ie.de
*)

unit acs_streams;

interface

uses
  Classes, SysUtils, ACS_Classes, ACS_Strings;

const

  OUTBUF_SIZE = $4000;


type

  TACSStreamOut = class(TACSStreamedOutput)
  private
    function GetBPS: Integer;
    function GetCh: Integer;
    function GetSR: Integer;
  protected
    procedure Done; override;
    function DoOutput(Abort: Boolean): Boolean; override;
    procedure Prepare; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property OutSampleRate: Integer read GetSR;
    property OutBitsPerSample: Integer read GetBPS;
    property OutChannles: Integer read GetCh;
  end;

  TACSStreamIn = class(TACSStreamedInput)
  private
    FBPS: Integer;
    FChan: Integer;
    FFreq: Integer;
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
    property InSampleRate: Integer read FFreq write FFreq;
  end;


implementation

procedure TACSStreamOut.Prepare;
begin
  if not FStreamAssigned then
    raise EACSException.Create(strStreamObjectnotassigned);
  FInput.Init;
end;

procedure TACSStreamOut.Done;
begin
  FInput.Flush;
end;

function TACSStreamOut.DoOutput(Abort: Boolean): Boolean;
var
  Len: Integer;
  P: Pointer;
begin
  // No exceptions Here
  Result:=True;
  if not Busy then Exit;
  if Abort or (not CanOutput) then
  begin
    Result:=False;
    Exit;
  end;
  GetMem(P, OUTBUF_SIZE);
  while InputLock do;
  InputLock:=True;
  Len:=FInput.GetData(P, OUTBUF_SIZE);
  InputLock:=False;
  if Len > 0 then
  begin
    Result:=True;
    FStream.WriteBuffer(P^, Len);
  end
  else Result:=False;
  FreeMem(P);
end;

constructor TACSStreamOut.Create;
begin
  inherited Create(AOwner);
end;

destructor TACSStreamOut.Destroy;
begin
  inherited Destroy;
end;

constructor TACSStreamIn.Create;
begin
  inherited Create(AOwner);
  FBPS:=8;
  FChan:=1;
  FFreq:=8000;
  FSize:=-1;
end;

destructor TACSStreamIn.Destroy;
begin
  inherited Destroy;
end;

procedure TACSStreamIn.Init;
begin
  if Busy then
    raise EACSException.Create(strBusy);
  if not Assigned(FStream) then
    raise EACSException.Create(strStreamObjectnotassigned);
  FPosition:=FStream.Position;
  FBusy:=True;
  FSize:=FStream.Size;
end;

procedure TACSStreamIn.Flush;
begin
//  FStream.Position := 0;
  FBusy:=False;
end;

function TACSStreamIn.GetData(Buffer: Pointer; BufferSize: Integer): Integer;
begin
  Result:=FStream.Read(Buffer^, BufferSize);
  FPosition:=FStream.Position;
  //  Inc(FPosition, Result);
  if FPosition >= FSize then Result:=0;
end;

function TACSStreamOut.GetSR: Integer;
begin
  if not Assigned(Input) then
    raise EACSException.Create(strInputnotassigned);
  Result:=FInput.SampleRate;
end;

function TACSStreamOut.GetBPS: Integer;
begin
  if not Assigned(Input) then
    raise EACSException.Create(strInputnotassigned);
  Result:=FInput.BitsPerSample;
end;

function TACSStreamOut.GetCh: Integer;
begin
  if not Assigned(Input) then
    raise EACSException.Create(strInputnotassigned);
  Result:=FInput.Channels;
end;

function TACSStreamIn.GetBPS: Integer;
begin
  Result:=Self.FBPS;
end;

function TACSStreamIn.GetCh: Integer;
begin
  Result:=Self.FChan;
end;

function TACSStreamIn.GetSR: Integer;
begin
  Result:=Self.FFreq;
end;


end.
