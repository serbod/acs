(*
this file is a part of audio components suite v 2.4,
copyright (c) 2005 ross levis. all rights reserved.

Provides linear volume in volLeft and volRight in the range 0 (min) to 32767 (max).
Supports 8 and 16 bit samples, mono and stereo.
dbLeft and dbRight returns the volume in decibels.
Delay is in blocks of 50ms and is required due to unknown output buffer size.
Increase Delay to bring VU levels in line with output audio.
Suggest using a 50ms Timer to read values.

$Log: acs_volumequery.pas,v $
Revision 1.2  2006/08/31 20:10:54  z0m3ie
*** empty log message ***

Revision 1.1  2005/12/19 18:34:35  z0m3ie
*** empty log message ***

Revision 1.2  2005/12/04 16:54:34  z0m3ie
All classes are renamed, Style TAcs... than T... to avoid conflicts with other components (eg TMixer is TAcsMixer now)

Revision 1.1  2005/11/27 16:50:33  z0m3ie
add ACS VolumeQuerry
make ACS_VolumeQuerry localizeable
some little errorfixes (buffersize for linuxdrivers was initially 0)
make TAudioIn workable

*)

unit acs_volumequery;

{$ifdef fpc}
{$mode delphi}
{$endif}

interface

uses
   Classes, Math, ACS_Types, ACS_Classes,ACS_Strings;

type

  TAcsVolumeQuery = class(TAcsCustomConverter)
  private
    Lock : Boolean;
    FLeft, FRight: Array of Word;
    FDelay, F50ms: Word;
    FSR,FBPS,FCh: Integer;
  protected
    function GetBPS : Integer; override;
    function GetCh : Integer; override;
    function GetSR : Integer; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function volLeft: Word;
    function volRight: Word;
    function dbLeft: Single;
    function dbRight: Single;
    function GetData(Buffer : Pointer; BufferSize : Integer): Integer; override;
    procedure Init; override;
    procedure Flush; override;
  published
    property Delay: Word read FDelay write FDelay;
  end;

implementation

  constructor TAcsVolumeQuery.Create;
  begin
    inherited Create(AOwner);
  end;

  destructor TAcsVolumeQuery.Destroy;
  begin
    inherited Destroy;
  end;

  function TAcsVolumeQuery.GetBPS : Integer;
  begin
    if not Assigned(FInput) then
    raise EACSException.Create(strInputnotAssigned);
    Result := FInput.BitsPerSample;
  end;

  function TAcsVolumeQuery.GetCh : Integer;
  begin
    if not Assigned(FInput) then
    raise EACSException.Create(strInputnotAssigned);
    Result := FInput.Channels;
  end;

  function TAcsVolumeQuery.GetSR : Integer;
  begin
    if not Assigned(FInput) then
    raise EACSException.Create(strInputnotAssigned);
    Result := FInput.SampleRate;
  end;

  procedure TAcsVolumeQuery.Init;
  begin
    if not Assigned(FInput) then
    raise EACSException.Create(strInputnotAssigned);
    SetLength(FLeft,FDelay+1);
    SetLength(FRight,FDelay+1);
    FillChar(FLeft[0], SizeOf(Word)*(FDelay+1), 0);
    FillChar(FRight[0], SizeOf(Word)*(FDelay+1), 0);
    //
    FBusy := True;
    FInput.Init;
    // calc 50ms worth of data
    FSR := GetSR;
    FBPS := GetBPS;
    FCH := GetCh;
    FPosition := 0;
    //FSize := FInput.Size;
    F50ms := FSR * FBPS * FCh div 160;
    Lock := False;
  end;

  procedure TAcsVolumeQuery.Flush;
  begin
    FInput.Flush;
    FBusy := False;
    Lock := False;
  end;

  function TAcsVolumeQuery.GetData(Buffer: Pointer; BufferSize: Integer): Integer;
  var
    LVol, RVol, LMax, RMax: Word;
    i, NumSamples: Integer;
  begin
    if not Busy then  raise EACSException.Create(strStreamnotopen);
    //if FOrigBufferSize = -1 then FOrigBufferSize := BufferSize
    if BufferSize > F50ms then BufferSize := F50ms;
    while InputLock do;
    InputLock := True;
    Result := FInput.GetData(Buffer, BufferSize);
    InputLock := False;
    FPosition := FInput.Position;
    if Result = 0 then Exit;
    if Lock then Exit;
    Lock := True;
    //
    if FBPS = 8 then
    begin
      if FCh = 1 then NumSamples := Result
      else NumSamples := Result shr 1;
    end
    else begin
      if FCh = 1 then NumSamples := Result shr 1
      else NumSamples := Result shr 2;
    end;
    //
    LMax := 0;
    RMax := 0;
    for i := 0 to NumSamples-1 do
    begin
      if FBPS = 8 then
      begin
        if FCh = 1 then
        begin
          LVol := ABS(PACSBuffer8(Buffer)[i]-127)*256;
          RVol := LVol;
        end
        else begin
          LVol := ABS(PACSStereoBuffer8(Buffer)[i].Left-127)*256;
          RVol := ABS(PACSStereoBuffer8(Buffer)[i].Right-127)*256;
        end;
      end
      else begin
        if FCh = 1 then
        begin
          LVol := ABS(PACSBuffer16(Buffer)[i]);
          RVol := LVol;
        end
        else begin
          LVol := ABS(PACSStereoBuffer16(Buffer)[i].Left);
          RVol := ABS(PACSStereoBuffer16(Buffer)[i].Right);
        end;
      end;
      if LVol > LMax then LMax := LVol;
      if RVol > RMax then RMax := RVol;
    end;
    if FDelay > 0 then
    begin
      Move(FLeft[1],FLeft[0],FDelay*Sizeof(Word));
      Move(FRight[1],FRight[0],FDelay*Sizeof(Word));
    end;
    FLeft[FDelay] := LMax;
    FRight[FDelay] := RMax;
    Lock := False;
  end;

  function TAcsVolumeQuery.volLeft: Word;
  begin
    Lock := True;
    if Busy then Result := FLeft[0]
    else Result := 0;
    Lock := False;
  end;

  function TAcsVolumeQuery.volRight: Word;
  begin
    Lock := True;
    if Busy then Result := FRight[0]
    else Result := 0;
    Lock := False;
  end;

  function TAcsVolumeQuery.dbLeft: Single;
  begin
    Lock := True;
    if Busy then Result := 10 * Log10((FLeft[0]+1)/32768)
    else Result := -96;
    Lock := False;
  end;

  function TAcsVolumeQuery.dbRight: Single;
  begin
    Lock := True;
    if Busy then Result := 10 * Log10((FRight[0]+1)/32768)
    else Result := -96;
    Lock := False;
  end;

end.
