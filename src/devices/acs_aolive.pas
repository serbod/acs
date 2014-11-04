(*
  this file is a part of audio components suite v 2.3.
  copyright (c) 2002-2005 andrei borovsky. all rights reserved.
  see the license file for more details.
  you can contact me at mail@z0m3ie.de
*)

{$Log: acs_aolive.pas,v $
{Revision 1.2  2006/01/01 18:46:40  z0m3ie
{*** empty log message ***
{
{Revision 1.1  2005/12/19 18:36:05  z0m3ie
{*** empty log message ***
{
{Revision 1.2  2005/09/14 21:19:37  z0m3ie
{*** empty log message ***
{
{Revision 1.1  2005/09/13 21:53:45  z0m3ie
{maked seperat driver (not complete jet)
{}

unit acs_aolive;

interface

uses
  Classes,ACS_Classes,ACS_Audio,libao,SysUtils,ACS_Strings,ACS_Types;

const
  LATENCY = 45;//how to measure ?

type

  { TAOLiveAudioOut }

  TAOLiveAudioOut = class(TAcsAudioOutDriver)
  private
    _device : PAODevice;
    FVolume : Byte;
    FDrivers : TStringList;
    FCurrentDriver,
    FDefaultDriver : String;
    function IsDevicePlayable(const Dev : String) : Boolean;
  protected
    procedure Done; override;
    function DoOutput(Abort : Boolean):Boolean; override;
    procedure Prepare; override;
    procedure SetDevice(Ch : Integer);override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

  constructor TAOLiveAudioOut.Create;
  var
    DrList : PPAOInfo;
    DrCount, i : Integer;
    Info : PAOInfo;
  begin
    if not LibaoLoaded then
    raise EACSException.Create(Format(strCoudntloadlib,[LibaoPath]));
    inherited Create(AOwner);
    if AOInitialized = 0 then
    ao_initialize;
    Inc(AOInitialized);
    FDrivers := TStringList.Create;
    DrList := ao_driver_info_list(DrCount);
    for i := 0 to DrCount-1 do
    begin
      if DrList^._type = AO_TYPE_LIVE then
      begin
        FDrivers.Add(String(DrList^.short_name));
      end;
      Inc(DrList);
    end;
    Info := ao_driver_info(ao_default_driver_id);
    FDefaultDriver := Info.short_name;
    FVolume := 255;
  end;

  destructor TAOLiveAudioOut.Destroy;
  begin
    FDrivers.Free;
    if AOInitialized = 1 then
    ao_shutdown;
    Dec(AOInitialized);
    inherited Destroy;
  end;

  procedure TAOLiveAudioOut.Prepare;
  var
    did : Integer;
    sf : ao_sample_format;
    opt : PAOOption;
    Info : PAOInfo;
  begin
    FInput.Init;
    if FCurrentDriver = '' then
    begin
      did := ao_default_driver_id;
      Info := ao_driver_info(did);
      FCurrentDriver := Info.short_name;
    end
    else did := ao_driver_id(@FCurrentDriver[1]);
    opt := nil;
    sf.bits := Finput.BitsPerSample;
    sf.rate := Finput.SampleRate;
    sf.channels := Finput.Channels;
    sf.byte_format := AO_FMT_NATIVE;
    _device := ao_open_live(did, @sf, opt);
    FreeOptionsList(Opt);
    if _device = nil then
    raise EACSException.Create(Format(strDevnotplayable,['+FCurrentDriver+']));
  end;

procedure TAOLiveAudioOut.SetDevice(Ch: Integer);
begin
  if ch < FDrivers.Count-1 then
    if IsDevicePlayable(FDrivers[ch]) then
      FCurrentDriver := FDrivers[ch];
end;

procedure TAOLiveAudioOut.Done;
begin
  Finput.Flush;
  if _device <> nil then
  ao_close(_device);
end;

function TAOLiveAudioOut.DoOutput(Abort : Boolean):Boolean;
var
  Len, i : Integer;
  P : Pointer;
  P1 : PACSBuffer8;
  P2 : PACSBuffer16;
begin
  // No exceptions Here
  Result := True;
  if not CanOutput then Exit;
  Len := 0;
  if Abort then
  begin
    ao_close(_device);
    _device := nil;
    Result := False;
    Exit;
  end;
  try
    P := @FBuffer[0];
    while InputLock do;
    InputLock := True;
    Len := Finput.GetData(P, FBufferSize);
    InputLock := False;
    if FVolume < 255 then
    begin
      if FInput.BitsPerSample = 16 then
      begin
        P2 := @FBuffer[0];
        for i := 0 to (Len shr 1) -1 do
        P2[i] := Round(P2[i]*(FVolume/255));
      end else
      begin
        P1 := @FBuffer[0];
        for i := 0 to Len - 1 do
        P1[i] := Round(P1[i]*(FVolume/255));
      end;
    end;
    ao_play(_device, P, Len);
  except
  end;
  if Len > 0 then Result := True
  else Result := False;
end;

  function TAOLiveAudioOut.IsDevicePlayable(const Dev : String) : Boolean;
  var
    i, did : Integer;
    sf : ao_sample_format;
    opt : PAOOption;
  begin
    Result := True;
    if Dev = '' then Exit;
    if Busy then
    raise EACSException.Create(strBusy);
    for i := 0 to FDrivers.Count-1 do
    if FDrivers.Strings[i] = Dev then
    begin
      did := ao_driver_id(@Dev[1]);
      sf.bits := 16;
      sf.rate := 22050;
      sf.channels := 2;
      sf.byte_format := AO_FMT_NATIVE;
      opt := nil;
      _device := ao_open_live(did, @sf, opt);
      if _device <> nil then
      begin
        ao_close(_device);
        FreeOptionsList(Opt);
        Exit;
      end else Break;
    end;
    Result := False;
  end;

initialization
  RegisterAudioOut('AOlive',TAOLiveAudioOut,LATENCY);

end.

