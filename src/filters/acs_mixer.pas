(*
  this file is a part of audio components suite v 2.4.
  copyright (c) 2002-2005 andrei borovsky. all rights reserved.
  see the license file for more details.
  you can contact me at mail@z0m3ie.de
*)

unit acs_mixer;

interface

uses
  Classes, SysUtils, ACS_Classes, ACS_Strings
  {$ifdef fpc}
  ,LazUTF8
  {$endif}
  {$IFDEF MSWINDOWS}
  ,MMSystem, Windows, Dialogs
  ,Math
  {$ELSE}
  ,Soundcard, baseunix
  {$ENDIF}
  ;

type
  TAcsMixerChannel = (mcUnknown,
                   mcVolume,
                   mcTreble,
                   mcBass,
                   mcSynth,
                   mcPCM,
                   mcSpeaker,
                   mcLine,
                   mcMic,
                   mcCD,
                   mcIMix,
                   mcAltPCM,
                   mcRecLev,
                   mcDigital,
                   mcMonitor,
                   mcHeadphone,
                   mcTelephone);

  {$IFDEF MSWINDOWS}
const
  FirstSource =    MIXERLINE_COMPONENTTYPE_SRC_UNDEFINED - MIXERLINE_COMPONENTTYPE_SRC_FIRST ;
  LastSource =     MIXERLINE_COMPONENTTYPE_SRC_ANALOG    - MIXERLINE_COMPONENTTYPE_SRC_FIRST ;
  FirstDest  =     MIXERLINE_COMPONENTTYPE_DST_FIRST;
  LastDest   =     MIXERLINE_COMPONENTTYPE_DST_LAST;

type
  {$IFDEF LCL}
  TMixerLine = MIXERLINE;
  TMixerCaps = MIXERCAPS;
  TMixerControl = MIXERCONTROL;
  TMixerLineControls = MIXERLINECONTROLS;
  TMixerControlDetails = MIXERCONTROLDETAILS;
  {$ENDIF}

  TDataArray = ARRAY[FirstSource..LastSource] OF MIXERCONTROLDETAILS_UNSIGNED;
  PDataArray = ^TDataArray;
  PControlEntry = ^TControlEntry;
  TControlEntry = RECORD
    IsInited         : Boolean;
    CHandle          : Thandle;
    CDestination     : INTEGER;
    CID              : INTEGER;
    CName            : String[MIXER_SHORT_NAME_CHARS];
    CConnect         : INTEGER;
    CCControls       : INTEGER;
    CControlTyp      : INTEGER;
    CKanal           : INTEGER;
    CControl         : INTEGER;
    CComponentTyp    : DWORD;
    CMin, Cmax       : INTEGER;
    Cdetails         : TDataArray;
    CMultItems       : INTEGER;
    CcSteps          : DWORD;
  END;
  {$ENDIF}

  TAcsMixerLevel = record
  case Word of
    1:
    (
      Left, Right: Byte;
    );
    2: (Main: Byte;);
  end;

  { TAcsMixer }

  TAcsMixer = class(TComponent)
  private
    FDevNum: Integer;
    FChannels: array of TAcsMixerChannel;
    {$IFDEF LINUX}
    _mix_fd: Integer;
    FFileName: String;
    {$ELSE}
    FMixerHandle: HMixer;
    FMixerCaps: TMixerCaps;
    FControls: array of TControlEntry;
    FMuteControls: array of TControlEntry;
    {$ENDIF}
    FMixerName: String;
    function GetRecSource: Integer;
    function GetVolume(vChannel: integer): TAcsMixerLevel;
    procedure SetVolume(vChannel: integer; vLevel: TAcsMixerLevel);
    procedure SetRecSource(vChannel: integer);
    procedure SetDevNum(Num: Integer);
    function GetChannel(Num: Integer): TAcsMixerChannel;
    function GetDevCount: Integer;
    function GetChannelCount: Integer;
    function GetChannelName(vChannel: Integer): string;
    function GetMute(vChannel: integer): Boolean;
    procedure SetMute(vChannel: integer; Mute: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function IsStereo(vChannel: Integer): Boolean;
    function IsRecordable(vChannel: Integer): Boolean;
    property Channel[vChannel: Integer]: TAcsMixerChannel read GetChannel;
    property Level[vChannel: Integer]: TAcsMixerLevel read GetVolume write SetVolume;
    property Mute[vChannels: Integer]: Boolean read GetMute write SetMute;
    property ChannelName[vChannel: Integer]: string read GetChannelName;
    property RecordSource: Integer read GetRecSource write SetRecSource;
    property DevCount: Integer read GetDevCount;
    property ChannelCount: Integer read GetChannelCount;
  published
    property DevNum: Integer read FDevNum write SetDevNum;
    property MixerName: String read FMixerName;
  end;

var
  MixersCount: Byte;

  function ChannelToStr(ch: TAcsMixerChannel): String;
  function GetMixerName(DevNum: integer): String;

implementation

{$I ACS_Mixer.inc}

function ChannelToStr(ch: TAcsMixerChannel): String;
begin
  case ch of
    mcVolume:  Result:=strMixerVolume;
    mcTreble:  Result:=strMixerTreble;
    mcBass:    Result:=strMixerBass;
    mcSynth:   Result:=strMixerSynth;
    mcPCM:     Result:=strMixerPCM;
    mcSpeaker: Result:=strMixerSpeaker;
    mcLine:    Result:=strMixerLine;
    mcMic:     Result:=strMixerMic;
    mcCD:      Result:=strMixerCD;
    mcIMix:    Result:=strMixerIMix;
    mcAltPCM:  Result:=strMixerAlt;
    mcRecLev:  Result:=strMixerRec;
    mcUnknown: Result:=strMixerUnknown;
    else       Result:=IntToStr(Integer(ch));
  end;
end;

constructor TAcsMixer.Create;
begin
  inherited Create(AOwner);
  //if MixersCount > 0 then SetDevNum(0);
end;

function TAcsMixer.GetChannel(Num: Integer): TAcsMixerChannel;
begin
  if (Num < 0) or (Num > (Length(FChannels)-1)) then
    Result:=mcUnknown
  else
    Result:=FChannels[Num];
end;
  
function TAcsMixer.GetDevCount: Integer;
begin
  Result:=MixersCount;
end;

function TAcsMixer.GetChannelCount: Integer;
begin
  Result:=Length(FChannels);
end;

function TAcsMixer.GetChannelName(vChannel: Integer): string;
begin
  if (vChannel > -1) and (vChannel < ChannelCount) then
    Result:=ChannelToStr(FChannels[vChannel])
  else
    Result:='';
end;

initialization
  MixersCount:=CountMixers;
end.
