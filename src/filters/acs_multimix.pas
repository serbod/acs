(*
this file is a part of audio components suite,
copyright (c) 2005 ross levis. all rights reserved.
see the license file for more details.

TMultiMixer provides for an unlimited number of inputs (channels)
to be mixed into one audio buffer.  Only supports 44100/16/2

TMultiMixer
 - property TotalChannels: Integer; // get/set the number of channels
 - property Channel[Index: Integer]: TChannel; default
    TChannel
     - procedure Preload; // runs Input.Init to make starting faster (optional)
     - procedure Start; // Start channel
     - procedure Stop; // Stop channel
     - property Input: TAcsInput
     - property Volume: Word; // 0 = silent, 32768 = 100%

eg.
  MultiMixer.TotalChannels := 1;
  MultiMixer[0].Input := VorbisIn1;
  AudioOut1.Run;
  MultiMixer.TotalChannels := 2; // Channels can be added or removed while playing
  MultiMixer[1].Input := WAVEIn1;
  MultiMixer[1].Volume := 16384; // 50% volume
  MultiMixer[1].Start; // while at least 1 mixer is playing, others
  MultiMixer[0].Stop; //  can be started and stopped individually.

*)

{
$Log: acs_multimix.pas,v $
Revision 1.3  2006/07/04 17:12:45  z0m3ie
ACS 2.4 alt wiederhergestellt (unterschiedliche Sampleformate ...)

Revision 1.1  2005/12/19 18:34:35  z0m3ie
*** empty log message ***

Revision 1.3  2005/12/04 16:54:34  z0m3ie
All classes are renamed, Style TAcs... than T... to avoid conflicts with other components (eg TMixer is TAcsMixer now)

Revision 1.2  2005/09/13 21:54:11  z0m3ie
acs is localizeable now (ACS_Strings)

Revision 1.1  2005/09/12 22:04:52  z0m3ie
modified structure again, fileformats are now in an sperat folder.
all File In/Out classes are capsulated from TFileIn and TFileOut

Revision 1.1  2005/08/25 21:02:31  z0m3ie
TMultiMixer by Ross Levis added

}

{$hints off}
unit acs_multimix;

{$ifdef fpc}
{$mode delphi}
{$endif}

interface

uses
  Classes, SysUtils, ACS_Types, ACS_Classes, ACS_Strings;

const
  BUF_SIZE = 8820;

type
  TAcsMultiMixer = class;

  TAcsChannel = class
  private
    FOwner: TAcsMultiMixer;
    FInput: TAcsCustomInput;
    FVolume: Word;
    EndOfInput: Boolean;
    Preloaded: Boolean;
    InBuf: array[1..BUF_SIZE] of Byte;
  public
    constructor Create(AOwner: TAcsMultiMixer); virtual;
    destructor Destroy; override;
    procedure Preload;
    procedure Start;
    procedure Stop;
    property Input: TAcsCustomInput read FInput write FInput;
    property Volume: Word read FVolume write FVolume;
  end;

  TAcsMultiMixer = class(TAcsCustomInput)
  private
    FChannel: array of TAcsChannel;
    FTotalChannels: Integer;
    OutBuf: array[1..BUF_SIZE] of Byte;
    Buisy : Boolean;
    FLock: Boolean;
    function GetChannel(Index: Integer): TAcsChannel;
    procedure SetTotalChannels(Num: Integer);
    function GetBPS : Integer; override;
    function GetCh : Integer; override;
    function GetSR : Integer; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Channel[Index: Integer]: TAcsChannel read GetChannel; default;
    function GetData(Buffer: Pointer; BufferSize: Integer): Integer; override;
    procedure Init; override;
    procedure Flush; override;
  published
    property TotalChannels: Integer read FTotalChannels write SetTotalChannels;
  end;

implementation

  // TChannel

  constructor TAcsChannel.Create;
  begin
    inherited Create;
    FOwner := AOwner;
    FVolume := 32768;
    EndOfInput := True;
    Preloaded := False;
  end;

  destructor TAcsChannel.Destroy;
  begin
    inherited Destroy;
  end;

  procedure TAcsChannel.Preload;
  begin
    if EndOfInput and not Preloaded and Assigned(FInput) then
    begin
      FInput.Init;
      Preloaded := True;
    end;
  end;

  procedure TAcsChannel.Start;
  begin
    if FOwner.Buisy and EndOfInput and Assigned(FInput) then
    begin
      if not Preloaded then Preload;
      EndOfInput := False;
    end;
  end;

  procedure TAcsChannel.Stop;
  begin
    if not EndOfInput then
    begin
      EndOfInput := True;
      Preloaded := False;
      while FOwner.Flock do;
      FOwner.FLock := True;
      FInput.Flush;
      FOwner.Flock := False;
    end;
  end;

  // TAcsMultiMixer

  constructor TAcsMultiMixer.Create;
  begin
    inherited Create(AOwner);
    FLock := False;
  end;

  destructor TAcsMultiMixer.Destroy;
  begin
    SetTotalChannels(0); // free channels
    inherited Destroy;
  end;

  procedure TAcsMultiMixer.SetTotalChannels(Num: Integer);
  var
    chan: Integer;
  begin
    if (Num >= 0) and (Num <> FTotalChannels) then
    begin
      while Flock do;
      FLock := True;
      if Num < FTotalChannels then // remove channels
      begin
        for chan := FTotalChannels-1 downto Num do
        with FChannel[chan] do
        begin
          if not EndOfInput then FInput.Flush;
          Free;
        end;
        SetLength(FChannel,Num);
      end
      else begin // add channels
        SetLength(FChannel,Num);
        for chan := FTotalChannels to Num-1 do
          FChannel[chan] := TAcsChannel.Create(Self);
      end;
      FTotalChannels := Num;
      FLock := False;
    end;
  end;

  function TAcsMultiMixer.GetBPS;
  begin
    Result := 16;
  end;

  function TAcsMultiMixer.GetCh;
  begin
    Result:= 2;
  end;

  function TAcsMultiMixer.GetSR;
  begin
    Result := 44100;
  end;

  procedure TAcsMultiMixer.Init;
  var
    chan: Integer;
  begin
    Buisy := True;
    FPosition := 0;
    for chan := 0 to FTotalChannels-1 do
     FChannel[chan].Start;
    FSize := 0;
    FLock := False;
  end;

  procedure TAcsMultiMixer.Flush;
  var
    chan: Integer;
  begin
    for chan := 0 to FTotalChannels-1 do
    with FChannel[chan] do
    begin
      if Assigned(FInput) then FInput.Flush;
      EndOfInput := True;
      Preloaded := False;
    end;
    Buisy := False;
  end;

  function TAcsMultiMixer.GetData(Buffer: Pointer; BufferSize: Integer): Integer;
  var
    i, chan, ReadSize, BufSize: Integer;
    InBuf16, OutBuf16: PAcsBuffer16;
  begin
    if not Buisy then raise EAcsException.Create(strStreamnotopen);
    begin
      while Flock do sleep(0);
      Flock := True;
      BufSize := 0;
      if BufferSize > BUF_SIZE then BufferSize := BUF_SIZE;
      for chan := 0 to FTotalChannels-1 do
      with FChannel[chan] do
      if not EndOfInput then
      begin
        ReadSize := FInput.GetData(@InBuf[1], BufferSize);
        while (ReadSize < BufferSize) and (ReadSize <> 0) do
        begin
          Result := FInput.GetData(@InBuf[ReadSize+1], BufferSize-ReadSize);
          Inc(ReadSize, Result);
        end;
        FillChar(InBuf[ReadSize+1], BufferSize-ReadSize, 0); // zero rest of buffer
        if ReadSize = 0 then EndOfInput := True
        else if ReadSize > BufSize then BufSize := ReadSize;
      end;
      if BufSize = 0 then
      begin
        Flock := False;
        Result := 0;
        Exit;
      end;
      // mix
      FillChar(OutBuf[1], BufferSize, 0);
      OutBuf16 := @OutBuf;
      for chan := 0 to FTotalChannels-1 do
      with FChannel[chan] do
      if not EndOfInput then
      begin
        InBuf16 := @InBuf;
        for i := 0 to (BufSize shr 1) - 1 do
          OutBuf16[i] := OutBuf16[i] + (InBuf16[i] * FVolume div 32768);
      end;
      Flock := False;
    end;
    Result := BufSize;
    Move(OutBuf[1], Buffer^, Result);
    Inc(FPosition, Result);
  end;

  function TAcsMultiMixer.GetChannel(Index: Integer): TAcsChannel;
  begin
    Result := FChannel[Index];
  end;

end.
