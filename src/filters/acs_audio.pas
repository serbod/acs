(*
  this file is a part of audio components suite
  see the license file for more details.
  you can contact me at mail@z0m3ie.de
*)

{
@abstract(this unit introduces the base classes for acs)
@author(Andrei Borovsky (2003-2005))
@author(Christian Ulrich (2005))
}

unit acs_audio;

interface

uses
  acs_types, acs_classes, Classes, ACS_Strings, SysUtils;

const
  DefaultBufferSize = $8000;

type
  { Audio Formats used in DeviceInfo record
    format constants mask : af<SampleRate><Mono/Stereo><BitsPerSample>
    where 1, 2, 4 means sample rate of 11025, 22050, and 44100 Hz respectively
    M, S means mono or stereo, 08, 16 means 8 or 16 bits per sample.
    For example, af4S16 corresponds to 44100 Hz stereo 16 bit format.
  }

  TAcsAudioFormat = (af1M08, af1M16, af1S08, af1S16, af2M08, af2M16, af2S08, af2S16,
                  af4M08, af4M16, af4S08, af4S16);
  TAcsAudioFormats = set of TAcsAudioFormat;
  TAcsBaseAudioOut = class;
  TAcsBaseAudioIn = class;

  { This record is used to get an deviceinfo from the Drivers
  }
  TAcsDeviceInfo = record
    DeviceName: String;
    DrvVersion: LongWord;
    Formats: TAcsAudioFormats;
    Stereo: Boolean;
  end;
  

  { TAcsAudioOut }
  TAcsAudioOut = class(TComponent)
  private
    FDriver: string;
    FOutput: TACSBaseAudioOut;
    FInput: TACSCustomInput;
    FOnDone: TACSOutputDoneEvent;
    FOnProgress: TACSOutputProgressEvent;
    FOnThreadException: TACSThreadExceptionEvent;
    FLatency: Integer;
    FBufferSize: Integer;
    function GetBufferSize: Integer;
    function GetBusy: Boolean;
    function GetDelay: Integer;
    function GetPriority: TTPriority;
    function GetProgress: real;
    function GetStatus: TACSOutputStatus;
    function GetSuspend: Boolean;
    function GetTE: Integer;
    procedure SetBufferSize(const AValue: Integer);
    procedure SetDelay(const AValue: Integer);
    procedure SetPriority(const AValue: TTPriority);
    procedure SetSuspend(const AValue: Boolean);

    procedure ThreadException(Sender: TComponent; E: Exception);
    procedure OutputDone(Sender: TComponent);
    procedure OutputProgress(Sender: TComponent);
  protected
    FBaseChannel: Integer;
    FVolume: Byte;
    
    procedure SetInput(Input: TACSCustomInput);

    procedure SetDevice(Ch: Integer); virtual;
    function GetDeviceInfo: TAcsDeviceInfo; virtual;
    function GetDeviceCount: Integer; virtual;
    procedure SetDriver(Driver: string); virtual;
    procedure SetDefaultDriver();

    function GetDriverName(idx: Integer): string;
    function GetDriversCount: Integer;

    procedure Done;
    function DoOutput(Abort: Boolean):Boolean;
    procedure Prepare;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    { The result returns an deviceinfo record that can be used to enumerate devices
      just set device property from 0 to DeviceCount-1 and read deviceInfo to
      enumerate all Devices from current Driver
    }
    property DeviceInfo: TAcsDeviceInfo read GetDeviceInfo;
    { Returns the count of devices supported by actual driver
    }
    property DeviceCount: Integer read GetDeviceCount;
    { This can be used to enumerate the Drivers
      just use Driverscount as index it returns the DriverName
    }
    property Drivers[idx: Integer]: string read GetDriverName;
    { Returns the total count of avalible drivers
    }
    property DriversCount: Integer read GetDriversCount;
    { pauses the output.
    }
    procedure Pause; virtual;
    { Resumes previously paused output.
    }
    procedure Resume; virtual;
    { This is the most important method in the output components.
      After an input component has been assigned, call Run to start audio-processing chain.
    }
    procedure Run;
    { Stops the running output process.
    }
    procedure Stop;
    { Output components perform output in their own threads.
      Use this property to set the priority for the thread.
    }
    property ThreadPriority:  TTPriority read GetPriority write SetPriority;
    { Read Progress to get the output progress in percents.
      This value is meaningful only after the input component has been set
      and only if the input component can tell the size of its stream.
    }
    property Progress: Real read GetProgress;
    { This property indicates the output component's current status. Possible values are:

      tosPlaying: the component is working;

      tosPaused: the component is paused (the Pause method was called);

      tosIdle: the component is idle;
    }
    property Status: TACSOutputStatus read GetStatus;
    property TimeElapsed: Integer read GetTE;
    property Latency: Integer read FLatency;
  published
    { The output buffer size in bytes default is 4000
    }
    property BufferSize: Integer read GetBufferSize write SetBufferSize;
    { use this property to set an driver, on create of this component the driver
      with lowest latency is used for default
    }
    property Driver: string read FDriver write SetDriver;
    { Use this property to set the output device
    }
    property Busy: Boolean read GetBusy;
    property Device: Integer read FBaseChannel write SetDevice;
    property Volume: Byte read FVolume write FVolume;
    property Input: TACSCustomInput read FInput write SetInput;
    { Use this property to set the delay (in milliseconds) in output thread.
      This property allows the user to reduce the stress the output thread puts
      on the CPU (especially under Windows).
      Be careful with this property when using TAudioOut component.
      Assigning too large values to it can cause dropouts in audio playback.
    }
    property Delay: Integer read GetDelay write SetDelay;
    property SuspendWhenIdle: Boolean read GetSuspend write SetSuspend;
    property OnDone: TACSOutputDoneEvent read FOnDone write FOndone;
    property OnProgress: TACSOutputProgressEvent read FOnProgress write FOnProgress;
    property OnThreadException: TACSThreadExceptionEvent read FOnThreadException write FOnThreadException;
  end;

  { TAcsAudioIn component }

  TAcsAudioIn = class(TACSCustomInput)
  private
    FInput: TACSBaseAudioIn;
    FDriver: string;
    procedure SetDevice(Ch: Integer);
    function GetDeviceInfo: TAcsDeviceInfo;
    function GetDriverName(idx: Integer): string;
    function GetDriversCount: Integer;
    procedure SetDriver(Driver: string);
  protected
    FBPS: Integer;
    FChan: Integer;
    FFreq: Integer;
    FRecTime: Integer;
    FBaseChannel: Integer;
    function GetBPS: Integer; override;
    function GetCh: Integer; override;
    function GetSR: Integer; override;
    function GetTotalTime: Real; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetData(Buffer: Pointer; BufferSize: Integer): Integer; override;
    procedure Init; override;
    procedure Flush; override;
    { The result returns an deviceinfo record that can be used to enumerate devices
      just set device property from 0 to DeviceCount-1 and read deviceInfo to
      enumerate all Devices from current Driver
    }
    property DeviceInfo: TAcsDeviceInfo read GetDeviceInfo;
    { This can be used to enumerate the Drivers
      just use Driverscount as index it returns the DriverName
    }
    property Drivers[idx: Integer]: string read GetDriverName;
    { Returns the total count of avalible drivers
    }
    property DriversCount: Integer read GetDriversCount;
  published
    { use this property to set an driver, on create of this component the driver
      with lowest latency is used for default
    }
    property Driver: string read FDriver write SetDriver stored True;
    { Use this property to set the output device
    }
    property Device: Integer read FBaseChannel write SetDevice stored True;
    { Use this property to set the number of bits per sample for the input audio stream.
      Possible values are 8 and 16.
    }
    property InBitsPerSample: Integer read GetBPS write FBPS stored True;
    { Use this property to set the number of channels for the input audio stream.
      Possible values are 1 (mono) and 2 (stereo).
    }
    property InChannels: Integer read GetCh write FChan stored True;
    { Use this property to set the sample rate for the input audio stream.
      Possible values are determined by the soundcard hardware.
    }
    property InSampleRate: Integer read GetSR write FFreq stored True;
    { This property allow you to set the record duration time in seconds.
      If you assign -1 to this property TAudioIn will never stop recording by itself.
      In both cases you can stop recording at any time by calling Stop method of
      the respective output component.
    }
    property RecTime: Integer read FRecTime write FRecTime stored True;
  end;


  { TBaseAudioOut

    This class is an abstract base class for the drivers
  }
  TAcsBaseAudioOut = class(TACSCustomOutput)
  protected
    FDriver: string;
    FOutput: TAcsAudioOut;
    FBaseChannel: Integer;
    FVolume: Byte;
    procedure SetDevice(Ch: Integer); virtual; abstract;
    function GetDeviceInfo: TAcsDeviceInfo; virtual; abstract;
    function GetDeviceCount: Integer; virtual; abstract;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property DeviceInfo: TAcsDeviceInfo read GetDeviceInfo;
    property DeviceCount: Integer read GetDeviceCount;
    property BufferSize: Integer read FBufferSize write FBufferSize;
  published
    property Device: Integer read FBaseChannel write SetDevice stored True;
    property Volume: Byte read FVolume write FVolume;
  end;


  { TAcsBaseAudioIn

    This class is an abstract base class for the drivers
  }
  TAcsBaseAudioIn = class(TACSCustomInput)
  protected
    FInput: TAcsAudioIn;
    FDriver: string;
    FBPS: Integer;
    FChan: Integer;
    FFreq: Integer;
    FRecTime: Integer;
    FBaseChannel: Integer;
    procedure SetDevice(Ch: Integer); virtual; abstract;
    function GetDeviceInfo: TAcsDeviceInfo; virtual; abstract;
    function GetDeviceCount: Integer; virtual; abstract;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property DeviceInfo: TAcsDeviceInfo read GetDeviceInfo;
  published
    (* Property: DeviceNumber
         Use this property to select the recording device by number. The
         property default value is 0 which corresponds to the default audio
         input device in your system. Valid numbers range from 0 to
         <DeviceCount> - 1. *)
    property Device: Integer read FBaseChannel write SetDevice;
    (* Property: InBitsPerSample
        Use this property to set the number of bits per sample in the audio
        stream the component will provide. Possible values are 8, 16, and 24
        (the last one depends on the capabilities of your hardware). *)
    property InBitsPerSample: Integer read GetBPS write FBPS;
    (* Property: InChannels
        Use this property to set the number of channels in the audio stream
        the component will provide. Possible values are 1 (mono), and 2
        (stereo). *)
    property InChannels: Integer read GetCh write FChan;
    (* Property: InSampleRate
        Use this property to set the sample rate of the audio stream the
        component will provide. Possible values range from 4000 to 128000
        (depends on the capabilities of your hardware). *)
    property InSampleRate: Integer read GetSR write FFreq;
    (* Property: RecTime
         Use this property to set the recording duration (in seconds). If set,
         this property overrides the value of <BytesToRead>. If you set this
         property value to -1 (the default) the component will be endlessly
         recording until you stop it. *)
    property RecTime: Integer read FRecTime write FRecTime;
  end;

  TAcsAudioOutClass = class of TAcsBaseAudioOut;
  TAcsAudioInClass = class of TAcsBaseAudioIn;

  TAcsOutDriverInfo = record
    DriverName: string;
    Latency: Integer;
    DrvClass: TAcsAudioOutClass;
  end;

  TAcsInDriverInfo = record
    DriverName: string;
    Latency: Integer;
    DrvClass: TAcsAudioInClass;
  end;

  { This procedure must be used to register drivers to the system
    just call them at initialization of the driver main unit
  }
  procedure RegisterAudioOut(DrvName: string; OutClass: TAcsAudioOutClass; Latency: Integer);

  { This procedure must be used to register drivers to the system
    just call them at initialization of the driver main unit
  }
  procedure RegisterAudioIn(DrvName: string; InClass: TAcsAudioInClass; Latency: Integer);

var
  OutDriverInfos: array of TAcsOutDriverInfo;
  InDriverInfos: array of TAcsInDriverInfo;

implementation

{ TAudioOut }

function TAcsAudioOut.GetDelay: Integer;
begin
  if Assigned(FOutput) then
    Result:=FOutput.GetDelay
  else
    Result:=-1;
end;

function TAcsAudioOut.GetBufferSize: Integer;
begin
  if Assigned(FOutput) then
    Result:=FOutput.BufferSize
  else
    Result:=-1;
end;

function TAcsAudioOut.GetBusy: Boolean;
begin
  if Assigned(FOutput) then
    Result:=FOutput.Busy
  else
    Result:=False;
end;

function TAcsAudioOut.GetPriority: TTPriority;
begin
  if Assigned(FOutput) then
    Result:=FOutput.GetPriority
  else
    Result:=tpNormal;
    //raise EAcsException(strNoDriverselected);
end;

function TAcsAudioOut.GetProgress: real;
begin
  if Assigned(FOutput) then
    Result:=FOutput.GetProgress
  else
    Result:=0;
    //raise EAcsException.Create(strNoDriverselected);
end;

function TAcsAudioOut.GetStatus: TACSOutputStatus;
begin
  if Assigned(FOutput) then
    Result:=FOutput.Status
  else
    Result:=tosUndefined;
end;

function TAcsAudioOut.GetSuspend: Boolean;
begin
  if Assigned(FOutput) then
    Result:=FOutput.GetSuspend
  else
    Result:=False;
end;

function TAcsAudioOut.GetTE: Integer;
begin
  if Assigned(FOutput) then
    Result:=FOutput.GetTE
  else
    Result:=0;
  //  raise EAcsException.Create(strNoDriverselected);
end;

procedure TAcsAudioOut.SetBufferSize(const AValue: Integer);
begin
  if Assigned(FOutput) then FOutput.BufferSize:=AValue;
  if AValue>0 then FBufferSize:=AValue;
end;

procedure TAcsAudioOut.SetDelay(const AValue: Integer);
begin
  if Assigned(FOutput) then FOutput.SetDelay(AValue);
end;

procedure TAcsAudioOut.SetPriority(const AValue: TTPriority);
begin
  if Assigned(FOutput) then FOutput.SetPriority(AValue);
end;

procedure TAcsAudioOut.SetSuspend(const AValue: Boolean);
begin
  if Assigned(FOutput) then FOutput.SetSuspend(AValue);
end;

procedure TAcsAudioOut.ThreadException(Sender: TComponent; E: Exception);
begin
  if Assigned(FOnThreadException) then
    FOnThreadException(Sender,E);
end;

procedure TAcsAudioOut.OutputDone(Sender: TComponent);
begin
  if Assigned(FOnDone) then FOnDone(Sender);
end;

procedure TAcsAudioOut.OutputProgress(Sender: TComponent);
begin
  if Assigned(FOnProgress) then FOnProgress(Sender);
end;

procedure TAcsAudioOut.SetInput(Input: TACSCustomInput);
begin
  FInput:=Input;
  if Assigned(FOutput) then FOutput.Input:=Input;
end;

procedure TAcsAudioOut.SetDevice(Ch: Integer);
begin
  FBaseChannel:=Ch;
  if Assigned(FOutput) then FOutput.SetDevice(Ch);
end;

function TAcsAudioOut.GetDeviceInfo: TAcsDeviceInfo;
begin
  if Assigned(FOutput) then Result:=FOutput.DeviceInfo;
end;

function TAcsAudioOut.GetDeviceCount: Integer;
begin
  if Assigned(FOutput) then
    Result:=FOutput.GetDeviceCount
  else
    Result:=0;
    //raise EAcsException.Create(strNoDriverselected);
end;

procedure TAcsAudioOut.SetDriver(Driver: string);
var
  i: Integer;
begin
  if Driver = '' then
  begin
    // get default driver
    Exit;
  end;
  if Assigned(FOutput) then FreeAndNil(FOutput);
  for i:=0 to Length(OutDriverInfos)-1 do
  begin
    if OutDriverInfos[i].DriverName = Driver then
    begin
      FOutput:=OutDriverInfos[i].DrvClass.Create(nil);
      try
        FOutput.SetDevice(FBaseChannel);
      except
        FOutput.SetDevice(0);
      end;
      FDriver:=OutDriverInfos[i].DriverName;
      FLatency:=OutDriverInfos[i].Latency;
      if Assigned(FInput) then FOutput.Input:=FInput;
      FOutput.OnDone:=OutputDone;
      FOutput.OnProgress:=OutputProgress;
      FOutput.OnThreadException:=ThreadException;
      if FBufferSize > 0 then FOutput.BufferSize:=FBufferSize;
      Exit;
    end;
  end;
end;

procedure TAcsAudioOut.SetDefaultDriver;
var
  lowestindex, lowest, minlat, i: Integer;
  Done: Boolean;
begin
  minlat:=0;
  Done:=False;

  FDriver := 'No Driver';
  while not Done do
  begin
    lowest:=99999;
    for i:=0 to Length(OutDriverInfos)-1 do
    begin
      if (OutDriverInfos[i].Latency < lowest) and (OutDriverInfos[i].Latency > minlat) then
      begin
        lowest:=OutDriverInfos[i].Latency;
        lowestindex:=i;
      end;
    end;

    Done:=True;
    if lowest < 99999 then
    begin
      try
        SetDriver(OutDriverInfos[lowestindex].DriverName);
      except
        minlat:=lowest+1;
        Done:=False;
      end;
    end;
  end;
end;

function TAcsAudioOut.GetDriverName(idx: Integer): string;
begin
  Result:='';
  if (idx < 0) or (idx > length(OutDriverInfos)-1) then Exit;
  Result:=OutDriverInfos[idx].DriverName;
end;

function TAcsAudioOut.GetDriversCount: Integer;
begin
  Result:=Length(OutDriverInfos);
end;

procedure TAcsAudioOut.Done;
begin
  if Assigned(FOutput) then Foutput.Done;
    //raise EAcsException.Create(strNoDriverselected);
end;

function TAcsAudioOut.DoOutput(Abort: Boolean): Boolean;
begin
  if not Assigned(FOutput) then
    Result:=FOutput.DoOutput(Abort)
  else
    Result:=False;
  //  raise EAcsException.Create(strNoDriverselected);
end;

procedure TAcsAudioOut.Prepare;
begin
  if Assigned(FOutput) then FOutput.Prepare;
  //  raise EAcsException.Create(strNoDriverselected);
end;

constructor TAcsAudioOut.Create(AOwner: TComponent);
//var
  //lowestindex, lowest, minlat, i: Integer;
  //exc: Boolean;
//label retry;
begin
  inherited Create(AOwner);
  FDriver:='';
  FOutput:=nil;
  FInput:=nil;
  { // serbod 2014-10-05
    // dangerous code for constructor
  minlat:=0;
retry:
  lowest:=99999;
  for i:=0 to Length(OutDriverInfos)-1 do
  begin
    if (OutDriverInfos[i].Latency < lowest) and (OutDriverInfos[i].Latency > minlat) then
    begin
      lowest:=OutDriverInfos[i].Latency;
      lowestindex:=i;
    end;
  end;

  if lowest < 99999 then
  begin
    try
      SetDriver(OutDriverInfos[lowestindex].DriverName);
      exc:=false;
    except
      minlat:=lowest+1;
      exc:=true;
    end;
    if exc then
      goto retry;
  end
  else
    FDriver := 'No Driver';
  }
end;

destructor TAcsAudioOut.Destroy;
begin
  FreeAndNil(FOutput);
  inherited Destroy;
end;

procedure TAcsAudioOut.Pause;
begin
  if Assigned(FOutput) then FOutput.Pause;
end;

procedure TAcsAudioOut.Resume;
begin
  if Assigned(FOutput) then FOutput.Resume;
end;

procedure TAcsAudioOut.Run;
begin
  if not Assigned(FOutput) then SetDefaultDriver();
  if Assigned(FOutput) then FOutput.Run;
end;

procedure TAcsAudioOut.Stop;
begin
  if Assigned(FOutput) then FOutput.Stop;
end;

{ TAcsAudioIn }

function TAcsAudioIn.GetBPS: Integer;
begin
  if Assigned(FInput) then Result:=FInput.GetBPS
  else Result:=inherited GetBPS;
end;

function TAcsAudioIn.GetCh: Integer;
begin
  if Assigned(FInput) then Result:=FInput.GetCh
  else Result:=inherited GetCh;
  //  raise EAcsException.Create(strNoDriverselected);
end;

function TAcsAudioIn.GetSR: Integer;
begin
  if Assigned(FInput) then Result := FInput.GetSR
  else Result:=inherited GetSR;
  //  raise EAcsException.Create(strNoDriverselected);
end;

procedure TAcsAudioIn.SetDevice(Ch: Integer);
begin
  if Assigned(FInput) then FInput.SetDevice(Ch);
  //  raise EAcsException.Create(strNoDriverselected);
end;

function TAcsAudioIn.GetDeviceInfo: TAcsDeviceInfo;
begin
  Result.DeviceName:='';
  Result.DrvVersion:=0;
  Result.Formats:=[];
  Result.Stereo:=False;
  if not Assigned(FInput) then Exit;
  //  raise EAcsException.Create(strNoDriverselected);
  //TODO: Complete
end;

function TAcsAudioIn.GetTotalTime: Real;
begin
  if Assigned(FInput) then Result:=FInput.GetTotalTime
  else Result:=inherited GetTotalTime;
  //  raise EAcsException.Create(strNoDriverselected);
end;

function TAcsAudioIn.GetDriverName(idx: Integer): string;
begin
  Result:='';
  if (idx < 0) or (idx > length(InDriverInfos)-1) then Exit;
  Result:=InDriverInfos[idx].DriverName;
end;

function TAcsAudioIn.GetDriversCount: Integer;
begin
  Result:=Length(InDriverInfos);
end;

procedure TAcsAudioIn.SetDriver(Driver: string);
var
  i: Integer;
begin
  if Assigned(FInput) then FreeAndNil(FInput);
  for i:=0 to Length(InDriverInfos)-1 do
  begin
    if InDriverInfos[i].DriverName = Driver then
    begin
      FDriver:=InDriverInfos[i].DriverName;
      FInput:=InDriverInfos[i].DrvClass.Create(nil);
      FInput.SetDevice(FBaseChannel);
      Exit;
    end;
  end;
end;

constructor TAcsAudioIn.Create(AOwner: TComponent);
{
var
  lowestindex, lowest, i: Integer;
  minlat: Integer;
  exc: Boolean;
label retry;
}
begin
  inherited Create(AOwner);
  FInput:=nil;
  FDriver:='';
  { // serbod 2014-10-05
    // dangerous code for constructor
  minlat:=0;
retry:
  lowest := 99999;
  for i := 0 to length(InDriverInfos)-1 do
    if (InDriverInfos[i].Latency < lowest) and (InDriverInfos[i].Latency > minlat) then
    begin
      lowest := InDriverInfos[i].Latency;
      lowestindex := i;
    end;
  if lowest < 99999 then
  begin
    try
      SetDriver(InDriverInfos[lowestindex].DriverName);
      exc := false;
    except
      minlat := lowest+1;
      exc := true;
    end;
    if exc then
      goto retry;
  end
  else
    FDriver := 'No Driver';
  }
end;

destructor TAcsAudioIn.Destroy;
begin
  inherited Destroy;
end;

function TAcsAudioIn.GetData(Buffer: Pointer; BufferSize: Integer): Integer;
begin
  Result:=0;
  if Assigned(FInput) then Result:=FInput.GetData(Buffer,BufferSize);
    //raise EAcsException.Create(strNoDriverselected);
end;

procedure TAcsAudioIn.Init;
begin
  if Assigned(FInput) then FInput.Init;
    //raise EAcsException.Create(strNoDriverselected);
end;

procedure TAcsAudioIn.Flush;
begin
  if not Assigned(FInput) then FInput.Flush;
    //raise EAcsException.Create(strNoDriverselected);
end;

procedure RegisterAudioOut(DrvName: string; OutClass: TAcsAudioOutClass; Latency: Integer);
begin
  SetLength(OutDriverInfos, Length(OutDriverInfos)+1);
  OutDriverInfos[Length(OutDriverInfos)-1].DriverName:=DrvName;
  OutDriverInfos[Length(OutDriverInfos)-1].Latency:=Latency;
  OutDriverInfos[Length(OutDriverInfos)-1].DrvClass:=OutClass;
end;

procedure RegisterAudioIn(DrvName: string; InClass: TAcsAudioInClass; Latency: Integer);
begin
  SetLength(InDriverInfos, Length(InDriverInfos)+1);
  InDriverInfos[Length(InDriverInfos)-1].DriverName:=DrvName;
  InDriverInfos[Length(InDriverInfos)-1].Latency:=Latency;
  InDriverInfos[Length(InDriverInfos)-1].DrvClass:=InClass;
end;

{ TAcsBaseAudioOut }

constructor TAcsBaseAudioOut.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDriver:='';
  FOutput:=nil;
end;

destructor TAcsBaseAudioOut.Destroy;
begin
  inherited Destroy;
end;

{ TBaseAudioIn }

constructor TAcsBaseAudioIn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDriver:='';
  FInput:=nil;
end;

destructor TAcsBaseAudioIn.Destroy;
begin
  inherited Destroy;
end;

initialization

  SetLength(OutDriverInfos, 0);
  SetLength(InDriverInfos, 0);

finalization

  SetLength(OutDriverInfos, 0);
  SetLength(InDriverInfos, 0);

end.
