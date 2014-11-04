(*
  this file is a part of audio components suite.
  see the license file for more details.
  you can contact me at mail@z0m3ie.de
*)

{
@abstract(this unit introduces the base classes for acs)
@author(Andrei Borovsky (2003-2005))
@author(Christian Ulrich (2005))
}

unit acs_classes;

interface

uses

{$IFDEF MSWINDOWS}
  Windows, Dialogs,
{$ENDIF}
  acs_strings, Classes, SysUtils, acs_types;

type

  TAcsOutputStatus = (tosUndefined, tosPlaying, tosPaused, tosIdle);
  TAcsFileOutputMode = (foRewrite = 0, foAppend);
  TAcsOutputFunc = function(Abort: Boolean): Boolean of object;

  //TThreadDoneEvent = procedure of object;

  TAcsThreadExceptionEvent = procedure(Sender: TComponent; E: Exception) of object;
  TAcsHandleThreadException = procedure(E: Exception) of object;
  TAcsOutputDoneEvent = procedure(Sender: TComponent) of object;
  TAcsOutputProgressEvent = procedure(Sender: TComponent) of object;
  
{$IFDEF LINUX}
// File access mask constants
const
  famUserRead = 64;
  famUserWrite = 128;
  famGroupRead = 8;
  famGroupWrite = 16;
  famOthersRead = 1;
  famOthersWrite = 2;
{$ENDIF}

type
{$IFNDEF FPC}
{$IFDEF LINUX}
  TTPriority = Integer;
{$ENDIF}
{$IFDEF MSWINDOWS}
  TTPriority = TThreadPriority;
{$ENDIF}
{$ELSE}
  TTPriority = TThreadPriority;
{$ENDIF}
  {Basic exception class for ACS}
  EAcsException = class(Exception)
  end;

  {Basic Thread class for ACS}

  { TACSThread }

  TAcsThread = class(TThread)
  private
    procedure CallOnProgress;
  public
    Parent: TObject;
    Terminating: Boolean;
    Stop: Boolean;
    HandleException: TACSHandleThreadException;
    Delay: Integer;
    CS: TRTLCriticalSection;
    constructor Create;
    procedure DoPause;
    procedure DoResume;
    procedure Execute; override;
  end;

  TAcsVerySmallThread = class(TThread)
  public
    FOnDone: TACSOutputDoneEvent;
    Sender: TComponent;
    procedure Execute; override;
    procedure CallOnDone;
  end;


  { TACSCustomInput }
  { TACSInput is the base class for all input and converter components. }
  TAcsCustomInput = class(TComponent)
  protected
    FPosition: Integer;
    FSize: Integer;
    FBusy: Boolean;
    BufStart: Integer;
    BufEnd: Integer;
    FBuffer: array of Byte;
    (* We don't declare the buffer size variable here
     because different descendants may need different buffer sizes *)
    function GetBPS: Integer; virtual;
    function GetCh: Integer; virtual;
    function GetSR: Integer; virtual;
    function GetTotalTime: Real; virtual;
    procedure SetBufferSize(AValue: Integer); virtual;
    function GetBufferSize: Integer;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    { This is an abstract method.
      In TACSInput descendands this function reads the audio stream data into the Buffer.
      Returns the number of bytes actually read.
      Returns zero at the end of stream.
      
      Note: usually this method is called internally by the output or converter component
      to which the input component is assigned. You can call this method if you want to
      get direct access to the audio stream. In such a case the sequence of calls should
      look like this:
      
      InputComponent.Init;
      
      InputComponent.GetData(...); // in a loop
      
      InputComponent.Flush;
    }
    function GetData(Buffer: Pointer; BufferSize: Integer): Integer; virtual; abstract;
    { This function is called from output component when the buffer must be filled.
    }
    procedure Reset; virtual;
    { This is an abstract method. In TACSInput descendands it prepares input
      component for reading data.
    }
    procedure Init; virtual; abstract;
    { This is an abstract method. In TACSInput descendands it closes the current input,
      clearing up all temporary structures alocated during data transfer.
      
      Note: usually this method is called internally by the output or converter component to
      which the input component is assigned. You can call this method if you want to get direct
      access to the audio stream.
    }
    procedure Flush; virtual; abstract;
    { Read this property to determine the number of bits per sample in the input audio stream.
      Possible values are 8 and 16.
    }
    property BitsPerSample: Integer read GetBPS;
    property Position: Integer read FPosition;
    { Read this property to get the sample rate (sampling frequency) in Hz
      for the input audio stream.
    }
    property SampleRate: Integer read GetSR;
    { Read Channles to determine the number of channels in the input audio stream.
      Possible values are 1 (mono) and 2 (stereo).
    }
    property Channels: Integer read GetCh;
    { Read Size to determine the size of the input stream in bytes.
    }
    property Size: Integer read FSize;
    { Read this property to get the total time of the input stream in seconds.
      If the total time cannot be determined this property contains 0.
    }
    property TotalTime: Real read GetTotalTime;
    { This property sets the buffersize of the component
    }
    property BufferSize: Integer read GetBufferSize write SetBufferSize;
    property Busy: Boolean read FBusy;
  end;


  { TACSCustomOutput }
  { TACSOutput is the base class for all ACS output components. }
  TACSCustomOutput = class(TComponent)
  protected
    CanOutput: Boolean;
    CurProgr: real;
    Thread: TACSThread;
    FInput: TACSCustomInput;
    FOnDone: TACSOutputDoneEvent;
    FOnProgress: TACSOutputProgressEvent;
    Busy: Boolean;  // Set to true by Run and to False by WhenDone.
    FOnThreadException: TACSThreadExceptionEvent;
    InputLock: Boolean;
    FBufferSize: Integer;
    FBuffer: PAcsBuffer8;
    //FBuffer: array of Byte;

    { Read data from Input into Buffer, return bytes read
      AEndOfInput set to True if end of input buffer is reached }
    function FillBufferFromInput(var AEndOfInput: Boolean): Integer;
    function GetPriority: TTPriority;
    function GetSuspend: Boolean;
    function GetProgress: Real;
    procedure SetInput(AInput: TACSCustomInput); virtual;
    procedure SetPriority(Priority: TTPriority);
    procedure SetSuspend(v: Boolean);
    procedure WhenDone;
    function GetTE: Integer;
    function GetStatus: TACSOutputStatus;
    function GetDelay: Integer;
    procedure SetDelay(Value: Integer);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure HandleThreadException(E: Exception);
    procedure SetBufferSize(AValue: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Prepare; virtual; abstract; // Calls FInput.init
    function DoOutput(Abort: Boolean): Boolean; virtual; abstract;
    procedure Done; virtual; abstract; // Calls FInput.Flush
    {$IFDEF MSWINDOWS}
    procedure Abort;
    {$ENDIF}
    { pauses the output (the output thread is suspended).
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
    { Use this property to set the delay (in milliseconds) in output thread.
      This property allows the user to reduce the stress the output thread puts
      on the CPU (especially under Windows).
      Be careful with this property when using TAudioOut component.
      Assigning too large values to it can cause dropouts in audio playback.
    }
    property Delay: Integer read GetDelay write SetDelay;
    { Output components perform output in their own threads.
      Use this property to set the priority for the thread.
    }
    property ThreadPriority: TTPriority read GetPriority write SetPriority;
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
  published
    { This property allows you to set the input component for the output component.
      The valid input components must be descendants of TACSInput.
    }
    property Input: TACSCustomInput read FInput write SetInput;
    { If this property is set to True the output thread suspends every time the
      output component becomes inactive, otherwise the thread executes permanently.
      This property is introduced to simplify the debugging process ander
      Kylix IDE. Output components perform output in their own threads.
      The signals received by multi-threaded applications under IDE can cause problems.
      Set this property to False when debugging your application
      and to True in the final release.
    }
    property SuspendWhenIdle: Boolean read GetSuspend write SetSuspend;
    { This event is invoked when the output is finished.

      Note: don't try to start other output job before OnDone event from the
      previous job is triggered. An attempt to do so will cause the
      "Component is buisy" exception.
      It is a good practice to desable output-starting controls in the
      method where the output component's Run method is called and to enable
      them in the output component's OnDone event handler.
    }
    property OnDone: TACSOutputDoneEvent read FOnDone write FOndone;
    { This event is invoked every time the Progress property value changes.
      Be careful when referencing GUI interface components in OnProgress event's handler.
    }
    property OnProgress: TACSOutputProgressEvent read FOnProgress write FOnProgress;
    { Thread procedure should never terminate.
      For this reason all the exceptions that might arise in the thread
      procedure are caught within the procedure itself.
      The thread generates OnThreadException event to inform the application
      that an exception has occurred.
      Avoid any potential exception-rising actions in this eventís handler!
      Use the handler to reset application controls (if needed) and call the
      input componentís Reset method.
      See the CD ripper demo for an example of handling thread exceptions.
    }
    property OnThreadException: TACSThreadExceptionEvent read FOnThreadException write FOnThreadException;
    { This Property sets the BufferSize of the component
    }
    property BufferSize: Integer read FBufferSize write SetBufferSize;
  end;

  { TACSStreamedInput introduces Stream property.
    This property allows input components that descend from TACSStreamedInput
    to get data not only from files on disc but from any kind of stream.
    The descendats of this class are TACSFileIn (and all its descendats except TMACIn),
    and TStreamIn component that is designed to read raw audio data from streams.
  }
  TACSStreamedInput = class(TACSCustomInput)
  protected
    FStream: TStream;
    FStreamAssigned: Boolean;
    FSeekable: Boolean;
    procedure SetStream(AStream: TStream);
  public
    { If you write network applications with ACS, you are encouraged to use custom
      streams as it is described in programmer's introduction
      (see also OggStream demo).

      Usually these custom streams will not be seekable.
      Unfortunately TStream class has no way to indicate it is not seekable
      (all standart TStream descendats are seekable).
      So this property has been added to allow you to specify if the stream is seekable or not.
      The default value of this property is True, and if the stream you're planning
      to use with this component is not seekable, you have to set Seekable to False.
      Note that if the stream is not seekable, such properties as Channels, SampleRate,
      BitsPerSample, and Valid will not return correct values until the input
      component starts performing the actual playback.
    }
    property Seekable: Boolean read FSeekable write FSeekable;
    { Use this property to set the input stream for the input component.
      Remember that you have to create, destroy and position the input stream explicitly.
      In TACSFileIn descendants the stream assigned to this property takes over
      the FileName property, i. e. if both Stream and FileName property are assigned,
      the stream and not the file will be used for the actual input.
      To unassign this property set it to nil.
      If the stream is seekable it will be reset to the beginning at the end of the playback.
    }
    property Stream: TStream read FStream write SetStream;
    constructor Create(AOwner: TComponent); override;
  end;

  { TACSStreamedOutput introduces Stream property.
    This property allows output components that descend from TACSStreamedOutput
    to store data not only to files on disc but to any kind of stream as well.
  }
  TACSStreamedOutput = class(TACSCustomOutput)
  protected
    { Use this property to set the output stream for the corresponding output component.
      Remember that you have to create, destroy and position the input stream explicitly.
      In file-handling TACSStreamedOutput descendants the stream assigned to this property
      takes over the FileName property, i. e. if both Stream and FileName property are assigned,
      the stream and not the file will be used for the actual output.
      To unassign this property set it to nil.
    }
    FStream: TStream;
    FStreamAssigned: Boolean;
    procedure SetStream(AStream: TStream);
  public
    property Stream: TStream read FStream write SetStream;
  end;
  
  { This class introduces an simple handler for an file tag it can hold any streamable
    piece of data or an string
  }
  TACSFileTag = class
  private
    function GetName: string; virtual;
  public
    property Name: string read GetName;
    function AsString: string; virtual;
    function Streamable: Boolean; virtual;
    procedure SaveToStream(Stream: TStream); virtual;
    procedure LoadFromStream(Stream: TStream); virtual;
  end;
  
  { This class introduces an base class for file tag lists
  }

  { TACSFileInfo }

  TACSFileInfo = class
  private
    procedure ReadFromFile; virtual;
    procedure SetStringTag(Idx: string; const AValue: TACSFileTag);
    function GetStringTag(Idx: string): TACSFileTag;
  public
    procedure SaveToFile; virtual;
    property Tags[Idx: string]: TACSFileTag read GetStringTag write SetStringTag;
  end;
  

  { TACSCustomFileIn }
  { TACSFileIn is the base class for all input components that read data from files
    (or from other streams in the corresponding file format).
    It introduces such properties as FileName, StartSample, EndSample, Loop,
    and Valid and Jump, Seek, SetStartTime, SetEndTime methods.
  }
  TACSCustomFileIn = class(TACSStreamedInput)
  protected
    FFileName: TFileName;
    FOffset: Real;
    FOpened: Integer;
    FValid: Boolean;
    FBPS, FSR, FChan: Integer;
    FTime: Integer;
    FLoop: Boolean;
    FStartSample: Integer;
    FEndSample: Integer;
    FTotalSamples: Integer;
    function GetBPS: Integer; override;
    function GetCh: Integer; override;
    function GetSR: Integer; override;
    function GetTime: Integer;
    function GetValid: Boolean;
    (* Note on FSize calculation:
      FSize is calculated in OpenFile method as the FULL file size.
      More precise calculations regarding StartSample/EndSample are done in Init. *)
    procedure OpenFile; virtual;
    procedure CloseFile; virtual;
    function GetTotalTime: Real; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Reset; override;
    procedure Flush; override;
    procedure Init; override;
    function Seek(SampleNum: Integer): Boolean; virtual; abstract;
    function SetStartTime(Minutes, Seconds: Integer): Boolean;
    function SetEndTime(Minutes, Seconds: Integer): Boolean;
    procedure Jump(Offs: Real);
    { Read this property to determine the total playing time of the file in seconds.
    }
    property Time: Integer read GetTime;
    { Use this property to read the total number of samples in the file/stream assigned
      to the component. The sample in this context means the minimum playable
      portion of audio data. For mono 8-bit sound 1 sample = 1 byte,
      while for stereo 16-bit sound 1 sample = 4 bytes.
    }
    property TotalSamples: Integer read FTotalSamples;
    { Read this property to determine if the file is valid.
      It is a good practice to check this property before performing other
      operations on audio stream.
    }
    property Valid: Boolean read GetValid;
  published
    { Use this property to set the number of the end sample in the file that
      the playback should stop at.
      The value of -1 (default) tells the component to play the file up to the end.
      The sample in this context means the minimum playable portion of audio data.
      For mono 8-bit sound 1 sample = 1 byte,
      while for stereo 16-bit sound 1 sample = 4 bytes.
      You can get the total number of samples in the file by reading TotalSamples property.

      Note that in some cases (for example, with ADPCM files) sample positioning is not quite precise.
    }
    property EndSample: Integer read FEndSample write FEndSample;
    { Use this property to set the name of the file to read data from.
    }
    property FileName: TFileName read FFileName write FFileName stored True;
    { The default value of Loop is False. If this property is set to True,
    the file playback will be looped, in other words the file will start playing
    again right after it is finished. Note, that if Loop is set to True, it will not
    present end conditions to the corresponding output component.
    You can stop the looped input component either by setting Loop to False,
    or by calling Stop method of the corresponding output component.
    }
    property Loop: Boolean read FLoop write FLoop;
    { Use this property to set the number of the sample in the file that the
      playback should start from. The value of 0 (default) tells the component
      to play the file from the beginning.
      The sample in this context means the minimum playable portion of audio data.
      For mono 8-bit sound 1 sample = 1 byte, while for stereo 16-bit sound 1 sample = 4 bytes.
      You can get the total number of samples in the file by reading TotalSamples property.

      Note that in some cases (for example, with ADPCM files) sample positioning is not quite precise.
    }
    property StartSample: Integer read FStartSample write FStartSample;
  end;

  TACSCustomFileOut = class(TACSStreamedOutput)
  protected
    FFileName: TFileName;
    FFileMode: TACSFileOutputMode;
    FAccessMask: Integer;
    procedure SetFileMode(AMode: TACSFileOutputMode); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    { Use this property to set the file access mask. The default value is $1B6,
    which corresponds to rw-rw-rw- access mask.
    }
    property AccessMask: Integer read FAccessMask write FAccessMask;
  published
    { There are two possible values for this property: foRewrite and foAppend.
      When foRewrite (default value) is set and the file with the specified
      name already exists (or a stream is assigned) the contents of the file
      (stream) is rewritten.

      In the foAppend mode the new data is appended to the end of the existing
      file (stream).
      Currently only TWaveOut and TVorbisOut components support foAppend mode.
    }
    property FileMode: TACSFileOutputMode read FFileMode write SetFileMode;
    { Use this property to set the name of the file to save data to.
    }
    property FileName: TFileName read FFileName write FFileName;
  end;

  TACSCustomConverter = class(TACSCustomInput)
  protected
    InputLock: Boolean;
    FInput: TACSCustomInput;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetInput(AInput: TACSCustomInput); virtual;
  public
    procedure UnlockInput;
  published
    property Input: TACSCustomInput read FInput write SetInput;
  end;

const

  STREAM_BUFFER_SIZE = $80000;

type

  // Circular buffer with TStream interface

  TACSBufferMode = (bmBlock, bmReport);

  TACSBufferStream = class(TStream)
  private
    Buff: array[0..STREAM_BUFFER_SIZE-1] of Byte;
    ReadCur: Integer;
    WriteCur: Integer;
    FBufferMode: TACSBufferMode;
//    fBreak: Boolean;
    FBytesInBuffer: Integer;
    BlockEventName: String;
//    LockEventName: String;
    FBytesRead: Integer;
    {$IFDEF MSWINDOWS}
    BlockEvent: THandle;
    CS: TRTLCriticalSection;
    {$ENDIF}
  public
    constructor Create;
    destructor Destroy; override;
    function Read(var Buffer; Count: Longint): Longint; override;
    procedure Reset;
    function Seek(Offset: Longint; Origin: Word): Longint; overload; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; overload; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    property BytesInBuffer: Integer read FBytesInBuffer;
    property BufferMode: TACSBufferMode read FBufferMode write FBufferMode;
  end;



implementation

constructor TACSCustomInput.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPosition:=0;
end;

destructor TACSCustomInput.Destroy;
begin
  inherited Destroy;
end;

procedure TACSThread.Execute;
var
  DoneThread: TACSVerySmallThread;
  ParentComponent: TACSCustomOutput;
  Res: Boolean;
begin
  ParentComponent:=TACSCustomOutput(Parent);
  //    if not bSuspend then
  while not Terminating do
  begin
    EnterCriticalSection(CS);
    if Delay > 5 then Sleep(Delay);
    try
      if ParentComponent.Progress <> ParentComponent.CurProgr then
      begin
        ParentComponent.CurProgr:=ParentComponent.Progress;
        if Assigned(ParentComponent.FOnProgress) then Synchronize(CallOnProgress);
      end;
      Res:=ParentComponent.DoOutput(Stop);
      if Stop or (not Res) then
      begin
        Stop:=False;
        ParentComponent.WhenDone;
        // This ensures that OnDone event is called outside this thread
        DoneThread:=TACSVerySmallThread.Create(True);
        DoneThread.Sender:=ParentComponent;
        DoneThread.FOnDone:=ParentComponent.FOnDone;
        DoneThread.FreeOnTerminate:=True;
        Stop:=True;
        Res:=True;
        DoneThread.Suspended:=False;
        //Dont Suspend the thread maybe Stop has been set to false during OnDone
        //check this first
        if (Stop or (not Res)) then Self.Suspended:=True;
      end;
    except
      on E: Exception do
      begin
        Stop:=False;
        HandleException(E);
        //if bSuspend then
        Self.Suspended:=True;
      end;
    end;
    LeaveCriticalSection(CS);
  end;
  //DoOutput(True);  // Why I'm doing this? I don't remember :-)
  Terminating:=False;
end;

constructor TACSCustomOutput.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Thread:=TACSThread.Create;
  Thread.Parent:=Self;
//  Thread.DoOutput:=Self.DoOutput;
//  Thread.FOnDone:=Self.WhenDone;
  Thread.FreeOnTerminate:=True;
  Thread.HandleException:=HandleThreadException;
  {$IFDEF MSWINDOWS}
  SetSuspend(True);
  InitializeCriticalSection(Thread.CS);
  {$ENDIF}
end;

destructor TACSCustomOutput.Destroy;
begin
//  if Thread.Suspended then
//  Thread.Resume;
  if not Thread.Suspended then
  Thread.Terminating:=True;
  while Thread.Terminating do;
  {$IFDEF MSWINDOWS}
  DeleteCriticalSection(Thread.CS);
  {$ENDIF}
  inherited Destroy;
end;

procedure TACSCustomOutput.WhenDone;
begin
  if not Busy then Exit;
  CanOutput:=False;
  Done;
  Busy:=False;
end;

procedure TACSCustomOutput.Run;
begin
  if Busy then
    raise EAcsException.Create(strBusy);
  if not Assigned(FInput) then
    raise EAcsException.Create(strInputnotAssigned);
  InputLock:=False;
  Thread.Suspended:=True;
  try
    Prepare;
    Busy:=True;
    Thread.Stop:=False;
    CanOutput:=True;
    Thread.Suspended:=False;
  except
    on E: Exception do HandleThreadException(E);
  end;
end;

procedure TACSCustomOutput.Stop;
begin
  Thread.Stop:=True;
end;

function TACSCustomOutput.GetStatus: TACSOutputStatus;
begin
  if Busy then
  begin
    if Self.Thread.Suspended then
      Result:=tosPaused
    else
      Result:=tosPlaying;
  end
  else
    Result:=tosIdle
end;

procedure TACSCustomOutput.SetPriority(Priority: TTPriority);
begin
  Thread.Priority:=Priority;
end;

function TACSCustomOutput.FillBufferFromInput(var AEndOfInput: Boolean
  ): Integer;
var
  n: Integer;
begin
  Result:=0;
  if not Assigned(FInput) then Exit;
  while Result < FBufferSize do
  begin
    n:=FInput.GetData(@FBuffer^[Result], FBufferSize-Result);
    if n = 0 then
    begin
      AEndOfInput:=True;
      Break;
    end;
    Inc(Result, n);
  end;
end;

function TACSCustomOutput.GetPriority: TTPriority;
begin
  Result:=Thread.Priority;
end;

procedure TACSCustomOutput.SetInput(AInput: TACSCustomInput);
var
  OldInput, NewInput: TACSCustomInput;
begin
  if Busy then
  begin
    NewInput:=AInput;
    NewInput.Init;
    OldInput:=FInput;
    while InputLock do;
    InputLock:=True;
    FInput:=NewInput;
    InputLock:=False;
    OldInput.Flush;
  end
  else
    FInput:=AInput;
end;

function TACSCustomOutput.GetProgress: Real;
begin
  if not Assigned(FInput) then
  begin
    Result:=0;
    Exit;
  end;
  case Finput.Size of
    0: Result:=0;
    -1: Result:=-1;
    else Result:=(FInput.Position/FInput.Size)*100;
  end;
end;

procedure TACSCustomOutput.Pause;
begin
  Thread.DoPause;
end;

procedure TACSCustomOutput.Resume;
begin
  Thread.DoResume;
end;

function TACSCustomOutput.GetSuspend: Boolean;
begin
  Result:=Thread.Suspended;
end;

procedure TACSCustomOutput.SetSuspend(v: Boolean);
begin
  Thread.Suspended:=v;
end;

constructor TACSStreamedInput.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FStream:=nil;
  FSeekable:=True;
end;

function TACSCustomFileIn.GetBPS: Integer;
begin
  if FSeekable then
  begin
    { TODO : Optimize }
    OpenFile;
    Result:=FBPS;
    CloseFile;
  end
  else Result:=FBPS;
end;

function TACSCustomFileIn.GetCh: Integer;
begin
  if FSeekable then
  begin
    OpenFile;
    Result:=FChan;
    CloseFile;
  end
  else Result:=FChan;
end;

function TACSCustomFileIn.GetSR: Integer;
begin
  if FSeekable then
  begin
    OpenFile;
    Result:=FSR;
    CloseFile;
  end
  else Result:=FSR;
end;

function TACSCustomFileIn.GetTime: Integer;
begin
  if FSeekable then
  begin
    OpenFile;
    Result:=FTime;
    CloseFile;
  end
  else Result:=FTime;
end;

function TACSCustomFileIn.GetValid: Boolean;
begin
  Result:=False;
  if (not FStreamAssigned) and (FileName = '') then Exit;
  if FSeekable then
  begin
    OpenFile;
    Result:=FValid;
    CloseFile;
  end
  else Result:=FValid;
end;

procedure TACSCustomFileIn.OpenFile;
begin
  FSize:=0;
end;

procedure TACSCustomFileIn.CloseFile;
begin

end;

procedure TACSCustomFileIn.Init;
begin
  if Busy then raise EAcsException.Create(strBusy);
  if not FStreamAssigned then
    if FFileName = '' then raise EAcsException.Create(strFilenamenotassigned);
  OpenFile;
  if StartSample <> 0 then Seek(StartSample);
  if (StartSample <> 0) or (FEndSample <> -1) then
  begin
    FSize:=FEndSample-FStartSample;
    if FEndSample = -1 then FSize:=FSize+FTotalSamples+1;
    FSize:=FSize*(BitsPerSample shr 3)*FChan;
  end;
  FBusy:=True;
  BufStart:=1;
  BufEnd:=0;
  FPosition:=0;
end;

procedure TACSCustomFileIn.Flush;
begin
  CloseFile;
  FBusy:=False;
end;

procedure TACSCustomFileIn.Jump(Offs: Real);
begin
  FOffset:=Offs;
end;

function TACSCustomOutput.GetTE: Integer;
begin
   if not Assigned(FInput) then
   Result:=0
   else
   Result:=Round(FInput.Position / ((FInput.BitsPerSample shr 3) * FInput.Channels * FInput.SampleRate));
end;

function TACSCustomOutput.GetDelay: Integer;
begin
  if Assigned(Thread) then Result:=Thread.Delay
  else Result:=0;
end;

procedure TACSCustomOutput.SetDelay(Value: Integer);
begin
  if Assigned(Thread) then
  begin
    if Value <= 100 then Thread.Delay:=Value;
  end;
end;

function TACSCustomInput.GetBPS: Integer;
begin
  Result:=-1;
end;

function TACSCustomInput.GetCh: Integer;
begin
  Result:=-1;
end;

function TACSCustomInput.GetSR: Integer;
begin
  Result:=-1;
end;

function TACSCustomInput.GetTotalTime: Real;
begin
  Result:=0;  // Default result for the streams.
end;

procedure TACSCustomInput.SetBufferSize(AValue: Integer);
begin
  if Busy then
    raise EAcsException.Create(strBusy);
  if AValue >= 0 then SetLength(FBuffer, AValue);
end;

function TACSCustomInput.GetBufferSize: Integer;
begin
  Result:=Length(FBuffer);
end;

function TACSCustomFileIn.GetTotalTime: Real;
begin
  OpenFile;
  Result:=0;
  if (SampleRate = 0) or (Channels = 0) or (BitsPerSample = 0) then
  else
    Result:=Size/(SampleRate*Channels*(BitsPerSample shr 3));
  CloseFile;
end;

procedure TACSStreamedInput.SetStream(AStream: TStream);
begin
  FStream:=AStream;
  FStreamAssigned:=Assigned(FStream);
end;

procedure TACSStreamedOutput.SetStream(AStream: TStream);
begin
  FStream:=AStream;
  FStreamAssigned:=Assigned(FStream);
end;

procedure TACSCustomOutput.Notification(AComponent: TComponent; Operation: TOperation);
begin
  // Remove the following two lines if they cause troubles in your IDE
  if (AComponent = FInput) and (Operation = opRemove) then Input:=nil;
  inherited Notification(AComponent, Operation);
end;

procedure TACSCustomInput.Reset;
begin
  try
    Flush;
  except
  end;
  FBusy:=False;
end;

procedure TACSCustomOutput.HandleThreadException(E: Exception);
var
  Conv: TACSCustomConverter;
begin
 InputLock:=False;
 if Status <> tosIdle then
 begin
   try
    if FInput is TACSCustomConverter then
    begin
      Conv:=(FInput as TACSCustomConverter);
      Conv.UnlockInput;
    end;
   except
   end;
   try
     Done;
   except
   end;
 end;
 CanOutput:=False;
 Busy:=False;
 if Assigned(FOnThreadException) then FOnThreadException(Self, E);
end;

procedure TACSCustomOutput.SetBufferSize(AValue: Integer);
begin
  if Busy then
    raise EAcsException.Create(strBusy);
  if AValue > 0 then FBufferSize:=AValue;
end;


procedure TACSCustomFileIn.Reset;
begin
  inherited Reset;
  FOpened:=0;
end;


constructor TACSCustomFileOut.Create;
begin
  inherited Create(AOwner);
  {$IFDEF LINUX}
  FAccessMask := $1B6; // rw-rw-rw-
  {$ENDIF}
end;

procedure TACSCustomFileOut.SetFileMode;
begin
  FFileMode:=foRewrite;
end;

procedure TACSCustomConverter.Notification;
begin
  // Remove the following two lines if they cause troubles in your IDE
  if (AComponent = FInput) and (Operation = opRemove) then Input:=nil;
  inherited Notification(AComponent, Operation);
end;

procedure TACSCustomConverter.SetInput(AInput: TACSCustomInput);
var
  OldInput, NewInput: TACSCustomInput;
begin
  if AInput = Self then Exit;
  if Busy then
  begin
    NewInput:=AInput;
    NewInput.Init;
    OldInput:=FInput;
    while InputLock do;
    InputLock:=True;
    FInput:=NewInput;
    InputLock:=False;
    OldInput.Flush;
  end
  else
    FInput:=AInput;
end;

procedure TACSCustomConverter.UnlockInput;
var
  Conv: TACSCustomConverter;
begin
  InputLock:=False;
  if Assigned(FInput) then
  if (FInput is TACSCustomConverter) then
  begin
    Conv:=(FInput as TACSCustomConverter);
    Conv.UnlockInput;
  end;
end;

function TACSCustomFileIn.SetStartTime(Minutes, Seconds: Integer): Boolean;
var
  Sample: Integer;
begin
  Result:=False;
  if not FSeekable then Exit;
  OpenFile;
  CloseFile;
  Sample:=(Minutes*60+Seconds)*FSR;
  if Sample > FTotalSamples then Exit;
  FStartSample:=Sample;
  Result:=True;
end;

function TACSCustomFileIn.SetEndTime(Minutes, Seconds: Integer): Boolean;
var
  Sample: Integer;
begin
  Result:=False;
  if not FSeekable then Exit;
  OpenFile;
  CloseFile;
  Sample:=(Minutes*60+Seconds)*FSR;
  if Sample > FTotalSamples then Exit;
  FEndSample:=Sample;
  Result:=True;
end;

constructor TACSCustomFileIn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FStartSample:=0;
  FEndSample:=-1;
end;

procedure TACSVerySmallThread.Execute;
begin
 Synchronize(CallOnDone);
end;

procedure  TACSVerySmallThread.CallOnDone;
begin
 if Assigned(FOnDone) then FOnDone(Sender);
end;

procedure TACSThread.CallOnProgress;
var
  ParentComponent: TACSCustomOutput;
begin
  ParentComponent:=TACSCustomOutput(Parent);
  ParentComponent.FOnProgress(ParentComponent);
end;

constructor TACSThread.Create;
begin
 inherited Create(True);
end;

procedure TACSThread.DoPause;
begin
  if not Suspended then Suspended:=True;
end;

procedure TACSThread.DoResume;
begin
  if Suspended then Suspended:=False;
end;

{$IFDEF MSWINDOWS}
procedure TACSCustomOutput.Abort;
begin
  TerminateThread(Thread.Handle, 0);
  WhenDone;
end;
{$ENDIF}

constructor TACSBufferStream.Create;
begin
  inherited Create;
  {$IFDEF MSWINDOWS}
  BlockEventName:='Block'+IntToStr(LongWord(Self)); // this way we guarantee that the name is unique
  BlockEvent:=CreateEvent(nil, True, False, @BlockEventName[1]);
  InitializeCriticalSection(CS);
  {$ENDIF}
end;

destructor TACSBufferStream.Destroy;
begin
  {$IFDEF MSWINDOWS}
  DeleteCriticalSection(CS);
  CloseHandle(BlockEvent);
  {$ENDIF}
  inherited Destroy;
end;

function TACSBufferStream.Write(const Buffer; Count: Longint): Longint;
var
  addr: Pointer;
  S1, S2: Integer;
begin
  if WriteCur >= ReadCur then
  begin
    {$IFDEF MSWINDOWS}
    EnterCriticalSection(CS);
    {$ENDIF}
    S1:=STREAM_BUFFER_SIZE-WriteCur;
    if (Count <= S1) then
    begin
      Move(Buffer, Buff[WriteCur], Count);
      Inc(WriteCur, Count);
      Inc(FBytesInBuffer, Count);
      Result:=Count;
    end
    else
    begin
      Move(Buffer, Buff[WriteCur], S1);
      //addr:=Pointer(Cardinal(@Buffer) + S1);
      addr:=@Buffer;
      Inc(addr, S1);
      S2:=Count-S1;
      if S2 > ReadCur then S2:=ReadCur;
      Move(addr^, Buff[0], S2);
      WriteCur:=S2;
      Inc(FBytesInBuffer, S1+S2);
      Result:=S1+S2;
    end;
  end
  else
  begin
    S2:=ReadCur-WriteCur;
    if Count < S2 then S2:=Count;
    Move(Buffer, Buff[WriteCur], S2);
    Inc(WriteCur, S2);
    Inc(FBytesInBuffer, S2);
    Result:=S2;
  end;
  {$IFDEF MSWINDOWS}
  if Result <> 0 then
  if FBufferMode = bmBlock then PulseEvent(BlockEvent);
  LeaveCriticalSection(CS);
  {$ENDIF}
end;

function TACSBufferStream.Read(var Buffer; Count: Integer): Integer;
var
  addr: Pointer;
  S1, S2: Integer;
begin
  {$IFDEF MSWINDOWS}
  if (ReadCur = WriteCur) and (FBufferMode = bmBlock) then
  begin
    WaitForSingleObject(BlockEvent, INFINITE);
    ResetEvent(BlockEvent);
  end;
  EnterCriticalSection(CS);
  {$ENDIF}
  if ReadCur <= WriteCur then
  begin
    S2:=WriteCur-ReadCur;
    if Count < S2 then S2:=Count;
    Move(Buff[ReadCur], Buffer, S2);
    Inc(ReadCur, S2);
    Dec(FBytesInBuffer, S2);
    Result:=S2;
  end
  else
  begin
    S1:=STREAM_BUFFER_SIZE-ReadCur;
    if Count <= S1 then
    begin
      Move(Buff[ReadCur], Buffer, Count);
      Inc(ReadCur, Count);
      Dec(FBytesInBuffer, Count);
      Result:=Count;
    end
    else
    begin
      S2:=WriteCur;
      if Count-S1 < S2 then S2:=Count;
      Move(Buff[ReadCur], Buffer, S1);
      //addr := Pointer(Cardinal(@Buffer)+S1);
      addr:=@Buffer;
      Inc(addr, S1);
      Move(Buff[0], addr^, S2);
      ReadCur:=S2;
      Dec(FBytesInBuffer, S1+S2);
      Result:=S1+S2;
    end;
  end;
  Inc(FBytesRead, Result);
  {$IFDEF MSWINDOWS}
  LeaveCriticalSection(CS);
  {$ENDIF}
end;

procedure TACSBufferStream.Reset;
begin
  {$IFDEF MSWINDOWS}
  EnterCriticalSection(CS);
  {$ENDIF}
  ReadCur:=0;
  WriteCur:=0;
  FBytesRead:=0;
  {$IFDEF MSWINDOWS}
  SetEvent(BlockEvent);
  LeaveCriticalSection(CS);
 {$ENDIF}
end;

// the following property is implemented 'cause tstreams position property uses them

function TACSBufferStream.Seek(Offset: Longint; Origin: Word): Integer;
begin
  if (Offset = 0) and (Origin = 0) then Result:=FBytesRead
  else Result:=0;
  //raise EAcsException.Create(strSeeknotimplemented);
end;

function TACSBufferStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  if (Offset = 0) and (Origin = soCurrent) then Result:=FBytesRead
  else Result:=0;
  //raise EAcsException.Create(strSeeknotimplemented);
end;

{ TACSFileInfo }

function TACSFileInfo.GetStringTag(Idx: string): TACSFileTag;
begin
 Result:=nil;
end;

procedure TACSFileInfo.ReadFromFile;
begin
end;

procedure TACSFileInfo.SetStringTag(Idx: string; const AValue: TACSFileTag);
begin
end;

procedure TACSFileInfo.SaveToFile;
begin
end;

{ TACSFileTag }

function TACSFileTag.GetName: string;
begin
 Result:='';
end;

function TACSFileTag.AsString: string;
begin
 Result:='';
end;

function TACSFileTag.Streamable: Boolean;
begin
  Result:=False;
end;

procedure TACSFileTag.SaveToStream(Stream: TStream);
begin
end;

procedure TACSFileTag.LoadFromStream(Stream: TStream);
begin
end;

end.


