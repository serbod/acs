(*
Base ACS classes

This file is a part of Audio Components Suite.
All rights reserved. See the license file for more details.

Copyright (c) 2002-2009, Andrei Borovsky, anb@symmetrica.net
Copyright (c) 2005-2006  Christian Ulrich, mail@z0m3ie.de
Copyright (c) 2014-2015  Sergey Bodrov, serbod@gmail.com
*)

{
@abstract(this unit introduces the base classes for acs)
@author(Andrei Borovsky (2003-2005))
@author(Christian Ulrich (2005))
@author(Sergey Bodrov (2014-2015))
}

unit acs_classes;

interface

uses

{$IFDEF MSWINDOWS}
  Windows, Dialogs,
{$ENDIF}
  acs_strings, Classes, SysUtils, syncobjs;

const

  STREAM_BUFFER_SIZE = $80000;

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
  {Basic exception class for ACS}
  EAcsException = class(Exception)
  end;

  {Basic Thread class for ACS}

  { TAcsThread }

  TAcsThread = class(TThread)
  private
    FHandleException: TAcsHandleThreadException;
    procedure CallOnProgress();
    procedure CallOnDone();
  public
    Parent: TComponent;
    //Terminating: Boolean;
    Stop: Boolean;
    Delay: Integer;
    CS: TRTLCriticalSection;
    constructor Create();
    destructor Destroy(); override;
    procedure DoPause();
    procedure DoResume();
    procedure Execute(); override;
    property HandleException: TAcsHandleThreadException read FHandleException write FHandleException;
  end;

  TAcsVerySmallThread = class(TThread)
  public
    FOnDone: TAcsOutputDoneEvent;
    Sender: TComponent;
    procedure Execute(); override;
    procedure CallOnDone();
  end;


  TAcsBufferMode = (bmBlock, bmReport);

  // Circular buffer with TStream interface
  TAcsCircularBuffer = class(TStream)
  private
    Buff: array[0..STREAM_BUFFER_SIZE-1] of Byte;
    ReadCur: Integer;
    WriteCur: Integer;
    FBufferMode: TAcsBufferMode;
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
    constructor Create();
    destructor Destroy; override;
    procedure Reset();
    function Read(var Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; overload; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; overload; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    property BytesInBuffer: Integer read FBytesInBuffer;
    property BufferMode: TAcsBufferMode read FBufferMode write FBufferMode;
  end;

  { TAcsAudioBuffer }
  { Audio sample buffer - store raw audio data }
  TAcsAudioBuffer = class(TMemoryStream)
  protected
    FLock: TMultiReadExclusiveWriteSynchronizer;
    function GetSamplesCount(): Integer; virtual;
    procedure SetSamplesCount(AValue: Integer); virtual;
    function GetByte(Index: Integer): Byte; virtual;
    procedure SetByte(Index: Integer; AValue: Byte); virtual;
    function GetSamplePtr(Index: Integer): Pointer; virtual;
  public
    { 1-mono, 2-stereo, etc.. }
    ChannelsCount: Integer;
    { Bits per sample channel - 4, 8, 16, 20, 24, 32 }
    BitDepth: Integer;
    { Samples per second - 8000, 11025, 22050, 44100, 48000, 96000, 192000 (Hz)}
    SampleRate: Integer;
    { Time, when buffer content start play (milliseconds) }
    Timestamp: Int64;
    { Sound source position in 3D space }
    //Position3D: TPoint3D;
    { Sound source velocity, for Doppler effect }
    //Velocity: Integer;
    constructor Create();
    destructor Destroy(); override;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    { Size of sound sample in bytes }
    function BytesPerSample(): Integer;
    { Size of buffer in sound samples }
    property SamplesCount: Integer read GetSamplesCount write SetSamplesCount;
    { Access to bytes. Inline code from GetByte or SetByte for optimization. }
    property Bytes[Index: Integer]: Byte read GetByte write SetByte;
    { Access to samples. Inline code from GetSamplePtr for optimization. }
    property Samples[Index: Integer]: Pointer read GetSamplePtr;
  end;


  { TAcsCustomInput }
  { TAcsInput is the base class for all input and converter components. }
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
    function GetBPS(): Integer; virtual;
    function GetCh(): Integer; virtual;
    function GetSR(): Integer; virtual;
    function GetTotalTime(): Real; virtual;
    procedure SetBufferSize(AValue: Integer); virtual;
    function GetBufferSize(): Integer;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy(); override;
    { This is an abstract method.
      In TAcsInput descendands this function reads the audio stream data into the Buffer.
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
    function GetData(Buffer: Pointer; BufferSize: Integer): Integer; virtual; overload;
    function GetData(AStream: TStream): Integer; virtual; overload;
    { This function is called from output component when the buffer must be filled.
    }
    procedure Reset(); virtual;
    { This is an abstract method. In TAcsInput descendands it prepares input
      component for reading data.
    }
    procedure Init(); virtual; abstract;
    { This is an abstract method. In TAcsInput descendands it closes the current input,
      clearing up all temporary structures alocated during data transfer.
      
      Note: usually this method is called internally by the output or converter component to
      which the input component is assigned. You can call this method if you want to get direct
      access to the audio stream.
    }
    procedure Flush(); virtual; abstract;
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


  { TAcsCustomOutput }
  { TAcsCustomOutput is the base class for all ACS output components. }
  TAcsCustomOutput = class(TComponent)
  protected
    CanOutput: Boolean;
    CurProgr: Real;
    Thread: TAcsThread;
    FInput: TAcsCustomInput;
    FOnDone: TAcsOutputDoneEvent;
    FOnProgress: TAcsOutputProgressEvent;
    FActive: Boolean;  // Set to true by Run and to False by WhenDone.
    FOnThreadException: TAcsThreadExceptionEvent;
    InputLock: Boolean;
    FBufferSize: Integer;
    FBuffer: TAcsAudioBuffer;

    { Read data from Input into Buffer, return bytes read
      AEndOfInput set to True if end of input buffer is reached }
    function FillBufferFromInput(var AEndOfInput: Boolean): Integer; virtual; overload;
    function FillBufferFromInput(): Integer; virtual; overload;
    function GetPriority(): TThreadPriority; virtual;
    procedure SetPriority(Priority: TThreadPriority); virtual;
    function GetProgress(): Real; virtual;
    procedure SetInput(AInput: TAcsCustomInput); virtual;
    { Called from Thread when thread stopped }
    procedure WhenDone(); virtual;
    { Time elapsed }
    function GetTE(): Integer; virtual;
    function GetStatus(): TAcsOutputStatus; virtual;
    function GetDelay(): Integer; virtual;
    procedure SetDelay(Value: Integer); virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure HandleThreadException(E: Exception); virtual;
    function GetBufferSize(): Integer;
    procedure SetBufferSize(AValue: Integer); virtual;
  public
    LastErrorDescription: string;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy(); override;

    { Set default buffer size and calls FInput.Init() }
    procedure Prepare(); virtual;
    { Called from Thread }
    function DoOutput(Abort: Boolean): Boolean; virtual; abstract;
    { Calls FInput.Flush() and reset buffer to zero }
    procedure Done(); virtual;

    { This is the most important method in the output components.
      After an input component has been assigned, call Run to start audio-processing chain. }
    procedure Run();
    { Stops the running output process. }
    procedure Stop();
    { pauses the output (the output thread is suspended). }
    procedure Pause(); virtual;
    { Resumes previously paused output. }
    procedure Resume(); virtual;

    { Set to true by Run and to False by WhenDone. }
    property Active: Boolean read FActive;
    { Use this property to set the delay (in milliseconds) in output thread.
      This property allows the user to reduce the stress the output thread puts
      on the CPU (especially under Windows).
      Be careful with this property when using TAudioOut component.
      Assigning too large values to it can cause dropouts in audio playback. }
    property Delay: Integer read GetDelay write SetDelay;
    { Output components perform output in their own threads.
      Use this property to set the priority for the thread. }
    property ThreadPriority: TThreadPriority read GetPriority write SetPriority;
    { Read Progress to get the output progress in percents.
      This value is meaningful only after the input component has been set
      and only if the input component can tell the size of its stream. }
    property Progress: Real read GetProgress;
    { This property indicates the output component's current status. Possible values are:

      tosPlaying: the component is working;
      
      tosPaused: the component is paused (the Pause method was called);
      
      tosIdle: the component is idle; }
    property Status: TAcsOutputStatus read GetStatus;
    property TimeElapsed: Integer read GetTE;
  published
    { This property allows you to set the input component for the output component.
      The valid input components must be descendants of TAcsInput. }
    property Input: TAcsCustomInput read FInput write SetInput;
    { This event is invoked when the output is finished.

      Note: don't try to start other output job before OnDone event from the
      previous job is triggered. An attempt to do so will cause the
      "Component is buisy" exception.
      It is a good practice to desable output-starting controls in the
      method where the output component's Run method is called and to enable
      them in the output component's OnDone event handler. }
    property OnDone: TAcsOutputDoneEvent read FOnDone write FOndone;
    { This event is invoked every time the Progress property value changes.
      Be careful when referencing GUI interface components in OnProgress event's handler. }
    property OnProgress: TAcsOutputProgressEvent read FOnProgress write FOnProgress;
    { Thread procedure should never terminate.
      For this reason all the exceptions that might arise in the thread
      procedure are caught within the procedure itself.
      The thread generates OnThreadException event to inform the application
      that an exception has occurred.
      Avoid any potential exception-rising actions in this event’s handler!
      Use the handler to reset application controls (if needed) and call the
      input component’s Reset method.
      See the CD ripper demo for an example of handling thread exceptions. }
    property OnThreadException: TAcsThreadExceptionEvent read FOnThreadException write FOnThreadException;
    { This Property sets the BufferSize of the component }
    property BufferSize: Integer read GetBufferSize write SetBufferSize;
  end;

  { TAcsStreamedInput introduces Stream property.
    This property allows input components that descend from TAcsStreamedInput
    to get data not only from files on disc but from any kind of stream.
    The descendats of this class are TAcsFileIn (and all its descendats except TMACIn),
    and TStreamIn component that is designed to read raw audio data from streams.
  }
  TAcsStreamedInput = class(TAcsCustomInput)
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
      In TAcsFileIn descendants the stream assigned to this property takes over
      the FileName property, i. e. if both Stream and FileName property are assigned,
      the stream and not the file will be used for the actual input.
      To unassign this property set it to nil.
      If the stream is seekable it will be reset to the beginning at the end of the playback.
    }
    property Stream: TStream read FStream write SetStream;
    constructor Create(AOwner: TComponent); override;
  end;

  { TAcsStreamedOutput introduces Stream property.
    This property allows output components that descend from TAcsStreamedOutput
    to store data not only to files on disc but to any kind of stream as well.
  }
  TAcsStreamedOutput = class(TAcsCustomOutput)
  protected
    { Use this property to set the output stream for the corresponding output component.
      Remember that you have to create, destroy and position the input stream explicitly.
      In file-handling TAcsStreamedOutput descendants the stream assigned to this property
      takes over the FileName property, i. e. if both Stream and FileName property are assigned,
      the stream and not the file will be used for the actual output.
      To unassign this property set it to nil.
    }
    FStream: TStream;
    procedure SetStream(AStream: TStream);
  public
    property Stream: TStream read FStream write SetStream;
  end;
  
  { This class introduces an simple handler for an file tag it can hold any streamable
    piece of data or an string
  }
  TAcsFileTag = class
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

  { TAcsFileInfo }

  TAcsFileInfo = class
  private
    procedure ReadFromFile; virtual;
    procedure SetStringTag(Idx: string; const AValue: TAcsFileTag);
    function GetStringTag(Idx: string): TAcsFileTag;
  public
    procedure SaveToFile; virtual;
    property Tags[Idx: string]: TAcsFileTag read GetStringTag write SetStringTag;
  end;
  

  { TAcsCustomFileIn }
  { TAcsFileIn is the base class for all input components that read data from files
    (or from other streams in the corresponding file format).
    It introduces such properties as FileName, StartSample, EndSample, Loop,
    and Valid and Jump, Seek, SetStartTime, SetEndTime methods.
  }
  TAcsCustomFileIn = class(TAcsStreamedInput)
  protected
    FFileName: TFileName;
    FOffset: Real;
    FOpened: Boolean;
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

  { TAcsCustomFileOut }

  TAcsCustomFileOut = class(TAcsStreamedOutput)
  protected
    FFileName: TFileName;
    FFileMode: TAcsFileOutputMode;
    FAccessMask: Integer;
    procedure SetFileMode(AMode: TAcsFileOutputMode); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    { Use this property to set the file access mask. The default value is $1B6,
    which corresponds to rw-rw-rw- access mask.
    }
    property AccessMask: Integer read FAccessMask write FAccessMask;
    { check and open file stream }
    procedure Prepare(); override;
    { close file stream }
    procedure Done(); override;
  published
    { There are two possible values for this property: foRewrite and foAppend.
      When foRewrite (default value) is set and the file with the specified
      name already exists (or a stream is assigned) the contents of the file
      (stream) is rewritten.

      In the foAppend mode the new data is appended to the end of the existing
      file (stream).
      Currently only TWaveOut and TVorbisOut components support foAppend mode.
    }
    property FileMode: TAcsFileOutputMode read FFileMode write SetFileMode;
    { Use this property to set the name of the file to save data to.
    }
    property FileName: TFileName read FFileName write FFileName;
  end;

  { TAcsCustomConverter }
  { TAcsCustomConverter is the base class for all ACS converter classes.
  Converters are intermediate sections in the audio processing chain.
  Being the descendants of TAcsCustomInput they can be linked to the output
  components, while their Input property allows to link them with input
  components and other converters.

  TAcsCustomConverter introduces new published property - Input - the very
  property that distinguishes a converter from an input component. }
  TAcsCustomConverter = class(TAcsCustomInput)
  protected
    InputLock: Boolean;
    FInput: TAcsCustomInput;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetInput(AInput: TAcsCustomInput); virtual;
  public
    procedure UnlockInput();
  published
    property Input: TAcsCustomInput read FInput write SetInput;
  end;


implementation

{ TAcsAudioBuffer }

constructor TAcsAudioBuffer.Create();
begin
  inherited Create;
  FLock:=TMultiReadExclusiveWriteSynchronizer.Create();
end;

destructor TAcsAudioBuffer.Destroy();
begin
  FreeAndNil(FLock);
  inherited Destroy;
end;

function TAcsAudioBuffer.GetSamplesCount(): Integer;
begin
  Result:=(Self.Size div BytesPerSample());
end;

procedure TAcsAudioBuffer.SetSamplesCount(AValue: Integer);
begin
  if AValue >= 0 then
  begin
    if FLock.BeginWrite() then
    begin
      Self.Size:=AValue * BytesPerSample();
      FLock.EndWrite();
    end;
  end;
end;

function TAcsAudioBuffer.GetByte(Index: Integer): Byte;
begin
  if (Index >= 0) and (Index < Size) then
    Result:=PByte(Self.Memory)[Index]
  else
    Result:=0;
end;

procedure TAcsAudioBuffer.SetByte(Index: Integer; AValue: Byte);
begin
  if (Index >= 0) and (Index < Size) then
    PByte(Self.Memory)[Index]:=AValue;
end;

function TAcsAudioBuffer.GetSamplePtr(Index: Integer): Pointer;
begin
  if (Index >= 0) and (Index < Size) then
    Result:=Self.Memory + (Index * BytesPerSample())
  else
    Result:=nil;
end;

function TAcsAudioBuffer.Read(var Buffer; Count: Longint): Longint;
begin
  FLock.BeginRead();
  Result:=inherited Read(Buffer, Count);
  FLock.EndRead();
end;

function TAcsAudioBuffer.Write(const Buffer; Count: Longint): Longint;
begin
  if FLock.BeginWrite() then
  begin
    Result:=inherited Write(Buffer, Count);
    FLock.EndWrite();
  end
  else Result:=0;
end;

function TAcsAudioBuffer.BytesPerSample(): Integer;
begin
  Result:=SampleRate * (BitDepth div 8) * ChannelsCount;
end;

{ TAcsVerySmallThread }

procedure TAcsVerySmallThread.CallOnDone();
begin
 if Assigned(FOnDone) then FOnDone(Sender);
end;

procedure TAcsVerySmallThread.Execute();
begin
 Synchronize(CallOnDone);
end;

{ TAcsThread }

constructor TAcsThread.Create();
begin
 inherited Create(True);
 InitCriticalSection(CS);
end;

destructor TAcsThread.Destroy();
begin
  DoneCriticalSection(CS);
  inherited Destroy();
end;

procedure TAcsThread.CallOnProgress();
begin
  if Assigned((Parent as TAcsCustomOutput).OnProgress) then
    (Parent as TAcsCustomOutput).OnProgress(Parent);
end;

procedure TAcsThread.CallOnDone();
begin
  if Assigned((Parent as TAcsCustomOutput).FOnDone) then
    (Parent as TAcsCustomOutput).FOnDone(Parent);
end;

procedure TAcsThread.DoPause();
begin
  Suspended:=True;
end;

procedure TAcsThread.DoResume();
begin
  Suspended:=False;
end;

procedure TAcsThread.Execute();
var
  //DoneThread: TAcsVerySmallThread;
  ParentComponent: TAcsCustomOutput;
  Res: Boolean;
begin
  ParentComponent:=(Parent as TAcsCustomOutput);
  // init
  try
    ParentComponent.Prepare();
  except
    on E: Exception do
    begin
      if Assigned(Self.HandleException) then Self.HandleException(E);
      Terminate();
    end;
  end;

  // main loop
  //EnterCriticalSection(CS);
  while not Terminated do
  begin
    if Delay > 5 then Sleep(Delay);
    try
      if ParentComponent.Progress <> ParentComponent.CurProgr then
      begin
        ParentComponent.CurProgr:=ParentComponent.Progress;
        if Assigned(ParentComponent.FOnProgress) then Synchronize(CallOnProgress);
      end;
      Res:=ParentComponent.DoOutput(Stop);
      if (not Res) then Terminate();
    except
      on E: Exception do
      begin
        if Assigned(Self.HandleException) then Self.HandleException(E);
        Terminate();
      end;
    end;
  end;
  //LeaveCriticalSection(CS);

  // done
  try
    ParentComponent.WhenDone();
  except
    on E: Exception do
    begin
      if Assigned(Self.HandleException) then Self.HandleException(E);
      Terminate();
    end;
  end;

  Synchronize(CallOnDone);

  // This ensures that OnDone event is called outside this thread
  { // !!! for what?
  DoneThread:=TAcsVerySmallThread.Create(True);
  DoneThread.Sender:=ParentComponent;
  DoneThread.FOnDone:=ParentComponent.FOnDone;
  DoneThread.FreeOnTerminate:=True;
  DoneThread.Suspended:=False;
  }
end;

{ TAcsCustomInput }

constructor TAcsCustomInput.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBusy:=False;
  FPosition:=0;
end;

destructor TAcsCustomInput.Destroy();
begin
  inherited Destroy;
end;

function TAcsCustomInput.GetData(Buffer: Pointer; BufferSize: Integer): Integer;
begin
  Result:=Min(Self.BufferSize, BufferSize);
  Move(Buffer, FBuffer, Result);
end;

function TAcsCustomInput.GetData(AStream: TStream): Integer;
begin
  Result:=Min(Self.BufferSize, AStream.Size);
  AStream.Read(FBuffer, Result);
end;

function TAcsCustomInput.GetBPS(): Integer;
begin
  Result:=-1;
end;

function TAcsCustomInput.GetCh(): Integer;
begin
  Result:=-1;
end;

function TAcsCustomInput.GetSR(): Integer;
begin
  Result:=-1;
end;

function TAcsCustomInput.GetTotalTime: Real;
begin
  Result:=0;  // Default result for the streams.
end;

procedure TAcsCustomInput.SetBufferSize(AValue: Integer);
begin
  //if Busy then
  //  raise EAcsException.Create(strBusy);
  if AValue >= 0 then SetLength(FBuffer, AValue);
end;

function TAcsCustomInput.GetBufferSize: Integer;
begin
  Result:=Length(FBuffer);
end;

procedure TAcsCustomInput.Reset();
begin
  try
    Flush();
  except
  end;
  FBusy:=False;
end;

{ TAcsCustomOutput }

constructor TAcsCustomOutput.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FActive:=False;
  FBuffer:=TAcsAudioBuffer.Create();
  FBufferSize:=$4000; // default buffer size
  Thread:=nil;
end;

destructor TAcsCustomOutput.Destroy();
begin
  //if not Thread.Suspended then Thread.Terminating:=True;
  //while Thread.Terminating do Sleep(1);
  if Assigned(Thread) then FreeAndNil(Thread);
  FreeAndNil(FBuffer);
  inherited Destroy;
end;

procedure TAcsCustomOutput.Prepare();
begin
  if not Assigned(FInput) then
    raise EAcsException.Create(strInputNotAssigned);
  SetBufferSize(FBufferSize); // set default buffer size
  if Assigned(FInput) then FInput.Init();
end;

procedure TAcsCustomOutput.Done();
begin
  if Assigned(FInput) then FInput.Flush();
  SetBufferSize(0);
end;

procedure TAcsCustomOutput.Run();
begin
  if Active then Exit();
  if not Assigned(FInput) then
    raise EAcsException.Create(strInputnotAssigned);
  InputLock:=False;
  FActive:=True;

  Thread:=TAcsThread.Create();
  Thread.Parent:=Self;
//  Thread.DoOutput:=Self.DoOutput;
//  Thread.FOnDone:=Self.WhenDone;
  Thread.HandleException:=HandleThreadException;
  try
    Thread.Stop:=False;
    CanOutput:=True;
    Thread.Suspended:=False;
  except
    on E: Exception do HandleThreadException(E);
  end;
end;

procedure TAcsCustomOutput.Stop();
begin
  //Thread.Stop:=True;
  FreeAndNil(Thread);
end;

procedure TAcsCustomOutput.Pause();
begin
  if Assigned(Thread) then Thread.DoPause();
end;

procedure TAcsCustomOutput.Resume();
begin
  if Assigned(Thread) then Thread.DoResume();
end;

procedure TAcsCustomOutput.WhenDone();
begin
  if not Active then Exit;
  CanOutput:=False;
  Done();
  FActive:=False;
end;

function TAcsCustomOutput.GetStatus(): TAcsOutputStatus;
begin
  if Active then
  begin
    if Self.Thread.Suspended then
      Result:=tosPaused
    else
      Result:=tosPlaying;
  end
  else
    Result:=tosIdle
end;

function TAcsCustomOutput.GetPriority(): TThreadPriority;
begin
  Result:=tpNormal;
  if Assigned(Thread) then Result:=Thread.Priority;
end;

procedure TAcsCustomOutput.SetPriority(Priority: TThreadPriority);
begin
  if Assigned(Thread) then Thread.Priority:=Priority;
end;

function TAcsCustomOutput.FillBufferFromInput(var AEndOfInput: Boolean
  ): Integer;
var
  n: Integer;
begin
  Result:=0;
  if not Assigned(FInput) then Exit;
  while InputLock do;
  InputLock:=True;
  while Result < FBuffer.Size do
  begin
    n:=FInput.GetData(FBuffer.Memory+Result, FBuffer.Size-Result);
    if n = 0 then
    begin
      AEndOfInput:=True;
      Break;
    end;
    Inc(Result, n);
  end;
  InputLock:=False;
end;

function TAcsCustomOutput.FillBufferFromInput(): Integer;
begin
  Result:=0;
  if not Assigned(FInput) then Exit;
  while InputLock do;
  InputLock:=True;
  Result:=FInput.GetData(FBuffer);
  InputLock:=False;
end;

procedure TAcsCustomOutput.SetInput(AInput: TAcsCustomInput);
var
  OldInput, NewInput: TAcsCustomInput;
begin
  if Active then
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

function TAcsCustomOutput.GetProgress(): Real;
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

function TAcsCustomOutput.GetTE(): Integer;
begin
   if not Assigned(FInput) then
     Result:=0
   else
     Result:=Round(FInput.Position / ((FInput.BitsPerSample shr 3) * FInput.Channels * FInput.SampleRate));
end;

function TAcsCustomOutput.GetDelay(): Integer;
begin
  if Assigned(Thread) then Result:=Thread.Delay
  else Result:=0;
end;

procedure TAcsCustomOutput.SetDelay(Value: Integer);
begin
  if Assigned(Thread) then
  begin
    if Value <= 100 then Thread.Delay:=Value;
  end;
end;

procedure TAcsCustomOutput.Notification(AComponent: TComponent; Operation: TOperation);
begin
  // Remove the following two lines if they cause troubles in your IDE
  if (AComponent = FInput) and (Operation = opRemove) then Input:=nil;
  inherited Notification(AComponent, Operation);
end;

procedure TAcsCustomOutput.HandleThreadException(E: Exception);
var
  Conv: TAcsCustomConverter;
begin
 InputLock:=False;
 if Status <> tosIdle then
 begin
   try
    if FInput is TAcsCustomConverter then
    begin
      Conv:=(FInput as TAcsCustomConverter);
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
 FActive:=False;
 if Assigned(FOnThreadException) then FOnThreadException(Self, E);
end;

function TAcsCustomOutput.GetBufferSize(): Integer;
begin
  Result:=FBuffer.Size;
end;

procedure TAcsCustomOutput.SetBufferSize(AValue: Integer);
begin
  if AValue > 0 then FBuffer.Size:=AValue;
end;

{ TAcsStreamedInput }

constructor TAcsStreamedInput.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FStream:=nil;
  FSeekable:=True;
end;

{ TAcsCustomFileIn }

constructor TAcsCustomFileIn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FStartSample:=0;
  FEndSample:=-1;
end;

procedure TAcsCustomFileIn.Reset();
begin
  inherited Reset();
  FOpened:=False;
end;

procedure TAcsCustomFileIn.Flush();
begin
  CloseFile();
  FBusy:=False;
end;

procedure TAcsCustomFileIn.Init();
begin
  if Busy then raise EAcsException.Create(strBusy);
  if not Assigned(FStream) then
    if FFileName = '' then raise EAcsException.Create(strFilenamenotassigned);
  OpenFile();
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

function TAcsCustomFileIn.GetBPS(): Integer;
begin
  Result:=0;
  if Valid then Result:=FBPS;
end;

function TAcsCustomFileIn.GetCh(): Integer;
begin
  Result:=0;
  if Valid then Result:=FChan;
end;

function TAcsCustomFileIn.GetSR(): Integer;
begin
  Result:=0;
  if Valid then Result:=FSR;
end;

function TAcsCustomFileIn.GetTime(): Integer;
begin
  Result:=0;
  if Valid then Result:=FTime;
end;

function TAcsCustomFileIn.GetValid(): Boolean;
var
  WasOpened: Boolean;
begin
  Result:=False;
  WasOpened:=FOpened;
  if (not Assigned(FStream)) or (FileName = '') then
  begin
    FValid:=False;
    Exit;
  end;
  if FSeekable then
  begin
    OpenFile();
    Result:=FValid;
    if not WasOpened then CloseFile();
  end
  else Result:=FValid;
end;

procedure TAcsCustomFileIn.OpenFile();
begin
  FSize:=0;
end;

procedure TAcsCustomFileIn.CloseFile();
begin

end;

procedure TAcsCustomFileIn.Jump(Offs: Real);
begin
  FOffset:=Offs;
end;

function TAcsCustomFileIn.GetTotalTime(): Real;
begin
  Result:=0;
  if (not FSeekable) or (not Valid) then Exit;

  if (SampleRate = 0) or (Channels = 0) or (BitsPerSample = 0) then
  else
    Result:=Size / (SampleRate * Channels * (BitsPerSample div 8));
end;

function TAcsCustomFileIn.SetStartTime(Minutes, Seconds: Integer): Boolean;
var
  Sample: Integer;
begin
  Result:=False;
  if (not FSeekable) or Valid then Exit;

  Sample:=(Minutes*60+Seconds)*FSR;
  if Sample > FTotalSamples then Exit;
  FStartSample:=Sample;
  Result:=True;
end;

function TAcsCustomFileIn.SetEndTime(Minutes, Seconds: Integer): Boolean;
var
  Sample: Integer;
begin
  Result:=False;
  if (not FSeekable) or Valid then Exit;

  Sample:=(Minutes*60+Seconds)*FSR;
  if Sample > FTotalSamples then Exit;
  FEndSample:=Sample;
  Result:=True;
end;

procedure TAcsStreamedInput.SetStream(AStream: TStream);
begin
  FStream:=AStream;
  FStreamAssigned:=Assigned(FStream);
end;

procedure TAcsStreamedOutput.SetStream(AStream: TStream);
begin
  FStream:=AStream;
end;

{ TAcsCustomFileOut }

constructor TAcsCustomFileOut.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  {$IFDEF LINUX}
  FAccessMask := $1B6; // rw-rw-rw-
  {$ENDIF}
end;

procedure TAcsCustomFileOut.Prepare();
begin
  inherited Prepare();
  if Assigned(FStream) then FreeAndNil(FStream);
  if FFileName = '' then
    raise EAcsException.Create(strFilenamenotassigned);
  if (not FileExists(FFileName)) or (FFileMode = foRewrite) then
    FStream := TFileStream.Create(FFileName, fmCreate or fmShareExclusive, FAccessMask)
  else
    FStream := TFileStream.Create(FFileName, fmOpenReadWrite or fmShareExclusive, FAccessMask);
end;

procedure TAcsCustomFileOut.Done();
begin
  if Assigned(FStream) then FreeAndNil(FStream);
  inherited Done();
end;

procedure TAcsCustomFileOut.SetFileMode(AMode: TAcsFileOutputMode);
begin
  FFileMode:=foRewrite;
end;

{ TAcsCustomConverter }

procedure TAcsCustomConverter.Notification(AComponent: TComponent; Operation: TOperation);
begin
  // Remove the following two lines if they cause troubles in your IDE
  if (AComponent = FInput) and (Operation = opRemove) then Input:=nil;
  inherited Notification(AComponent, Operation);
end;

procedure TAcsCustomConverter.SetInput(AInput: TAcsCustomInput);
var
  OldInput, NewInput: TAcsCustomInput;
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

procedure TAcsCustomConverter.UnlockInput();
var
  Conv: TAcsCustomConverter;
begin
  InputLock:=False;
  if Assigned(FInput) then
  if (FInput is TAcsCustomConverter) then
  begin
    Conv:=(FInput as TAcsCustomConverter);
    Conv.UnlockInput();
  end;
end;

{ TAcsCircularBuffer }

constructor TAcsCircularBuffer.Create();
begin
  inherited Create;
  {$IFDEF MSWINDOWS}
  BlockEventName:='Block'+IntToStr(LongWord(Self)); // this way we guarantee that the name is unique
  BlockEvent:=CreateEvent(nil, True, False, @BlockEventName[1]);
  InitializeCriticalSection(CS);
  {$ENDIF}
end;

destructor TAcsCircularBuffer.Destroy();
begin
  {$IFDEF MSWINDOWS}
  DeleteCriticalSection(CS);
  CloseHandle(BlockEvent);
  {$ENDIF}
  inherited Destroy;
end;

procedure TAcsCircularBuffer.Reset();
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

function TAcsCircularBuffer.Write(const Buffer; Count: Longint): Longint;
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

function TAcsCircularBuffer.Read(var Buffer; Count: Integer): Integer;
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

// the following property is implemented 'cause tstreams position property uses them

function TAcsCircularBuffer.Seek(Offset: Longint; Origin: Word): Integer;
begin
  if (Offset = 0) and (Origin = 0) then Result:=FBytesRead
  else Result:=0;
end;

function TAcsCircularBuffer.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  if (Offset = 0) and (Origin = soCurrent) then Result:=FBytesRead
  else Result:=0;
end;

{ TAcsFileInfo }

function TAcsFileInfo.GetStringTag(Idx: string): TAcsFileTag;
begin
 Result:=nil;
end;

procedure TAcsFileInfo.ReadFromFile();
begin
end;

procedure TAcsFileInfo.SetStringTag(Idx: string; const AValue: TAcsFileTag);
begin
end;

procedure TAcsFileInfo.SaveToFile();
begin
end;

{ TAcsFileTag }

function TAcsFileTag.GetName(): string;
begin
 Result:='';
end;

function TAcsFileTag.AsString(): string;
begin
 Result:='';
end;

function TAcsFileTag.Streamable(): Boolean;
begin
  Result:=False;
end;

procedure TAcsFileTag.SaveToStream(Stream: TStream);
begin
end;

procedure TAcsFileTag.LoadFromStream(Stream: TStream);
begin
end;

end.


