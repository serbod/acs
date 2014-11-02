(*
  this file is a part of audio components suite v 2.3.
  copyright (c) 2002-2005 andrei borovsky. all rights reserved.
  see the license file for more details.
  you can contact me at mail@z0m3ie.de
*)

unit acs_misc;

interface

uses
  Classes, SysUtils, ACS_Types, ACS_Classes, ACS_Strings
  {$IFDEF LINUX}
  ,baseunix, LibAO
  {$ENDIF};

const
  BUF_SIZE = $4000;

type
  TACSOnBufferDone = procedure(Sender: TComponent) of object;

  TACSAudioProcessorInitEvent = procedure(Sender: TComponent; var TotalSize: Integer) of object;
  TACSAudioProcessorFlushEvent = procedure(Sender: TComponent) of object;

  TACSGetParameterEvent = procedure(Sender: TComponent; var Param: Integer) of object;

  TACSGetRealParameterEvent = procedure(Sender: TComponent; var Param: real) of object;

  TACSGetDataEvent = procedure(Sender: TComponent; Data: Pointer; var n: Integer) of object;

  TACSMemoryIn = class(TACSCustomInput)
  private
    FBuffer: PACSBuffer8;
    FDataSize: Integer;
    FOnBufferDone: TACSOnBufferDone;
    Busy: Boolean;
    BufStart: Integer;
    BufEnd: Integer;
    FBPS, FSR, FChan: Integer;
    function GetBuffer: Pointer;
    procedure SetBuffer(v: Pointer);
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
    property DataBuffer: Pointer read GetBuffer write SetBuffer;
    property DataSize: Integer read FDataSize write FDataSize;
  published
    property GlobalSize: Integer read FSize write FSize;
    property InBitsPerSample: Integer read GetBPS write FBPS;
    property InChannels: Integer read GetCh write FChan;
    property InSampleRate: Integer read GetSR write FSR;
    property OnBufferDone: TACSOnBufferDone read FOnBufferDone write FOnBufferDone;
  end;

  TACSAudioProcessor = class(TACSCustomConverter)
  private
    FOnInit: TACSAudioProcessorInitEvent;
    FOnFlush: TACSAudioProcessorFlushEvent;
    FOnGetData: TACSGetDataEvent;
    FOnGetSampleRate: TACSGetParameterEvent;
    FOnGetBitsPerSample: TACSGetParameterEvent;
    FOnGetChannels: TACSGetParameterEvent;
    FOnGetTotalTime: TACSGetRealParameterEvent;
    FOnGetSize: TACSGetParameterEvent;
  protected
    function GetBPS: Integer; override;
    function GetCh: Integer; override;
    function GetSR: Integer; override;
    function GetTotalTime: Real; override;
  public
    function GetData(Buffer: Pointer; BufferSize: Integer): Integer; override;
    procedure Init; override;
    procedure Flush; override;
  published
    property OnFlush: TACSAudioProcessorFlushEvent read FOnFlush write FOnFlush;
    property OnGetBitsPerSample: TACSGetParameterEvent read FOnGetBitsPerSample write FOnGetBitsPerSample;
    property OnGetChannels: TACSGetParameterEvent read FOnGetChannels write FOnGetChannels;
    property OnGetData: TACSGetDataEvent read FOnGetData write FOnGetData;
    property OnGetSampleRate: TACSGetParameterEvent read FOnGetSampleRate write FOnGetSampleRate;
    property OnGetSize: TACSGetParameterEvent read FOnGetSize write FOnGetSize;
    property OnGetTotalTime: TACSGetrealParameterEvent read FOnGetTotalTime write FOnGetTotalTime;
    property OnInit: TACSAudioProcessorInitEvent read FOnInit write FOnInit;
  end;

  TACSNULLOut = class(TACSCustomOutput)
  private
    Buf: array[0..BUF_SIZE-1] of Byte;
  public
    procedure Done; override;
    function DoOutput(Abort: Boolean): Boolean; override;
    procedure Prepare; override;
  end;

  TACSInputItem = class(TCollectionItem)
  protected
    FInput: TACSCustomInput;
    function GetOwner: TPersistent; override;
  published
    property Input: TACSCustomInput read FInput write FInput;
  end;

  TACSInputItems = class(TOwnedCollection)
  end;

  TACSInputChangedEvent = procedure(Sender: TComponent; var Index: Integer; var Continue: Boolean) of object;

  { TACSInputList }

  TACSInputList = class(TACSCustomInput)
  private
    FCurrentInput: Integer;
    FInputItems: TACSInputItems;
    Lock: Boolean;
    FOnInputChanged: TACSInputChangedEvent;
    FIndicateProgress: Boolean;
    procedure SetCurrentInput(AInput: Integer);
    procedure SetInputItems(AItems: TACSInputItems);
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
    property CurrentInput: Integer read FCurrentInput write SetCurrentInput;
  published
    property IndicateProgress: Boolean read FIndicateProgress write FIndicateProgress;
    property InputItems: TACSInputItems read FInputItems write SetInputItems;
    property OnInputChanged: TACSInputChangedEvent read FOnInputChanged write FOnInputChanged;
  end;

  {
  TAcsSplitter = class(TACSCustomConverter)
  protected
    function GetBPS: Integer; override;
    function GetCh: Integer; override;
    function GetSR: Integer; override;
    procedure SetValuesCount(AValue: integer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetData(Buffer: Pointer; BufferSize: Integer): Integer; override;
    procedure GetValues(var Values: array of Double);
    procedure Init; override;
    procedure Flush; override;
  end;
  }

implementation

{$IFDEF LINUX}

var
  AOInitialized : Integer = 0;

{$ENDIF}

constructor TACSMemoryIn.Create;
begin
  inherited Create(AOwner);
  FSize:=-1;
end;

destructor TACSMemoryIn.Destroy;
begin
  inherited Destroy;
end;

function TACSMemoryIn.GetBPS: Integer;
begin
  if not (FBPS in [8, 16]) then FBPS:=16;
  Result:=FBPS;
end;

function TACSMemoryIn.GetCh: Integer;
begin
  if not (FChan in [1..2]) then FChan:=1;
  Result:=FChan;
end;

function TACSMemoryIn.GetSR: Integer;
begin
  if (FSR < 4000) or (FSR > 48000) then FSR:=8000;
  Result:=FSR;
end;

procedure TACSMemoryIn.Init;
begin
  FPosition:=0;
  BufEnd:=FDataSize;
  BufStart:=1;
  Busy:=True;
end;

procedure TACSMemoryIn.Flush;
begin
  Busy:=False;
  FDataSize:=0;
end;

function TACSMemoryIn.GetData(Buffer: Pointer; BufferSize: Integer): Integer;
begin
  Result:=0;
  if not Busy then raise EACSException.Create(strStreamnotopen);
  if not Assigned(FBuffer) then Exit;
  if BufStart > BufEnd then
  begin
    BufStart:=1;
    if FDataSize = 0 then
    begin
      if Assigned(FOnBufferDone) then FOnBufferDone(Self) else Exit;
    end;
    BufEnd:=FDataSize;
    if FDataSize = 0 then Exit;
  end;
  if BufferSize < (BufEnd-BufStart+1) then
    Result:=BufferSize
  else
    Result:=BufEnd-BufStart+1;
  Move(FBuffer[BufStart-1], Buffer^, Result);
  Inc(BufStart, Result);
  Inc(FPosition, Result);
  Dec(FDataSize, Result);
end;

function TACSMemoryIn.GetBuffer: Pointer;
begin
  Result:=Pointer(FBuffer);
end;

procedure TACSMemoryIn.SetBuffer;
begin
  FBuffer:=PACSBuffer8(v);
end;

function TACSAudioProcessor.GetBPS: Integer;
begin
  Result:=0;
  //if not Assigned(FInput) then raise EACSException.Create(strInputnotAssigned);
  if Assigned(FOnGetBitsPerSample) then
    FOnGetBitsPerSample(Self, Result)
  else
    if Assigned(FInput) then Result:=FInput.BitsPerSample;
end;

function TACSAudioProcessor.GetSR: Integer;
begin
  Result:=0;
  //if not Assigned(FInput) then raise EACSException.Create(strInputnotAssigned);
  if Assigned(FOnGetSampleRate) then
    FOnGetSampleRate(Self, Result)
  else
    if Assigned(FInput) then Result:=FInput.SampleRate;
end;

function TACSAudioProcessor.GetCh: Integer;
begin
  Result:=0;
  //if not Assigned(FInput) then raise EACSException.Create(strInputnotAssigned);
  if Assigned(FOnGetChannels) then
    FOnGetChannels(Self, Result)
  else
    if Assigned(FInput) then Result:=FInput.Channels;
end;

function TACSAudioProcessor.GetTotalTime: Real;
begin
  Result:=0;
  //if not Assigned(FInput) then raise EACSException.Create(strInputnotAssigned);
  if Assigned(FOnGetTotalTime) then
    FOnGetTotalTime(Self, Result)
  else
    if Assigned(FInput) then Result:=FInput.TotalTime;
end;

function TACSAudioProcessor.GetData(Buffer: Pointer; BufferSize: Integer): Integer;
begin
  //if not Assigned(FInput) then raise EACSException.Create(strInputnotAssigned);
  if Assigned(FOnGetData) then
  begin
    Result:=BufferSize;
    FOnGetData(Self, Buffer, Result);
  end
  else
    if Assigned(FInput) then Result:=FInput.GetData(Buffer, BufferSize);
  Inc(FPosition, Result);
//  if Result=0 then Result:=(Result shl 1);
end;

procedure TACSAudioProcessor.Init;
begin
  //if not Assigned(FInput) then raise EACSException.Create(strInputnotAssigned);
  if Assigned(FOnInit) then FOnInit(Self, FSize)
  else
  begin
    if Assigned(FInput) then
    begin
      FInput.Init;
      if Assigned(FOnGetSize) then
        FOnGetSize(Self, FSize)
      else
        FSize:=Finput.Size;
    end;
  end;
  FBusy:=True;
  FPosition:=0;
end;

procedure TACSAudioProcessor.Flush;
begin
  //if not Assigned(FInput) then raise EACSException.Create(strInputnotAssigned);
  if Assigned(FOnFlush) then
    FOnFlush(Self)
  else
    if Assigned(FInput) then FInput.Flush;
  FBusy:=False;
end;

procedure TACSNULLOut.Prepare;
begin
  //if not Assigned(FInput) then raise EACSException.Create(strInputnotAssigned);
  if Assigned(FInput) then FInput.Init;
end;

function TACSNULLOut.DoOutput(Abort: Boolean): Boolean;
begin
  Result:=True;
  if not Busy then Exit;
  if Abort or (not CanOutput) then
  begin
    Result:=False;
    Exit;
  end;

  if Assigned(FInput) and (FInput.GetData(@Buf[0], BUF_SIZE) > 0) then Result:=True
  else
  begin
    Result:=False;
    Exit;
  end;
end;

procedure TACSNULLOut.Done;
begin
  if Assigned(FInput) then FInput.Flush;
end;

function TACSInputItem.GetOwner : TPersistent;
begin
  Result:=Collection;
end;

constructor TACSInputList.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FInputItems:=TACSInputItems.Create(Self, TACSInputItem);
  FPosition:=0;
  FSize:=-1;
  FIndicateProgress:=True;
end;

destructor TACSInputList.Destroy;
begin
  FInputItems.Free;
  inherited Destroy;
end;

procedure TACSInputList.SetCurrentInput(AInput: Integer);
var
  Item: TACSInputItem;
begin
  if AInput <> 0 then
  begin
    if (AInput < 0) or (AInput >= FInputItems.Count) then
      raise EACSException.Create(Format(strListIndexOOB,[AInput]));
    if Busy then
    begin
      while Lock do;
      Lock:=True;
      Item:=TACSInputItem(InputItems.Items[FCurrentInput]);
      Item.Input.Flush;
      Item:=TACSInputItem(InputItems.Items[AInput]);
      Item.Input.Init;
      if FIndicateProgress then
      FSize:=Item.Input.Size
      else FSize:=-1;
      FPosition:=0;
      Lock:=False;
    end;
  end;
  FCurrentInput:=AInput;
end;

function TACSInputList.GetBPS: Integer;
var
  Item: TACSInputItem;
begin
  Result:=0;
  if Busy then
  begin
    Item:=TACSInputItem(InputItems.Items[FCurrentInput]);
    if Assigned(Item.Input) then Result:=Item.Input.BitsPerSample;
  end
  else
  begin
    if InputItems.Count > 0 then
    begin
      Item:=TACSInputItem(InputItems.Items[0]);
      if Assigned(Item.Input) then Result:=Item.Input.BitsPerSample;
    end;
  end;
end;

function TACSInputList.GetCh: Integer;
var
  Item: TACSInputItem;
begin
  Result:=0;
  if Busy then
  begin
    Item:=TACSInputItem(InputItems.Items[FCurrentInput]);
    if Assigned(Item.Input) then Result:=Item.Input.Channels;
  end
  else
  begin
    if InputItems.Count > 0 then
    begin
      Item:=TACSInputItem(InputItems.Items[0]);
      if Assigned(Item.Input) then Result:=Item.Input.Channels;
    end;
  end;
end;

function TACSInputList.GetSR: Integer;
var
  Item: TACSInputItem;
begin
  Result:=0;
  if Busy then
  begin
    Item:=TACSInputItem(InputItems.Items[FCurrentInput]);
    if Assigned(Item.Input) then Result:=Item.Input.SampleRate;
  end else
  if InputItems.Count > 0 then
  begin
    Item:=TACSInputItem(InputItems.Items[0]);
    if Assigned(Item.Input) then Result:=Item.Input.SampleRate;
  end;
end;

procedure TACSInputList.Init;
var
  Item: TACSInputItem;
begin
  if Busy then
    raise EACSException.Create(strBusy);
  if InputItems.Count = 0 then
    raise EACSException.Create(strNoInputItems);
  Item:=TACSInputItem(InputItems.Items[FCurrentInput]);
  if not Assigned(Item.Input) then
    raise EACSException.Create(Format(strNoInputAssigned,[FCurrentInput]));
  FBusy:=True;
  Item.Input.Init;
  if FIndicateProgress then FSize:=Item.Input.Size else FSize:=-1;
  FPosition:=0;
end;

procedure TACSInputList.Flush;
var
  Item: TACSInputItem;
begin
  Item:=TACSInputItem(InputItems.Items[FCurrentInput]);
  if Assigned(Item.Input) then Item.Input.Flush;
  FCurrentInput:=0;
  Lock:=False;
  FBusy:=False;
end;

function TACSInputList.GetData(Buffer: Pointer; BufferSize: Integer): Integer;
var
  Item: TACSInputItem;
  IsContinue: Boolean;
begin
  Result:=0;
  while Lock do;
  Lock:=True;
  Item:=TACSInputItem(InputItems.Items[FCurrentInput]);
  if Assigned(Item.Input) then Result:=Item.Input.GetData(Buffer, BufferSize);
  while Result=0 do
  begin
    if FCurrentInput < InputItems.Count-1 then
    begin
      { TODO : Item.Input check }
      Item.Input.Flush;
      Inc(FCurrentInput);
      IsContinue:=True;
      if Assigned(FonInputChanged) then
        FonInputChanged(Self, FCurrentInput, IsContinue);
      if IsContinue then
      begin
        Item:=TACSInputItem(InputItems.Items[FCurrentInput]);
        if not Assigned(Item.Input) then
          raise EACSException.Create(Format(strNoInputAssigned, [FCurrentInput]));
        Item.Input.Init;
        if FIndicateProgress then
          FSize:=Item.Input.Size
        else FSize:=-1;
        FPosition:=0;
        Result:=Item.Input.GetData(Buffer, BufferSize);
      end
      else Break;
    end
    else Break;
  end;
  if FIndicateProgress then FPosition:=Item.Input.Position;
  Lock:=False;
end;

procedure TACSInputList.SetInputItems(AItems: TACSInputItems);
begin
  FInputItems.Assign(AItems);
end;
end.
