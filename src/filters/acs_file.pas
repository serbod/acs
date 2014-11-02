(*
  this file is a part of audio components suite.
  see the license file for more details.
  you can contact me at mail@z0m3ie.de
*)

{
@abstract(this unit introduces the base classes for acs)
@author(Christian Ulrich (2005))

this unit introduces basic fileformat support
}

unit acs_file;

interface

uses
  Classes, ACS_Classes, Dialogs, SysUtils, ACS_Strings;

type
  TACSFileInClass = class of TACSCustomFileIn;
  TACSFileOutClass = class of TACSCustomFileOut;

  TACSFileCapTyp = (fcLoad, fcSave);
  TACSFileCapTyps = set of TACSFileCapTyp;


  TACSFormatClass = class of TComponent;

  { TACSFileFormat }
  TACSFileFormat = class
  public
    FileClass       : TACSFormatClass;
    Extension       : String;
    Description     : String;
  end;

  { TACSFileFormatsList

    To this List all Filefomats must be added,
    use initialization section of your format units to add your format to acs
    so the user must only add your unit to the uses clausle to have support for
    your fileformat.
  }
  TACSFileFormatsList = class (TList)
  public
    destructor Destroy; override;
    procedure Add(const Ext, Desc: String; AClass: TACSFormatClass);
    function FindExt(Ext: string; Typs: TACSFileCapTyps): TACSFormatClass;
    function FindFromFileName(const FileName: String; Typs: TACSFileCapTyps): TACSFormatClass;
    procedure Remove(AClass: TACSFormatClass);
    procedure BuildFilterStrings(var Descriptions: String; Typs: TACSFileCapTyps);
  end;


  { TACSFileIn

    This class is an wrapper for all fileformats
  }
  TACSFileIn = CLASS(TACSCustomFileIn)
  private
    FEndSample: Integer;
    FFileName: string;
    FInput: TACSCustomFileIn;
    FDialog: TOpenDialog;
    FLoop: Boolean;
    FStartSample: Integer;
    FTotalSamples: Integer;
    function GetTime: Integer;
    function GetValid: Boolean;
    procedure SetFileName(const AValue: String);
    function GetSize: Integer;
    function GetPosition: Integer;
  protected
    function GetBPS: Integer; override;
    function GetCh: Integer; override;
    function GetSR: Integer; override;
    function GetTotalTime: Real; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Open;
    procedure Flush; override;
    procedure Init; override;
    procedure Reset; override;
    function Seek(SampleNum: Integer): Boolean; override;
    function GetData(Buffer: Pointer; BufferSize: Integer): Integer; override;
    function SetStartTime(Minutes, Seconds: Integer): Boolean;
    function SetEndTime(Minutes, Seconds: Integer): Boolean;
    procedure Jump(Offs: Real);
    property Time: Integer read GetTime;
    property TotalSamples: Integer read FTotalSamples;
    property Valid: Boolean read GetValid;
    property Size: Integer read GetSize;
    property Position: Integer read GetPosition;
  published
    property EndSample: Integer read FEndSample write FEndSample;
    property FileName: string read FFileName write SetFileName;
    property Loop: Boolean read FLoop write FLoop;
    property StartSample: Integer read FStartSample write FStartSample;
  end;


  { TACSFileOut

    This class is an wrapper for all fileformats
  }
  TACSFileOut = class(TComponent)
  private
    FBufferSize: Integer;
    FFileMode: TACSFileOutputMode;
    FFileName: string;
    FOnDone:TACSOutputDoneEvent;
    FOnProgress: TACSOutputProgressEvent;
    FOnThreadException: TACSThreadExceptionEvent;
    FOutput: TACSCustomFileOut;
    FDialog: TSaveDialog;
    FInput: TACSCustomInput;
{$IFDEF LINUX}
    FAccessMask : Integer;
{$ENDIF}
    function GetDelay: Integer;
    function GetPriority: TTPriority;
    function GetProgress: Real;
    function GetStatus: TACSOutputStatus;
    function GetTE: Integer;
    procedure SetDelay(const AValue: Integer);
    procedure SetPriority(const AValue: TTPriority);

    procedure ThreadException(Sender: TComponent; E : Exception);
    procedure OutputDone(Sender: TComponent);
    procedure OutputProgress(Sender: TComponent);
  protected
    FBaseChannel: Integer;
    procedure SetInput(AInput: TACSCustomInput);
    procedure Done;
    function DoOutput(Abort: Boolean): Boolean;
    procedure Prepare;
    procedure SetFileMode(aMode: TACSFileOutputMode); virtual;
    procedure SetFileName(const AValue: string);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Open;
    property Buffersize: Integer read FBufferSize write FBufferSize;
    procedure Pause; virtual;
    procedure Resume; virtual;
    procedure Run;
    procedure Stop;
    property Delay: Integer read GetDelay write SetDelay;
    property ThreadPriority: TTPriority read GetPriority write SetPriority;
    property Progress: Real read GetProgress;
    property Status: TACSOutputStatus read GetStatus;
    property TimeElapsed: Integer read GetTE;
{$IFDEF LINUX}
    property AccessMask: Integer read FAccessMask write FAccessMask;
{$ENDIF}
  published
    property FileMode: TACSFileOutputMode read FFileMode write SetFileMode;
    property FileName: string read FFileName write SetFileName;
    property Input: TACSCustomInput read FInput write SetInput;
    property OnDone: TACSOutputDoneEvent read FOnDone write FOndone;
    property OnProgress: TACSOutputProgressEvent read FOnProgress write FOnProgress;
    property OnThreadException: TACSThreadExceptionEvent read FOnThreadException write FOnThreadException;
  end;

var
  FileFormats: TACSFileFormatsList;

implementation

{ TACSFileIn }

function TACSFileIn.GetBPS: Integer;
begin
  Result:=0;
  if Assigned(FInput) then Result:=FInput.BitsPerSample;
    //raise EACSException.Create(strNoFileOpened);
end;

function TACSFileIn.GetCh: Integer;
begin
  Result:=0;
  if Assigned(FInput) then Result:=FInput.Channels;
    //raise EACSException.Create(strNoFileOpened);
end;

function TACSFileIn.GetSR: Integer;
begin
  Result:=0;
  if Assigned(FInput) then Result:=FInput.SampleRate;
    //raise EACSException.Create(strNoFileOpened);
end;

function TACSFileIn.GetTime: Integer;
begin
  Result:=0;
  if Assigned(FInput) then Result:=FInput.Time;
    //raise EACSException.Create(strNoFileOpened);
end;

function TACSFileIn.GetValid: Boolean;
begin
  Result:=False;
  if Assigned(FInput) then Result:=FInput.Valid;
    //raise EACSException.Create(strNoFileOpened);
end;

function TACSFileIn.GetTotalTime: Real;
begin
  Result:=0;
  if Assigned(FInput) then Result:=FInput.TotalTime
end;

procedure TACSFileIn.Reset;
begin
  if Assigned(FInput) then FInput.Reset;
end;

procedure TACSFileIn.SetFileName(const AValue: string);
begin
  FFileName:=AValue;
  if Assigned(FInput) then FreeAndNil(FInput);
  if AValue='' then Exit;
  FInput:=TACSFileInClass(FileFormats.FindFromFileName(AValue, [fcLoad])).Create(nil);
  if Assigned(FInput) then FInput.FileName:=FFilename;
end;

function TACSFileIn.GetSize: Integer;
begin
  Result:=0;
  if Assigned(FInput) then Result:=FInput.Size;
end;

function TACSFileIn.GetPosition: Integer;
begin
  Result:=0;
  if Assigned(FInput) then Result:=FInput.Position;
end;

constructor TACSFileIn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FInput:=nil;
  FFileName:='';
  FOpened:=0;
  FValid:=False;
end;

destructor TACSFileIn.Destroy;
begin
  if Assigned(FInput) then FreeAndNil(FInput);
  inherited Destroy;
end;

procedure TACSFileIn.Open;
var
  desc: string;
begin
  desc:='';
  FDialog:=TOpenDialog.Create(nil);
  FileFormats.BuildFilterStrings(desc, [fcLoad]);
  FDialog.Filter:=desc;
  if FDialog.Execute then
  begin
    if Assigned(FInput) then FreeAndNil(FInput);
    FInput:=TACSFileInClass(FileFormats.FindFromFileName(FDialog.FileName, [fcLoad])).Create(nil);
    FFileName:=FDialog.FileName;
    if Assigned(FInput) then FInput.FileName:=FFilename;
  end;
  FDialog.Free;
end;

procedure TACSFileIn.Flush;
begin
  if Assigned(FInput) then FInput.Flush;
end;

procedure TACSFileIn.Init;
begin
  if Assigned(FInput) then FInput.Init;
    //raise EACSException.Create(strNoFileOpened);
end;

function TACSFileIn.Seek(SampleNum: Integer): Boolean;
begin
  Result:=False;
  if Assigned(FInput) then Result:=FInput.Seek(SampleNum);
    //EACSException.Create(strnoFileOpened);
end;

function TACSFileIn.GetData(Buffer: Pointer; BufferSize: Integer): Integer;
begin
  Result:=0;
  if Assigned(FInput) then Result:=FInput.GetData(Buffer, BufferSize);
    //raise EACSException.Create(strNoFileOpened);
end;

function TACSFileIn.SetStartTime(Minutes, Seconds: Integer): Boolean;
begin
  Result:=False;
  if Assigned(FInput) then Result:=FInput.SetStartTime(Minutes, Seconds);
    //raise EACSException.Create(strNoFileOpened);
end;

function TACSFileIn.SetEndTime(Minutes, Seconds: Integer): Boolean;
begin
  Result:=False;
  if Assigned(FInput) then Result:=FInput.SetEndTime(Minutes, Seconds);
    //raise EACSException.Create(strNoFileOpened);
end;

procedure TACSFileIn.Jump(Offs: Real);
begin
  if Assigned(FInput) then FInput.Jump(Offs);
    //raise EACSException.Create(strNoFileOpened);
end;

{ TACSFileOut }

procedure TACSFileOut.SetFileName(const AValue: string);
begin
  if FFileName=AValue then Exit;
  if Assigned(FOutput) then FreeAndNil(FOutput);
  FOutput:=TACSFileOutClass(FileFormats.FindFromFileName(AValue, [fcSave])).Create(nil);
  if Assigned(FOutput) then
  begin
    FOutput.FileName:=AValue;
    FOutput.FileMode:=FFileMode; //GAK:20060731
    FOutput.Input:=FInput;
    FOutput.OnDone:=OutputDone;
    FOutput.OnProgress:=OutputProgress;
    FOutput.OnThreadException:=ThreadException;
    FFileName:=AValue; //GAK:20060731
  end;
end;

constructor TACSFileOut.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FInput:=nil;
  FOutput:=nil;
end;

procedure TACSFileOut.Done;
begin
  if Assigned(FOutput) then FOutput.Done;
    //raise EACSException.Create(strNoFileOpened);
end;

function TACSFileOut.DoOutput(Abort: Boolean): Boolean;
begin
  Result:=False;
  if not Assigned(FOutput) then Result:=FOutput.DoOutput(Abort);
    //raise EACSException.Create(strNoFileOpened);
end;

procedure TACSFileOut.Prepare;
begin
  if Assigned(FOutput) then FOutput.Prepare;
    //raise EACSException.Create(strNoFileOpened);
end;

function TACSFileOut.GetDelay: Integer;
begin
  Result:=0;
  if not Assigned(FOutput) then Result:=FOutput.Delay;
    //raise EACSException.Create(strNoFileOpened);
end;

function TACSFileOut.GetPriority: TTPriority;
begin
  Result:=TTPriority.tpNormal;
  if not Assigned(FOutput) then Result:=FOutput.ThreadPriority;
    //raise EACSException.Create(strNoFileOpened);
end;

function TACSFileOut.GetProgress: Real;
begin
  Result:=0;
  if Assigned(FOutput) then Result:=FOutput.Progress;
    //raise EACSException.Create(strNoFileOpened);
end;

function TACSFileOut.GetStatus: TACSOutputStatus;
begin
  Result:=TACSOutputStatus.tosUndefined;
  if Assigned(FOutput) then Result:=FOutput.Status;
    //raise EACSException.Create(strNoFileOpened);
end;

function TACSFileOut.GetTE: Integer;
begin
  Result:=0;
  if Assigned(FOutput) then Result:=FOutput.TimeElapsed;
    //raise EACSException.Create(strNoFileOpened);
end;

procedure TACSFileOut.SetDelay(const AValue: Integer);
begin
  if Assigned(FOutput) then FOutput.Delay:=AValue;
    //raise EACSException.Create(strNoFileOpened);
end;

procedure TACSFileOut.SetPriority(const AValue: TTPriority);
begin
  if Assigned(FOutput) then FOutput.ThreadPriority:=AValue;
    //raise EACSException.Create(strNoFileOpened);
end;

procedure TACSFileOut.ThreadException(Sender: TComponent; E: Exception);
begin
  if Assigned(OnThreadException) then OnThreadException(Sender, E);
end;

procedure TACSFileOut.OutputDone(Sender: TComponent);
begin
  if Assigned(OnDone) then OnDone(Sender);
end;

procedure TACSFileOut.OutputProgress(Sender: TComponent);
begin
  if Assigned(OnProgress) then OnProgress(Sender);
end;

procedure TACSFileOut.SetInput(AInput: TACSCustomInput);
begin
  FInput:=AInput;
  if Assigned(FOutput) then FOutput.Input:=FInput;
end;

procedure TACSFileOut.SetFileMode(AMode: TACSFileOutputMode);
begin
  // GAK:20060731 changed whole of this method, as it was stopping component loading/creating
  if AMode <> FFileMode then
  begin
    FFileMode:=AMode;
    if Assigned(FOutput) then FOutput.FileMode:=AMode;
  end;
end;

procedure TACSFileOut.Open;
var
  desc: string;
begin
  desc:='';
  FDialog:=TSaveDialog.Create(nil);
  FileFormats.BuildFilterStrings(desc, [fcSave]);
  FDialog.Filter:=desc;
  if FDialog.Execute then
  begin
    FOutput:=TACSFileOutClass(FileFormats.FindFromFileName(FDialog.FileName, [fcSave])).Create(nil);
    FileName:=FDialog.FileName;
    FOutput.FileMode:=FFileMode;
    FOutput.Input:=FInput;
    FInput:=FInput;
    FOutput.OnDone:=OutputDone;
    FOutput.OnProgress:=OutputProgress;
    FOutput.OnThreadException:=ThreadException;
  end;
  FDialog.Free;
end;

procedure TACSFileOut.Pause;
begin
  if Assigned(FOutput) then FOutput.Pause;
    //raise EACSException.Create(strNoFileOpened);
end;

procedure TACSFileOut.Resume;
begin
  if Assigned(FOutput) then FOutput.Resume;
    //raise EACSException.Create(strNoFileOpened);
end;

procedure TACSFileOut.Run;
begin
  if Assigned(FOutput) then FOutput.Run;
    //raise EACSException.Create(strNoFileOpened);
end;

procedure TACSFileOut.Stop;
begin
  if Assigned(FOutput) then FOutput.Stop;
end;

destructor TACSFileOut.Destroy;
begin
  if Assigned(FOutput) then FreeAndNil(FOutput);
  inherited Destroy;
end;

{ TACSFileFormatsList }

destructor TACSFileFormatsList.Destroy;
var
  i: integer;
begin
  for i:=0 to Count-1 do TACSFileFormat(Items[i]).Free;
  inherited Destroy;
end;

procedure TACSFileFormatsList.Add(const Ext, Desc: String; AClass: TACSFormatClass);
var
  newRec: TACSFileFormat;
begin
  newRec:=TACSFileFormat.Create;
  with newRec do
  begin
    Extension:=LowerCase(Ext);
    FileClass:=AClass;
    Description:=Desc;
  end;
  inherited Add(newRec);
end;

function TACSFileFormatsList.FindExt(Ext: string; Typs: TACSFileCapTyps): TACSFormatClass;
var
  i: Integer;
begin
  Ext:=LowerCase(Ext);
  for i:=Count-1 downto 0 do
  begin
    with TACSFileFormat(Items[i]) do
    begin
      if ((fcLoad in Typs) and (TACSFileFormat(Items[i]).FileClass.InheritsFrom(TACSCustomFileIn)))
      or ((fcSave in Typs) and (TACSFileFormat(Items[i]).FileClass.InheritsFrom(TACSCustomFileOut))) then
      begin
        if Extension=Ext then
        begin
          Result:=TACSFileFormat(Items[i]).FileClass;
          Exit;
        end;
      end;
    end;
  end;
  Result:=nil;
end;

function TACSFileFormatsList.FindFromFileName(const FileName: String;
  Typs: TACSFileCapTyps): TACSFormatClass;
var
  Ext: String;
begin
  Ext:=ExtractFileExt(Filename);
  System.Delete(Ext, 1, 1);
  Result:=FindExt(Ext, Typs);
  if not Assigned(Result) then
    raise EACSException.CreateFmt(strUnknownExtension, [Ext]);
end;

procedure TACSFileFormatsList.Remove(AClass: TACSFormatClass);
var
  i: Integer;
begin
  for i:=Count-1 downto 0 do
  begin
    if TACSFileFormat(Items[i]).FileClass.InheritsFrom(AClass) then
    begin
      TACSFileFormat(Items[i]).Free;
      Delete(i);
    end;
  end;
end;

procedure TACSFileFormatsList.BuildFilterStrings(var Descriptions: String;
  Typs: TACSFileCapTyps);
var
  k, i: Integer;
  FileFormat: TACSFileFormat;
  sFilters: string;
begin
  Descriptions:='';
  sFilters := '';
  k:=0;
  for i:=0 to Count-1 do
  begin
    FileFormat:=TACSFileFormat(Items[i]);
    if ((fcLoad in Typs) and (FileFormat.FileClass.InheritsFrom(TACSCustomFileIn)))
    or ((fcSave in Typs) and (FileFormat.FileClass.InheritsFrom(TACSCustomFileOut))) then
    begin
      with FileFormat do
      begin
        if k<>0 then
        begin
          Descriptions:=Descriptions+'|';
          sFilters:=sFilters+';';
        end;
        Descriptions:=Descriptions+Description+' (*.'+Extension+')|'+'*.'+Extension;
        sFilters:=sFilters+'*.'+Extension;
        Inc(k);
      end;
    end;
  end;
  Descriptions:=strAllFormats+'|'+sFilters+'|'+Descriptions;
end;

initialization

  FileFormats:=TACSFileFormatsList.Create;

finalization

  FileFormats.Free;

end.

