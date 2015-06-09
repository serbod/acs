unit MainForm;

{$mode objfpc}{$H+}

interface

uses
  {$IFDEF WINDOWS}
  ACS_DXAudio,  //DirectSound Driver
  {$ELSE}
  acs_alsaaudio, //Alsa Driver
  ACS_AOLive,    //AO Live Driver
  {$ENDIF}
  acs_stdaudio, //Wavemapper Driver
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, EditBtn, acs_file, acs_audio, acs_allformats, acs_classes;

type

  { TFormMain }

  TFormMain = class(TForm)
    AcsAudioIn1: TAcsAudioIn;
    AcsAudioOut1: TAcsAudioOut;
    AcsFileIn1: TAcsFileIn;
    AcsFileOut1: TAcsFileOut;
    btnStart: TButton;
    btnStop: TButton;
    FileNameEditOut: TFileNameEdit;
    FileNameEditInput: TFileNameEdit;
    gbInput: TGroupBox;
    gbOutput: TGroupBox;
    gbProgress: TGroupBox;
    lbOutPosition: TLabel;
    ListBoxAudioInDrivers: TListBox;
    ListBoxAudioInDevices: TListBox;
    ListBoxAudioOutDevices: TListBox;
    ListBoxFileOutDrivers: TListBox;
    ListBoxAudioOutDrivers: TListBox;
    ListBoxFileInDrivers: TListBox;
    MemoInputFileInfo: TMemo;
    MemoAudioInInfo: TMemo;
    pgcOutputTypes: TPageControl;
    pgcInputTypes: TPageControl;
    ProgressBar1: TProgressBar;
    tsInputFile: TTabSheet;
    tsOutFile: TTabSheet;
    tsOutAudioDevice: TTabSheet;
    tsInputAudioDevice: TTabSheet;
    procedure btnStartClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure FileNameEditInputAcceptFileName(Sender: TObject; var Value: String
      );
    procedure FileNameEditOutAcceptFileName(Sender: TObject; var Value: String);
    procedure FileNameEditOutEditingDone(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ListBoxAudioInDriversSelectionChange(Sender: TObject;
      User: boolean);
    procedure ListBoxAudioOutDriversSelectionChange(Sender: TObject;
      User: boolean);
    procedure ListBoxFileOutDriversSelectionChange(Sender: TObject;
      User: boolean);
  private
    { private declarations }
    procedure OnProgressHandler(Sender: TComponent);
  public
    { public declarations }
    AcsOut: TAcsCustomOutput;
    AcsIn: TAcsCustomInput;
    procedure PrepareAudioChain();
    procedure Start();
    procedure Stop();
  end;

var
  FormMain: TFormMain;

implementation

{$R *.lfm}

function SecondsToStr(AValue: Real): string;
begin
  Result:=Format('%.2d:%.2d:%.2d', [Round((AValue-30) / 60) mod 120, Round(AValue) mod 60, Round(AValue*100) mod 100]);
end;

{ TFormMain }

procedure TFormMain.FileNameEditInputAcceptFileName(Sender: TObject;
  var Value: String);
begin
  AcsFileIn1.FileName:=Value;
  MemoInputFileInfo.Clear();
  if AcsFileIn1.Valid then
  begin
    MemoInputFileInfo.Append('Sample rate: '+IntToStr(AcsFileIn1.SampleRate));
    MemoInputFileInfo.Append('Bits per sample: '+IntToStr(AcsFileIn1.BitsPerSample));
    MemoInputFileInfo.Append('Channels: '+IntToStr(AcsFileIn1.Channels));
    MemoInputFileInfo.Append('Size, bytes: '+IntToStr(AcsFileIn1.Size));
    MemoInputFileInfo.Append('Total samples: '+IntToStr(AcsFileIn1.TotalSamples));
    MemoInputFileInfo.Append('Total time: '+SecondsToStr(AcsFileIn1.TotalTime));
    MemoInputFileInfo.Append('Buffer size: '+IntToStr(AcsFileIn1.BufferSize));
  end
  else
  begin
    MemoInputFileInfo.Append('Invalid file format');
  end;
end;

procedure TFormMain.FileNameEditOutAcceptFileName(Sender: TObject;
  var Value: String);
begin
  AcsFileOut1.FileName:=Value;
end;

procedure TFormMain.FileNameEditOutEditingDone(Sender: TObject);
begin
  AcsFileOut1.FileName:=FileNameEditOut.FileName;
end;

procedure TFormMain.btnStartClick(Sender: TObject);
begin
  Start();
end;

procedure TFormMain.btnStopClick(Sender: TObject);
begin
  Stop();
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  // fill audio in drivers
  AcsAudioIn1.GetDriversList(ListBoxAudioInDrivers.Items);

  // fill file in drivers
  AcsFileIn1.GetDriversList(ListBoxFileInDrivers.Items);

  // fill file out drivers
  AcsFileOut1.GetDriversList(ListBoxFileOutDrivers.Items);

  // fill audio out drivers
  AcsAudioOut1.GetDriversList(ListBoxAudioOutDrivers.Items);

  btnStop.Enabled:=False;
end;

procedure TFormMain.ListBoxAudioInDriversSelectionChange(Sender: TObject;
  User: boolean);
var
  i, n: Integer;
begin
  if not User then Exit;
  for i:=0 to ListBoxAudioInDrivers.Count-1 do
  begin
    if ListBoxAudioInDrivers.Selected[i] then
    begin
      AcsAudioIn1.Driver:=AcsAudioIn1.Drivers[i];
      // fill devices list
      ListBoxAudioInDevices.Clear();
      for n:=0 to AcsAudioIn1.DeviceCount-1 do
      begin
        ListBoxAudioInDevices.AddItem(AcsAudioIn1.DeviceInfo[n].DeviceName, nil);
        if AcsAudioIn1.Device = n then ListBoxAudioInDevices.Selected[n]:=True;
      end;
      Exit;
    end;
  end;
end;

procedure TFormMain.ListBoxAudioOutDriversSelectionChange(Sender: TObject;
  User: boolean);
var
  i, n: Integer;
begin
  if not User then Exit;
  for i:=0 to ListBoxAudioOutDrivers.Count-1 do
  begin
    if ListBoxAudioOutDrivers.Selected[i] then
    begin
      AcsAudioOut1.Driver:=AcsAudioOut1.Drivers[i];
      // fill devices list
      ListBoxAudioOutDevices.Clear();
      for n:=0 to AcsAudioOut1.DeviceCount-1 do
      begin
        ListBoxAudioOutDevices.AddItem(AcsAudioOut1.DeviceInfo[n].DeviceName, nil);
        if AcsAudioOut1.Device = n then ListBoxAudioOutDevices.Selected[n]:=True;
      end;
      Exit;
    end;
  end;
end;

procedure TFormMain.ListBoxFileOutDriversSelectionChange(Sender: TObject;
  User: boolean);
var
  i: integer;
  SelectedExt: string;
begin
  if not User then Exit;
  for i:=0 to ListBoxFileOutDrivers.Count-1 do
  begin
    if ListBoxFileOutDrivers.Selected[i] then
    begin
      //ListBoxFileOutDrivers.Items.Objects[i];
    end;
  end;
  //SelectedExt:=
end;

procedure TFormMain.OnProgressHandler(Sender: TComponent);
begin
  ProgressBar1.Position:=Round(AcsFileIn1.Progress);
  lbOutPosition.Caption:=SecondsToStr(AcsOut.TimeElapsed);
end;

procedure TFormMain.PrepareAudioChain();
begin
  AcsIn:=AcsAudioIn1;
  if pgcInputTypes.ActivePage = tsInputFile then AcsIn:=AcsFileIn1;

  if pgcOutputTypes.ActivePage = tsOutAudioDevice then
  begin
    AcsOut:=AcsAudioOut1;
  end
  else if pgcOutputTypes.ActivePage = tsOutFile then
  begin
    AcsOut:=AcsFileOut1;
  end;
  AcsOut.Input:=AcsIn;
  AcsOut.OnProgress:=@OnProgressHandler;
end;

procedure TFormMain.Start();
begin
  PrepareAudioChain();
  AcsOut.Run();

  btnStart.Enabled:=not AcsOut.Active;
  btnStop.Enabled:=AcsOut.Active;
end;

procedure TFormMain.Stop();
begin
  if not Assigned(AcsOut) then Exit;
  AcsOut.Stop();
  btnStart.Enabled:=not AcsOut.Active;
  btnStop.Enabled:=AcsOut.Active;
end;

end.

