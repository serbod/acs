unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, Buttons,
  StdCtrls, acs_file, acs_audio, EditBtn,acs_classes,acs_mixer;

type

  { TfMain }

  TfMain = class(TForm)
    bOpen: TBitBtn;
    Mixer1: TACSMixer;
    AudioIn1: TACSAudioIn;
    FileOut1: TACSFileOut;
    bRecord: TBitBtn;
    bStop: TBitBtn;
    cbRecordSource: TComboBox;
    Label1: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure bOpenClick(Sender: TObject);
    procedure bRecordClick(Sender: TObject);
    procedure bStopClick(Sender: TObject);
    procedure cbRecordSourceChange(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  fMain: TfMain;

implementation

{ TfMain }

procedure TfMain.FormCreate(Sender: TObject);
var
  i : Integer;
begin
  for i := 0 to Mixer1.Channelcount-1 do
    if Mixer1.IsRecordable(i) then
      cbRecordSource.Items.Add(Mixer1.ChannelName[i]);
  cbRecordSource.Text := Mixer1.ChannelName[Mixer1.RecordSource];
end;

procedure TfMain.bOpenClick(Sender: TObject);
begin
  FileOut1.Open;
  if FileExists(FileOut1.FileName) then
    FileOut1.FileMode := foAppend;
end;

procedure TfMain.bRecordClick(Sender: TObject);
begin
  FileOut1.Run;
end;

procedure TfMain.bStopClick(Sender: TObject);
begin
  FileOut1.Stop;
end;

procedure TfMain.cbRecordSourceChange(Sender: TObject);
var
  i : Integer;
begin
  for i := 0 to Mixer1.Channelcount-1 do
    if Mixer1.ChannelName[i] = cbRecordSource.Text then
      Mixer1.RecordSource := i;
end;

initialization
  {$I umain.lrs}

end.

