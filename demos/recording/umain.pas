unit uMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ACS_Audio, ComCtrls, StdCtrls, ACS_Misc,
  ACS_VolumeQuery, ExtCtrls;

type

  { TfMain }

  TfMain = class(TForm)
    AudioIn1: TACSAudioIn;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    NULLOut1: TACSNULLOut;
    ProgressBar: TProgressBar;
    Timer1: TTimer;
    VolumeQuery: TACSVolumeQuery;
    procedure FormShow(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  fMain: TfMain;

implementation

{ TfMain }

procedure TfMain.FormShow(Sender: TObject);
begin
  NULLOut1.Run;
end;

procedure TfMain.Timer1Timer(Sender: TObject);
begin
  ProgressBar.Position := (round((VolumeQuery.dbLeft+VolumeQuery.dbRight)/2)+96);
end;

initialization
  {$I umain.lrs}

end.

