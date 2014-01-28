unit JPEGForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  ComCtrls, StdCtrls;

type
   TQualityChangeEvent = procedure(AQual: TJPEGQualityRange);

  { TJPEGQualitySelector }

  TJPEGQualitySelector = class(TForm)
    TrackBar1: TTrackBar;
    Label1: TLabel;
    Button1: TButton;
    procedure TrackBar1Change(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
  end;

var
  SetQuality: TQualityChangeEvent;

implementation

{$R *.lfm}

{ TJPEGQualitySelector }

procedure TJPEGQualitySelector.TrackBar1Change(Sender: TObject);
begin
  Label1.Caption := Format('Quality: %d%%', [TTrackBar(Sender).Position]);
end;

procedure TJPEGQualitySelector.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  SetQuality(TrackBar1.Position);
end;

procedure TJPEGQualitySelector.FormCreate(Sender: TObject);
begin
  TrackBar1.Position:=70;
end;

end.

