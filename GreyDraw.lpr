program GreyDraw;

{$mode objfpc}{$h+}

uses
	Forms,
	Interfaces,
	MainForm,
	Drawable;

{$R *.res}

begin
	Application.Initialize;
  Application.CreateForm(TGreyDrawForm, GreyDrawForm);
	Application.Run;
end.
