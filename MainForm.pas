unit MainForm;

{$mode objfpc}{$h+}

interface

uses
  SysUtils, Classes, Math, Graphics, Controls, Forms, Dialogs, ExtCtrls, Menus,
  StdCtrls, Buttons, BGRABitmap, BGRABitmapTypes, Grids, Spin, ActnList, Properties,
  RTTICtrls, Drawable, Transformations, Tools, typinfo, types, DrawComponents;

type

  { TGreyDrawForm }

  TGreyDrawForm = class(TForm)
    NormalScaleAction: TAction;
    MinusAction: TAction;
    EscapeAction: TAction;
    FODialog: TOpenDialog;
    PlusAction: TAction;
    FormActionList: TActionList;
    BgDlg:     TColorDialog;
    BdDlg:     TColorDialog;
    FSDialog: TSaveDialog;
    SMinusBtn: TSpeedButton;
    SPlusBtn:  TSpeedButton;
    SNormalBtn: TSpeedButton;
    stub:      TPanel;
    HScrollBar: TScrollBar;
    PropPanel: TPanel;
    Shape1:    TShape;
    Shape2:    TShape;
    VScrollBar: TScrollBar;
    ToolPropsPanel: TPanel;
    ToolsPanel: TPanel;
    PropsPanel: TPanel;
    ColorsPanel: TPanel;
    MainMenu1: TMainMenu;
    FileMenu:  TMenuItem;
    EditMenu:  TMenuItem;
    HelpMenu:  TMenuItem;
    ExitItem:  TMenuItem;
    AboutItem: TMenuItem;
    BrushColorPanel: TPanel;
    PenColorPanel: TPanel;
    ViewPort:  TPaintBox;
    procedure AboutItemClick(Sender: TObject);
    procedure ActionExecute(Sender: TObject);
    procedure BrushColorPanelClick(Sender: TObject);
    procedure ExitItemClick(Sender: TObject);
    procedure HScrollBarChange(Sender: TObject);
    procedure PenColorPanelClick(Sender: TObject);
    procedure SMinusBtnClick(Sender: TObject);
    procedure SNormalBtnClick(Sender: TObject);
    procedure SPlusBtnClick(Sender: TObject);
    procedure ToolButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ViewPortMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ViewPortMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure ViewPortMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ViewPortMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure ViewPortPaint(Sender: TObject);
    procedure ViewPortResize(Sender: TObject);
    procedure CreateToolButtons;
    procedure SetScrollRect;
    procedure VScrollBarChange(Sender: TObject);
  end;

var
  GreyDrawForm: TGreyDrawForm;
  FirstBuffer, SecondBuffer: TBGRABitmap;

implementation

{$R *.lfm}

{ TGreyDrawForm }

procedure TGreyDrawForm.AboutItemClick(Sender: TObject);
begin
  MessageDlg('Савва Суренков, Б8103а(1)', mtInformation, [mbClose], 0);
end;

procedure TGreyDrawForm.ActionExecute(Sender: TObject);
begin
  case (Sender as TAction).Name of
    'EscapeAction': FigureClosed := True;
    'PlusAction': SPlusBtnClick(SPlusBtn);
    'MinusAction': SMinusBtnClick(SMinusBtn);
    'NormalScaleAction': SNormalBtnClick(SNormalBtn);
  end;
end;

procedure TGreyDrawForm.BrushColorPanelClick(Sender: TObject);
begin
  if BgDlg.Execute then
  begin
    BrushColorPanel.Color := BgDlg.Color;
    DrawProperty.SetBrushColor(BgDlg.Color);
    FigureClosed := True;
  end;
end;

procedure TGreyDrawForm.ExitItemClick(Sender: TObject);
begin
  Self.Close;
end;

procedure TGreyDrawForm.HScrollBarChange(Sender: TObject);
begin
  Offset.x := -HScrollBar.Position;
  ViewPort.Invalidate;
end;

procedure TGreyDrawForm.PenColorPanelClick(Sender: TObject);
begin
  if BdDlg.Execute then
  begin
    PenColorPanel.Color := BdDlg.Color;
    DrawProperty.SetPenColor(BdDlg.Color);
    FigureClosed := True;
  end;
end;

procedure TGreyDrawForm.SMinusBtnClick(Sender: TObject);
var
  Center: TPointF;
begin
  Scaling /= 1.3;
  Self.SetScrollRect;
  ViewPort.Invalidate;
end;

procedure TGreyDrawForm.SNormalBtnClick(Sender: TObject);
var
  Center: TPointF;
begin
  Scaling := 1;
  Self.SetScrollRect;
  ViewPort.Invalidate;
end;

procedure TGreyDrawForm.SPlusBtnClick(Sender: TObject);
var
  Center: TPointF;
begin
  Scaling *= 1.3;
  Self.SetScrollRect;
  ViewPort.Invalidate;
end;

procedure TGreyDrawForm.ToolButtonClick(Sender: TObject);
var
  i: Integer;
begin
  FigureClosed := True;
  if CurrentTool <> (Sender as TToolButton).Tool then
  begin
    CurrentTool := (Sender as TToolButton).Tool;
    for i := PropPanel.ControlCount - 1 downto 0 do PropPanel.Controls[i].Free;
    CurrentTool.CreateControls(TWinControl(PropPanel));
  end;
end;

procedure TGreyDrawForm.FormCreate(Sender: TObject);
begin
  FirstBuffer := TBGRABitmap.Create(ClientWidth, ClientHeight, BGRAWhite);
  Self.CreateToolButtons;
  CurrentTool := TLineTool;
  Self.SetScrollRect;
  ViewPortCenter      := ScreenToWorld(ViewPort.Width div 2, ViewPort.Height div 2);
  Self.DoubleBuffered := True;
  PropPanel.DoubleBuffered := True;
end;

procedure TGreyDrawForm.ViewPortMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  BMouseDown := True;
  CurrentTool.MouseDown(X, Y, Shift);
  ViewPort.Invalidate;
end;

procedure TGreyDrawForm.ViewPortMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  CursorPos.x := X;
  CursorPos.y := Y;
  CurrentTool.MouseMove(X, Y, Shift);
  Self.SetScrollRect;
  ViewPort.Invalidate;
end;

procedure TGreyDrawForm.ViewPortMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  BMouseDown := False;
  CurrentTool.MouseUp(X, Y, Shift);
  ViewPort.Invalidate;
end;

procedure TGreyDrawForm.ViewPortMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  if ssCtrl in Shift then if WheelDelta > 0 then Self.SPlusBtnClick(SPlusBtn)
    else
      Self.SMinusBtnClick(SMinusBtn)
  else if ssShift in Shift then
  begin
    HScrollBar.Position := HScrollBar.Position - WheelDelta;
  end
  else
  begin
    VScrollBar.Position := VScrollBar.Position - WheelDelta;
  end;
  ViewPort.Invalidate;
end;

procedure TGreyDrawForm.ViewPortPaint(Sender: TObject);
var
  i: Integer;
begin
  FirstBuffer.Fill(clWhite);
  for i := 0 to High(FiguresList) do FiguresList[i].Draw(FirstBuffer);
  for i := 0 to High(FiguresList) do
    if FiguresList[i].Hovered or FiguresList[i].Selected then
      FiguresList[i].DrawSelection(FirstBuffer);
  for i := AnchorsList.Count - 1 downto 0 do
    (AnchorsList.Items[i] as TPointAnchor).Draw(FirstBuffer);
  with SelectionRect do
  begin
    FirstBuffer.PenStyle := psDash;
    FirstBuffer.RectangleAntialias(Left, Top, Right, Bottom, SelectedColor, 1);
  end;
  FirstBuffer.Draw(ViewPort.Canvas, 0, 0, True);
end;

procedure TGreyDrawForm.ViewPortResize(Sender: TObject);
begin
  VScrollBar.PageSize := ViewPort.Height;
  HScrollBar.PageSize := ViewPort.Width;
  CursorPos.x := ViewPort.Width div 2;
  CursorPos.y := ViewPort.Height div 2;
  Self.SetScrollRect;
  ViewPort.Invalidate;
end;

procedure TGreyDrawForm.CreateToolButtons;
var
  i:      Integer;
  Button: TToolButton;
begin
  for i := 0 to High(FigureToolsList) do
  begin
    Button := TToolButton.Create(ToolsPanel);
    with Button do
    begin
      Parent   := ToolsPanel;
      Height   := 30;
      Width    := 30;
      Left     := 15;
      Top      := i * Height + (i + 1) * Left;
      Name     := FigureToolsList[i].ClassName();
      Glyph    := LoadImage(FigureToolsList[i].Image);
      Hint     := FigureToolsList[i].Hint;
      Tool     := FigureToolsList[i];
      ShowHint := True;
      DoubleBuffered := True;
      OnClick  := @ToolButtonClick;
    end;
  end;
end;

procedure TGreyDrawForm.SetScrollRect;
var
  i: Integer;
  CRect, Rect: TRectF;
begin
  with Rect do
  begin
    Left   := 0;
    Right  := ViewPort.Width;
    Top    := 0;
    Bottom := ViewPort.Height;
  end;
  for i := 0 to High(FiguresList) do
  begin
    CRect := FiguresList[i].Rect;
    with Rect do
    begin
      Left   := Min(Left, CRect.Left);
      Right  := Max(Right, CRect.Right);
      Top    := Min(Top, CRect.Top);
      Bottom := Max(Bottom, CRect.Bottom);
    end;
  end;
  HScrollBar.Min := trunc(Rect.Left);
  HScrollBar.Max := ceil(Rect.Right);
  VScrollBar.Min := trunc(Rect.Top);
  VScrollBar.Max := ceil(Rect.Bottom);
end;

procedure TGreyDrawForm.VScrollBarChange(Sender: TObject);
begin
  Offset.y := -VScrollBar.Position;
  ViewPort.Invalidate;
end;

end.
