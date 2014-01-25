unit MainForm;

{$mode objfpc}{$h+}

interface

uses
  SysUtils, Classes, Math, Graphics, Controls, Forms, Dialogs, ExtCtrls, Menus,
  StdCtrls, Buttons, Grids, Spin, ActnList, typinfo, types, DrawComponents,
  FileUtil, Clipbrd, ExtDlgs, fpjson, BGRABitmap, BGRABitmapTypes, Properties,
  Drawable, Utils, Transformations, Tools, History, Loaders, GDExport;

type
  { TGreyDrawForm }

  TGreyDrawForm = class(TForm)
    DeleteButton: TSpeedButton;
    Palette:   TDrawGrid;
    UndoButton: TSpeedButton;
    RedoButton: TSpeedButton;
    Sep1:      TMenuItem;
    CutAction: TMenuItem;
    CopyAction: TMenuItem;
    DeleteAction: TMenuItem;
    Sep2:      TMenuItem;
    RedoAction: TMenuItem;
    UndoAction: TMenuItem;
    PasteAction: TMenuItem;
    CutButton: TSpeedButton;
    CopyButton: TSpeedButton;
    PasteButton: TSpeedButton;
    RaiseButton: TSpeedButton;
    SinkButton: TSpeedButton;
    SinkBottomButton: TSpeedButton;
    RaiseTopButton: TSpeedButton;
    SaveItem:  TMenuItem;
    OpenItem:  TMenuItem;
    NewItem:   TMenuItem;
    NormalScaleAction: TAction;
    MinusAction: TAction;
    EscapeAction: TAction;
    FODialog:  TOpenDialog;
    PlusAction: TAction;
    FormActionList: TActionList;
    BgDlg:     TColorDialog;
    BdDlg:     TColorDialog;
    FSDialog:  TSaveDialog;
    SMinusBtn: TSpeedButton;
    NewFileButton: TSpeedButton;
    OpenFileButton: TSpeedButton;
    SaveFileButton: TSpeedButton;
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
    MenuItem1: TMenuItem;
    ExportItem: TMenuItem;
    ExportDalog: TSaveDialog;
    procedure AboutItemClick(Sender: TObject);
    procedure ActionExecute(Sender: TObject);
    procedure BrushColorPanelClick(Sender: TObject);
    procedure CopyActionClick(Sender: TObject);
    procedure CutActionClick(Sender: TObject);
    procedure DeleteActionClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure PaletteDrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
    procedure PaletteMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure NewFileButtonClick(Sender: TObject);
    procedure PasteActionClick(Sender: TObject);
    procedure ExitItemClick(Sender: TObject);
    procedure HScrollBarChange(Sender: TObject);
    procedure OpenFileButtonClick(Sender: TObject);
    procedure PenColorPanelClick(Sender: TObject);
    procedure RaiseButtonClick(Sender: TObject);
    procedure RaiseTopButtonClick(Sender: TObject);
    procedure RedoActionClick(Sender: TObject);
    procedure SaveFileButtonClick(Sender: TObject);
    procedure SinkBottomButtonClick(Sender: TObject);
    procedure SinkButtonClick(Sender: TObject);
    procedure SMinusBtnClick(Sender: TObject);
    procedure SNormalBtnClick(Sender: TObject);
    procedure SPlusBtnClick(Sender: TObject);
    procedure ToolButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure UndoActionClick(Sender: TObject);
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
    function ScrollRect: TRectF;
    procedure SetScrollRect;
    procedure SetScale(AScale: float);
    function SaveAction: Integer;
    function CanBeClosed: Boolean;
    procedure ToggleChangeTag(AChanged: Boolean);
    procedure VScrollBarScroll(Sender: TObject; ScrollCode: TScrollCode;
      var ScrollPos: Integer);
    procedure HScrollBarScroll(Sender: TObject; ScrollCode: TScrollCode;
      var ScrollPos: Integer);
    procedure ExportItemClick(Sender: TObject);
  end;

const
  ChangesTag = '•';

var
  GreyDrawForm: TGreyDrawForm;
  FirstBuffer, SecondBuffer: TBGRABitmap;
  OpenedFile:   String = 'Untitled';
  FileLoader:   TFileLoader;
  FileSaver:    TFileSaver;
  Exporter: TExporter;
  HasUnsavedChanges: Boolean;

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
    'PlusAction': SPlusBtn.Click;
    'MinusAction': SMinusBtn.Click;
    'NormalScaleAction': SNormalBtn.Click;
  end;
end;

procedure TGreyDrawForm.BrushColorPanelClick(Sender: TObject);
begin
  if BgDlg.Execute then
  begin
    BrushColorPanel.Color := BgDlg.Color;
    TBrushProperty.SetBrushColor(BgDlg.Color);
    FigureClosed := True;
  end;
end;

procedure TGreyDrawForm.CopyActionClick(Sender: TObject);
var
  i: Integer;
  j: TJSONObject;
begin
  j := TJSONObject.Create;
  for i := 0 to High(FiguresList) do
    if FiguresList[i].Selected then
      j.Add(FiguresList[i].CreateUUID, GreyDrawSaveToJSONData(FiguresList[i]));
  Clipboard.AsText := j.FormatJSON();
  j.Destroy;
end;

procedure TGreyDrawForm.CutActionClick(Sender: TObject);
begin
  Self.CopyActionClick(Sender);
  Self.DeleteActionClick(Sender);
  AnchorsList.Clear;
  ViewPort.Invalidate;
end;

procedure TGreyDrawForm.DeleteActionClick(Sender: TObject);
var
  i: Integer = 0;
  j: Integer;
begin
  while i <= High(FiguresList) do
  begin
    if FiguresList[i].Selected then
    begin
      FiguresList[i].Free;
      for j := i + 1 to High(FiguresList) do
        FiguresList[j - 1] := FiguresList[j];
      SetLength(FiguresList, Length(FiguresList) - 1);
    end
    else
      Inc(i);
  end;
  AnchorsList.Clear;
  ViewPort.Invalidate;
end;

procedure TGreyDrawForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := CanBeClosed;
end;

procedure TGreyDrawForm.PaletteDrawCell(Sender: TObject; aCol, aRow: Integer;
  aRect: TRect; aState: TGridDrawState);
begin
  with Sender as TDrawGrid do
  begin
    case aRow of
      0: Canvas.Brush.Color := clRed - aCol * 4;
      1: Canvas.Brush.Color := clBlue + aCol * 4;
      2: Canvas.Brush.Color := clGreen + aCol * 4;
      3: Canvas.Brush.Color := clLime + aCol * 4;
      4: Canvas.Brush.Color := clBlack + TColor($040404) * ACol;
    end;
    Canvas.FillRect(aRect);
    Canvas.Brush.Color := clWhite;
  end;
end;

procedure TGreyDrawForm.PaletteMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  with Sender as TDrawGrid do
    case Button of
      mbLeft:
      begin
        TBrushProperty.SetBrushColor(Canvas.Pixels[X, Y]);
        BrushColorPanel.Color := Canvas.Pixels[X, Y];
      end;
      mbRight:
      begin
        TPenProperty.SetPenColor(Canvas.Pixels[X, Y]);
        PenColorPanel.Color := Canvas.Pixels[X, Y];
      end;
    end;
end;

procedure TGreyDrawForm.NewFileButtonClick(Sender: TObject);
var
  i: Integer;
begin
  if HasUnsavedChanges then
    if MessageDlg('Вы хотите отбросить имеющиеся изменения и создать новый файл?',
      mtWarning, mbYesNo, 0) = mrYes then
    begin
      for i := 0 to High(FiguresList) do
        FiguresList[i].Destroy;
      SetLength(FiguresList, 0);
      HistoryClear;
      OpenedFile := 'Untitled';
      ViewPort.Invalidate;
      Self.ToggleChangeTag(True);
    end;
end;

procedure TGreyDrawForm.PasteActionClick(Sender: TObject);
begin
  GreyDrawLoadFromText(Clipboard.AsText);
end;

procedure TGreyDrawForm.ExitItemClick(Sender: TObject);
begin
  self.Close;
end;

procedure TGreyDrawForm.HScrollBarChange(Sender: TObject);
begin
  Offset.x := -HScrollBar.Position;
  ViewPort.Invalidate;
end;

procedure TGreyDrawForm.OpenFileButtonClick(Sender: TObject);
var
  FFile: Text;
begin
  if FODialog.Execute then
  begin
    OpenedFile := FODialog.FileName;
    Self.ToggleChangeTag(False);
    AssignFile(FFile, FODialog.FileName);
    Reset(FFile);
    ReadLn(FFile, CurrentMode);
    case CurrentMode of
      'CryoDraw vector image ver.3':
      begin
        FileLoader := @CryoDrawLoadFromFile;
        FileSaver  := @CryoDrawSaveToFile;
      end
      else
      begin
        FileLoader := @GreyDrawLoadFromFile;
        FileSaver  := @GreyDrawSaveToFile;
      end
    end;
    CloseFile(FFile);
    FileLoader(OpenedFile);
    HistoryClear;
  end;
end;

procedure TGreyDrawForm.PenColorPanelClick(Sender: TObject);
begin
  if BdDlg.Execute then
  begin
    PenColorPanel.Color := BdDlg.Color;
    TPenProperty.SetPenColor(BdDlg.Color);
    FigureClosed := True;
  end;
end;

procedure TGreyDrawForm.RaiseButtonClick(Sender: TObject);
var
  i: Integer;
  P: TFigure;
begin
  for i := 0 to High(FiguresList) do
    if FiguresList[i].Selected and (FiguresList[i] <>
      FiguresList[High(FiguresList)]) then
    begin
      P := FiguresList[i + 1];
      FiguresList[i + 1] := FiguresList[i];
      FiguresList[i] := P;
    end;
  ViewPort.Invalidate;
end;

procedure TGreyDrawForm.RaiseTopButtonClick(Sender: TObject);
var
  i:    Integer = 0;
  j, k: Integer;
begin
  for i := 0 to High(FiguresList) do
    if FiguresList[i].Selected then
      Inc(k);
  if k < High(FiguresList) then
    while i < High(FiguresList) do
    begin
      if FiguresList[i].Selected then
      begin
        SetLength(FiguresList, Length(FiguresList) + 1);
        FiguresList[High(FiguresList)] := FiguresList[i];
        for j := i + 1 to High(FiguresList) do
          FiguresList[j - 1] := FiguresList[j];
        SetLength(FiguresList, Length(FiguresList) - 1);
      end
      else
        Inc(i);
    end;
  ViewPort.Invalidate;
end;

procedure TGreyDrawForm.RedoActionClick(Sender: TObject);
begin
  GreyDrawLoadFromText(HistoryStepForward, True);
  AnchorsList.Clear;
end;

procedure TGreyDrawForm.SaveFileButtonClick(Sender: TObject);
begin
  SaveAction;
end;

procedure TGreyDrawForm.SinkBottomButtonClick(Sender: TObject);
var
  i, j: Integer;
begin
  i := High(FiguresList);
  while i > 0 do
  begin
    if FiguresList[i].Selected then
    begin
      SetLength(FiguresList, Length(FiguresList) + 1);
      for j := High(FiguresList) - 1 downto 0 do
        FiguresList[j + 1] := FiguresList[j];
      FiguresList[0] := FiguresList[i + 1];
      for j := i + 2 to High(FiguresList) do
        FiguresList[j - 1] := FiguresList[j];
      SetLength(FiguresList, Length(FiguresList) - 1);
    end
    else
      Dec(i);
  end;
  ViewPort.Invalidate;
end;

procedure TGreyDrawForm.SinkButtonClick(Sender: TObject);
var
  i: Integer;
  P: TFigure;
begin
  for i := High(FiguresList) downto 0 do
    if FiguresList[i].Selected and (FiguresList[i] <> FiguresList[0]) then
    begin
      P := FiguresList[i - 1];
      FiguresList[i - 1] := FiguresList[i];
      FiguresList[i] := P;
    end;
  ViewPort.Invalidate;
end;

procedure TGreyDrawForm.SMinusBtnClick(Sender: TObject);
var
  A, B: TPointF;
begin
  B := WorldToScreen(ViewPort.Width, ViewPort.Height);
  Self.SetScale(Scaling / 1.3);
  A := WorldToScreen(ViewPort.Width, ViewPort.Height);
  try
    Offset.x += round(B.x - A.x) div 2;
    Offset.y += round(B.y - A.y) div 2;
  except

  end;
  Self.SetScrollRect;
  ViewPort.Invalidate;
end;

procedure TGreyDrawForm.SNormalBtnClick(Sender: TObject);
var
  N: TPointF;
  c: float;
begin
  N.x := ViewPort.Width / abs(ScrollRect.Bottom - ScrollRect.Top);
  N.y := ViewPort.Height / abs(ScrollRect.Right - ScrollRect.Left);
  c   := min(N.x, N.y);
  Self.SetScale(c);
  Self.SetScrollRect;
  ViewPort.Invalidate;
end;

procedure TGreyDrawForm.SPlusBtnClick(Sender: TObject);
var
  A, B: TPointF;
begin
  A := WorldToScreen(ViewPort.Width, ViewPort.Height);
  Self.SetScale(Scaling * 1.3);
  B := WorldToScreen(ViewPort.Width, ViewPort.Height);
  try
    Offset.x -= round(B.x - A.x) div 2;
    Offset.y -= round(B.y - A.y) div 2;
  except

  end;
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
    for i := PropPanel.ControlCount - 1 downto 0 do
      PropPanel.Controls[i].Free;
    CurrentTool.CreateControls(TWinControl(PropPanel));
  end;
end;

procedure TGreyDrawForm.FormCreate(Sender: TObject);
begin
  FirstBuffer := TBGRABitmap.Create(ClientWidth, ClientHeight, BGRAWhite);
  ValidEvent  := @ViewPort.Invalidate;
  ChangeEvent := @ToggleChangeTag;
  Self.CreateToolButtons;
  CurrentTool := TLineTool;
  Self.SetScrollRect;
  GPanel := PropPanel;
  ViewPortCenter      := ScreenToWorld(ViewPort.Width div 2, ViewPort.Height div 2);
  Self.DoubleBuffered := True;
  PropPanel.DoubleBuffered := True;
  Self.ToggleChangeTag(True);
  Init;
end;

procedure TGreyDrawForm.UndoActionClick(Sender: TObject);
begin
  GreyDrawLoadFromText(HistoryStepBack, True);
  AnchorsList.Clear;
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
  CursorPos := Point(X, Y);
  CurrentTool.MouseMove(X, Y, Shift);
  Self.SetScrollRect;
  ViewPort.Invalidate;
end;

procedure TGreyDrawForm.ViewPortMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  BMouseDown := False;
  CurrentTool.MouseUp(X, Y, Shift);
  if CurrentTool.ClassParent <> TTool then
    HistoryPush(GreyDrawSaveToText);
  ViewPort.Invalidate;
end;

procedure TGreyDrawForm.ViewPortMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  if ssCtrl in Shift then
    if WheelDelta > 0 then
      Self.SPlusBtnClick(SPlusBtn)
    else
      Self.SMinusBtnClick(SMinusBtn)
  else if ssShift in Shift then
    with HScrollBar do
      SetParams(Position - WheelDelta, Min, Max)
  else
    with VScrollBar do
      SetParams(Position - WheelDelta, Min, Max);
  ViewPort.Invalidate;
end;

procedure TGreyDrawForm.ViewPortPaint(Sender: TObject);
var
  i: Integer;
begin
  FirstBuffer.Fill(clWhite);
  for i := 0 to High(FiguresList) do
    FiguresList[i].Draw(FirstBuffer);
  for i := 0 to High(FiguresList) do
    if FiguresList[i].Hovered or FiguresList[i].Selected then
      FiguresList[i].DrawSelection(FirstBuffer);
  for i := AnchorsList.Count - 1 downto 0 do
    (AnchorsList.Items[i] as TVertexAnchor).Draw(FirstBuffer);
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
  CursorPos := Point(ViewPort.Width div 2, ViewPort.Height div 2);
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

function TGreyDrawForm.ScrollRect: TRectF;
var
  i:    Integer;
  Rect: TRectF;
begin
  with Result do
  begin
    Left   := 0;
    Right  := ViewPort.Width;
    Top    := 0;
    Bottom := ViewPort.Height;
  end;
  for i := 0 to High(FiguresList) do
  begin
    Rect := FiguresList[i].Rect;
    with Result do
    begin
      Left   := Min(Left, Rect.Left);
      Right  := Max(Right, Rect.Right);
      Top    := Min(Top, Rect.Top);
      Bottom := Max(Bottom, Rect.Bottom);
    end;
  end;
end;

procedure TGreyDrawForm.SetScrollRect;
var
  R: TRectF;
begin
  R := Self.ScrollRect;
  HScrollBar.SetParams(HScrollBar.Position, trunc(R.Left), ceil(R.Right));
  VScrollBar.SetParams(VScrollBar.Position, trunc(R.Top), ceil(R.Bottom));
  HScrollBar.Visible := abs(HScrollBar.Max - HScrollBar.Min) > ViewPort.Width;
  VScrollBar.Visible := abs(VScrollBar.Max - VScrollBar.Min) > ViewPort.Height;
  stub.Visible := HScrollBar.Visible or VScrollBar.Visible;
end;

procedure TGreyDrawForm.SetScale(AScale: float);
begin
  if AScale > MaxScaleSize then
    AScale := MaxScaleSize;
  if AScale < MinScaleSize then
    AScale := MinScaleSize;
  Scaling  := AScale;
  SNormalBtn.Caption := Format('%3.1f%%', [AScale * 100]);
end;

function TGreyDrawForm.SaveAction: Integer;
begin
  if FileExists(OpenedFile) then
    GreyDrawSaveToFile(OpenedFile)
  else
  if FSDialog.Execute then
  begin
    if FileExists(FSDialog.FileName) then
      if MessageDlg('Вы хотите перезаписать файл?', mtWarning, mbYesNo, 0) <> mrYes then
        exit(0);
    OpenedFile := FSDialog.FileName;
    case FSDialog.FilterIndex of
      1: FileSaver := @GreyDrawSaveToFile;
      2: FileSaver := @CryoDrawSaveToFile;
    end;
    FileSaver(OpenedFile);
  end;
  Self.ToggleChangeTag(False);
  Result := 1;
end;

function TGreyDrawForm.CanBeClosed: Boolean;
var
  s: String;
begin
  Result := False;
  if HasUnsavedChanges then
    case MessageDlg('Сохранить изменения?',
        Concat('Сохранить изменения в "', OpenedFile, '" перед закрытием?'),
        mtWarning, mbYesNoCancel, 0, mbCancel) of
      mrYes: if SaveAction = 1 then
          Result   := True;
      mrNo: Result := True;
    end
  else
    Result := True;
end;

procedure TGreyDrawForm.ToggleChangeTag(AChanged: Boolean);
begin
  if AChanged then
  begin
    Self.Caption      := Concat(OpenedFile, ChangesTag, ' - GreyDraw');
    HasUnsavedChanges := True;
  end
  else
  begin
    Self.Caption      := Concat(OpenedFile, ' - GreyDraw');
    HasUnsavedChanges := False;
  end;
end;

procedure TGreyDrawForm.VScrollBarScroll(Sender: TObject; ScrollCode: TScrollCode;
  var ScrollPos: Integer);
begin
  Offset.y := -ScrollPos;
  ViewPort.Invalidate;
end;

procedure TGreyDrawForm.HScrollBarScroll(Sender: TObject; ScrollCode: TScrollCode;
  var ScrollPos: Integer);
begin
  Offset.x := -ScrollPos;
  ViewPort.Invalidate;
end;

procedure TGreyDrawForm.ExportItemClick(Sender: TObject);
begin
  if ExportDalog.Execute then
  begin
    if FileExists(ExportDalog.FileName) then
      if MessageDlg('Вы хотите перезаписать файл?', mtWarning, mbYesNo, 0) <> mrYes then
        exit;
    case ExportDalog.FilterIndex of
      1: Exporter := @BMPExport;
      2: Exporter := @PNGExport;
      3: Exporter := @JPEGExport;
      4: Exporter := @SVGExport;
    end;
    Exporter(ExportDalog.FileName);
  end;
end;

initialization

end.
