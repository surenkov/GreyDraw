unit Tools;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, FileUtil, Controls, StdCtrls, ExtCtrls, Forms,
  Dialogs, Math, typinfo, BGRABitmap, BGRABitmapTypes, LCLIntf, LCLType,
  Properties, Drawable, Loaders, Transformations, Utils;

type

  { TTool }

  TTool = class(TPersistent)
    class procedure CreateControls(var AOwner: TWinControl); virtual;
    class procedure MouseDown(X, Y: Integer; Shift: TShiftState); virtual;
    class procedure MouseMove(X, Y: Integer; Shift: TShiftState); virtual;
    class procedure MouseUp(X, Y: Integer; Shift: TShiftState); virtual;
    class procedure CreateFigure; virtual; abstract;
    class function Image: String; virtual; abstract;
    class function Hint: String; virtual; abstract;
  end;

  TToolClass = class of TTool;
  TToolClassList = array of TToolClass;

  { TPenFigureTool }

  TPenFigureTool = class(TTool)
    class procedure MouseDown(X, Y: Integer; Shift: TShiftState); override;
    class procedure CreateFigure; override;
    class procedure CreateControls(var AOwner: TWinControl); override;
  end;

  { TBrushFigureTool }

  TBrushFigureTool = class(TPenFigureTool)
    class procedure CreateFigure; override;
    class procedure CreateControls(var AOwner: TWinControl); override;
  end;

  { TLineTool }

  TLineTool = class(TPenFigureTool)
    class procedure CreateControls(var AOwner: TWinControl); override;
    class procedure MouseDown(X, Y: Integer; Shift: TShiftState); override;
    class procedure MouseMove(X, Y: Integer; Shift: TShiftState); override;
    class procedure CreateFigure; override;
    class function Image: String; override;
    class function Hint: String; override;
  end;

  { TPolylineTool }

  TPolylineTool = class(TPenFigureTool)
    class procedure CreateControls(var AOwner: TWinControl); override;
    class procedure MouseDown(X, Y: Integer; Shift: TShiftState); override;
    class procedure MouseMove(X, Y: Integer; Shift: TShiftState); override;
    class procedure CreateFigure; override;
    class function Image: String; override;
    class function Hint: String; override;
  end;

  { TBezierTool }

  TBezierTool = class(TPenFigureTool)
    class procedure CreateControls(var AOwner: TWinControl); override;
    class procedure MouseDown(X, Y: Integer; Shift: TShiftState); override;
    class procedure MouseMove(X, Y: Integer; Shift: TShiftState); override;
    class procedure CreateFigure; override;
    class function Image: String; override;
    class function Hint: String; override;
  end;

  { TPolygonTool }

  TPolygonTool = class(TBrushFigureTool)
    class procedure CreateControls(var AOwner: TWinControl); override;
    class procedure MouseDown(X, Y: Integer; Shift: TShiftState); override;
    class procedure MouseMove(X, Y: Integer; Shift: TShiftState); override;
    class procedure CreateFigure; override;
    class function Image: String; override;
    class function Hint: String; override;
  end;

  { TRectangleTool }

  TRectangleTool = class(TBrushFigureTool)
    class procedure CreateControls(var AOwner: TWinControl); override;
    class procedure MouseDown(X, Y: Integer; Shift: TShiftState); override;
    class procedure MouseMove(X, Y: Integer; Shift: TShiftState); override;
    class procedure CreateFigure; override;
    class function Image: String; override;
    class function Hint: String; override;
  end;

  { TRoundRectTool }

  TRoundRectTool = class(TBrushFigureTool)
    class procedure CreateControls(var AOwner: TWinControl); override;
    class procedure MouseDown(X, Y: Integer; Shift: TShiftState); override;
    class procedure MouseMove(X, Y: Integer; Shift: TShiftState); override;
    class procedure CreateFigure; override;
    class function Image: String; override;
    class function Hint: String; override;
  end;

  { TEllipseTool }

  TEllipseTool = class(TBrushFigureTool)
    class procedure CreateControls(var AOwner: TWinControl); override;
    class procedure MouseDown(X, Y: Integer; Shift: TShiftState); override;
    class procedure MouseMove(X, Y: Integer; Shift: TShiftState); override;
    class procedure CreateFigure; override;
    class function Image: String; override;
    class function Hint: String; override;
  end;

  { TRegularPolygonTool }

  TRegularPolygonTool = class(TBrushFigureTool)
    class procedure CreateControls(var AOwner: TWinControl); override;
    class procedure MouseDown(X, Y: Integer; Shift: TShiftState); override;
    class procedure MouseMove(X, Y: Integer; Shift: TShiftState); override;
    class procedure CreateFigure; override;
    class function Image: String; override;
    class function Hint: String; override;
  end;

  { TTextTool }

  TTextTool = class(TTool)
    class procedure CreateControls(var AOwner: TWinControl); override;
    class procedure MouseDown(X, Y: Integer; Shift: TShiftState); override;
    class procedure MouseMove(X, Y: Integer; Shift: TShiftState); override;
    class procedure CreateFigure; override;
    class function Image: String; override;
    class function Hint: String; override;
  end;

  { TDragTool }

  TDragTool = class(TTool)
    class procedure MouseDown(X, Y: Integer; Shift: TShiftState); override;
    class procedure MouseMove(X, Y: Integer; Shift: TShiftState); override;
    class procedure MouseUp(X, Y: Integer; Shift: TShiftState); override;
    class function Image: String; override;
    class function Hint: String; override;
  end;

  { TSelectTool }

  TSelectTool = class(TTool)
    class procedure CreateControls(var AOwner: TWinControl); override;
    class procedure MouseDown(X, Y: Integer; Shift: TShiftState); override;
    class procedure MouseMove(X, Y: Integer; Shift: TShiftState); override;
    class procedure MouseUp(X, Y: Integer; Shift: TShiftState); override;
    class function Image: String; override;
    class function Hint: String; override;
  end;

  { TSprayTool }

  TSprayTool = class(TPenFigureTool)
  private
    class var Radius, Intensity: Integer;
  published
    class procedure CreateControls(var AOwner: TWinControl); override;
    class procedure MouseDown(X, Y: Integer; Shift: TShiftState); override;
    class procedure MouseMove(X, Y: Integer; Shift: TShiftState); override;
    class procedure MouseUp(X, Y: Integer; Shift: TShiftState); override;
    class procedure OnTimer(Sender: TObject);
    class procedure CreateFigure; override;
    class function Image: String; override;
    class function Hint: String; override;
  end;

var
  FigureToolsList, EditorTools: TToolClassList;
  CurrentTool: TToolClass;
  FigureClosed: Boolean = True;
  ControlsCreated: Boolean;
  DeltaX, DeltaY: Single;
  GPanel: TPanel;
  SelectionRect: TRect;

procedure RegisterTool(AClass: TToolClass);

implementation

var
  index:     Integer;
  DeltaP:    TPointF;
  JustTimer: TTimer;


procedure RegisterTool(AClass: TToolClass);
begin
  SetLength(FigureToolsList, Length(FigureToolsList) + 1);
  FigureToolsList[High(FigureToolsList)] := AClass;
  RegisterClass(AClass);
end;

{ TSprayTool }

class procedure TSprayTool.CreateControls(var AOwner: TWinControl);
begin
  ctrllol:=0;
  TSprayProperty.CreateControls(AOwner);
end;

class procedure TSprayTool.MouseDown(X, Y: Integer; Shift: TShiftState);
begin
  Self.CreateFigure;
  inherited MouseDown(X, Y, Shift);
  CurrentFigure^.AddPoint(X, Y);
  JustTimer.OnTimer  := @OnTimer;
  JustTimer.Interval := 10;
  JustTimer.Enabled  := True;
end;

class procedure TSprayTool.MouseMove(X, Y: Integer; Shift: TShiftState);
begin
  if BMouseDown then
    CurrentFigure^.AddPoint(ScreenToWorld(X, Y));
end;

class procedure TSprayTool.MouseUp(X, Y: Integer; Shift: TShiftState);
begin
  inherited MouseUp(X, Y, Shift);
  JustTimer.Enabled := False;
end;

class procedure TSprayTool.OnTimer(Sender: TObject);
begin
  CurrentFigure^.AddPoint(CurrentFigure^.GetPoint);
  ValidEvent;
end;

class procedure TSprayTool.CreateFigure;
begin
  SetLength(FiguresList, Length(FiguresList) + 1);
  CurrentFigure  := @FiguresList[High(FiguresList)];
  CurrentFigure^ := TSprayFigure.Create;
  inherited CreateFigure;
  TSprayProperty.SetDefaultProperties(FiguresList);
end;

class function TSprayTool.Image: String;
begin
  Result := 'spray.png';
end;

class function TSprayTool.Hint: String;
begin
  Result := 'Спрей';
end;

{ TPenFigureTool }

class procedure TPenFigureTool.MouseDown(X, Y: Integer; Shift: TShiftState);
begin
  inherited MouseDown(X, Y, Shift);
  ChangeEvent(True);
end;

class procedure TPenFigureTool.CreateFigure;
var
  F: TFigure;
begin
  for F in FiguresList do
    F.Selected := False;
  CurrentFigure^.Selected := True;
  TPenProperty.SetDefaultProperties(FiguresList);
  ValidEvent;
end;

class procedure TPenFigureTool.CreateControls(var AOwner: TWinControl);
begin
  ctrllol := 0;
  TPenProperty.CreateControls(AOwner);
end;

{ TTextTool }

class procedure TTextTool.CreateControls(var AOwner: TWinControl);
begin
  ctrllol := 0;
  TTextProperty.CreateControls(AOwner);
end;

class procedure TTextTool.MouseDown(X, Y: Integer; Shift: TShiftState);
begin
  inherited MouseDown(X, Y, Shift);
end;

class procedure TTextTool.MouseMove(X, Y: Integer; Shift: TShiftState);
begin
  inherited MouseMove(X, Y, Shift);
end;

class procedure TTextTool.CreateFigure;
begin

end;

class function TTextTool.Image: String;
begin
  Result := 'text.png';
end;

class function TTextTool.Hint: String;
begin
  Result := 'Текст';
end;

{ TRegularPolygonTool }

class procedure TRegularPolygonTool.CreateControls(var AOwner: TWinControl);
begin
  inherited CreateControls(AOwner);
  TRegularProperty.CreateControls(AOwner);
end;

class procedure TRegularPolygonTool.MouseDown(X, Y: Integer; Shift: TShiftState);
begin
  Self.CreateFigure;
  inherited MouseDown(X, Y, Shift);
  CurrentFigure^.AddPoint(X, Y);
  CurrentFigure^.AddPoint(X + 1, Y + 1);
end;

class procedure TRegularPolygonTool.MouseMove(X, Y: Integer; Shift: TShiftState);
begin
  if BMouseDown then
    CurrentFigure^.ChangePoint(ScreenToWorld(X, Y));
end;

class procedure TRegularPolygonTool.CreateFigure;
begin
  SetLength(FiguresList, Length(FiguresList) + 1);
  CurrentFigure  := @FiguresList[High(FiguresList)];
  CurrentFigure^ := TRegularPolygon.Create;
  inherited CreateFigure;
  TRegularProperty.SetDefaultProperties(FiguresList);
end;

class function TRegularPolygonTool.Image: String;
begin
  Result := 'rightpoly.png';
end;

class function TRegularPolygonTool.Hint: String;
begin
  Result := 'Правильный многоугольник';
end;

{ TSelectTool }

class procedure TSelectTool.CreateControls(var AOwner: TWinControl);
begin

end;

type
  TSelectionState = (ssNone, ssPoint, ssPoints);
  TMovementState = (msNone, msMoved);

var
  SelectState:   TSelectionState = ssNone;
  MovementState: TMovementState = msNone;

class procedure TSelectTool.MouseDown(X, Y: Integer; Shift: TShiftState);
var
  i, j, k: Integer;
  A: TVertexAnchor;
  R: HRGN;
  CurrentState: TSelectionState = ssNone;
  BreakFlag: Boolean = False;
begin
  for i := High(FiguresList) downto 0 do
    for j := FiguresList[i].PointsCount downto 0 do
      if TVertexAnchor.IsSelected(FiguresList[i].GetPoint(j), X, Y) then
        CurrentState := ssPoint;
  if CurrentState = ssNone then
    for i := High(FiguresList) downto 0 do
    begin
      R := FiguresList[i].Region;
      try
        if PtInRegion(R, X, Y) then
          CurrentState := ssPoints;
      finally
        DeleteObject(R);
      end;
    end;
  case CurrentState of
    ssPoint:
    begin
      for i := AnchorsList.Count - 1 downto 0 do
        with AnchorsList.Items[i] as TVertexAnchor do
          if not (ssShift in Shift) then
            Selected := IsSelected(X, Y)
          else
          if IsSelected(X, Y) then
            Selected := True;
    end;
    ssPoints:
    begin
      if not (ssShift in Shift) then
      begin
        for i := High(FiguresList) downto 0 do
          FiguresList[i].Selected := False;
        AnchorsList.Clear;
      end;
      for i := High(FiguresList) downto 0 do
      begin
        R := FiguresList[i].Region;
        try
          if PtInRegion(R, X, Y) then
          begin
            if not FiguresList[i].Selected then
            begin
              FiguresList[i].Selected := True;
              for j := FiguresList[i].PointsCount downto 0 do
              begin
                A := AnchorsList.Add as TVertexAnchor;
                A.SetPoint(FiguresList[i].GetPointAddr(j));
                A.Selected := True;
              end;
            end;
            DeleteObject(R);
            Break;
          end;
        finally
          DeleteObject(R);
        end;
      end;
    end;
    ssNone:
    begin
      AnchorsList.Clear;
      for i := High(FiguresList) downto 0 do
        FiguresList[i].Selected := False;
    end;
  end;
  SelectState   := CurrentState;
  SelectionRect := Rect(X, Y, X, Y);
end;

class procedure TSelectTool.MouseMove(X, Y: Integer; Shift: TShiftState);
var
  i, j:  Integer;
  A:     TVertexAnchor;
  R:     HRGN;
  CurrentState: TSelectionState = ssNone;
  P, SP: TPointF;
begin
  if BMouseDown then
  begin
    SP := ScreenToWorld(X, Y);
    if SelectState = ssNone then
    begin
      AnchorsList.Clear;
      SelectionRect := Rect(SelectionRect.Left, SelectionRect.Top, X, Y);
      for i := 0 to High(FiguresList) do
      begin
        R := FiguresList[i].Region;
        try
          FiguresList[i].Selected := RectInRegion(R, SelectionRect);
        finally
          DeleteObject(R);
        end;
      end;
      for i := 0 to High(FiguresList) do
        if FiguresList[i].Selected then
          for j := 0 to FiguresList[i].PointsCount do
          begin
            A := AnchorsList.Add as TVertexAnchor;
            A.SetPoint(FiguresList[i].GetPointAddr(j));
            A.Selected := True;
          end;
    end
    else
      for i := 0 to AnchorsList.Count - 1 do
        with AnchorsList.Items[i] as TVertexAnchor do
          if Selected then
          begin
            P := GetPoint;
            ChangePoint(P.x - DeltaP.x + SP.x, P.y - DeltaP.y + SP.y);
            MovementState := msMoved;
          end;
  end;
  inherited MouseMove(X, Y, Shift);
end;

class procedure TSelectTool.MouseUp(X, Y: Integer; Shift: TShiftState);

  function GetLeastCommonClass(AFClass, ASClass: TClass): TClass;
  begin
    if ASClass.InheritsFrom(AFClass) then
      exit(AFClass)
    else if AFClass.InheritsFrom(ASClass) then
      exit(ASClass)
    else
      exit(GetLeastCommonClass(AFClass.ClassParent, ASClass.ClassParent));
  end;

var
  i, j: Integer;
  lcc:  TClass = nil;
begin
  if (MovementState = msMoved) and (SelectState <> ssNone) then
    ChangeEvent(True);
  for i := 0 to High(FiguresList) do
    if FiguresList[i].Selected then
    begin
      lcc := FiguresList[i].ClassType;
      break;
    end;
  for i := 0 to High(FiguresList) do
    if FiguresList[i].Selected then
    begin
      lcc := GetLeastCommonClass(lcc, FiguresList[i].ClassType);
    end;
  for i := GPanel.ControlCount - 1 downto 0 do
    GPanel.Controls[i].Free;
  if lcc <> nil then
    TToolClass(GetClass(Concat(lcc.ClassName, 'Tool'))).CreateControls(
      TWinControl(GPanel));
  SelectionRect := Rect(0, 0, 0, 0);
  if AnchorsList.Count = 0 then
    SelectState := ssNone
  else
    SelectState := ssPoint;
  for i := 0 to High(FiguresList) do
    if FiguresList[i].Selected then
      Inc(j);
  if j > 1 then
    SelectState := ssPoints;
  MovementState := msNone;
end;

class function TSelectTool.Image: String;
begin
  Result := 'figuresel.png';
end;

class function TSelectTool.Hint: String;
begin
  Result := 'Выделение фигуры';
end;

{ TRoundRectTool }

class procedure TRoundRectTool.CreateControls(var AOwner: TWinControl);
begin
  inherited CreateControls(AOwner);
  TRoundProperty.CreateControls(AOwner);
end;

class procedure TRoundRectTool.MouseDown(X, Y: Integer; Shift: TShiftState);
begin
  Self.CreateFigure;
  inherited MouseDown(X, Y, Shift);
  CurrentFigure^.AddPoint(X, Y);
  CurrentFigure^.AddPoint(X, Y);
end;

class procedure TRoundRectTool.MouseMove(X, Y: Integer; Shift: TShiftState);
begin
  if BMouseDown then
    CurrentFigure^.ChangePoint(ScreenToWorld(X, Y));
end;

class procedure TRoundRectTool.CreateFigure;
begin
  SetLength(FiguresList, Length(FiguresList) + 1);
  CurrentFigure  := @FiguresList[High(FiguresList)];
  CurrentFigure^ := TRoundRect.Create;
  inherited CreateFigure;
  TRoundProperty.SetDefaultProperties(FiguresList);
end;

class function TRoundRectTool.Image: String;
begin
  Result := 'rectangle.png';
end;

class function TRoundRectTool.Hint: String;
begin
  Result := 'Прямоугольник со скругленными краями';
end;

{ TBrushFigureTool }

class procedure TBrushFigureTool.CreateFigure;
begin
  inherited CreateFigure;
  TBrushProperty.SetDefaultProperties(FiguresList);
end;

class procedure TBrushFigureTool.CreateControls(var AOwner: TWinControl);
begin
  inherited CreateControls(AOwner);
  TBrushProperty.CreateControls(AOwner);
end;

{ TDragTool }

class procedure TDragTool.MouseDown(X, Y: Integer; Shift: TShiftState);
begin
  DeltaX := X;
  DeltaY := Y;
  Screen.Cursor := crSizeAll;
end;

class procedure TDragTool.MouseMove(X, Y: Integer; Shift: TShiftState);
begin
  if BMouseDown then
  begin
    Offset.x += trunc(X - DeltaX);
    Offset.y += trunc(Y - DeltaY);
    DeltaX   := X;
    DeltaY   := Y;
  end;
end;

class procedure TDragTool.MouseUp(X, Y: Integer; Shift: TShiftState);
begin
  BMouseDown    := False;
  Screen.Cursor := crDefault;
end;

class function TDragTool.Image: String;
begin
  Result := 'drag.png';
end;

class function TDragTool.Hint: String;
begin
  Result := 'Инструмент "Рука"';
end;

{ TEllipseTool }

class procedure TEllipseTool.CreateControls(var AOwner: TWinControl);
begin
  inherited CreateControls(AOwner);
end;

class procedure TEllipseTool.MouseDown(X, Y: Integer; Shift: TShiftState);
begin
  Self.CreateFigure;
  inherited MouseDown(X, Y, Shift);
  CurrentFigure^.AddPoint(X, Y);
  CurrentFigure^.AddPoint(X, Y);
end;

class procedure TEllipseTool.MouseMove(X, Y: Integer; Shift: TShiftState);
begin
  if BMouseDown then
    CurrentFigure^.ChangePoint(ScreenToWorld(X, Y));
end;

class procedure TEllipseTool.CreateFigure;
begin
  SetLength(FiguresList, Length(FiguresList) + 1);
  CurrentFigure  := @FiguresList[High(FiguresList)];
  CurrentFigure^ := TEllipse.Create;
  inherited CreateFigure;
end;

class function TEllipseTool.Image: String;
begin
  Result := 'ellipse.png';
end;

class function TEllipseTool.Hint: String;
begin
  Result := 'Эллипс';
end;

{ TRectangleTool }

class procedure TRectangleTool.CreateControls(var AOwner: TWinControl);
begin
  inherited CreateControls(AOwner);
end;

class procedure TRectangleTool.MouseDown(X, Y: Integer; Shift: TShiftState);
var
  i: Integer;
begin
  Self.CreateFigure;
  inherited MouseDown(X, Y, Shift);
  CurrentFigure^.AddPoint(X, Y);
  CurrentFigure^.AddPoint(X, Y);
end;

class procedure TRectangleTool.MouseMove(X, Y: Integer; Shift: TShiftState);
begin
  if BMouseDown then
    CurrentFigure^.ChangePoint(ScreenToWorld(X, Y));
end;

class procedure TRectangleTool.CreateFigure;
begin
  SetLength(FiguresList, Length(FiguresList) + 1);
  CurrentFigure  := @FiguresList[High(FiguresList)];
  CurrentFigure^ := TRectangle.Create;
  inherited CreateFigure;
  TRoundProperty.SetDefaultProperties(FiguresList);
end;

class function TRectangleTool.Image: String;
begin
  Result := 'rectangle.png';
end;

class function TRectangleTool.Hint: String;
begin
  Result := 'Прямоугольник';
end;

{ TPolygonTool }

class procedure TPolygonTool.CreateControls(var AOwner: TWinControl);
begin
  inherited CreateControls(AOwner);
end;

class procedure TPolygonTool.MouseDown(X, Y: Integer; Shift: TShiftState);
begin
  if FigureClosed then
  begin
    Self.CreateFigure;
    inherited MouseDown(X, Y, Shift);
    CurrentFigure^.AddPoint(X, Y);
  end;
  CurrentFigure^.AddPoint(X, Y);
end;

class procedure TPolygonTool.MouseMove(X, Y: Integer; Shift: TShiftState);
begin
  if BMouseDown then
    CurrentFigure^.ChangePoint(ScreenToWorld(X, Y));
end;

class procedure TPolygonTool.CreateFigure;
begin
  SetLength(FiguresList, Length(FiguresList) + 1);
  CurrentFigure  := @FiguresList[High(FiguresList)];
  CurrentFigure^ := TPolygon.Create;
  FigureClosed   := False;
  inherited CreateFigure;
end;

class function TPolygonTool.Image: String;
begin
  Result := 'polygon.png';
end;

class function TPolygonTool.Hint: String;
begin
  Result := 'Многоугольник';
end;

{ TBezierTool }

class procedure TBezierTool.CreateControls(var AOwner: TWinControl);
begin
  inherited CreateControls(AOwner);
end;

class procedure TBezierTool.MouseDown(X, Y: Integer; Shift: TShiftState);
begin
  if FigureClosed then
  begin
    Self.CreateFigure;
    inherited MouseDown(X, Y, Shift);
    CurrentFigure^.AddPoint(X, Y);
  end
  else
  begin
    case index mod 3 of
      0: CurrentFigure^.AddPoint(X, Y, index + 1);
      1: CurrentFigure^.AddPoint(X, Y, index + 1);
      2: CurrentFigure^.AddPoint(X, Y, index + 1);
    end;
    Inc(index);
  end;
end;

class procedure TBezierTool.MouseMove(X, Y: Integer; Shift: TShiftState);
begin
  if BMouseDown then
    case index mod 3 of
      0: CurrentFigure^.ChangePoint(ScreenToWorld(X, Y), index + 1);
      1: CurrentFigure^.ChangePoint(ScreenToWorld(X, Y), index + 1);
      2: CurrentFigure^.ChangePoint(ScreenToWorld(X, Y), index + 1);
    end;
end;

class procedure TBezierTool.CreateFigure;
begin
  SetLength(FiguresList, Length(FiguresList) + 1);
  CurrentFigure := @FiguresList[High(FiguresList)];
  CurrentFigure^ := TBezier.Create;
  FigureClosed := False;
  index := 0;
  inherited CreateFigure;
end;

class function TBezierTool.Image: String;
begin
  Result := 'bezier.png';
end;

class function TBezierTool.Hint: String;
begin
  Result := 'Кривая Безье';
end;

{ TPolylineTool }

class procedure TPolylineTool.CreateControls(var AOwner: TWinControl);
begin
  inherited CreateControls(AOwner);
end;

class procedure TPolylineTool.MouseDown(X, Y: Integer; Shift: TShiftState);
begin
  if FigureClosed then
  begin
    Self.CreateFigure;
    inherited MouseDown(X, Y, Shift);
    CurrentFigure^.AddPoint(X, Y);
  end;
  CurrentFigure^.AddPoint(X, Y);
end;

class procedure TPolylineTool.MouseMove(X, Y: Integer; Shift: TShiftState);
begin
  if BMouseDown then
    CurrentFigure^.ChangePoint(ScreenToWorld(X, Y));
end;

class procedure TPolylineTool.CreateFigure;
begin
  SetLength(FiguresList, Length(FiguresList) + 1);
  CurrentFigure  := @FiguresList[High(FiguresList)];
  CurrentFigure^ := TPolyline.Create;
  FigureClosed   := False;
  inherited CreateFigure;
end;

class function TPolylineTool.Image: String;
begin
  Result := 'polyline.png';
end;

class function TPolylineTool.Hint: String;
begin
  Result := 'Ломаная линия';
end;

{ TLineTool }

class procedure TLineTool.CreateControls(var AOwner: TWinControl);
begin
  inherited CreateControls(AOwner);
end;

class procedure TLineTool.MouseDown(X, Y: Integer; Shift: TShiftState);
begin
  Self.CreateFigure;
  inherited MouseDown(X, Y, Shift);
  CurrentFigure^.AddPoint(X, Y);
  CurrentFigure^.AddPoint(X, Y);
end;

class procedure TLineTool.MouseMove(X, Y: Integer; Shift: TShiftState);
begin
  if BMouseDown then
    CurrentFigure^.ChangePoint(ScreenToWorld(X, Y));
end;

class procedure TLineTool.CreateFigure;
begin
  SetLength(FiguresList, Length(FiguresList) + 1);
  CurrentFigure  := @FiguresList[High(FiguresList)];
  CurrentFigure^ := TLine.Create;
  inherited CreateFigure;
end;

class function TLineTool.Image: String;
begin
  Result := 'line.png';
end;

class function TLineTool.Hint: String;
begin
  Result := 'Линия';
end;

{ TTool }

class procedure TTool.CreateControls(var AOwner: TWinControl);
begin

end;

class procedure TTool.MouseDown(X, Y: Integer; Shift: TShiftState);
var
  i: Integer;
begin
  AnchorsList.Clear;
  DeltaP := ScreenToWorld(X, Y);
  for i := High(FiguresList) downto 0 do
    FiguresList[i].Selected := @FiguresList[i] = CurrentFigure;
end;

class procedure TTool.MouseMove(X, Y: Integer; Shift: TShiftState);
begin
  DeltaP := ScreenToWorld(X, Y);
end;

class procedure TTool.MouseUp(X, Y: Integer; Shift: TShiftState);
begin
  DeltaP := PointF(0, 0);
end;

initialization
  JustTimer := TTimer.Create(nil);
  JustTimer.Enabled := False;

  RegisterTool(TLineTool);
  RegisterTool(TPolylineTool);
  RegisterTool(TBezierTool);
  RegisterTool(TPolygonTool);
  RegisterTool(TRegularPolygonTool);
  RegisterTool(TRectangleTool);
  RegisterTool(TRoundRectTool);
  RegisterTool(TEllipseTool);
  RegisterTool(TSprayTool);
  RegisterTool(TDragTool);
  RegisterTool(TSelectTool);

end.
