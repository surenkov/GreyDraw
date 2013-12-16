unit Tools;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Drawable, Graphics, Transformations, typinfo, Math,
  BGRABitmap, BGRABitmapTypes, Properties, Controls, StdCtrls, Forms, Dialogs,
  LCLIntf, LCLType;

type

  { TTool }

  TTool = class(TPersistent)
    class procedure CreateControls(var AOwner: TWinControl); virtual; abstract;
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
    class procedure CreateControls(var AOwner: TWinControl); override;
  end;

  { TBrushFigureTool }

  TBrushFigureTool = class(TPenFigureTool)
    class procedure CreateControls(var AOwner: TWinControl); override;
  end;

  { TLineTool }

  TLineTool = class(TPenFigureTool)
  public
    class procedure CreateControls(var AOwner: TWinControl); override;
    class procedure MouseDown(X, Y: Integer; Shift: TShiftState); override;
    class procedure MouseMove(X, Y: Integer; Shift: TShiftState); override;
    class procedure CreateFigure; override;
    class function Image: String; override;
    class function Hint: String; override;
  end;

  { TPolylineTool }

  TPolylineTool = class(TPenFigureTool)
  public
    class procedure CreateControls(var AOwner: TWinControl); override;
    class procedure MouseDown(X, Y: Integer; Shift: TShiftState); override;
    class procedure MouseMove(X, Y: Integer; Shift: TShiftState); override;
    class procedure CreateFigure; override;
    class function Image: String; override;
    class function Hint: String; override;
  end;

  { TBezierTool }

  TBezierTool = class(TPenFigureTool)
  public
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

  { TRightPolygonTool }

  TRightPolygonTool = class(TBrushFigureTool)
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

  { TSelectFigureTool }

  TSelectFigureTool = class(TTool)
    class procedure CreateControls(var AOwner: TWinControl); override;
    class procedure MouseDown(X, Y: Integer; Shift: TShiftState); override;
    class procedure MouseMove(X, Y: Integer; Shift: TShiftState); override;
    class procedure MouseUp(X, Y: Integer; Shift: TShiftState); override;
    class function Image: String; override;
    class function Hint: String; override;
  end;

  { TSelectPointTool }

  TSelectPointTool = class(TTool)
    class procedure CreateControls(var AOwner: TWinControl); override;
    class procedure MouseDown(X, Y: Integer; Shift: TShiftState); override;
    class procedure MouseMove(X, Y: Integer; Shift: TShiftState); override;
    class procedure MouseUp(X, Y: Integer; Shift: TShiftState); override;
    class function Image: String; override;
    class function Hint: String; override;
  end;

var
  FigureToolsList, EditorTools: TToolClassList;
  CurrentTool:     TToolClass;
  FigureClosed:    Boolean = True;
  ControlsCreated: Boolean;
  DeltaX, DeltaY:  Single;
  SelectionRect:   TRect;

procedure RegisterTool(AClass: TToolClass);

implementation

uses MainForm;

var
  index:  Integer;
  DeltaP: TPointF;

procedure RegisterTool(AClass: TToolClass);
begin
  SetLength(FigureToolsList, Length(FigureToolsList) + 1);
  FigureToolsList[High(FigureToolsList)] := AClass;
  RegisterClass(AClass);
end;

{ TPenFigureTool }

class procedure TPenFigureTool.CreateControls(var AOwner: TWinControl);
begin
  ctrllol := 0;
  DrawProperty.CreatePenSpinBox(AOwner);
  DrawProperty.CreatePenComboBox(AOwner);
end;

{ TSelectPointTool }

class procedure TSelectPointTool.CreateControls(var AOwner: TWinControl);
begin

end;

class procedure TSelectPointTool.MouseDown(X, Y: Integer; Shift: TShiftState);
var
  i, j: Integer;
begin
  if not (ssCtrl in Shift) then
    for i := AnchorsList.Count - 1 downto 0 do
      with (AnchorsList.Items[i] as TPointAnchor) do
        if IsSelected(X, Y) then
        begin
          Selected := not (Selected and (ssShift in Shift));
          if not (ssShift in Shift) then
            for j := i - 1 downto 0 do
              (AnchorsList.Items[j] as TPointAnchor).Selected := False;
          Break;
        end
        else if not (ssShift in Shift) then
        begin
          Selected := False;
        end;
  with SelectionRect do
  begin
    Left   := X;
    Top    := Y;
    Right  := X;
    Bottom := Y;
  end;
  DeltaP := ScreenToWorld(X, Y);
end;

class procedure TSelectPointTool.MouseMove(X, Y: Integer; Shift: TShiftState);
var
  i, j:  Integer;
  P, SP: TPointF;
  Rgn:   HRGN;
  Rct:   TRect;
begin
  if BMouseDown then
  begin
    SP := ScreenToWorld(X, Y);
    if not (ssShift in Shift) or (ssCtrl in Shift) then
    begin
      for i := AnchorsList.Count - 1 downto 0 do
        with  AnchorsList.Items[i] as TPointAnchor do
          if Selected then
          begin
            P := GetPoint;
            ChangePoint(P.x - DeltaP.x + SP.x, P.y - DeltaP.y + SP.y);
          end;
    end
    else if ssShift in Shift then
    begin
      with SelectionRect do
      begin
        Left   := Min(Left, X);
        Top    := Min(Top, Y);
        Right  := Max(Right, X);
        Bottom := Max(Bottom, Y);
      end;
      for i := AnchorsList.Count - 1 downto 0 do
        with (AnchorsList.Items[i] as TPointAnchor) do
          Selected := PtInRect(SelectionRect, Point(WorldToScreen(GetPoint)));
    end;
    DeltaP := SP;
  end;
end;

class procedure TSelectPointTool.MouseUp(X, Y: Integer; Shift: TShiftState);
var
  i: Integer;
begin
  if not ((ssShift in Shift) or (ssCtrl in Shift)) then
    for i := AnchorsList.Count - 1 downto 0 do
      (AnchorsList.Items[i] as TPointAnchor).Selected := False;
  DeltaP := PointF(0, 0);
  with SelectionRect do
  begin
    Top    := 0;
    Left   := 0;
    Bottom := 0;
    Right  := 0;
  end;
  BMouseDown := False;
end;

class function TSelectPointTool.Image: String;
begin
  Result := 'pointsel.png';
end;

class function TSelectPointTool.Hint: String;
begin
  Result := 'Выделение точек';
end;

{ TTextTool }

class procedure TTextTool.CreateControls(var AOwner: TWinControl);
begin

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

{ TRightPolygonTool }

class procedure TRightPolygonTool.CreateControls(var AOwner: TWinControl);
begin
  inherited CreateControls(AOwner);
  DrawProperty.CreateAngleSpinBox(AOwner);
end;

class procedure TRightPolygonTool.MouseDown(X, Y: Integer; Shift: TShiftState);
begin
  Self.CreateFigure;
  inherited MouseDown(X, Y, Shift);
  CurrentFigure^.AddPoint(X, Y);
  CurrentFigure^.AddPoint(X + 1, Y + 1);
end;

class procedure TRightPolygonTool.MouseMove(X, Y: Integer; Shift: TShiftState);
begin
  if BMouseDown then
    CurrentFigure^.ChangePoint(ScreenToWorld(X, Y));
end;

class procedure TRightPolygonTool.CreateFigure;
begin
  SetLength(FiguresList, Length(FiguresList) + 1);
  CurrentFigure  := @FiguresList[High(FiguresList)];
  CurrentFigure^ := TRightPolygon.Create;
  (CurrentFigure^ as TRightPolygon).AngleCount := 3;
end;

class function TRightPolygonTool.Image: String;
begin
  Result := 'rightpoly.png';
end;

class function TRightPolygonTool.Hint: String;
begin
  Result := 'Правильный многоугольник';
end;

{ TSelectFigureTool }

class procedure TSelectFigureTool.CreateControls(var AOwner: TWinControl);
begin

end;

class procedure TSelectFigureTool.MouseDown(X, Y: Integer; Shift: TShiftState);
var
  i, j: Integer;
  A:    TCollectionItem;
  R:    HWND;
begin
  if not (ssCtrl in Shift) then
  begin
    for i := High(FiguresList) downto 0 do
    begin
      R := FiguresList[i].Region;
      try
        if PtInRegion(R, X, Y) then
        begin
          FiguresList[i].Selected :=
            not (FiguresList[i].Selected and (ssShift in Shift));
          FiguresList[i].Hovered := False;
          CurrentFigure := @FiguresList[i];
          if not (ssShift in Shift) then
            for j := i - 1 downto 0 do
              FiguresList[j].Selected := False;
          Break;
        end
        else if not (ssShift in Shift) then
        begin
          FiguresList[i].Selected := False;
          CurrentFigure := nil;
        end;
      finally
        DeleteObject(R);
      end;
    end;
  end;
  with SelectionRect do
  begin
    Left   := X;
    Top    := Y;
    Right  := X;
    Bottom := Y;
  end;
  AnchorsList.Clear;
  for i := High(FiguresList) downto 0 do
    if FiguresList[i].Selected then
      for j := 0 to FiguresList[i].PointsCount do
      begin
        A := AnchorsList.Add;
        (A as TPointAnchor).SetPoint(FiguresList[i].GetPointAddr(j));
      end;
  DeltaP := ScreenToWorld(X, Y);
end;

class procedure TSelectFigureTool.MouseMove(X, Y: Integer; Shift: TShiftState);
var
  i, j:  Integer;
  P, SP: TPointF;
  R:     HRGN;
  sel:   Boolean = False;
begin
  if BMouseDown then
  begin
    SP := ScreenToWorld(X, Y);
    if not (ssShift in Shift) or (ssCtrl in Shift) then
    begin
      for i := High(FiguresList) downto 0 do
        if FiguresList[i].Selected then
        begin
          for j := FiguresList[i].PointsCount downto 0 do
          begin
            P := FiguresList[i].GetPoint(j);
            FiguresList[i].ChangePoint(P.x - DeltaP.x + SP.x, P.y - DeltaP.y + SP.y, j);
          end;
        end;
    end
    else if ssShift in Shift then
    begin
      with SelectionRect do
      begin
        Right  := X;
        Bottom := Y;
      end;
      for i := High(FiguresList) downto 0 do
      begin
        R := FiguresList[i].Region;
        try
          FiguresList[i].Selected := RectInRegion(R, SelectionRect);
        finally
          DeleteObject(R);
        end;
      end;
    end;
  end
  else
    for i := High(FiguresList) downto 0 do
    begin
      R := FiguresList[i].Region;
      try
        if PtInRegion(R, X, Y) then
        begin
          FiguresList[i].Hovered := True;
          for j := i - 1 downto 0 do
            FiguresList[j].Hovered := False;
          Break;
        end
        else
          FiguresList[i].Hovered := False;
      finally
        DeleteObject(R);
      end;
    end;
  DeltaP := SP;
end;

class procedure TSelectFigureTool.MouseUp(X, Y: Integer; Shift: TShiftState);

  function GetLeastCommonClass(AFClass, ASClass: TClass): TClass;
  begin
    if (AFClass = ASClass) or ASClass.InheritsFrom(AFClass) then
      exit(AFClass)
    else if AFClass.InheritsFrom(ASClass) then
      exit(ASClass)
    else
      exit(GetLeastCommonClass(AFClass.ClassParent, ASClass.ClassParent));
  end;

var
  i:   Integer;
  lcc: TClass = nil;
  classname1: String;
begin
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
  for i := GreyDrawForm.PropPanel.ControlCount - 1 downto 0 do
    GreyDrawForm.PropPanel.Controls[i].Free;
  if lcc <> nil then
  begin
    GreyDrawForm.PropPanel.Caption := lcc.ClassName;
    TTool(GetClass(Concat(lcc.ClassName, 'Tool'))).CreateControls(TWinControl(GreyDrawForm.PropPanel));
  end;
  DeltaP := PointF(0, 0);
  with SelectionRect do
  begin
    Top    := 0;
    Left   := 0;
    Bottom := 0;
    Right  := 0;
  end;
  BMouseDown := False;
end;

class function TSelectFigureTool.Image: String;
begin
  Result := 'figuresel.png';
end;

class function TSelectFigureTool.Hint: String;
begin
  Result := 'Выделение фигуры';
end;

{ TRoundRectTool }

class procedure TRoundRectTool.CreateControls(var AOwner: TWinControl);
begin
  inherited CreateControls(AOwner);
  DrawProperty.CreateRXRY(AOwner);
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
  (CurrentFigure^ as TRoundRect).RX := 5;
  (CurrentFigure^ as TRoundRect).RY := 5;
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

class procedure TBrushFigureTool.CreateControls(var AOwner: TWinControl);
begin
  inherited CreateControls(AOwner);
  DrawProperty.CreateBrushComboBox(AOwner);
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

class procedure TTool.MouseDown(X, Y: Integer; Shift: TShiftState);
var
  i: Integer;
begin
  AnchorsList.Clear;
  for i := High(FiguresList) downto 0 do
    FiguresList[i].Selected := @FiguresList[i] = CurrentFigure;
end;

class procedure TTool.MouseMove(X, Y: Integer; Shift: TShiftState);
begin
end;

class procedure TTool.MouseUp(X, Y: Integer; Shift: TShiftState);
begin
end;

initialization
  RegisterTool(TLineTool);
  RegisterTool(TPolylineTool);
  RegisterTool(TBezierTool);
  RegisterTool(TPolygonTool);
  RegisterTool(TRightPolygonTool);
  RegisterTool(TRectangleTool);
  RegisterTool(TRoundRectTool);
  RegisterTool(TEllipseTool);
  RegisterTool(TDragTool);
  RegisterTool(TSelectFigureTool);
  RegisterTool(TSelectPointTool);

end.
