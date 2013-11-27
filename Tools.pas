unit Tools;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Drawable, Graphics, Transformations, typinfo, Math,
  BGRABitmap, BGRABitmapTypes, Properties, Controls, StdCtrls, Forms;

type

  { TTool }

  TTool = class
    class procedure CreateControls(var AOwner: TWinControl); virtual;
    class procedure MouseDown(X, Y: integer; Shift: TShiftState); virtual;
    class procedure MouseMove(X, Y: integer; Shift: TShiftState); virtual;
    class procedure MouseUp(X, Y: integer; Shift: TShiftState); virtual;
    class procedure CreateFigure; virtual; abstract;
    class function Image: string; virtual; abstract;
    class function Hint: string; virtual; abstract;
  end;

  TToolClass = class of TTool;
  TToolClassList = array of TToolClass;

  { TFilledTool }

  TFilledTool = class(TTool)
    class procedure CreateControls(var AOwner: TWinControl); override;
  end;

  { TLineTool }

  TLineTool = class(TTool)
  public
    class procedure CreateControls(var AOwner: TWinControl); override;
    class procedure MouseDown(X, Y: integer; Shift: TShiftState); override;
    class procedure MouseMove(X, Y: integer; Shift: TShiftState); override;
    class procedure CreateFigure; override;
    class function Image: string; override;
    class function Hint: string; override;
  end;

  { TPolylineTool }

  TPolylineTool = class(TTool)
  public
    class procedure CreateControls(var AOwner: TWinControl); override;
    class procedure MouseDown(X, Y: integer; Shift: TShiftState); override;
    class procedure MouseMove(X, Y: integer; Shift: TShiftState); override;
    class procedure CreateFigure; override;
    class function Image: string; override;
    class function Hint: string; override;
  end;

  { TBezierTool }

  TBezierTool = class(TTool)
  public
    class procedure CreateControls(var AOwner: TWinControl); override;
    class procedure MouseDown(X, Y: integer; Shift: TShiftState); override;
    class procedure MouseMove(X, Y: integer; Shift: TShiftState); override;
    class procedure CreateFigure; override;
    class function Image: string; override;
    class function Hint: string; override;
  end;

  { TPolygonTool }

  TPolygonTool = class(TFilledTool)
    class procedure CreateControls(var AOwner: TWinControl); override;
    class procedure MouseDown(X, Y: integer; Shift: TShiftState); override;
    class procedure MouseMove(X, Y: integer; Shift: TShiftState); override;
    class procedure CreateFigure; override;
    class function Image: string; override;
    class function Hint: string; override;
  end;

  { TRectTool }

  TRectTool = class(TFilledTool)
    class procedure CreateControls(var AOwner: TWinControl); override;
    class procedure MouseDown(X, Y: integer; Shift: TShiftState); override;
    class procedure MouseMove(X, Y: integer; Shift: TShiftState); override;
    class procedure CreateFigure; override;
    class function Image: string; override;
    class function Hint: string; override;
  end;

  { TRoundRectTool }

  TRoundRectTool = class(TFilledTool)
    class procedure CreateControls(var AOwner: TWinControl); override;
    class procedure MouseDown(X, Y: integer; Shift: TShiftState); override;
    class procedure MouseMove(X, Y: integer; Shift: TShiftState); override;
    class procedure CreateFigure; override;
    class function Image: string; override;
    class function Hint: string; override;
  end;

  { TEllipseTool }

  TEllipseTool = class(TFilledTool)
    class procedure CreateControls(var AOwner: TWinControl); override;
    class procedure MouseDown(X, Y: integer; Shift: TShiftState); override;
    class procedure MouseMove(X, Y: integer; Shift: TShiftState); override;
    class procedure CreateFigure; override;
    class function Image: string; override;
    class function Hint: string; override;
  end;

  { TRightPolyTool }

  TRightPolyTool = class(TTool)
    class procedure CreateControls(var AOwner: TWinControl); override;
    class procedure MouseDown(X, Y: integer; Shift: TShiftState); override;
    class procedure MouseMove(X, Y: integer; Shift: TShiftState); override;
    class procedure CreateFigure; override;
    class function Image: string; override;
    class function Hint: string; override;
  end;

  { TTextTool }

  TTextTool = class(TTool)
    class procedure CreateControls(var AOwner: TWinControl); override;
    class procedure MouseDown(X, Y: integer; Shift: TShiftState); override;
    class procedure MouseMove(X, Y: integer; Shift: TShiftState); override;
    class procedure CreateFigure; override;
    class function Image: string; override;
    class function Hint: string; override;
  end;

  { TDragTool }

  TDragTool = class(TTool)
    class procedure MouseDown(X, Y: integer; Shift: TShiftState); override;
    class procedure MouseMove(X, Y: integer; Shift: TShiftState); override;
    class procedure MouseUp(X, Y: integer; Shift: TShiftState); override;
    class function Image: string; override;
    class function Hint: string; override;
  end;

  { TSelectTool }

  TSelectTool = class(TTool)
    class procedure CreateControls(var AOwner: TWinControl); override;
    class procedure MouseDown(X, Y: integer; Shift: TShiftState); override;
    class procedure MouseMove(X, Y: integer; Shift: TShiftState); override;
    class procedure MouseUp(X, Y: integer; Shift: TShiftState); override;
    class function Image: string; override;
    class function Hint: string; override;
  end;

var
  FigureToolsList, EditorTools: TToolClassList;
  CurrentTool: TToolClass;
  FigureClosed: boolean = True;
  ControlsCreated: boolean;
  DeltaX, DeltaY: single;

procedure RegisterTool(AClass: TToolClass);

implementation

var
  index: integer;
  multiselect: boolean;

procedure RegisterTool(AClass: TToolClass);
begin
  SetLength(FigureToolsList, Length(FigureToolsList) + 1);
  FigureToolsList[High(FigureToolsList)] := AClass;
end;

{ TTextTool }

class procedure TTextTool.CreateControls(var AOwner: TWinControl);
begin
  inherited CreateControls(AOwner);
end;

class procedure TTextTool.MouseDown(X, Y: integer; Shift: TShiftState);
begin
  inherited MouseDown(X, Y, Shift);
end;

class procedure TTextTool.MouseMove(X, Y: integer; Shift: TShiftState);
begin
  inherited MouseMove(X, Y, Shift);
end;

class procedure TTextTool.CreateFigure;
begin

end;

class function TTextTool.Image: string;
begin
  Result := 'text.png';
end;

class function TTextTool.Hint: string;
begin
  Result := 'Текст';
end;

{ TRightPolyTool }

class procedure TRightPolyTool.CreateControls(var AOwner: TWinControl);
begin
  inherited CreateControls(AOwner);
  DrawProperty.CreateAngleSpinBox(AOwner);
end;

class procedure TRightPolyTool.MouseDown(X, Y: integer; Shift: TShiftState);
begin
  Self.CreateFigure;
  inherited MouseDown(X, Y, Shift);
  CurrentFigure^.AddPoint(X, Y);
  CurrentFigure^.AddPoint(X + 1, Y + 1);
end;

class procedure TRightPolyTool.MouseMove(X, Y: integer; Shift: TShiftState);
begin
  if BMouseDown then
    CurrentFigure^.ChangePoint(X, Y);
end;

//class procedure TRightPolyTool.MouseUp(X, Y: integer; Shift: TShiftState);
//begin
//  inherited MouseUp(X, Y, Shift);
//end;

class procedure TRightPolyTool.CreateFigure;
begin
  SetLength(FiguresList, Length(FiguresList) + 1);
  CurrentFigure := @FiguresList[High(FiguresList)];
  CurrentFigure^ := TRightPolygon.Create;
  (CurrentFigure^ as TRightPolygon).AngleCount := 3;
end;

class function TRightPolyTool.Image: string;
begin
  Result := 'rightpoly.png';
end;

class function TRightPolyTool.Hint: string;
begin
  Result := 'Правильный многоугольник';
end;

{ TSelectTool }

class procedure TSelectTool.CreateControls(var AOwner: TWinControl);
begin
end;

class procedure TSelectTool.MouseDown(X, Y: integer; Shift: TShiftState);
begin
  multiselect := False;
end;

class procedure TSelectTool.MouseMove(X, Y: integer; Shift: TShiftState);
var
  i, j: integer;
begin
  if BMouseDown then
    multiselect := True;
  for i := High(FiguresList) downto 0 do
  begin
    if FiguresList[i].IsContains(X, Y) then
    begin
      FiguresList[i].Hovered := True;
      for j := i - 1 downto 0 do
        FiguresList[j].Hovered := False;
      Break;
    end
    else
      FiguresList[i].Hovered := False;
  end;
end;

class procedure TSelectTool.MouseUp(X, Y: integer; Shift: TShiftState);
var
  i, j: integer;
begin
  BMouseDown := False;
  for i := High(FiguresList) downto 0 do
  begin
    if FiguresList[i].IsContains(X, Y) then
    begin
      FiguresList[i].Selected := True;
      CurrentFigure := @FiguresList[i];
      for j := i - 1 downto 0 do
        FiguresList[j].Selected := False;
      Break;
    end
    else
      FiguresList[i].Selected := False;
  end;
end;

class function TSelectTool.Image: string;
begin
  Result := 'select.png';
end;

class function TSelectTool.Hint: string;
begin
  Result := 'Выделение фигуры';
end;

{ TRoundRectTool }

class procedure TRoundRectTool.CreateControls(var AOwner: TWinControl);
begin
  inherited CreateControls(AOwner);
  DrawProperty.CreateRXRY(AOwner);
end;

class procedure TRoundRectTool.MouseDown(X, Y: integer; Shift: TShiftState);
begin
  Self.CreateFigure;
  inherited MouseDown(X, Y, Shift);
  CurrentFigure^.AddPoint(X, Y);
  CurrentFigure^.AddPoint(X, Y);
end;

class procedure TRoundRectTool.MouseMove(X, Y: integer; Shift: TShiftState);
begin
  if BMouseDown then
    CurrentFigure^.ChangePoint(X, Y);
end;

class procedure TRoundRectTool.CreateFigure;
begin
  SetLength(FiguresList, Length(FiguresList) + 1);
  CurrentFigure := @FiguresList[High(FiguresList)];
  CurrentFigure^ := TRoundRectangle.Create;
  (CurrentFigure^ as TRoundRectangle).RX := 5;
  (CurrentFigure^ as TRoundRectangle).RY := 5;
end;

class function TRoundRectTool.Image: string;
begin
  Result := 'rectangle.png';
end;

class function TRoundRectTool.Hint: string;
begin
  Result := 'Прямоугольник со скругленными краями';
end;

{ TFilledTool }

class procedure TFilledTool.CreateControls(var AOwner: TWinControl);
begin
  inherited CreateControls(AOwner);
  DrawProperty.CreateBrushComboBox(AOwner);
end;

{ TDragTool }

class procedure TDragTool.MouseDown(X, Y: integer; Shift: TShiftState);
begin
  DeltaX := X;
  DeltaY := Y;
  Screen.Cursor := crSizeAll;
end;

class procedure TDragTool.MouseMove(X, Y: integer; Shift: TShiftState);
begin
  if BMouseDown then
  begin
    Offset.x += trunc(X - DeltaX);
    Offset.y += trunc(Y - DeltaY);
    DeltaX := X;
    DeltaY := Y;
  end;
end;

class procedure TDragTool.MouseUp(X, Y: integer; Shift: TShiftState);
begin
  BMouseDown := False;
  Screen.Cursor := crDefault;
end;

class function TDragTool.Image: string;
begin
  Result := 'drag.png';
end;

class function TDragTool.Hint: string;
begin
  Result := 'Инструмент "Рука"';
end;

{ TEllipseTool }

class procedure TEllipseTool.CreateControls(var AOwner: TWinControl);
begin
  inherited CreateControls(AOwner);
end;

class procedure TEllipseTool.MouseDown(X, Y: integer; Shift: TShiftState);
begin
  Self.CreateFigure;
  inherited MouseDown(X, Y, Shift);
  CurrentFigure^.AddPoint(X, Y);
  CurrentFigure^.AddPoint(X, Y);
end;

class procedure TEllipseTool.MouseMove(X, Y: integer; Shift: TShiftState);
begin
  if BMouseDown then
    CurrentFigure^.ChangePoint(X, Y);
end;

class procedure TEllipseTool.CreateFigure;
begin
  SetLength(FiguresList, Length(FiguresList) + 1);
  CurrentFigure := @FiguresList[High(FiguresList)];
  CurrentFigure^ := TEllipse.Create;
end;

class function TEllipseTool.Image: string;
begin
  Result := 'ellipse.png';
end;

class function TEllipseTool.Hint: string;
begin
  Result := 'Эллипс';
end;

{ TRectTool }

class procedure TRectTool.CreateControls(var AOwner: TWinControl);
begin
  inherited CreateControls(AOwner);
end;

class procedure TRectTool.MouseDown(X, Y: integer; Shift: TShiftState);
var
  i: integer;
begin
  Self.CreateFigure;
  inherited MouseDown(X, Y, Shift);
  CurrentFigure^.AddPoint(X, Y);
  CurrentFigure^.AddPoint(X, Y);
end;

class procedure TRectTool.MouseMove(X, Y: integer; Shift: TShiftState);
begin
  if BMouseDown then
    CurrentFigure^.ChangePoint(X, Y);
end;

class procedure TRectTool.CreateFigure;
begin
  SetLength(FiguresList, Length(FiguresList) + 1);
  CurrentFigure := @FiguresList[High(FiguresList)];
  CurrentFigure^ := TRectangle.Create;
end;

class function TRectTool.Image: string;
begin
  Result := 'rectangle.png';
end;

class function TRectTool.Hint: string;
begin
  Result := 'Прямоугольник';
end;

{ TPolygonTool }

class procedure TPolygonTool.CreateControls(var AOwner: TWinControl);
begin
  inherited CreateControls(AOwner);
end;

class procedure TPolygonTool.MouseDown(X, Y: integer; Shift: TShiftState);
begin
  if FigureClosed then
  begin
    Self.CreateFigure;
    CurrentFigure^.AddPoint(X, Y);
  end;
  CurrentFigure^.AddPoint(X, Y);
end;

class procedure TPolygonTool.MouseMove(X, Y: integer; Shift: TShiftState);
begin
  if BMouseDown then
    CurrentFigure^.ChangePoint(X, Y);
end;

class procedure TPolygonTool.CreateFigure;
begin
  SetLength(FiguresList, Length(FiguresList) + 1);
  CurrentFigure := @FiguresList[High(FiguresList)];
  CurrentFigure^ := TPolygon.Create;
  FigureClosed := False;
end;

class function TPolygonTool.Image: string;
begin
  Result := 'polygon.png';
end;

class function TPolygonTool.Hint: string;
begin
  Result := 'Многоугольник';
end;

{ TBezierTool }

class procedure TBezierTool.CreateControls(var AOwner: TWinControl);
begin
  inherited CreateControls(AOwner);
end;

class procedure TBezierTool.MouseDown(X, Y: integer; Shift: TShiftState);
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

class procedure TBezierTool.MouseMove(X, Y: integer; Shift: TShiftState);
begin
  if BMouseDown then
    case index mod 3 of
      0: CurrentFigure^.ChangePoint(X, Y, index + 1);
      1: CurrentFigure^.ChangePoint(X, Y, index + 1);
      2: CurrentFigure^.ChangePoint(X, Y, index + 1);
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

class function TBezierTool.Image: string;
begin
  Result := 'bezier.png';
end;

class function TBezierTool.Hint: string;
begin
  Result := 'Кривая Безье';
end;

{ TPolylineTool }

class procedure TPolylineTool.CreateControls(var AOwner: TWinControl);
begin
  inherited CreateControls(AOwner);
end;

class procedure TPolylineTool.MouseDown(X, Y: integer; Shift: TShiftState);
begin
  if FigureClosed then
  begin
    Self.CreateFigure;
    inherited MouseDown(X, Y, Shift);
    CurrentFigure^.AddPoint(X, Y);
  end;
  CurrentFigure^.AddPoint(X, Y);
end;

class procedure TPolylineTool.MouseMove(X, Y: integer; Shift: TShiftState);
begin
  if BMouseDown then
    CurrentFigure^.ChangePoint(X, Y);
end;

class procedure TPolylineTool.CreateFigure;
begin
  SetLength(FiguresList, Length(FiguresList) + 1);
  CurrentFigure := @FiguresList[High(FiguresList)];
  CurrentFigure^ := TPolyline.Create;
  FigureClosed := False;
end;

class function TPolylineTool.Image: string;
begin
  Result := 'polyline.png';
end;

class function TPolylineTool.Hint: string;
begin
  Result := 'Ломаная линия';
end;

{ TLineTool }

class procedure TLineTool.CreateControls(var AOwner: TWinControl);
begin
  inherited CreateControls(AOwner);
end;

class procedure TLineTool.MouseDown(X, Y: integer; Shift: TShiftState);
begin
  Self.CreateFigure;
  inherited MouseDown(X, Y, Shift);
  CurrentFigure^.AddPoint(X, Y);
  CurrentFigure^.AddPoint(X, Y);
end;

class procedure TLineTool.MouseMove(X, Y: integer; Shift: TShiftState);
begin
  if BMouseDown then
    CurrentFigure^.ChangePoint(X, Y);
end;

class procedure TLineTool.CreateFigure;
begin
  SetLength(FiguresList, Length(FiguresList) + 1);
  CurrentFigure := @FiguresList[High(FiguresList)];
  CurrentFigure^ := TLine.Create;
end;

class function TLineTool.Image: string;
begin
  Result := 'line.png';
end;

class function TLineTool.Hint: string;
begin
  Result := 'Линия';
end;

{ TTool }

class procedure TTool.CreateControls(var AOwner: TWinControl);
begin
  ctrllol := 0;
  DrawProperty.CreatePenSpinBox(AOwner);
  DrawProperty.CreatePenComboBox(AOwner);
end;

class procedure TTool.MouseDown(X, Y: integer; Shift: TShiftState);
var
  i: integer;
begin
  for i := High(FiguresList) downto 0 do
    if @FiguresList[i] <> CurrentFigure then
      FiguresList[i].Selected := False
    else
      FiguresList[i].Selected := True;
end;

class procedure TTool.MouseMove(X, Y: integer; Shift: TShiftState);
begin

end;

class procedure TTool.MouseUp(X, Y: integer; Shift: TShiftState);
begin

end;

initialization
  RegisterTool(TLineTool);
  RegisterTool(TPolylineTool);
  RegisterTool(TBezierTool);
  RegisterTool(TPolygonTool);
  RegisterTool(TRightPolyTool);
  RegisterTool(TRectTool);
  RegisterTool(TRoundRectTool);
  RegisterTool(TEllipseTool);
  RegisterTool(TDragTool);
  RegisterTool(TSelectTool);

end.
