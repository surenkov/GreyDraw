unit Properties;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, Drawable, LCLType, Controls, StdCtrls, ExtCtrls, Spin,
  BGRABitmap, BGRABitmapTypes, Graphics, Forms, typinfo, jsonparser;

type

  { DrawProperty }

  DrawProperty = class
    class procedure CreateLabel(var AOwner: TWinControl; ACaption: String);
    class procedure CreateFontComboBox(var AOwner: TWinControl);
    class procedure CreateBrushComboBox(var AOwner: TWinControl);
    class procedure CreatePenComboBox(var AOwner: TWinControl);
    class procedure CreatePenSpinBox(var AOwner: TWinControl);
    class procedure CreateAngleSpinBox(var AOwner: TWinControl);
    class procedure CreateRXRY(var AOwner: TWinControl);
  public
    class procedure PenSizeChange(Sender: TObject);
    class procedure AngleCountChange(Sender: TObject);
    class procedure SetPenStyle(Sender: TObject);
    class procedure SetPenColor(AColor: TColor);
    class procedure SetFontName(Sender: TObject);
    class procedure SetFontStyle(Sender: TObject);
    class procedure SetBrushStyle(Sender: TObject);
    class procedure SetBrushColor(AColor: TColor);
    class procedure RXChange(Sender: TObject);
    class procedure RYChange(Sender: TObject);
    class procedure DrawComboBoxPenItem(Control: TWinControl;
      index: Integer; Rect: TRect; State: TOwnerDrawState);
    class procedure DrawComboBoxBrushItem(Control: TWinControl;
      index: Integer; Rect: TRect; State: TOwnerDrawState);
  end;

var
  ctrllol: Integer;

implementation

uses MainForm;

var
  lolpadding, i: Integer;
  Ctrl:      TControl;
  PenStyles: array [1..5] of TPenStyle;
  BrushStyles: array [1..8] of TBrushStyle;

{ DrawProperty }

class procedure DrawProperty.CreateLabel(var AOwner: TWinControl; ACaption: String);
begin
  Ctrl := TLabel.Create(AOwner);
  with Ctrl as TLabel do
  begin
    Parent  := AOwner;
    Top     := 10 + ctrllol * 30;
    Left    := 10;
    Caption := ACaption;
    lolpadding := Width;
  end;
end;

class procedure DrawProperty.CreateFontComboBox(var AOwner: TWinControl);
begin
  Self.CreateLabel(AOwner, 'Шрифт: ');
  Ctrl := TComboBox.Create(AOwner);
  with Ctrl as TComboBox do
  begin
    Parent   := AOwner;
    Top      := 10 + ctrllol * 30;
    Left     := 20 + lolpadding;
    Width    := 115;
    Style    := csOwnerDrawFixed;
    OnChange := @SetFontName;
    Items.AddStrings(Screen.Fonts);
    ItemIndex := Items.IndexOf('Arial');
    (CurrentFigure^ as TTextFigure).FontName := 'Arial';
    Name      := 'FontComboBox';
  end;
  Inc(ctrllol);
end;

class procedure DrawProperty.CreateBrushComboBox(var AOwner: TWinControl);
var
  i: Integer;
begin
  Self.CreateLabel(AOwner, 'Тип заливки: ');
  Ctrl := TComboBox.Create(AOwner);
  with Ctrl as TComboBox do
  begin
    Parent   := AOwner;
    Top      := 10 + ctrllol * 30;
    Left     := 20 + lolpadding;
    Width    := 115;
    Style    := csOwnerDrawFixed;
    OnChange := @SetBrushStyle;
    OnDrawItem := @DrawComboBoxBrushItem;
    for i := 0 to High(BrushStyles) - 1 do Items.Add(IntToStr(i));
    ItemIndex := 0;
    Name      := 'BrushComboBox';
  end;
  Inc(ctrllol);
end;

class procedure DrawProperty.CreatePenComboBox(var AOwner: TWinControl);
var
  i: Integer;
begin
  Self.CreateLabel(AOwner, 'Тип кисти: ');
  Ctrl := TComboBox.Create(AOwner);
  with Ctrl as TComboBox do
  begin
    Parent   := AOwner;
    Top      := 10 + ctrllol * 30;
    Left     := 20 + lolpadding;
    Width    := 130;
    OnChange := @SetPenStyle;
    Style    := csOwnerDrawFixed;
    OnDrawItem := @DrawComboBoxPenItem;
    for i := 0 to High(PenStyles) - 1 do Items.Add(IntToStr(i));
    ItemIndex := 0;
    Name      := 'PenComboBox';
  end;
  Inc(ctrllol);
end;

class procedure DrawProperty.CreatePenSpinBox(var AOwner: TWinControl);
begin
  Self.CreateLabel(AOwner, 'Толщина кисти: ');
  Ctrl := TSpinEdit.Create(AOwner);
  with Ctrl as TSpinEdit do
  begin
    MinValue := 1;
    MaxValue := 300;
    Value    := 1;
    Parent   := AOwner;
    Top      := 10 + ctrllol * 30;
    Left     := 20 + lolpadding;
    Width    := 98;
    OnChange := @PenSizeChange;
    Name     := 'PenSpinBox';
  end;
  Inc(ctrllol);
end;

class procedure DrawProperty.CreateAngleSpinBox(var AOwner: TWinControl);
begin
  Self.CreateLabel(AOwner, 'Количество углов: ');
  Ctrl := TSpinEdit.Create(AOwner);
  with Ctrl as TSpinEdit do
  begin
    MinValue := 3;
    MaxValue := 255;
    Value    := 3;
    Parent   := AOwner;
    Top      := 10 + ctrllol * 30;
    Left     := 20 + lolpadding;
    Width    := 85;
    OnChange := @AngleCountChange;
    Name     := 'AngleSpinBox';
  end;
  Inc(ctrllol);
end;

class procedure DrawProperty.CreateRXRY(var AOwner: TWinControl);
begin
  Self.CreateLabel(AOwner, 'Скругление (Х, Y): ');
  Ctrl := TSpinEdit.Create(AOwner);
  with Ctrl as TSpinEdit do
  begin
    MinValue := 1;
    MaxValue := 100;
    Value    := 5;
    Parent   := AOwner;
    Top      := 10 + ctrllol * 30;
    Left     := 20 + lolpadding;
    Width    := 42;
    lolpadding += Width;
    OnChange := @RXChange;
    Name     := 'RXSpinBox';
  end;
  Ctrl := TSpinEdit.Create(AOwner);
  with Ctrl as TSpinEdit do
  begin
    MinValue := 1;
    MaxValue := 100;
    Value    := 5;
    Parent   := AOwner;
    Top      := 10 + ctrllol * 30;
    Left     := 20 + lolpadding;
    Width    := 41;
    OnChange := @RYChange;
    Name     := 'RYSpinBox';
  end;
  Inc(ctrllol);
end;

class procedure DrawProperty.PenSizeChange(Sender: TObject);
begin
  GPenSize := (Sender as TSpinEdit).Value;
  for i := High(FiguresList) downto 0 do
    if FiguresList[i].Selected and IsPublishedProp(FiguresList[i], 'PenSize') then
      SetPropValue(FiguresList[i], 'PenSize', GPenSize);
  GreyDrawForm.ViewPort.Invalidate;
end;

class procedure DrawProperty.AngleCountChange(Sender: TObject);
begin
  GAngleCount := (Sender as TSpinEdit).Value;
  for i := High(FiguresList) downto 0 do
    if FiguresList[i].Selected and IsPublishedProp(FiguresList[i], 'AngleCount') then
      SetPropValue(FiguresList[i], 'AngleCount', GAngleCount);
  GreyDrawForm.ViewPort.Invalidate;
end;

class procedure DrawProperty.SetPenStyle(Sender: TObject);
begin
  GPenStyle := PenStyles[(Sender as TComboBox).ItemIndex + 1];
  for i := High(FiguresList) downto 0 do
    if FiguresList[i].Selected and IsPublishedProp(FiguresList[i], 'PenStyle') then
      SetPropValue(CurrentFigure^, 'PenStyle', GPenStyle);
  GreyDrawForm.ViewPort.Invalidate;
end;

class procedure DrawProperty.SetPenColor(AColor: TColor);
begin
  GPenColor := ColorToBGRA(AColor);
  for i := High(FiguresList) downto 0 do
    if FiguresList[i].Selected and IsPublishedProp(FiguresList[i], 'PenColor') then
      SetPropValue(FiguresList[i], 'PenColor', AColor);
  GreyDrawForm.ViewPort.Invalidate;
end;

class procedure DrawProperty.SetFontName(Sender: TObject);
begin
  GFontName := (Sender as TComboBox).Caption;
  for i := High(FiguresList) downto 0 do
    if FiguresList[i].Selected and IsPublishedProp(CurrentFigure^, 'FontName') then
      SetPropValue(FiguresList[i], 'FontName', GFontName);
  GreyDrawForm.ViewPort.Invalidate;
end;

class procedure DrawProperty.SetFontStyle(Sender: TObject);
begin
  GreyDrawForm.ViewPort.Invalidate;
end;

class procedure DrawProperty.SetBrushStyle(Sender: TObject);
begin
  GBrushStyle := BrushStyles[(Sender as TComboBox).ItemIndex + 1];
  for i := High(FiguresList) downto 0 do
    if FiguresList[i].Selected and IsPublishedProp(FiguresList[i], 'BrushStyle') then
      SetPropValue(FiguresList[i], 'BrushStyle', GBrushStyle);
  GreyDrawForm.ViewPort.Invalidate;
end;

class procedure DrawProperty.SetBrushColor(AColor: TColor);
begin
  GBrushColor := ColorToBGRA(AColor);
  for i := High(FiguresList) downto 0 do
    if FiguresList[i].Selected and IsPublishedProp(FiguresList[i], 'BrushColor') then
      SetPropValue(FiguresList[i], 'BrushColor', AColor);
  GreyDrawForm.ViewPort.Invalidate;
end;

class procedure DrawProperty.RXChange(Sender: TObject);
begin
  GRoundX := (Sender as TSpinEdit).Value;
  for i := High(FiguresList) downto 0 do
    if FiguresList[i].Selected and IsPublishedProp(FiguresList[i], 'RX') then
      SetPropValue(CurrentFigure^, 'RX', GRoundX);
  GreyDrawForm.ViewPort.Invalidate;
end;

class procedure DrawProperty.RYChange(Sender: TObject);
begin
  GRoundY := (Sender as TSpinEdit).Value;
  for i := High(FiguresList) downto 0 do
    if FiguresList[i].Selected and IsPublishedProp(FiguresList[i], 'RY') then
      SetPropValue(FiguresList[i], 'RY', GRoundY);
  GreyDrawForm.ViewPort.Invalidate;
end;

class procedure DrawProperty.DrawComboBoxPenItem(Control: TWinControl;
  index: Integer; Rect: TRect; State: TOwnerDrawState);
begin
  with (Control as TComboBox).Canvas do
  begin
    Pen.Width   := 1;
    Pen.Color   := clBlack;
    Brush.Style := bsSolid;
    Pen.Style   := PenStyles[index + 1];
    FillRect(Rect);
    Line(Rect.Left, (Rect.Top + Rect.Bottom) div 2,
      Rect.Right, (Rect.Top + Rect.Bottom) div 2);
  end;
end;

class procedure DrawProperty.DrawComboBoxBrushItem(Control: TWinControl;
  index: Integer; Rect: TRect; State: TOwnerDrawState);
begin
  with (Control as TComboBox).Canvas do
  begin
    Pen.Style   := psClear;
    Brush.Style := bsSolid;
    FillRect(Rect);
    Brush.Style := BrushStyles[index + 1];
    if index <> 1 then Brush.Color := clBlack;
    FillRect(Rect);
  end;
end;

initialization
  PenStyles[1]   := psSolid;
  PenStyles[2]   := psDash;
  PenStyles[3]   := psDot;
  PenStyles[4]   := psDashDot;
  PenStyles[5]   := psDashDotDot;
  BrushStyles[1] := bsSolid;
  BrushStyles[2] := bsClear;
  BrushStyles[3] := bsHorizontal;
  BrushStyles[4] := bsVertical;
  BrushStyles[5] := bsFDiagonal;
  BrushStyles[6] := bsBDiagonal;
  BrushStyles[7] := bsCross;
  BrushStyles[8] := bsDiagCross;
end.
