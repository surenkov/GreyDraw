unit Properties;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, Drawable, LCLType, Controls, StdCtrls, ExtCtrls, Spin, Loaders,
  BGRABitmap, BGRABitmapTypes, Graphics, Forms, typinfo, jsonparser, History, Math,
  variants;

type

  TInvalidateEvent = procedure of object;
  TChangeEvent = procedure(AChanged: Boolean) of object;

  { TAbstractProperty }

  TAbstractProperty = class
    class procedure SetDefaultProperties(AFigures: array of TFigure); virtual; abstract;
    class procedure CreateLabel(AOwner: TWinControl; ACaption: String);
    class procedure CreateControls(AOwner: TWinControl);
      virtual; abstract;
  end;

  { TPenProperty }

  TPenProperty = class(TAbstractProperty)
  private
    class var
    FPenColor: TBGRAPixel;
    FPenSize:  float;
    FPenStyle: TPenStyle;
    class procedure PenSizeChanged(Sender: TObject);
    class procedure PenStyleChanged(Sender: TObject);
    class procedure PenColorChanged(Sender: TObject);
    class procedure DrawComboBoxPenItem(Control: TWinControl;
      index: Integer; ARect: TRect; State: TOwnerDrawState);
  public
    class procedure SetDefaultProperties(AFigures: array of TFigure); override;
    class procedure SetPenColor(AColor: TColor);
    class procedure SetPenOpacity(AOpacity: Byte);
    class procedure SetPenSize(ASize: float);
    class procedure SetPenStyle(AStyle: TPenStyle);
    class procedure CreateControls(AOwner: TWinControl); override;
  end;

  { TBrushProperty }

  TBrushProperty = class(TAbstractProperty)
  private
    class var
    FBrushColor: TBGRAPixel;
    FBrushStyle: TBrushStyle;
    class procedure BrushStyleChanged(Sender: TObject);
    class procedure BrushColorChanged(Sender: TObject);
    class procedure DrawComboBoxBrushItem(Control: TWinControl;
      index: Integer; ARect: TRect; State: TOwnerDrawState);
  public
    class procedure SetDefaultProperties(AFigures: array of TFigure); override;
    class procedure SetBrushColor(AColor: TColor);
    class procedure SetBrushOpacity(AOpacity: Byte);
    class procedure SetBrushStyle(AStyle: TBrushStyle);
    class procedure CreateControls(AOwner: TWinControl);
      override;
  end;

  { TRoundProperty }

  TRoundProperty = class(TAbstractProperty)
  private
    class var FX, FY: Integer;
    class procedure XChanged(Sender: TObject);
    class procedure YChanged(Sender: TObject);
  public
    class procedure SetX(AX: Integer);
    class procedure SetY(AY: Integer);
    class procedure SetXY(AX, AY: Integer);
    class procedure SetDefaultProperties(AFigures: array of TFigure); override;
    class procedure CreateControls(AOwner: TWinControl);
      override;
  end;

  { TRegularProperty }

  TRegularProperty = class(TAbstractProperty)
  private
    class var FVertexes: Integer;
    class procedure VertexesChanged(Sender: TObject);
  public
    class procedure SetVertexes(AVertexes: Integer);
    class procedure SetDefaultProperties(AFigures: array of TFigure); override;
    class procedure CreateControls(AOwner: TWinControl);
      override;
  end;

  { TTextProperty }

  TTextProperty = class(TAbstractProperty)
    class procedure CreateControls(AOwner: TWinControl);
      override;
  end;

  { TSprayProperty }

  TSprayProperty = class(TAbstractProperty)
  private
    class var FSprayRadius, FSprayIntensity, FSeed: Integer;
    class procedure IntensityChanged(Sender: TObject);
    class procedure RadiusChanged(Sender: TObject);
  public
    class procedure SetDefaultProperties(AFigures: array of TFigure); override;
    class procedure SetSpayIntensity(AIntensity: Integer);
    class procedure SetSparyRadius(ARadius: Integer);
    class procedure SetSpraySeed(ASeed: Integer);
    class procedure CreateControls(AOwner: TWinControl);
      override;
  end;

procedure Init;

var
  ctrllol:     Integer;
  ValidEvent:  TInvalidateEvent;
  ChangeEvent: TChangeEvent;

implementation

var
  lolpadding, i: Integer;
  PenStyles:     array [0..4] of TPenStyle;
  BrushStyles:   array [0..6] of TBrushStyle;

procedure SetProps(AFigure: TFigure; APropName: String; AValue: Variant);
begin
  if IsPublishedProp(AFigure, APropName) then
    SetPropValue(AFigure, APropName, AValue);
end;

{ TAbstractProperty }

class procedure TAbstractProperty.CreateLabel(AOwner: TWinControl; ACaption: String);
begin
  with TLabel.Create(AOwner) do
  begin
    Parent  := AOwner;
    Top     := 10 + ctrllol * 30;
    Left    := 10;
    Caption := ACaption;
    lolpadding := Width;
  end;
end;

{ TPenProperty }

class procedure TPenProperty.CreateControls(AOwner: TWinControl);
var
  i: Integer;
begin
  Self.CreateLabel(AOwner, 'Тип кисти: ');
  with TComboBox.Create(AOwner) do
  begin
    Parent   := AOwner;
    Top      := 10 + ctrllol * 30;
    Left     := 20 + lolpadding;
    Width    := 130;
    OnChange := @PenStyleChanged;
    Style    := csOwnerDrawFixed;
    ReadOnly := True;
    OnDrawItem := @DrawComboBoxPenItem;
    for i := 0 to 4 do
      Items.Add('');
    for i := 0 to 4 do
      if PenStyles[i] = FPenStyle then
        ItemIndex := i
      else
        ItemIndex := 0;
    Name := 'PenComboBox';
  end;
  Inc(ctrllol);
  Self.CreateLabel(AOwner, 'Толщина: ');
  with TSpinEdit.Create(AOwner) do
  begin
    Parent   := AOwner;
    Top      := 10 + ctrllol * 30;
    Left     := 20 + lolpadding;
    Width    := 130;
    Value    := FPenSize;
    OnChange := @PenSizeChanged;
    Name     := 'PenSizeSpin';
  end;
  Inc(ctrllol);
end;


class procedure TPenProperty.SetPenColor(AColor: TColor);
var
  F: TFigure;
begin
  FPenColor := ColorToBGRA(AColor);
  for F in FiguresList do
    if F.Selected then
      SetProps(F, 'PenColor', BGRAToColor(FPenColor));
  ChangeEvent(True);
  ValidEvent;
end;

class procedure TPenProperty.SetPenOpacity(AOpacity: Byte);
var
  F: TFigure;
begin
  FPenColor.alpha := AOpacity;
  for F in FiguresList do
    if F.Selected then
      SetProps(F, 'PenOpacity', FPenColor.alpha);
  ChangeEvent(True);
  ValidEvent;
end;

class procedure TPenProperty.SetPenSize(ASize: float);
var
  F: TFigure;
begin
  FPenSize := ASize;
  for F in FiguresList do
    if F.Selected then
      SetProps(F, 'PenSize', FPenSize);
  ChangeEvent(True);
  ValidEvent;
end;

class procedure TPenProperty.SetPenStyle(AStyle: TPenStyle);
var
  F: TFigure;
begin
  FPenStyle := AStyle;
  for F in FiguresList do
    if F.Selected then
      SetProps(F, 'PenStyle', FPenStyle);
  ChangeEvent(True);
  ValidEvent;
end;

class procedure TPenProperty.PenSizeChanged(Sender: TObject);
begin
  with Sender as TSpinEdit do
    Self.SetPenSize(Value);
end;

class procedure TPenProperty.PenStyleChanged(Sender: TObject);
begin
  with Sender as TComboBox do
    Self.SetPenStyle(PenStyles[ItemIndex]);
end;

class procedure TPenProperty.PenColorChanged(Sender: TObject);
begin

end;

class procedure TPenProperty.DrawComboBoxPenItem(Control: TWinControl;
  index: Integer; ARect: TRect; State: TOwnerDrawState);
begin
  with (Control as TComboBox) do
  begin
    Canvas.FillRect(ARect);
    Canvas.Pen.Style := PenStyles[index];
    Canvas.Pen.Color := clBlack;
    Canvas.Line(
      ARect.Left, Arect.Top + (ARect.Bottom - ARect.Top) div 2,
      ARect.Right, Arect.Top + (ARect.Bottom - ARect.Top) div 2
      );
  end;
end;

class procedure TPenProperty.SetDefaultProperties(AFigures: array of TFigure);
var
  F: TFigure;
begin
  for F in AFigures do
  begin
    if F.Selected then
    begin
      SetProps(F, 'PenColor', BGRAToColor(FPenColor));
      SetProps(F, 'PenOpacity', FPenColor.alpha);
      SetProps(F, 'PenSize', FPenSize);
      SetProps(F, 'PenStyle', FPenStyle);
    end;
  end;
  ValidEvent;
end;

{ TBrushProperty }

class procedure TBrushProperty.CreateControls(AOwner: TWinControl);
var
  i: Integer;
begin
  Self.CreateLabel(AOwner, 'Тип заливки: ');
  with TComboBox.Create(AOwner) do
  begin
    Parent   := AOwner;
    Top      := 10 + ctrllol * 30;
    Left     := 20 + lolpadding;
    Width    := 115;
    Style    := csOwnerDrawFixed;
    ReadOnly := True;
    OnChange := @BrushStyleChanged;
    OnDrawItem := @DrawComboBoxBrushItem;
    for i := 0 to 6 do
      Items.Add('');
    for i := 0 to 6 do
      if BrushStyles[i] = FBrushStyle then
        ItemIndex := i
      else
        ItemIndex := 0;
    Name := 'BrushComboBox';
  end;
  Inc(ctrllol);
end;

class procedure TBrushProperty.SetBrushColor(AColor: TColor);
var
  F: TFigure;
begin
  FBrushColor := ColorToBGRA(AColor);
  for F in FiguresList do
    if F.Selected then
      SetProps(F, 'BrushColor', BGRAToColor(FBrushColor));
  ChangeEvent(True);
  ValidEvent;
end;

class procedure TBrushProperty.SetBrushOpacity(AOpacity: Byte);
var
  F: TFigure;
begin
  FBrushColor.alpha := AOpacity;
  for F in FiguresList do
    if F.Selected then
      SetProps(F, 'BrushOpacity', FBrushColor.alpha);
  ChangeEvent(True);
  ValidEvent;
end;

class procedure TBrushProperty.SetBrushStyle(AStyle: TBrushStyle);
var
  F: TFigure;
begin
  FBrushStyle := AStyle;
  for F in FiguresList do
    if F.Selected then
      SetProps(F, 'BrushStyle', FBrushStyle);
  ChangeEvent(True);
  ValidEvent;
end;

class procedure TBrushProperty.BrushStyleChanged(Sender: TObject);
begin
  with Sender as TComboBox do
    Self.SetBrushStyle(BrushStyles[ItemIndex]);
end;

class procedure TBrushProperty.BrushColorChanged(Sender: TObject);
begin

end;

class procedure TBrushProperty.DrawComboBoxBrushItem(Control: TWinControl;
  index: Integer; ARect: TRect; State: TOwnerDrawState);
begin
  with (Control as TComboBox) do
  begin
    Canvas.FillRect(ARect);
    Canvas.Brush.Style := BrushStyles[index];
    Canvas.Brush.Color := clblack;
    Canvas.FillRect(ARect);
  end;
end;

class procedure TBrushProperty.SetDefaultProperties(AFigures: array of TFigure);
var
  F: TFigure;
begin
  for F in AFigures do
  begin
    if F.Selected then
    begin
      SetProps(F, 'BrushColor', BGRAToColor(FBrushColor));
      SetProps(F, 'BrushOpacity', FBrushColor.alpha);
      SetProps(F, 'BrushStyle', FBrushStyle);
    end;
  end;
end;

{ TRoundProperty }

class procedure TRoundProperty.CreateControls(AOwner: TWinControl);
begin
  Self.CreateLabel(AOwner, 'Скругление (Х, Y): ');
  with TSpinEdit.Create(AOwner) do
  begin
    MinValue := 1;
    MaxValue := 100;
    Value    := FX;
    Parent   := AOwner;
    Top      := 10 + ctrllol * 30;
    Left     := 20 + lolpadding;
    Width    := 42;
    lolpadding += Width;
    OnChange := @XChanged;
    Name     := 'RXSpinBox';
  end;
  with TSpinEdit.Create(AOwner) do
  begin
    MinValue := 1;
    MaxValue := 100;
    Value    := FY;
    Parent   := AOwner;
    Top      := 10 + ctrllol * 30;
    Left     := 20 + lolpadding;
    Width    := 41;
    OnChange := @YChanged;
    Name     := 'RYSpinBox';
  end;
  Inc(ctrllol);
end;

class procedure TRoundProperty.XChanged(Sender: TObject);
begin
  with Sender as TSpinEdit do
    Self.SetX(Value);
end;

class procedure TRoundProperty.YChanged(Sender: TObject);
begin
  with Sender as TSpinEdit do
    Self.SetY(Value);
end;

class procedure TRoundProperty.SetX(AX: Integer);
var
  F: TFigure;
begin
  FX := AX;
  for F in FiguresList do
    if F.Selected then
      SetProps(F, 'RX', FX);
  ChangeEvent(True);
  ValidEvent;
end;

class procedure TRoundProperty.SetY(AY: Integer);
var
  F: TFigure;
begin
  FY := AY;
  for F in FiguresList do
    if F.Selected then
      SetProps(F, 'RY', FY);
  ChangeEvent(True);
  ValidEvent;
end;

class procedure TRoundProperty.SetXY(AX, AY: Integer);
begin
  SetX(AX);
  SetY(AY);
end;

class procedure TRoundProperty.SetDefaultProperties(AFigures: array of TFigure);
var
  F: TFigure;
begin
  for F in AFigures do
  begin
    if F.Selected then
    begin
      SetProps(F, 'RX', FX);
      SetProps(F, 'RY', FY);
    end;
  end;
  ChangeEvent(True);
  ValidEvent;
end;

{ TRegularProperty }

class procedure TRegularProperty.CreateControls(AOwner: TWinControl);
begin
  Self.CreateLabel(AOwner, 'Количество углов: ');
  with TSpinEdit.Create(AOwner) do
  begin
    MinValue := 3;
    MaxValue := 255;
    Value    := FVertexes;
    Parent   := AOwner;
    Top      := 10 + ctrllol * 30;
    Left     := 20 + lolpadding;
    Width    := 85;
    OnChange := @VertexesChanged;
    Name     := 'AngleSpinBox';
  end;
  Inc(ctrllol);
end;

class procedure TRegularProperty.SetVertexes(AVertexes: Integer);
var
  F: TFigure;
begin
  FVertexes := AVertexes;
  for F in FiguresList do
    if F.Selected then
      SetProps(F, 'Vertexes', FVertexes);
  ChangeEvent(True);
  ValidEvent;
end;

class procedure TRegularProperty.SetDefaultProperties(AFigures: array of TFigure);
var
  F: TFigure;
begin
  for F in AFigures do
  begin
    if F.Selected then
    begin
      SetProps(F, 'Vertexes', FVertexes);
    end;
  end;
end;

class procedure TRegularProperty.VertexesChanged(Sender: TObject);
begin
  with Sender as TSpinEdit do
    Self.SetVertexes(Value);
end;

{ TTextProperty }

class procedure TTextProperty.CreateControls(AOwner: TWinControl);
begin
  Self.CreateLabel(AOwner, 'Шрифт: ');
  with TComboBox.Create(AOwner) do
  begin
    Parent := AOwner;
    Top    := 10 + ctrllol * 30;
    Left   := 20 + lolpadding;
    Width  := 115;
    Style  := csOwnerDrawFixed;
    Items.AddStrings(Screen.Fonts);
    ItemIndex := Items.IndexOf('Arial');
    (CurrentFigure^ as TTextFigure).FontName := 'Arial';
    Name      := 'FontComboBox';
  end;
  Inc(ctrllol);
end;

{ TSprayProperty }

class procedure TSprayProperty.CreateControls(AOwner: TWinControl);
begin
  TPenProperty.CreateControls(AOwner);
  Self.CreateLabel(AOwner, 'Радиус: ');
  with TSpinEdit.Create(AOwner) do
  begin
    MinValue := 1;
    MaxValue := 100;
    Value    := FSprayRadius;
    Parent   := AOwner;
    Top      := 10 + ctrllol * 30;
    Left     := 20 + lolpadding;
    Width    := 41;
    OnChange := @RadiusChanged;
    Name     := 'RSprayRadius';
  end;
  Inc(ctrllol);
  Self.CreateLabel(AOwner, 'Интенсивность: ');
  with TSpinEdit.Create(AOwner) do
  begin
    MinValue := 10;
    MaxValue := 1000;
    Value    := FSprayIntensity;
    Parent   := AOwner;
    Top      := 10 + ctrllol * 30;
    Left     := 20 + lolpadding;
    Width    := 41;
    OnChange := @IntensityChanged;
    Name     := 'RSprayIntensity';
  end;
  Inc(ctrllol);
end;

class procedure TSprayProperty.IntensityChanged(Sender: TObject);
begin
  with Sender as TSpinEdit do
    Self.SetSpayIntensity(Value);
end;

class procedure TSprayProperty.RadiusChanged(Sender: TObject);
begin
  with Sender as TSpinEdit do
    Self.SetSparyRadius(Value);
end;

class procedure TSprayProperty.SetDefaultProperties(AFigures: array of TFigure);
var
  F: TFigure;
begin
  for F in AFigures do
  begin
    if F.Selected then
    begin
      SetProps(F, 'Intensity', FSprayIntensity);
      SetProps(F, 'Radius', FSprayRadius);
      SetProps(F, 'Seed', FSeed);
    end;
  end;
end;

class procedure TSprayProperty.SetSpayIntensity(AIntensity: Integer);
var
  F: TFigure;
begin
  FSprayIntensity := AIntensity;
  for F in FiguresList do
    if F.Selected then
      SetProps(F, 'Intensity', FSprayIntensity);
  ChangeEvent(True);
  ValidEvent;
end;

class procedure TSprayProperty.SetSparyRadius(ARadius: Integer);
var
  F: TFigure;
begin
  FSprayRadius := ARadius;
  for F in FiguresList do
    if F.Selected then
      SetProps(F, 'Radius', FSprayRadius);
  ChangeEvent(True);
  ValidEvent;
end;

class procedure TSprayProperty.SetSpraySeed(ASeed: Integer);
begin
  FSeed := Random(ASeed);
  ChangeEvent(True);
  ValidEvent;
end;

procedure Init;
begin
  with TPenProperty do
  begin
    SetPenColor(clBlack);
    SetPenOpacity(255);
    SetPenSize(1);
    SetPenStyle(psSolid);
  end;
  with TBrushProperty do
  begin
    SetBrushColor(clWhite);
    SetBrushOpacity(255);
    SetBrushStyle(bsSolid);
  end;
  TRegularProperty.SetVertexes(3);
  TRoundProperty.SetXY(5, 5);
  with TSprayProperty do
  begin
    SetSpraySeed(591234);
    SetSparyRadius(30);
    SetSpayIntensity(50);
  end;

end;

initialization
  PenStyles[0]   := psSolid;
  PenStyles[1]   := psDash;
  PenStyles[2]   := psDot;
  PenStyles[3]   := psDashDot;
  PenStyles[4]   := psDashDotDot;
  BrushStyles[0] := bsSolid;
  BrushStyles[1] := bsHorizontal;
  BrushStyles[2] := bsVertical;
  BrushStyles[3] := bsFDiagonal;
  BrushStyles[4] := bsBDiagonal;
  BrushStyles[5] := bsCross;
  BrushStyles[6] := bsDiagCross;
end.
