unit Drawable;

{$mode objfpc}

{$H+}{$M+}

interface

uses
  Classes, SysUtils, Forms, Math, Types, Graphics, Transformations, typinfo,
  BGRABitmap, BGRABitmapTypes, LCLIntf, LCLType;

type

  { TFigure }

  TFigure = class(TPersistent)
  private
    FPoints: TPointFList;
  public
    Selected, Hovered: Boolean;
    constructor Create; virtual; abstract;
    procedure Draw(var ACanvas: TBGRABitmap); virtual; abstract;
    procedure DrawSelection(var ACanvas: TBGRABitmap); virtual; abstract;
    procedure Clear; virtual;
    function Rect: TRectF; virtual;
    function WorldRect: TRectF; virtual;
    procedure AddPoint(X, Y: Integer; APos: Integer = -1); virtual;
    procedure AddPoint(APoint: TPoint; APos: Integer = -1); virtual;
    procedure AddPoint(X, Y: Double; APos: Integer = -1); virtual;
    procedure AddPoint(APoint: TPointF; APos: Integer = -1); virtual;
    procedure RemovePoint(APos: Integer = -1); virtual;
    procedure ChangePoint(X, Y: Double; APos: Integer = -1); virtual;
    procedure ChangePoint(APoint: TPointF; APos: Integer = -1); virtual;
    function Region: HRGN; virtual; abstract;
    function GetPoint(APos: Integer = -1): TPointF;
    function GetPointAddr(APos: Integer = -1): PPointF;
    procedure GetPointAddr(var APoint: PPoint; APos: Integer = -1);
    function PointsCount: Integer;
  end;

  PFigure = ^TFigure;
  TFigureList = array of TFigure;
  TFigureClass = class of TFigure;
  TFigureClassList = array of TFigureClass;

  { TPointAnchor }

  TPointAnchor = class(TCollectionItem)
  private
    FPoint: PPointF;
  public
    Selected: Boolean;
    procedure ChangePoint(X, Y: Integer);
    procedure ChangePoint(APoint: TPoint);
    procedure ChangePoint(X, Y: Double);
    procedure ChangePoint(APoint: TPointF);
    function IsSelected(X, Y: Integer): Boolean;
    function IsSelected(APoint: TPoint): Boolean;
    procedure SetPoint(APoint: PPointF);
    function GetPoint: TPointF;
    procedure Draw(var ACanvas: TBGRABitmap);
  end;

  PPointAnchor = ^TPointAnchor;

  { TPenFigure }

  TPenFigure = class(TFigure)
  private
    FPenColor:   TBGRAPixel;
    FPenStyle:   TPenStyle;
    FPenSize:    Single;
    FPenOpacity: Byte;
    procedure SetPenColor(AColor: TColor);
    function GetPenColor: TColor;
  public
    constructor Create; override;
    procedure Clear; override;
    procedure Draw(var ACanvas: TBGRABitmap); override;
  published
    property PenColor: TColor read GetPenColor write SetPenColor;
    property PenStyle: TPenStyle read FPenStyle write FPenStyle;
    property PenSize: Single read FPenSize write FPenSize;
    property PenOpacity: Byte read FPenOpacity write FPenOpacity;
  end;

  { TBrushFigure }

  TBrushFigure = class(TPenFigure)
  private
    FBrushColor:   TBGRAPixel;
    FBrushStyle:   TBrushStyle;
    FBrushOpacity: Byte;
    procedure SetBrushColor(AColor: TColor);
    function GetBrushColor: TColor;
  public
    constructor Create; override;
    procedure Clear; override;
    procedure Draw(var ACanvas: TBGRABitmap); override;
  published
    property BrushColor: TColor read GetBrushColor write SetBrushColor;
    property BrushStyle: TBrushStyle read FBrushStyle write FBrushStyle;
    property BrushOpacity: Byte read FBrushOpacity write FBrushOpacity;
  end;

  { TTextFigure }

  TTextFigure = class(TPenFigure)
  private
    FText:      String;
    FHAlign:    TAlignment;
    FVAlign:    TTextLayout;
    FFontStyle: TFontStyles;
    FFontName:  String;
    FFontHeight: Integer;
  public
    procedure Draw(var ACanvas: TBGRABitmap); override;
    procedure DrawSelection(var ACanvas: TBGRABitmap); override;
  published
    property Text: String read FText write FText;
    property HAlign: TAlignment read FHAlign write FHAlign;
    property VAlign: TTextLayout read FVAlign write FVAlign;
    property FontStyle: TFontStyles read FFontStyle write FFontStyle;
    property FontName: String read FFontName write FFontName;
    property FontHeight: Integer read FFontHeight write FFontHeight;
  end;

  { TLine }

  TLine = class(TPenFigure)
  public
    procedure Draw(var ACanvas: TBGRABitmap); override;
    procedure DrawSelection(var ACanvas: TBGRABitmap); override;
    function Region: HRGN; override;
  end;

  { TBezier }

  TBezier = class(TPenFigure)
  public
    procedure Draw(var ACanvas: TBGRABitmap); override;
    procedure DrawSelection(var ACanvas: TBGRABitmap); override;
    function Region: HRGN; override;
    function Rect: TRectF; override;
  end;

  { TPolyline }

  TPolyline = class(TPenFigure)
  public
    procedure Draw(var ACanvas: TBGRABitmap); override;
    procedure DrawSelection(var ACanvas: TBGRABitmap); override;
    function Region: HRGN; override;
  end;

  { TPolygon }

  TPolygon = class(TBrushFigure)
  public
    procedure Draw(var ACanvas: TBGRABitmap); override;
    procedure DrawSelection(var ACanvas: TBGRABitmap); override;
    function Region: HRGN; override;
  end;

  { TRightPolygon }

  TRightPolygon = class(TBrushFigure)
  private
    FAngleCount: Integer;
    function CreatePoints: TPointFList;
  public
    function Rect: TRectF; override;
    procedure Draw(var ACanvas: TBGRABitmap); override;
    procedure DrawSelection(var ACanvas: TBGRABitmap); override;
    function Region: HRGN; override;
  published
    property AngleCount: Integer read FAngleCount write FAngleCount;
  end;

  { TEllipse }

  TEllipse = class(TBrushFigure)
    function Rect: TRectF; override;
    function WorldRect: TRectF; override;
    procedure Draw(var ACanvas: TBGRABitmap); override;
    procedure DrawSelection(var ACanvas: TBGRABitmap); override;
    function Region: HRGN; override;
  end;

  { TRectangle }

  TRectangle = class(TBrushFigure)
    procedure Draw(var ACanvas: TBGRABitmap); override;
    procedure DrawSelection(var ACanvas: TBGRABitmap); override;
    function Region: HRGN; override;
  end;

  { TRoundRect }

  TRoundRect = class(TBrushFigure)
  private
    FRX, FRY: Single;
  public
    procedure Draw(var ACanvas: TBGRABitmap); override;
    procedure DrawSelection(var ACanvas: TBGRABitmap); override;
    function Region: HRGN; override;
  published
    property RX: Single read FRX write FRX;
    property RY: Single read FRY write FRY;
  end;

const
  SelectedColor: TBGRAPixel = (blue: 0; green: 100; red: 255; alpha: 255);
  EpsilonPoint: Single = 2;

var
  Closed:      Boolean;
  FiguresList: TFigureList;
  AnchorsList: TCollection;
  CurrentFigure: PFigure;
  CurrentAnchor: PPointAnchor;
  BMouseDown:  Boolean;

  // Property variables
  GPenColor:   TBGRAPixel = (blue: 0; green: 0; red: 0; alpha: 255);
  GBrushColor: TBGRAPixel = (blue: 255; green: 255; red: 255; alpha: 255);
  GBrushStyle: TBrushStyle = bsSolid;
  GPenStyle:   TPenStyle = psSolid;
  GPenSize:    Single = 1;
  GAngleCount: Cardinal = 3;
  GRoundX:     Cardinal = 5;
  GRoundY:     Cardinal = 5;
  GFontName:   String = 'Arial';
  GFontSize:   Integer = 10;
  GFontHorisontalAlignment: TAlignment = taLeftJustify;
  GFontVerticalAlignment: TTextLayout  = tlTop;

implementation

var
  Texture: TBGRABitmap;

{ TPointAnchor }

procedure TPointAnchor.ChangePoint(X, Y: Integer);
var
  P: TPointF;
begin
  P := ScreenToWorld(X, Y);
  FPoint^.x := P.x;
  FPoint^.y := P.y;
end;

procedure TPointAnchor.ChangePoint(APoint: TPoint);
begin
  FPoint^ := ScreenToWorld(APoint);
end;

procedure TPointAnchor.ChangePoint(X, Y: Double);
begin
  FPoint^.x := X;
  FPoint^.y := Y;
end;

procedure TPointAnchor.ChangePoint(APoint: TPointF);
begin
  Self.ChangePoint(APoint.x, APoint.y);
end;

function TPointAnchor.IsSelected(X, Y: Integer): Boolean;
var
  P: TPoint;
begin
  P.x    := X;
  P.y    := Y;
  Result := Self.IsSelected(P);
end;

function TPointAnchor.IsSelected(APoint: TPoint): Boolean;
var
  P: TPointF;
begin
  P      := WorldToScreen(FPoint^);
  Result := (abs(APoint.x - P.x) < EpsilonPoint * 2) and
    (abs(APoint.y - P.y) < EpsilonPoint * 2);
end;

procedure TPointAnchor.SetPoint(APoint: PPointF);
begin
  FPoint := APoint;
end;

function TPointAnchor.GetPoint: TPointF;
begin
  Result := FPoint^;
end;

procedure TPointAnchor.Draw(var ACanvas: TBGRABitmap);
var
  P: TPoint;
begin
  P := round(WorldToScreen(FPoint^));
  ACanvas.PenStyle := psSolid;
  ACanvas.RectangleAntialias(P.x - EpsilonPoint, P.y - EpsilonPoint,
    P.x + EpsilonPoint, P.y + EpsilonPoint, SelectedColor, 1, BGRAWhite);
  if Selected then ACanvas.FillRectAntialias(P.x - EpsilonPoint, P.y - EpsilonPoint,
      P.x + EpsilonPoint, P.y + EpsilonPoint, SelectedColor);
end;

{ TRightPolygon }

function TRightPolygon.Rect: TRectF;
var
  P: TPointFList;
  r: Double;
  i: Integer;
begin
  r := sqrt((FPoints[1].x - FPoints[0].x) * (FPoints[1].x - FPoints[0].x) +
    (Fpoints[1].y - FPoints[0].y) * (Fpoints[1].y - FPoints[0].y));
  if r = 0 then exit;
  P := WorldToScreen(Self.CreatePoints);
  with Result do
  begin
    Top    := P[0].y;
    Left   := P[0].x;
    Bottom := P[0].y;
    Right  := P[0].x;
  end;
  for i := 0 to High(P) do
  begin
    Result.Top    := Min(Rect.Top, P[i].y);
    Result.Left   := Min(Rect.Left, P[i].x);
    Result.Bottom := Max(Rect.Bottom, P[i].y);
    Result.Right  := Max(Rect.Right, P[i].x);
  end;
end;

procedure TRightPolygon.Draw(var ACanvas: TBGRABitmap);
var
  P: TPointFList;
begin
  inherited Draw(ACanvas);
  if sqrt((FPoints[1].x - FPoints[0].x) * (FPoints[1].x - FPoints[0].x) +
    (Fpoints[1].y - FPoints[0].y) * (Fpoints[1].y - FPoints[0].y)) > 0 then try
      P := WorldToScreen(Self.CreatePoints);
      ACanvas.FillPolyAntialias(P, Texture);
      ACanvas.DrawPolygonAntialias(P, FPenColor, FPenSize * Scaling);
    finally
      Texture.Free;
    end;
end;

procedure TRightPolygon.DrawSelection(var ACanvas: TBGRABitmap);
var
  P: TPointFList;
  R: TRect;
begin
  P := WorldToScreen(Self.CreatePoints);
  R := Self.Rect;
  ACanvas.DrawPolygonAntialias(P, SelectedColor, 1);
  if Selected then
  begin
    ACanvas.PenStyle := psDash;
    ACanvas.RectangleAntialias(R.Left, R.Top, R.Right, R.Bottom,
      SelectedColor, 1);
  end;
end;

function TRightPolygon.Region: HRGN;
var
  P: TPointList;
begin
  if sqrt((FPoints[1].x - FPoints[0].x) * (FPoints[1].x - FPoints[0].x) +
    (Fpoints[1].y - FPoints[0].y) * (Fpoints[1].y - FPoints[0].y)) > 0 then
    P := round(WorldToScreen(Self.CreatePoints))
  else
    SetLength(P, Length(FPoints));
  Result := CreatePolygonRgn(@P[0], Length(P), WINDING);
end;

function TRightPolygon.CreatePoints: TPointFList;
var
  x, y, r, angle: Double;
  i: Integer;
begin
  SetLength(Result, FAngleCount);
  Result[0] := FPoints[1];
  x     := FPoints[1].x - FPoints[0].x;
  y     := Fpoints[1].y - FPoints[0].y;
  r     := sqrt(x * x + y * y);
  angle := arctan2(y, x);
  for i := 1 to FAngleCount - 1 do
  begin
    angle += 2 * Pi / FAngleCount;
    Result[i].x := r * cos(angle) + FPoints[0].x;
    Result[i].y := r * sin(angle) + FPoints[0].y;
  end;
end;

{ TTextFigure }

procedure TTextFigure.Draw(var ACanvas: TBGRABitmap);
var
  R:  TRect;
  TS: TTextStyle;
begin
  inherited Draw(ACanvas);
  R := Self.Rect;
  TS.Alignment := FHAlign;
  ACanvas.FontStyle := FFontStyle;
  ACanvas.TextRect(R, R.Left, R.Top, FText, TS, FPenColor);
end;

procedure TTextFigure.DrawSelection(var ACanvas: TBGRABitmap);
begin

end;

{ TPenFigure }

procedure TPenFigure.SetPenColor(AColor: TColor);
begin
  FPenColor := ColorToBGRA(AColor, FPenOpacity);
end;

function TPenFigure.GetPenColor: TColor;
begin
  Result := BGRAToColor(FPenColor);
end;

constructor TPenFigure.Create;
begin
  FPenSize    := 1;
  FPenOpacity := 255;
  FPenColor   := GPenColor;
  FPenStyle   := GPenStyle;
end;

procedure TPenFigure.Clear;
begin
  inherited Clear;
  FPenStyle := psSolid;
  FPenColor := BGRABlack;
  FPenSize  := 1;
end;

procedure TPenFigure.Draw(var ACanvas: TBGRABitmap);
begin
  ACanvas.PenStyle := FPenStyle;
end;

{ TRoundRect }

procedure TRoundRect.Draw(var ACanvas: TBGRABitmap);
var
  P: TPointFList;
begin
  inherited Draw(ACanvas);
  P := WorldToScreen(FPoints);
  try
    ACanvas.FillRoundRectAntialias(P[0].x, P[0].y, P[1].x, P[1].y,
      FRX * Scaling, FRY * Scaling, Texture);
    ACanvas.RoundRectAntialias(P[0].x, P[0].y, P[1].x, P[1].y,
      FRX * Scaling, FRY * Scaling, FPenColor, FPenSize * Scaling);
  finally
    Texture.Free;
  end;
end;

procedure TRoundRect.DrawSelection(var ACanvas: TBGRABitmap);
var
  P: TPointList;
begin
  P := round(WorldToScreen(FPoints));
  ACanvas.RoundRectAntialias(P[0].x, P[0].y, P[1].x, P[1].y,
    FRX * Scaling, FRY * Scaling, SelectedColor, 1);
  if Selected then
  begin
    ACanvas.PenStyle := psDash;
    ACanvas.RectangleAntialias(P[0].x, P[0].y, P[1].x, P[1].y, SelectedColor, 1);
  end;
end;

function TRoundRect.Region: HRGN;
var
  P: TPointList;
begin
  P      := round(WorldToScreen(FPoints));
  Result := CreateRoundRectRgn(P[0].x, P[0].y, P[1].x, P[1].y,
    round(FRX * 2 * Scaling), round(2 * FRY * Scaling));
  ;
end;

{ TBrushFigure }

procedure TBrushFigure.SetBrushColor(AColor: TColor);
begin
  FBrushColor := ColorToBGRA(AColor, FBrushOpacity);
end;

function TBrushFigure.GetBrushColor: TColor;
begin
  Result := BGRAToColor(FBrushColor);
end;

constructor TBrushFigure.Create;
begin
  inherited Create;
  FBrushOpacity := 255;
  FBrushColor   := GBrushColor;
  FBrushStyle   := GBrushStyle;
end;

procedure TBrushFigure.Clear;
begin
  inherited Clear;
  FBrushColor := ColorToBGRA(clWhite);
end;

procedure TBrushFigure.Draw(var ACanvas: TBGRABitmap);
begin
  inherited Draw(ACanvas);
  Texture := ACanvas.CreateBrushTexture(FBrushStyle, FPenColor, FBrushColor) as
    TBGRABitmap;
end;

{ Base figure class implementation }

procedure TFigure.Clear;
begin
  SetLength(FPoints, 0);
end;

function TFigure.Rect: TRectF;
var
  P: TPointFList;
  i: Integer;
begin
  P := WorldToScreen(FPoints);
  with Result do
  begin
    Top    := P[0].y;
    Left   := P[0].x;
    Bottom := P[0].y;
    Right  := P[0].x;
  end;
  for i := 0 to High(P) do
  begin
    Result.Top    := Min(Rect.Top, P[i].y);
    Result.Left   := Min(Rect.Left, P[i].x);
    Result.Bottom := Max(Rect.Bottom, P[i].y);
    Result.Right  := Max(Rect.Right, P[i].x);
  end;
end;

function TFigure.WorldRect: TRectF;
var
  i: Integer;
begin
  with Result do
  begin
    Top    := FPoints[0].y;
    Left   := FPoints[0].x;
    Bottom := FPoints[0].y;
    Right  := FPoints[0].x;
  end;
  for i := 0 to High(FPoints) do
  begin
    Result.Top    := Min(Result.Top, FPoints[i].y);
    Result.Left   := Min(Result.Left, FPoints[i].x);
    Result.Bottom := Max(Result.Bottom, FPoints[i].y);
    Result.Right  := Max(Result.Right, FPoints[i].x);
  end;
end;

procedure TFigure.AddPoint(X, Y: Integer; APos: Integer);
var
  i: Integer;
begin
  if APos > High(FPoints) then SetLength(FPoints, APos + 1)
  else if APos < 0 then
  begin
    SetLength(FPoints, Length(FPoints) + 1);
    APos := High(FPoints);
  end
  else
  begin
    if (FPoints[APos].X <> 0) and (FPoints[APos].Y <> 0) then
    begin
      SetLength(FPoints, Length(FPoints) + 1);
      for i := High(FPoints) downto APos + 1 do FPoints[i] := FPoints[i - 1];
    end;
  end;
  FPoints[APos] := ScreenToWorld(X, Y);
end;

procedure TFigure.AddPoint(APoint: TPoint; APos: Integer);
begin
  Self.AddPoint(APoint.x, APoint.y, APos);
end;

procedure TFigure.AddPoint(X, Y: Double; APos: Integer);
var
  Point: TPointF;
begin
  Point.x := X;
  Point.y := Y;
  Self.AddPoint(Point, APos);
end;

procedure TFigure.AddPoint(APoint: TPointF; APos: Integer);
var
  i: Integer;
begin
  if APos > High(FPoints) then SetLength(FPoints, APos + 1)
  else if APos < 0 then
  begin
    SetLength(FPoints, Length(FPoints) + 1);
    APos := High(FPoints);
  end
  else
  begin
    if (FPoints[APos].X <> 0) and (FPoints[APos].Y <> 0) then
    begin
      SetLength(FPoints, Length(FPoints) + 1);
      for i := High(FPoints) downto APos + 1 do FPoints[i] := FPoints[i - 1];
    end;
  end;
  FPoints[APos] := APoint;
end;

procedure TFigure.RemovePoint(APos: Integer);
var
  i: Integer;
begin
  if (APos < 0) or (APos > High(FPoints)) then APos := High(FPoints)
  else
  begin
    for i := APos to High(FPoints) - 1 do FPoints[i] := FPoints[i + 1];
    SetLength(FPoints, Length(FPoints) - 1);
  end;
end;

procedure TFigure.ChangePoint(X, Y: Double; APos: Integer);
var
  Point: TPointF;
begin
  Point.X := X;
  Point.Y := Y;
  Self.ChangePoint(Point, APos);
end;

procedure TFigure.ChangePoint(APoint: TPointF; APos: Integer);
begin
  if (APos < 0) or (APos > High(FPoints)) then APos := High(FPoints);
  FPoints[APos] := APoint;
end;

function TFigure.GetPoint(APos: Integer): TPointF;
begin
  if (APos < 0) or (APos > High(FPoints)) then APos := High(FPoints);
  Result := FPoints[APos];
end;

procedure TFigure.GetPointAddr(var APoint: PPoint; APos: Integer);
begin
  if (APos < 0) or (APos > High(FPoints)) then APos := High(FPoints);
  APoint := @FPoints[APos];
end;

function TFigure.GetPointAddr(APos: Integer): PPointF;
begin
  if (APos < 0) or (APos > High(FPoints)) then APos := High(FPoints);
  Result := @FPoints[APos];
end;

function TFigure.PointsCount: Integer;
begin
  Result := High(FPoints);
end;

{ TPolyline }

procedure TPolyline.Draw(var ACanvas: TBGRABitmap);
begin
  inherited Draw(ACanvas);
  ACanvas.DrawPolyLineAntialias(WorldToScreen(FPoints), FPenColor, FPenSize * Scaling);
end;

procedure TPolyline.DrawSelection(var ACanvas: TBGRABitmap);
var
  i: Integer;
  P: TPointFList;
  R: TRectF;
begin
  P := WorldToScreen(FPoints);
  ACanvas.DrawPolyLineAntialias(P, SelectedColor, 1);
  if Selected then
  begin
    R := Self.Rect;
    ACanvas.PenStyle := psDash;
    ACanvas.RectangleAntialias(R.Left, R.Top, R.Right, R.Bottom, SelectedColor, 1);
  end;
end;

function TPolyline.Region: HRGN;
var
  P:  TPointList;
  P1: TPointFList;
  i:  Integer;
begin
  SetLength(P, Length(FPoints) * 2);
  SetLength(P1, Length(FPoints));
  P1 := WorldToScreen(FPoints);
  for i := 0 to High(P1) do
  begin
    P[i].x := round(P1[i].x - 4 * EpsilonPoint * Scaling);
    P[i].y := round(P1[i].y - 4 * EpsilonPoint * Scaling);
    P[High(P) - i].x := round(P1[i].x + 4 * EpsilonPoint * Scaling);
    P[High(P) - i].y := round(P1[i].y + 4 * EpsilonPoint * Scaling);
  end;
  Result := CreatePolygonRgn(@P[0], Length(P), WINDING);
end;

{ TLine }

procedure TLine.Draw(var ACanvas: TBGRABitmap);
var
  P: TPointFList;
begin
  inherited Draw(ACanvas);
  P := WorldToScreen(FPoints);
  ACanvas.DrawLineAntialias(P[0].x, P[0].y, P[1].x, P[1].y, FPenColor,
    FPenSize * Scaling, True);
end;

procedure TLine.DrawSelection(var ACanvas: TBGRABitmap);
var
  P: TPointList;
begin
  P := round(WorldToScreen(FPoints));
  ACanvas.PenStyle := psSolid;
  ACanvas.DrawLine(P[0].x, P[0].y, P[1].x, P[1].y, SelectedColor, True);
end;

function TLine.Region: HRGN;
var
  P:  TPointList;
  P1: TPointFList;
begin
  SetLength(P, 4);
  SetLength(P1, 2);
  P1     := WorldToScreen(FPoints);
  P[0].x := round(P1[0].x - 4 * EpsilonPoint * Scaling);
  P[0].y := round(P1[0].y - 4 * EpsilonPoint * Scaling);
  P[1].x := round(P1[0].x + 4 * EpsilonPoint * Scaling);
  P[1].y := round(P1[0].y + 4 * EpsilonPoint * Scaling);
  P[2].x := round(P1[1].x - 4 * EpsilonPoint * Scaling);
  P[2].y := round(P1[1].y - 4 * EpsilonPoint * Scaling);
  P[3].x := round(P1[1].x + 4 * EpsilonPoint * Scaling);
  P[3].y := round(P1[1].y + 4 * EpsilonPoint * Scaling);
  Result := CreatePolygonRgn(@P[0], Length(P), WINDING);
end;

{ TBezier }

procedure TBezier.Draw(var ACanvas: TBGRABitmap);
var
  P: TPointFList;
begin
  inherited Draw(ACanvas);
  ACanvas.DrawPolylineAntialias(ACanvas.ComputeOpenedSpline(
    WorldToScreen(FPoints), ssRoundOutside), FPenColor, FPenSize * Scaling, False);
end;

procedure TBezier.DrawSelection(var ACanvas: TBGRABitmap);
var
  R: TRect;
begin
  ACanvas.DrawPolylineAntialias(ACanvas.ComputeOpenedSpline(
    WorldToScreen(FPoints), ssRoundOutside), SelectedColor, 1, False);
  if Selected then
  begin
    R := Self.Rect;
    ACanvas.PenStyle := psDash;
    ACanvas.RectangleAntialias(R.Left, R.Top, R.Right, R.Bottom, SelectedColor, 1);
  end;
end;

function TBezier.Region: HRGN;
var
  P:    TPointList;
  P1:   TPointFList;
  cnvs: TBGRABitmap;
  i:    Integer;
begin
  cnvs := TBGRABitmap.Create;
  try
    P1 := cnvs.ComputeOpenedSpline(WorldToScreen(FPoints), ssRoundOutside);
  finally
    cnvs.Free;
  end;
  SetLength(P, Length(P1) * 2);
  for i := 0 to High(P1) do
  begin
    P[i].x := round(P1[i].x - 4 * EpsilonPoint * Scaling);
    P[i].y := round(P1[i].y - 4 * EpsilonPoint * Scaling);
    P[High(P) - i].x := round(P1[i].x + 4 * EpsilonPoint * Scaling);
    P[High(P) - i].y := round(P1[i].y + 4 * EpsilonPoint * Scaling);
  end;
  Result := CreatePolygonRgn(@P[0], Length(P), WINDING);
end;

function TBezier.Rect: TRectF;
var
  P:    TPointFList;
  cnvs: TBGRABitmap;
  i:    Integer;
begin
  cnvs := TBGRABitmap.Create;
  try
    P := cnvs.ComputeOpenedSpline(WorldToScreen(FPoints), ssRoundOutside);
  finally
    cnvs.Free;
  end;
  with Result do
  begin
    Top    := P[0].y;
    Left   := P[0].x;
    Bottom := P[0].y;
    Right  := P[0].x;
  end;
  for i := 0 to High(P) do
  begin
    Result.Top    := Min(Rect.Top, P[i].y);
    Result.Left   := Min(Rect.Left, P[i].x);
    Result.Bottom := Max(Rect.Bottom, P[i].y);
    Result.Right  := Max(Rect.Right, P[i].x);
  end;
end;

{ TPolygon }

procedure TPolygon.Draw(var ACanvas: TBGRABitmap);
begin
  inherited Draw(ACanvas);
  try
    ACanvas.FillPolyAntialias(WorldToScreen(FPoints), Texture);
    ACanvas.DrawPolygonAntialias(WorldToScreen(FPoints), FPenColor, FPenSize * Scaling);
  finally
    Texture.Free;
  end;
end;

procedure TPolygon.DrawSelection(var ACanvas: TBGRABitmap);
var
  R: TRect;
  P: TPointFList;
begin
  P := WorldToScreen(FPoints);
  ACanvas.DrawPolygonAntialias(P, SelectedColor, 1);
  if Selected then
  begin
    R := Self.Rect;
    ACanvas.PenStyle := psDash;
    ACanvas.RectangleAntialias(R.Left, R.Top, R.Right, R.Bottom, SelectedColor, 1);
  end;
end;

function TPolygon.Region: HRGN;
var
  P: TPointList;
begin
  P      := round(WorldToScreen(FPoints));
  Result := CreatePolygonRgn(@P[0], Length(P), WINDING);
end;

{ TEllipse }

function TEllipse.Rect: TRectF;
var
  P: TPointFList;
begin
  P := WorldToScreen(FPoints);
  with Result do
  begin
    Top    := P[0].y + Sign(P[0].y - P[1].y) * abs(P[0].y - P[1].y);
    Left   := P[0].x + Sign(P[0].x - P[1].x) * abs(P[0].x - P[1].x);
    Bottom := P[1].y;
    Right  := P[1].x;
  end;
end;

function TEllipse.WorldRect: TRectF;
begin
  with Result do
  begin
    Top    := min(FPoints[0].y - abs(FPoints[0].y - FPoints[1].y), FPoints[0].y);
    Left   := min(FPoints[0].x - abs(FPoints[0].x - FPoints[1].x), FPoints[0].x);
    Bottom := max(FPoints[0].y - abs(FPoints[0].y - FPoints[1].y), FPoints[1].y);
    Right  := max(FPoints[0].x - abs(FPoints[0].x - FPoints[1].x), FPoints[1].x);
  end;
end;

procedure TEllipse.Draw(var ACanvas: TBGRABitmap);
var
  P: TPointFList;
begin
  inherited Draw(ACanvas);
  P := WorldToScreen(FPoints);
  try
    ACanvas.FillEllipseAntialias(P[0].x, P[0].y, P[1].x - P[0].x,
      P[1].y - P[0].y, Texture);
    ACanvas.EllipseAntialias(P[0].x, P[0].y, P[1].x - P[0].x, P[1].y - P[0].y, FPenColor,
      FPenSize * Scaling);
  finally
    Texture.Free;
  end;
end;

procedure TEllipse.DrawSelection(var ACanvas: TBGRABitmap);
var
  P: TPointFList;
begin
  P := WorldToScreen(FPoints);
  ACanvas.EllipseAntialias(round(P[0].x), round(P[0].y), round(P[1].x - P[0].x),
    round(P[1].y - P[0].y), SelectedColor, 1.0);
  if Selected then
  begin
    ACanvas.PenStyle := psDash;
    ACanvas.RectangleAntialias(round(P[0].x + Sign(P[0].x - P[1].x) *
      abs(P[0].x - P[1].x)), round(P[0].y + Sign(P[0].y - P[1].y) *
      abs(P[0].y - P[1].y)), round(P[1].x), round(P[1].y), SelectedColor, 1);
  end;
end;

function TEllipse.Region: HRGN;
var
  P: TPointList;
begin
  P      := round(WorldToScreen(FPoints));
  Result := CreateEllipticRgn(P[0].x + Sign(P[0].x - P[1].x) *
    abs(P[0].x - P[1].x), P[0].y + Sign(P[0].y - P[1].y) * abs(P[0].y - P[1].y),
    P[1].x, P[1].y);
end;

{ TRectangle }

procedure TRectangle.Draw(var ACanvas: TBGRABitmap);
var
  P: TPointFList;
begin
  inherited Draw(ACanvas);
  P := WorldToScreen(FPoints);
  try
    ACanvas.FillRectAntialias(P[0].x, P[0].y, P[1].x, P[1].y, Texture);
    ACanvas.RectangleAntialias(P[0].x, P[0].y, P[1].x, P[1].y, FPenColor,
      FPenSize * Scaling);
  finally
    Texture.Free;
  end;
  P := WorldToScreen(FPoints);
end;

procedure TRectangle.DrawSelection(var ACanvas: TBGRABitmap);
var
  P: TPointFList;
begin
  P := WorldToScreen(FPoints);
  ACanvas.RectangleAntialias(P[0].x, P[0].y, P[1].x, P[1].y, SelectedColor, 1);
end;

function TRectangle.Region: HRGN;
var
  P: TPointList;
begin
  P      := round(WorldToScreen(FPoints));
  Result := CreateRectRgn(P[0].x, P[0].y, P[1].x, P[1].y);
end;

initialization
  AnchorsList := TCollection.Create(TPointAnchor);
end.
