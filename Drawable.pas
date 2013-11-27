unit Drawable;

{$mode objfpc}

{$H+}{$M+}

interface

uses
  Classes, SysUtils, Windows, Math, Types, Graphics, Transformations, typinfo,
  BGRABitmap, BGRABitmapTypes;

type

  IDrawable = interface
    procedure Draw(var ACanvas: TBGRABitmap);
  end;

  { TFigure }

  TFigure = class(TInterfacedPersistent, IDrawable)
  private
    FPoints: TPointFList;
  public
    Selected, Hovered: boolean;
    constructor Create; virtual;
    procedure Draw(var ACanvas: TBGRABitmap); virtual; abstract;
    procedure DrawSelection(var ACanvas: TBGRABitmap); virtual; abstract;
    procedure Clear; virtual;
    function Rect: TRectF; virtual;
    function WorldRect: TRectF; virtual;
    procedure AddPoint(X, Y: integer; APos: integer = -1); virtual;
    procedure AddPoint(APoint: TPoint; APos: integer = -1); virtual;
    procedure AddPoint(X, Y: double; APos: integer = -1); virtual;
    procedure AddPoint(APoint: TPointF; APos: integer = -1); virtual;
    procedure RemovePoint(APos: integer = -1); virtual;
    procedure ChangePoint(X, Y: integer; APos: integer = -1); virtual;
    procedure ChangePoint(APoint: TPoint; APos: integer = -1); virtual;
    procedure ChangePoint(X, Y: double; APos: integer = -1); virtual;
    procedure ChangePoint(APoint: TPointF; APos: integer = -1); virtual;
    function IsContains(APoint: TPoint): boolean; virtual; abstract;
    function IsContains(X, Y: integer): boolean; virtual;
    function GetPoint(APos: integer = -1; ScreenCoords: boolean = True): TPointF;
      virtual;
    function PointsCount: integer;
  end;

  PFigure = ^TFigure;
  TFigureList = array of TFigure;
  TFigureClass = class of TFigure;
  TFigureClassList = array of TFigureClass;

  { TPenFigure }

  TPenFigure = class(TFigure)
  private
    FPenColor: TBGRAPixel;
    procedure SetPenColor(AColor: TColor);
    function GetPenColor: TColor;
  public
    PenStyle: TPenStyle;
    PenSize: single;
    PenOpacity: byte;
    constructor Create; override;
    procedure Clear; override;
    procedure Draw(var ACanvas: TBGRABitmap); override;
  published
    property PenColor: TColor read GetPenColor write SetPenColor;
  end;

  { TBrushFigure }

  TBrushFigure = class(TPenFigure)
  private
    FBrushColor: TBGRAPixel;
    procedure SetBrushColor(AColor: TColor);
    function GetBrushColor: TColor;
  public
    BrushStyle: TBrushStyle;
    BrushOpacity: byte;
    constructor Create; override;
    procedure Clear; override;
    procedure Draw(var ACanvas: TBGRABitmap); override;
  published
    property BrushColor: TColor read GetBrushColor write SetBrushColor;
  end;

  { TTextFigure }

  TTextFigure = class(TPenFigure)
  public
    Text: string;
    TextStyle: TTextStyle;
    FontStyle: TFontStyles;
    FontName: string;
    FontHeight: integer;
    procedure Draw(var ACanvas: TBGRABitmap); override;
    procedure DrawSelection(var ACanvas: TBGRABitmap); override;
  end;

  { TLine }

  TLine = class(TPenFigure)
  public
    procedure Draw(var ACanvas: TBGRABitmap); override;
    procedure DrawSelection(var ACanvas: TBGRABitmap); override;
    function IsContains(APoint: TPoint): boolean; override;
  end;

  { TBezier }

  TBezier = class(TPenFigure)
  public
    procedure Draw(var ACanvas: TBGRABitmap); override;
    procedure DrawSelection(var ACanvas: TBGRABitmap); override;
    function IsContains(APoint: TPoint): boolean; override;
  end;

  { TPolyline }

  TPolyline = class(TPenFigure)
  public
    procedure Draw(var ACanvas: TBGRABitmap); override;
    procedure DrawSelection(var ACanvas: TBGRABitmap); override;
    function IsContains(APoint: TPoint): boolean; override;
  end;

  { TPolygon }

  TPolygon = class(TBrushFigure)
  public
    procedure Draw(var ACanvas: TBGRABitmap); override;
    procedure DrawSelection(var ACanvas: TBGRABitmap); override;
    function IsContains(APoint: TPoint): boolean; override;
  end;

  { TRightPolygon }

  TRightPolygon = class(TBrushFigure)
  public
    AngleCount: integer;
    function Rect: TRectF; override;
    procedure Draw(var ACanvas: TBGRABitmap); override;
    procedure DrawSelection(var ACanvas: TBGRABitmap); override;
    function IsContains(APoint: TPoint): boolean; override;
  private
    function CreatePoints: TPointFList;
  end;

  { TEllipse }

  TEllipse = class(TBrushFigure)
    function Rect: TRectF; override;
    function WorldRect: TRectF; override;
    procedure Draw(var ACanvas: TBGRABitmap); override;
    procedure DrawSelection(var ACanvas: TBGRABitmap); override;
    function IsContains(APoint: TPoint): boolean; override;
  end;

  { TRectangle }

  TRectangle = class(TBrushFigure)
    procedure Draw(var ACanvas: TBGRABitmap); override;
    procedure DrawSelection(var ACanvas: TBGRABitmap); override;
    function IsContains(APoint: TPoint): boolean; override;
  end;

  { TRoundRectangle }

  TRoundRectangle = class(TBrushFigure)
  public
    RX, RY: single;
    procedure Draw(var ACanvas: TBGRABitmap); override;
    procedure DrawSelection(var ACanvas: TBGRABitmap); override;
    function IsContains(APoint: TPoint): boolean; override;
  end;

const
  SelectedColor: TBGRAPixel = (blue: 0; green: 100; red: 255; alpha: 255);
  EpsilonPoint: single = 2;

var
  Closed: boolean;
  FiguresList: TFigureList;
  CurrentFigure: PFigure;
  BMouseDown: boolean;

implementation

var
  Texture: TBGRABitmap;

{ TRightPolygon }

function TRightPolygon.Rect: TRectF;
var
  P: TPointFList;
  r: double;
  i: integer;
begin
  r := sqrt((FPoints[1].x - FPoints[0].x) * (FPoints[1].x - FPoints[0].x) +
    (Fpoints[1].y - FPoints[0].y) * (Fpoints[1].y - FPoints[0].y));
  if r = 0 then
    exit;
  P := WorldToScreen(Self.CreatePoints);
  with Result do
  begin
    Top := P[0].y;
    Left := P[0].x;
    Bottom := P[0].y;
    Right := P[0].x;
  end;
  for i := 0 to High(P) do
  begin
    Result.Top := Min(Rect.Top, P[i].y);
    Result.Left := Min(Rect.Left, P[i].x);
    Result.Bottom := Max(Rect.Bottom, P[i].y);
    Result.Right := Max(Rect.Right, P[i].x);
  end;
end;

procedure TRightPolygon.Draw(var ACanvas: TBGRABitmap);
var
  P: TPointFList;
  r: double;
begin
  inherited Draw(ACanvas);
  r := sqrt((FPoints[1].x - FPoints[0].x) * (FPoints[1].x - FPoints[0].x) +
    (Fpoints[1].y - FPoints[0].y) * (Fpoints[1].y - FPoints[0].y));
  if r = 0 then
    exit;
  P := Self.CreatePoints;
  try
    ACanvas.FillPolyAntialias(WorldToScreen(P), Texture);
    ACanvas.DrawPolygonAntialias(WorldToScreen(P), FPenColor, PenSize * Scaling);
  finally
    Texture.Free;
  end;
end;

procedure TRightPolygon.DrawSelection(var ACanvas: TBGRABitmap);
var
  P: TPointFList;
  Rct: TRectF;
  r: double;
begin
  P := WorldToScreen(Self.CreatePoints);
  Rct := Self.Rect;

  ACanvas.DrawPolygonAntialias(P, SelectedColor, 1);
  if Selected then
  begin
    ACanvas.PenStyle := psDash;
    ACanvas.RectangleAntialias(Rct.Left, Rct.Top, Rct.Right, Rct.Bottom,
      SelectedColor, 1);
    ACanvas.PenStyle := psSolid;
    ACanvas.RectangleAntialias(P[0].x - EpsilonPoint, P[0].y - EpsilonPoint,
      P[0].x + EpsilonPoint, P[0].y + EpsilonPoint, SelectedColor, 1, BGRAWhite);
    ACanvas.RectangleAntialias(P[1].x - EpsilonPoint, P[1].y - EpsilonPoint,
      P[1].x + EpsilonPoint, P[1].y + EpsilonPoint, SelectedColor, 1, BGRAWhite);
  end;
end;

function TRightPolygon.IsContains(APoint: TPoint): boolean;
var
  P: TPointList;
  r: double;
  Region: HRGN;
begin
  r := sqrt((FPoints[1].x - FPoints[0].x) * (FPoints[1].x - FPoints[0].x) +
    (Fpoints[1].y - FPoints[0].y) * (Fpoints[1].y - FPoints[0].y));
  if r = 0 then
    exit(True);
  P := round(WorldToScreen(Self.CreatePoints));
  Region := CreatePolygonRgn(P[0], Length(P), WINDING);
  Result := PtInRegion(Region, APoint.x, APoint.y);
  DeleteObject(Region);
end;

function TRightPolygon.CreatePoints: TPointFList;
var
  x, y, r, angle: double;
  i: integer;
begin
  SetLength(Result, AngleCount);
  Result[0] := FPoints[1];
  x := FPoints[1].x - FPoints[0].x;
  y := Fpoints[1].y - FPoints[0].y;
  r := sqrt(x * x + y * y);
  angle := arctan2(y, x);
  for i := 1 to AngleCount - 1 do
  begin
    angle += 2 * Pi / AngleCount;
    Result[i].x := r * cos(angle) + FPoints[0].x;
    Result[i].y := r * sin(angle) + FPoints[0].y;
  end;
end;

{ TTextFigure }

procedure TTextFigure.Draw(var ACanvas: TBGRABitmap);
var
  R: TRect;
begin
  inherited Draw(ACanvas);
  R := Self.Rect;
  ACanvas.FontStyle := FontStyle;
  ACanvas.TextRect(R, R.Left, R.Top, Text, TextStyle, FPenColor);
end;

procedure TTextFigure.DrawSelection(var ACanvas: TBGRABitmap);
begin

end;

{ TPenFigure }

procedure TPenFigure.SetPenColor(AColor: TColor);
begin
  FPenColor := ColorToBGRA(AColor, PenOpacity);
end;

function TPenFigure.GetPenColor: TColor;
begin
  Result := BGRAToColor(FPenColor);
end;

constructor TPenFigure.Create;
begin
  inherited Create;
  PenSize := 1;
  PenOpacity := 255;
  FPenColor := BGRABlack;
  PenStyle := psSolid;
end;

procedure TPenFigure.Clear;
begin
  inherited Clear;
  PenStyle := psSolid;
  FPenColor := BGRABlack;
  PenSize := 1;
end;

procedure TPenFigure.Draw(var ACanvas: TBGRABitmap);
begin
  ACanvas.PenStyle := PenStyle;
end;

{ TRoundRectangle }

procedure TRoundRectangle.Draw(var ACanvas: TBGRABitmap);
var
  P: TPointFList;
begin
  inherited Draw(ACanvas);
  P := WorldToScreen(FPoints);
  try
    ACanvas.FillRoundRectAntialias(P[0].x, P[0].y, P[1].x, P[1].y,
      RX * Scaling, RY * Scaling, Texture);
    ACanvas.RoundRectAntialias(P[0].x, P[0].y, P[1].x, P[1].y,
      RX * Scaling, RY * Scaling, FPenColor, PenSize * Scaling);
  finally
    Texture.Free;
  end;
end;

procedure TRoundRectangle.DrawSelection(var ACanvas: TBGRABitmap);
var
  P: TPointFList;
begin
  P := WorldToScreen(FPoints);
  ACanvas.RoundRectAntialias(P[0].x, P[0].y, P[1].x, P[1].y,
    RX * Scaling, RY * Scaling, SelectedColor, 1);
  if Selected then
  begin
    ACanvas.PenStyle := psDash;
    ACanvas.RectangleAntialias(P[0].x, P[0].y, P[1].x, P[1].y, SelectedColor, 1);
    ACanvas.PenStyle := psSolid;
    ACanvas.RectangleAntialias(P[0].x - EpsilonPoint, P[0].y - EpsilonPoint,
      P[0].x + EpsilonPoint, P[0].y + EpsilonPoint, SelectedColor, 1, BGRAWhite);
    ACanvas.RectangleAntialias(P[1].x - EpsilonPoint, P[1].y - EpsilonPoint,
      P[1].x + EpsilonPoint, P[1].y + EpsilonPoint, SelectedColor, 1, BGRAWhite);
  end;
end;

function TRoundRectangle.IsContains(APoint: TPoint): boolean;
var
  P: TPointList;
  Region: HRGN;
begin
  P := round(WorldToScreen(FPoints));
  Region := CreateRoundRectRgn(P[0].x, P[0].y, P[1].x, P[1].y,
    round(RX * 2 * Scaling), round(2 * RY * Scaling));
  Result := PtInRegion(Region, APoint.x, APoint.y);
  DeleteObject(Region);
end;

{ TBrushFigure }

procedure TBrushFigure.SetBrushColor(AColor: TColor);
begin
  FBrushColor := ColorToBGRA(AColor, BrushOpacity);
end;

function TBrushFigure.GetBrushColor: TColor;
begin
  Result := BGRAToColor(FBrushColor);
end;

constructor TBrushFigure.Create;
begin
  inherited Create;
  BrushOpacity := 255;
  FBrushColor := BGRAWhite;
  BrushStyle := bsSolid;
end;

procedure TBrushFigure.Clear;
begin
  inherited Clear;
  FBrushColor := ColorToBGRA(clWhite);
end;

procedure TBrushFigure.Draw(var ACanvas: TBGRABitmap);
begin
  inherited Draw(ACanvas);
  Texture := ACanvas.CreateBrushTexture(BrushStyle, FPenColor, FBrushColor) as
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
  i: integer;
begin
  P := WorldToScreen(FPoints);
  with Result do
  begin
    Top := P[0].y;
    Left := P[0].x;
    Bottom := P[0].y;
    Right := P[0].x;
  end;
  for i := 0 to High(P) do
  begin
    Result.Top := Min(Rect.Top, P[i].y);
    Result.Left := Min(Rect.Left, P[i].x);
    Result.Bottom := Max(Rect.Bottom, P[i].y);
    Result.Right := Max(Rect.Right, P[i].x);
  end;
end;

function TFigure.WorldRect: TRectF;
var
  i: integer;
begin
  with Result do
  begin
    Top := FPoints[0].y;
    Left := FPoints[0].x;
    Bottom := FPoints[0].y;
    Right := FPoints[0].x;
  end;
  for i := 0 to High(FPoints) do
  begin
    Result.Top := Min(Result.Top, FPoints[i].y);
    Result.Left := Min(Result.Left, FPoints[i].x);
    Result.Bottom := Max(Result.Bottom, FPoints[i].y);
    Result.Right := Max(Result.Right, FPoints[i].x);
  end;
end;

procedure TFigure.AddPoint(X, Y: integer; APos: integer);
var
  i: integer;
begin
  if APos > High(FPoints) then
    SetLength(FPoints, APos + 1)
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
      for i := High(FPoints) downto APos + 1 do
        FPoints[i] := FPoints[i - 1];
    end;
  end;
  FPoints[APos] := ScreenToWorld(X, Y);
end;

procedure TFigure.AddPoint(APoint: TPoint; APos: integer);
begin
  Self.AddPoint(APoint.x, APoint.y, APos);
end;

procedure TFigure.AddPoint(X, Y: double; APos: integer);
var
  Point: TPointF;
begin
  Point.x := X;
  Point.y := Y;
  Self.AddPoint(Point, APos);
end;

procedure TFigure.AddPoint(APoint: TPointF; APos: integer);
var
  i: integer;
begin
  if APos > High(FPoints) then
    SetLength(FPoints, APos + 1)
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
      for i := High(FPoints) downto APos + 1 do
        FPoints[i] := FPoints[i - 1];
    end;
  end;
  FPoints[APos] := APoint;
end;

procedure TFigure.RemovePoint(APos: integer);
var
  i: integer;
begin
  if (APos < 0) or (APos > High(FPoints)) then
    APos := High(FPoints)
  else
  begin
    for i := APos to High(FPoints) - 1 do
      FPoints[i] := FPoints[i + 1];
    SetLength(FPoints, Length(FPoints) - 1);
  end;
end;

procedure TFigure.ChangePoint(X, Y: integer; APos: integer);
begin
  if (APos < 0) or (APos > High(FPoints)) then
    APos := High(FPoints);
  FPoints[APos] := ScreenToWorld(X, Y);
end;

procedure TFigure.ChangePoint(APoint: TPoint; APos: integer);
begin
  Self.ChangePoint(APoint.x, APoint.y, APos);
end;

procedure TFigure.ChangePoint(X, Y: double; APos: integer);
var
  Point: TPointF;
begin
  Point.X := X;
  Point.Y := Y;
  Self.ChangePoint(Point, APos);
end;

procedure TFigure.ChangePoint(APoint: TPointF; APos: integer);
begin
  if (APos < 0) or (APos > High(FPoints)) then
    APos := High(FPoints);
  FPoints[APos] := APoint;
end;

function TFigure.IsContains(X, Y: integer): boolean;
var
  P: TPoint;
begin
  P.x := X;
  P.y := Y;
  Self.IsContains(P);
end;

function TFigure.GetPoint(APos: integer; ScreenCoords: boolean): TPointF;
begin
  if (APos < 0) or (APos > High(FPoints)) then
    APos := High(FPoints);
  if ScreenCoords then
    Result := WorldToScreen(FPoints[APos])
  else
    Result := FPoints[APos];
end;

function TFigure.PointsCount: integer;
begin
  Result := Length(FPoints);
end;

constructor TFigure.Create;
begin
  Selected := False;
end;

{ TPolyline }

procedure TPolyline.Draw(var ACanvas: TBGRABitmap);
begin
  inherited Draw(ACanvas);
  ACanvas.DrawPolyLineAntialias(WorldToScreen(FPoints), FPenColor, PenSize * Scaling);
end;

procedure TPolyline.DrawSelection(var ACanvas: TBGRABitmap);
var
  i: integer;
  P: TPointFList;
begin
  P := WorldToScreen(FPoints);
  ACanvas.DrawPolyLineAntialias(P, SelectedColor, 1);
  if Selected then
    for i := 0 to High(P) do
    begin
      ACanvas.RectangleAntialias(P[i].x - EpsilonPoint, P[i].y - EpsilonPoint,
        P[i].x + EpsilonPoint, P[i].y + EpsilonPoint, SelectedColor, 1, BGRAWhite);
    end;
end;

function TPolyline.IsContains(APoint: TPoint): boolean;
var
  P: TPointList;
  P1: TPointFList;
  Region: HRGN;
  i: integer;
begin
  SetLength(P, Length(FPoints) * 2);
  SetLength(P1, Length(FPoints));
  P1 := WorldToScreen(FPoints);
  for i := 0 to High(FPoints) do
  begin
    P[i].x := round(P1[i].x - 4 * EpsilonPoint * Scaling);
    P[i].y := round(P1[i].y - 4 * EpsilonPoint * Scaling);
    P[High(P) - i].x := round(P1[i].x + 4 * EpsilonPoint * Scaling);
    P[High(P) - i].y := round(P1[i].y + 4 * EpsilonPoint * Scaling);
  end;
  Region := CreatePolygonRgn(P[0], Length(P), WINDING);
  Result := PtInRegion(Region, APoint.x, APoint.y);
  DeleteObject(Region);
end;

{ TLine }

procedure TLine.Draw(var ACanvas: TBGRABitmap);
var
  P: TPointFList;
begin
  inherited Draw(ACanvas);
  P := WorldToScreen(FPoints);
  ACanvas.DrawLineAntialias(P[0].x, P[0].y, P[1].x, P[1].y, FPenColor,
    PenSize * Scaling, True);
end;

procedure TLine.DrawSelection(var ACanvas: TBGRABitmap);
var
  P: TPointFList;
begin
  P := WorldToScreen(FPoints);
  ACanvas.PenStyle := psSolid;
  ACanvas.DrawLineAntialias(P[0].x, P[0].y, P[1].x, P[1].y, SelectedColor, 1);
  if Selected then
  begin
    ACanvas.RectangleAntialias(P[0].x - EpsilonPoint, P[0].y - EpsilonPoint,
      P[0].x + EpsilonPoint, P[0].y + EpsilonPoint, SelectedColor, 1, BGRAWhite);
    ACanvas.RectangleAntialias(P[1].x - EpsilonPoint, P[1].y - EpsilonPoint,
      P[1].x + EpsilonPoint, P[1].y + EpsilonPoint, SelectedColor, 1, BGRAWhite);
  end;
end;

function TLine.IsContains(APoint: TPoint): boolean;
var
  P: TPointList;
  P1: TPointFList;
  Region: HRGN;
begin
  SetLength(P, 4);
  SetLength(P1, 2);
  P1 := WorldToScreen(FPoints);
  P[0].x := round(P1[0].x - EpsilonPoint * 4 * Scaling);
  P[0].y := round(P1[0].y - EpsilonPoint * 4 * Scaling);
  P[1].x := round(P1[0].x + EpsilonPoint * 4 * Scaling);
  P[1].y := round(P1[0].y + EpsilonPoint * 4 * Scaling);
  P[2].x := round(P1[1].x - EpsilonPoint * 4 * Scaling);
  P[2].y := round(P1[1].y - EpsilonPoint * 4 * Scaling);
  P[3].x := round(P1[1].x + EpsilonPoint * 4 * Scaling);
  P[3].y := round(P1[1].y + EpsilonPoint * 4 * Scaling);
  Region := CreatePolygonRgn(P[0], Length(P), WINDING);
  Result := PtInRegion(Region, APoint.x, APoint.y);
  DeleteObject(Region);
end;

{ TBezier }

procedure TBezier.Draw(var ACanvas: TBGRABitmap);
var
  P: TPointFList;
begin
  inherited Draw(ACanvas);
  ACanvas.DrawPolylineAntialias(ACanvas.ComputeOpenedSpline(
    WorldToScreen(FPoints), ssRoundOutside), FPenColor, PenSize * Scaling, False);
end;

procedure TBezier.DrawSelection(var ACanvas: TBGRABitmap);
begin

end;

function TBezier.IsContains(APoint: TPoint): boolean;
begin
  //SetLength(P, Length(FPoints)* 2);
end;

{ TPolygon }

procedure TPolygon.Draw(var ACanvas: TBGRABitmap);
begin
  inherited Draw(ACanvas);
  try
    ACanvas.FillPolyAntialias(WorldToScreen(FPoints), Texture);
    ACanvas.DrawPolygonAntialias(WorldToScreen(FPoints), FPenColor, PenSize * Scaling);
  finally
    Texture.Free;
  end;
end;

procedure TPolygon.DrawSelection(var ACanvas: TBGRABitmap);
var
  R: TRect;
  P: TPointFList;
  i: integer;
begin
  R := Self.Rect;
  P := WorldToScreen(FPoints);
  ACanvas.DrawPolygonAntialias(P, SelectedColor, 1);
  if Selected then
  begin
    ACanvas.PenStyle := psDash;
    ACanvas.RectangleAntialias(R.Left, R.Top, R.Right, R.Bottom, SelectedColor, 1);
    ACanvas.PenStyle := psSolid;
    for i := 0 to High(P) do
    begin
      ACanvas.RectangleAntialias(P[i].x - EpsilonPoint, P[i].y - EpsilonPoint,
        P[i].x + EpsilonPoint, P[i].y + EpsilonPoint, SelectedColor, 1, BGRAWhite);
    end;
  end;
end;

function TPolygon.IsContains(APoint: TPoint): boolean;
var
  P: TPointList;
  Region: HRGN;
begin
  P := round(WorldToScreen(FPoints));
  Region := CreatePolygonRgn(P[0], Length(P), WINDING);
  Result := PtInRegion(Region, APoint.x, APoint.y);
  DeleteObject(Region);
end;

{ TEllipse }

function TEllipse.Rect: TRectF;
var
  P: TPointFList;
begin
  P := WorldToScreen(FPoints);
  with Result do
  begin
    Top := P[0].y - abs(P[0].y - P[1].y);
    Left := P[0].x - abs(P[0].x - P[1].x);
    Bottom := P[1].y;
    Right := P[1].x;
  end;
end;

function TEllipse.WorldRect: TRectF;
begin
  with Result do
  begin
    Top := FPoints[0].y - abs(FPoints[0].y - FPoints[1].y);
    Left := FPoints[0].x - abs(FPoints[0].x - FPoints[1].x);
    Bottom := FPoints[1].y;
    Right := FPoints[1].x;
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
      PenSize * Scaling);
  finally
    Texture.Free;
  end;
end;

procedure TEllipse.DrawSelection(var ACanvas: TBGRABitmap);
var
  P: TPointFList;
begin
  P := WorldToScreen(FPoints);
  ACanvas.EllipseAntialias(P[0].x, P[0].y, P[1].x - P[0].x, P[1].y -
    P[0].y, SelectedColor, 1.0);
  if Selected then
  begin
    ACanvas.PenStyle := psDash;
    ACanvas.RectangleAntialias(P[0].x + Sign(P[0].x - P[1].x) * abs(P[0].x - P[1].x),
      P[0].y + Sign(P[0].y - P[1].y) * abs(P[0].y - P[1].y), P[1].x,
      P[1].y, SelectedColor, 1);
    ACanvas.PenStyle := psSolid;
    ACanvas.RectangleAntialias(P[0].x - EpsilonPoint, P[0].y - EpsilonPoint,
      P[0].x + EpsilonPoint, P[0].y + EpsilonPoint, SelectedColor, 1, BGRAWhite);
    ACanvas.RectangleAntialias(P[1].x - EpsilonPoint, P[1].y - EpsilonPoint,
      P[1].x + EpsilonPoint, P[1].y + EpsilonPoint, SelectedColor, 1, BGRAWhite);
  end;
end;

function TEllipse.IsContains(APoint: TPoint): boolean;
var
  P: TPointList;
  Region: HRGN;
begin
  P := round(WorldToScreen(FPoints));
  Region := CreateEllipticRgn(P[0].x + Sign(P[0].x - P[1].x) * abs(P[0].x - P[1].x),
      P[0].y + Sign(P[0].y - P[1].y) * abs(P[0].y - P[1].y), P[1].x, P[1].y);
  Result := PtInRegion(Region, APoint.x, APoint.y);
  DeleteObject(Region);
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
      PenSize * Scaling);
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
  if Selected then
  begin
    ACanvas.RectangleAntialias(P[0].x - EpsilonPoint, P[0].y - EpsilonPoint,
      P[0].x + EpsilonPoint, P[0].y + EpsilonPoint, SelectedColor, 1, BGRAWhite);
    ACanvas.RectangleAntialias(P[1].x - EpsilonPoint, P[1].y - EpsilonPoint,
      P[1].x + EpsilonPoint, P[1].y + EpsilonPoint, SelectedColor, 1, BGRAWhite);
  end;
end;

function TRectangle.IsContains(APoint: TPoint): boolean;
var
  P: TPointList;
  Region: HRGN;
begin
  P := round(WorldToScreen(FPoints));
  Region := CreateRectRgn(P[0].x, P[0].y, P[1].x, P[1].y);
  Result := PtInRegion(Region, APoint.x, APoint.y);
  DeleteObject(Region);
end;

end.
