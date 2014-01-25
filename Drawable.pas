unit Drawable;

{$mode objfpc}

{$H+}{$M+}

interface

uses
  Classes, SysUtils, Forms, Math, Types, Graphics, Dialogs, LCLIntf, LCLType,
  typinfo, ExtCtrls, BGRABitmap, BGRABitmapTypes, Transformations, Utils;

type

  { TFigure }

  TFigure = class(TPersistent)
  private
    FPoints: TPointFList;
    FStringPoints: String;
  public
    Selected, Hovered: Boolean;
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
    function PointsCount: Integer; virtual;
    function CreateUUID: String;
  end;

  PFigure = ^TFigure;
  TFigureList = array of TFigure;
  TFigureClass = class of TFigure;
  TFigureClassList = array of TFigureClass;

  { TAbstractAnchor }

  TAbstractAnchor = class(TCollectionItem)
  public
    Selected: Boolean;
    procedure ChangePoint(X, Y: Integer); virtual; abstract;
    procedure ChangePoint(APoint: TPoint); virtual; abstract;
    procedure ChangePoint(X, Y: Double); virtual; abstract;
    procedure ChangePoint(APoint: TPointF); virtual; abstract;
    function IsSelected(X, Y: Integer): Boolean; virtual; abstract;
    function IsSelected(APoint: TPoint): Boolean; virtual; abstract;
    procedure Draw(var ACanvas: TBGRABitmap); virtual; abstract;
  end;

  { TVertexAnchor }

  TVertexAnchor = class(TAbstractAnchor)
  private
    FPoint: PPointF;
  public
    procedure ChangePoint(X, Y: Integer); override;
    procedure ChangePoint(APoint: TPoint); override;
    procedure ChangePoint(X, Y: Double); override;
    procedure ChangePoint(APoint: TPointF); override;
    function IsSelected(X, Y: Integer): Boolean; override;
    function IsSelected(APoint: TPoint): Boolean; override;
    class function IsSelected(APoint: TPointF; X, Y: Integer): Boolean;
    procedure SetPoint(APoint: PPointF);
    function GetPoint: TPointF;
    function GetPointAddr: PPointF;
    procedure Draw(var ACanvas: TBGRABitmap); override;
  end;

  PVertexAnchor = ^TVertexAnchor;

  { TEdgeAnchor }

  TEdgeAnchor = class(TAbstractAnchor)
    procedure ChangePoint(X, Y: Integer); override;
    procedure ChangePoint(APoint: TPoint); override;
    procedure ChangePoint(X, Y: Double); override;
    procedure ChangePoint(APoint: TPointF); override;
    function IsSelected(X, Y: Integer): Boolean; override;
    function IsSelected(APoint: TPoint): Boolean; override;
  end;

  { TPenFigure }

  TPenFigure = class(TFigure)
  private
    FPenColor:   TBGRAPixel;
    FPenOpacity: Byte;
    FPenStyle:   TPenStyle;
    FPenSize:    Single;
    procedure SetPenColor(AColor: TColor);
    function GetPenColor: TColor;
    procedure SetPenOpacity(AByte: Byte);
    function GetPenOpacity: Byte;
  public
    procedure Clear; override;
    procedure Draw(var ACanvas: TBGRABitmap); override;
  published
    property PenColor: TColor read GetPenColor write SetPenColor;
    property PenStyle: TPenStyle read FPenStyle write FPenStyle;
    property PenSize: Single read FPenSize write FPenSize;
    property PenOpacity: Byte read GetPenOpacity write SetPenOpacity;
  end;

  { TBrushFigure }

  TBrushFigure = class(TPenFigure)
  private
    FBrushColor: TBGRAPixel;
    FBrushStyle: TBrushStyle;
    procedure SetBrushColor(AColor: TColor);
    function GetBrushColor: TColor;
    procedure SetBrushOpacity(AByte: Byte);
    function GetBrushOpacity: Byte;
  public
    procedure Clear; override;
    procedure Draw(var ACanvas: TBGRABitmap); override;
  published
    property BrushColor: TColor read GetBrushColor write SetBrushColor;
    property BrushStyle: TBrushStyle read FBrushStyle write FBrushStyle;
    property BrushOpacity: Byte read GetBrushOpacity write SetBrushOpacity;
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
    //property FontStyle: TFontStyle read GetFontStyle write SetFontStyle;
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
    function WorldRect: TRectF; override;
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

  { TRegularPolygon }

  TRegularPolygon = class(TBrushFigure)
  private
    FVertexes: Integer;
  public
    function Rect: TRectF; override;
    procedure Draw(var ACanvas: TBGRABitmap); override;
    procedure DrawSelection(var ACanvas: TBGRABitmap); override;
    function Region: HRGN; override;
    function CreatePoints: TPointFList;
  published
    property Vertexes: Integer read FVertexes write FVertexes;
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

  { TSprayFigure }

  TSprayFigure = class(TPenFigure)
  private
    FSeed, FIntensity, FRadius: Integer;
  public
    procedure Draw(var ACanvas: TBGRABitmap); override;
    procedure DrawSelection(var ACanvas: TBGRABitmap); override;
    function Region: HRGN; override;
    //function PointsCount: Integer; override;
  published
    property Seed: Integer read FSeed write FSeed;
    property Intensity: Integer read FIntensity write FIntensity;
    property Radius: Integer read FRadius write FRadius;
  end;

  { TTimerInit }

  TTimerInit = class
  private
    class procedure OnTimer(Sender: TObject);
  public
    class procedure Init;
  end;

function BoundingRect(AFigures: TFigureList; Screen: Boolean = True): TRectF;
function BoundingRect(A, B: TRectF): TRectF;

const
  SelectedColor: TBGRAPixel = (blue: 0; green: 100; red: 255; alpha: 255);
  DeltaOffset: float = 2;

var
  Closed:      Boolean;
  FiguresList: TFigureList;
  AnchorsList: TCollection;
  CurrentFigure: PFigure;
  CurrentAnchor: PVertexAnchor;
  BMouseDown:  Boolean;

implementation

var
  Texture:   TBGRABitmap;
  JustTimer: TTimer;
  Delta:     Integer = 0;

{ TSprayFigure }

procedure TSprayFigure.Draw(var ACanvas: TBGRABitmap);
var
  i, j, s: Integer;
  PL:   TPointFList;
  P:    TPointF;
  f, r: float;
begin
  s := FSeed;
  inherited Draw(ACanvas);
  PL := WorldToScreen(FPoints);
  for i := 0 to High(PL) do
    for j := 0 to FIntensity do
    begin
      f   := rand(-100, 100, s);
      r   := rand(FRadius, s);
      P.x := PL[i].x + cos(f) * r * Scaling;
      P.y := PL[i].y + sin(f) * r * Scaling;
      ACanvas.DrawLineAntialias(P.x, P.y, P.x, P.y, FPenColor, 2 * FPenSize * Scaling);
    end;
end;

procedure TSprayFigure.DrawSelection(var ACanvas: TBGRABitmap);
var
  R: TRect;
begin
  //R := Self.Rect;
  //ACanvas.RectangleAntialias(R.Left, R.Top, R.Right, R.Bottom, SelectedColor, 1);
end;

function TSprayFigure.Region: HRGN;
var
  R: TRect;
begin
  R      := Self.Rect;
  Result := CreateRectRgn(R.Left, R.Top, R.Right, R.Bottom);
end;

 //function TSprayFigure.PointsCount: Integer;
 //begin
 //  Result := 0;
 //end;

{ TVertexAnchor }

procedure TVertexAnchor.ChangePoint(X, Y: Integer);
var
  P: TPointF;
begin
  P := ScreenToWorld(X, Y);
  FPoint^.x := P.x;
  FPoint^.y := P.y;
end;

procedure TVertexAnchor.ChangePoint(APoint: TPoint);
begin
  FPoint^ := ScreenToWorld(APoint);
end;

procedure TVertexAnchor.ChangePoint(X, Y: Double);
begin
  FPoint^.x := X;
  FPoint^.y := Y;
end;

procedure TVertexAnchor.ChangePoint(APoint: TPointF);
begin
  Self.ChangePoint(APoint.x, APoint.y);
end;

function TVertexAnchor.IsSelected(X, Y: Integer): Boolean;
var
  P: TPoint;
begin
  P.x    := X;
  P.y    := Y;
  Result := Self.IsSelected(P);
end;

function TVertexAnchor.IsSelected(APoint: TPoint): Boolean;
var
  P: TPointF;
begin
  P      := WorldToScreen(FPoint^);
  Result :=
    (abs(APoint.x - P.x) < DeltaOffset * 2) and
    (abs(APoint.y - P.y) < DeltaOffset * 2);
end;

class function TVertexAnchor.IsSelected(APoint: TPointF; X, Y: Integer): Boolean;
begin
  Result :=
    (abs(APoint.x - X) < DeltaOffset * 2) and (abs(APoint.y - Y) < DeltaOffset * 2);
end;

procedure TVertexAnchor.SetPoint(APoint: PPointF);
begin
  FPoint := APoint;
end;

function TVertexAnchor.GetPoint: TPointF;
begin
  Result := FPoint^;
end;

function TVertexAnchor.GetPointAddr: PPointF;
begin
  Result := FPoint;
end;

procedure TVertexAnchor.Draw(var ACanvas: TBGRABitmap);
var
  P: TPoint;
begin
  P := round(WorldToScreen(FPoint^));
  ACanvas.PenStyle := psSolid;
  ACanvas.RectangleAntialias(
    P.x - DeltaOffset, P.y - DeltaOffset,
    P.x + DeltaOffset, P.y + DeltaOffset, SelectedColor, 1, BGRAWhite);
  if Selected then
    ACanvas.FillRectAntialias(P.x - DeltaOffset, P.y - DeltaOffset,
      P.x + DeltaOffset, P.y + DeltaOffset, SelectedColor);
end;

{ TRegularPolygon }

function TRegularPolygon.Rect: TRectF;
var
  P: TPointFList;
  r: Double;
  i: Integer;
begin
  r := sqrt((FPoints[1].x - FPoints[0].x) * (FPoints[1].x - FPoints[0].x) +
    (Fpoints[1].y - FPoints[0].y) * (Fpoints[1].y - FPoints[0].y));
  if r = 0 then
    exit;
  P      := WorldToScreen(Self.CreatePoints);
  Result := RectF(P[0].x, P[0].y, P[0].x, P[0].y);
  for i := 0 to High(P) do
  begin
    Result.Top    := Min(Rect.Top, P[i].y);
    Result.Left   := Min(Rect.Left, P[i].x);
    Result.Bottom := Max(Rect.Bottom, P[i].y);
    Result.Right  := Max(Rect.Right, P[i].x);
  end;
end;

procedure TRegularPolygon.Draw(var ACanvas: TBGRABitmap);
var
  P: TPointFList;
begin
  inherited Draw(ACanvas);
  if sqrt((FPoints[1].x - FPoints[0].x) * (FPoints[1].x - FPoints[0].x) +
    (Fpoints[1].y - FPoints[0].y) * (Fpoints[1].y - FPoints[0].y)) > 0 then
    try
      P := WorldToScreen(Self.CreatePoints);
      ACanvas.FillPolyAntialias(P, Texture);
      ACanvas.DrawPolygonAntialias(P, FPenColor, FPenSize * Scaling);
    finally
      Texture.Free;
    end;
end;

procedure TRegularPolygon.DrawSelection(var ACanvas: TBGRABitmap);
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

function TRegularPolygon.Region: HRGN;
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

function TRegularPolygon.CreatePoints: TPointFList;
var
  x, y, r, angle: Double;
  i: Integer;
begin
  SetLength(Result, FVertexes);
  Result[0] := FPoints[1];
  x     := FPoints[1].x - FPoints[0].x;
  y     := Fpoints[1].y - FPoints[0].y;
  r     := sqrt(x * x + y * y);
  angle := arctan2(y, x);
  for i := 1 to FVertexes - 1 do
  begin
    angle += 2 * Pi / FVertexes;
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
  FPenColor := ColorToBGRA(AColor);
end;

function TPenFigure.GetPenColor: TColor;
begin
  Result := BGRAToColor(FPenColor);
end;

function TPenFigure.GetPenOpacity: Byte;
begin
  Result := FPenColor.alpha;
end;

procedure TPenFigure.SetPenOpacity(AByte: Byte);
begin
  FPenColor.alpha := AByte;
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
  FBrushColor := ColorToBGRA(AColor);
end;

function TBrushFigure.GetBrushColor: TColor;
begin
  Result := BGRAToColor(FBrushColor);
end;

procedure TBrushFigure.SetBrushOpacity(AByte: Byte);
begin
  FBrushColor.alpha := AByte;
end;

function TBrushFigure.GetBrushOpacity: Byte;
begin
  Result := FBrushColor.alpha;
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
  P      := WorldToScreen(FPoints);
  Result := RectF(P[0].x, P[0].y, P[0].x, P[0].y);
  for i := 0 to High(P) do
  begin
    Result := RectF(Min(Result.Left, P[i].x), Min(Result.Top, P[i].y),
      Max(Result.Right, P[i].x), Max(Result.Bottom, P[i].y));
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

procedure TFigure.RemovePoint(APos: Integer);
var
  i: Integer;
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
  if (APos < 0) or (APos > High(FPoints)) then
    APos := High(FPoints);
  FPoints[APos] := APoint;
end;

function TFigure.GetPoint(APos: Integer): TPointF;
begin
  if (APos < 0) or (APos > High(FPoints)) then
    APos := High(FPoints);
  Result := FPoints[APos];
end;

procedure TFigure.GetPointAddr(var APoint: PPoint; APos: Integer);
begin
  if (APos < 0) or (APos > High(FPoints)) then
    APos := High(FPoints);
  APoint := @FPoints[APos];
end;

function TFigure.GetPointAddr(APos: Integer): PPointF;
begin
  if (APos < 0) or (APos > High(FPoints)) then
    APos := High(FPoints);
  Result := @FPoints[APos];
end;

function TFigure.PointsCount: Integer;
begin
  Result := High(FPoints);
end;

function TFigure.CreateUUID: String;
var
  AGUID: TGuid;
begin
  if CreateGUID(AGUID) <> 0 then
    raise Exception.Create('GUID creation failed')
  else
    Result := GUIDToString(AGUID);
  Result   := Copy(Result, 2, Length(Result) - 2);
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
    P[i].x := round(P1[i].x - 4 * DeltaOffset * Scaling);
    P[i].y := round(P1[i].y - 4 * DeltaOffset * Scaling);
    P[High(P) - i].x := round(P1[i].x + 4 * DeltaOffset * Scaling);
    P[High(P) - i].y := round(P1[i].y + 4 * DeltaOffset * Scaling);
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
  i, j: Integer;
  P:    TPointList;
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
  P[0].x := round(P1[0].x - 4 * DeltaOffset * Scaling);
  P[0].y := round(P1[0].y - 4 * DeltaOffset * Scaling);
  P[1].x := round(P1[0].x + 4 * DeltaOffset * Scaling);
  P[1].y := round(P1[0].y + 4 * DeltaOffset * Scaling);
  P[2].x := round(P1[1].x - 4 * DeltaOffset * Scaling);
  P[2].y := round(P1[1].y - 4 * DeltaOffset * Scaling);
  P[3].x := round(P1[1].x + 4 * DeltaOffset * Scaling);
  P[3].y := round(P1[1].y + 4 * DeltaOffset * Scaling);
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
    P[i].x := round(P1[i].x - 4 * DeltaOffset * Scaling);
    P[i].y := round(P1[i].y - 4 * DeltaOffset * Scaling);
    P[High(P) - i].x := round(P1[i].x + 4 * DeltaOffset * Scaling);
    P[High(P) - i].y := round(P1[i].y + 4 * DeltaOffset * Scaling);
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

function TBezier.WorldRect: TRectF;
var
  P: TPointFList;
  i: Integer;
begin
  with TBGRABitmap.Create do
    try
      P := ComputeOpenedSpline(FPoints, ssRoundOutside);
    finally
      Free;
    end;
  Result := RectF(P[0].x, P[0].y, P[0].x, P[0].y);
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
    Top    := min(P[0].y - abs(P[0].y - P[1].y), P[0].y);
    Left   := min(P[0].x - abs(P[0].x - P[1].x), P[0].x);
    Bottom := max(P[0].y - abs(P[0].y - P[1].y), P[0].y);
    Right  := max(P[0].x - abs(P[0].x - P[1].x), P[0].x);
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
    ACanvas.RectangleAntialias(
      round(P[0].x + Sign(P[0].x - P[1].x) * abs(P[0].x - P[1].x)),
      round(P[0].y + Sign(P[0].y - P[1].y) * abs(P[0].y - P[1].y)),
      round(P[1].x), round(P[1].y), SelectedColor, 1);
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

{ TEdgeAnchor }

procedure TEdgeAnchor.ChangePoint(X, Y: Integer);
begin

end;

procedure TEdgeAnchor.ChangePoint(APoint: TPoint);
begin

end;

procedure TEdgeAnchor.ChangePoint(X, Y: Double);
begin

end;

procedure TEdgeAnchor.ChangePoint(APoint: TPointF);
begin

end;

function TEdgeAnchor.IsSelected(X, Y: Integer): Boolean;
begin

end;

function TEdgeAnchor.IsSelected(APoint: TPoint): Boolean;
begin

end;

{ TTimerInit }

const
  Shift = 5;

class procedure TTimerInit.OnTimer(Sender: TObject);
begin
  Delta += Shift;
  ValidEvent;
end;

class procedure TTimerInit.Init;
begin
  JustTimer := TTimer.Create(nil);
  JustTimer.Interval := 50;
  JustTimer.OnTimer := @OnTimer;
  JustTimer.Enabled := True;
end;

function BoundingRect(AFigures: TFigureList; Screen: Boolean): TRectF;
var
  i: Integer;
begin
  try
    Result := AFigures[0].Rect;
    for i := 1 to High(AFigures) do
      Result := BoundingRect(Result, AFigures[i].Rect);
  except
    Result := RectF(0, 0, 0, 0);
  end;
end;

function BoundingRect(A, B: TRectF): TRectF;
begin
  Result := RectF(Min(A.Left, B.Left), Min(A.Top, B.Top), Max(A.Right, B.Right),
    Max(A.Bottom, B.Bottom));
end;

initialization
  TTimerInit.Init;
  AnchorsList := TCollection.Create(TVertexAnchor);

  RegisterClass(TFigure);
  RegisterClass(TPenFigure);
  RegisterClass(TBrushFigure);
  RegisterClass(TTextFigure);
  RegisterClass(TLine);
  RegisterClass(TBezier);
  RegisterClass(TPolyline);
  RegisterClass(TPolygon);
  RegisterClass(TRegularPolygon);
  RegisterClass(TEllipse);
  RegisterClass(TRectangle);
  RegisterClass(TRoundRect);
  RegisterClass(TSprayFigure);

end.
