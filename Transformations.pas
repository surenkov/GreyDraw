unit Transformations;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, Graphics, BGRABitmap, BGRABitmapTypes, typinfo;

type
  TCtrlType = (ctButton, ctPanel);

  { TRectF }

  TRectF = record
    Left, Top, Right, Bottom: Double;
    class operator Implicit(a: TRect): TRectF;
    class operator Implicit(a: TRectF): TRect;
  end;

  TOffset = TPoint;
  TPointList = array of TPoint;
  TPointFList = array of TPointF;

var
  Offset, CursorPos: TOffset;
  ViewPortCenter: TPointF;
  Scaling: Single = 1;

function WorldToScreen(X, Y: Single): TPointF; overload;
function WorldToScreen(APoint: TPointF): TPointF; overload;
function WorldToScreen(APoints: TPointFList): TPointFList; overload;

function ScreenToWorld(X, Y: Integer): TPointF; overload;
function ScreenToWorld(APoint: TPoint): TPointF; overload;
function ScreenToWorld(APoints: TPointList): TPointFList; overload;

function Point(X, Y: Integer): TPoint; overload;
function Point(X, Y: Single): TPoint; overload;
function Point(APoint: TPointF): TPoint; overload;
function PointF(X, Y: Integer): TPointF; overload;
function PointF(X, Y: Single): TPointF; overload;
function PointF(APoint: TPoint): TPointF; overload;

function round(APoints: TPointFList): TPointList; overload;
function round(APoint: TPointF): TPoint; overload;

function LoadImage(AName: String; AType: TCtrlType = ctButton): TBitmap;

implementation

uses Drawable;

function WorldToScreen(X, Y: Single): TPointF;
begin
  Result.x := X * Scaling + Offset.X;
  Result.y := Y * Scaling + Offset.Y;
end;

function WorldToScreen(APoint: TPointF): TPointF;
begin
  Result := WorldToScreen(APoint.x, APoint.y);
end;

function WorldToScreen(APoints: TPointFList): TPointFList;
var
  i: Integer;
begin
  SetLength(Result, Length(APoints));
  for i := 0 to High(APoints) do
  begin
    Result[i] := WorldToScreen(APoints[i]);
  end;
end;

function ScreenToWorld(X, Y: Integer): TPointF;
begin
  Result.X := (X - Offset.X) / Scaling;
  Result.Y := (Y - Offset.Y) / Scaling;
end;

function ScreenToWorld(APoint: TPoint): TPointF;
begin
  Result := ScreenToWorld(APoint.x, APoint.y);
end;

function ScreenToWorld(APoints: TPointList): TPointFList;
var
  i: Integer;
begin
  SetLength(Result, Length(APoints));
  for i := 0 to High(APoints) do
  begin
    Result[i] := ScreenToWorld(APoints[i]);
  end;
end;

function Point(X, Y: Integer): TPoint;
begin
  Point.x := X;
  Point.y := Y;
end;

function Point(X, Y: Single): TPoint;
begin
  Point.x := round(X);
  Point.y := round(Y);
end;

function PointF(X, Y: Integer): TPointF;
begin
  PointF.x := X;
  PointF.y := Y;
end;

function PointF(X, Y: Single): TPointF;
begin
  PointF.x := X;
  PointF.y := Y;
end;

function PointF(APoint: TPoint): TPointF;
begin
  PointF.x := APoint.x;
  PointF.y := APoint.y;
end;

function Point(APoint: TPointF): TPoint;
begin
  Point.x := round(APoint.x);
  Point.y := round(APoint.y);
end;

function round(APoints: TPointFList): TPointList;
var
  i: Integer;
begin
  SetLength(Result, Length(APoints));
  for i := 0 to High(APoints) do Result[i] := round(APoints[i]);
end;

function round(APoint: TPointF): TPoint;
begin
  Result.x := round(APoint.x);
  Result.y := round(APoint.y);
end;

function LoadImage(AName: String; AType: TCtrlType): TBitmap;
var
  PNGImage: TPortableNetworkGraphic;
  CtrlDir:  String;
begin
  case AType of
    ctButton: CtrlDir := 'Buttons';
    ctPanel: CtrlDir  := 'Panels';
  end;
  PNGImage := TPortableNetworkGraphic.Create();
  CtrlDir  := 'Images\' + CtrlDir + '\' + AName;
  PNGImage.LoadFromFile(CtrlDir);
  Result := TBitmap.Create();
  Result.Assign(PNGImage);
end;

{ TRectF }

class operator TRectF.Implicit(a: TRect): TRectF;
begin
  Result.Top    := a.Top;
  Result.Left   := a.Left;
  Result.Bottom := a.Bottom;
  Result.Right  := a.Right;
end;

class operator TRectF.Implicit(a: TRectF): TRect;
begin
  Result.Top    := round(a.Top);
  Result.Left   := round(a.Left);
  Result.Bottom := round(a.Bottom);
  Result.Right  := round(a.Right);
end;

end.
