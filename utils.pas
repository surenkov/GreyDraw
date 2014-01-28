unit Utils;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, Math, Graphics, BGRABitmapTypes;

type

  TInvalidateEvent = procedure of object;
  TChangeEvent = procedure(AChanged: Boolean) of object;

  { TRectF }

  TRectF = record
    Left, Top, Right, Bottom: Double;
    class operator Implicit(a: TRect): TRectF;
    class operator Implicit(a: TRectF): TRect;
  end;

  TOffset = TPoint;
  TPointList = array of TPoint;
  TPointFList = array of TPointF;
  TPPointFList = array of PPointF;
  TRectList = array of TRect;
  TRectFList = array of TRectF;

  TCtrlType = (ctButton, ctPanel);

function round(APoints: TPointFList): TPointList; overload;
function round(APoint: TPointF): TPoint; overload;

function rand(ARange: Integer; var seed: Integer): Integer; overload;
function rand(AStart, AEnd: Integer; var seed: Integer): Integer; overload;

function RectF(ALeft, ATop, ARight, ABottom: float): TRectF;

function Point(X, Y: Int64): TPoint; overload;
function Point(X, Y: float): TPoint; overload;
function Point(APoint: TPointF): TPoint; overload;
function PointF(X, Y: Integer): TPointF; overload;
function PointF(X, Y: Single): TPointF; overload;
function PointF(APoint: TPoint): TPointF; overload;

function LoadImage(AName: String; AType: TCtrlType = ctButton): TBitmap;

const
  MinScale = 0.001;
  MaxScale = 68;

  GDEName = 'GreyDraw Editor';
  GDEVersion = '0.0.7';

  GDFSign = 'GDFImage';
  GDFVersion = '0.0.1';

var
  Offset, CursorPos: TOffset;
  ViewPortCenter: TPointF;
  Scaling: Single = 1;
  ChangeEvent: TChangeEvent;
  ValidEvent: TInvalidateEvent;


implementation

function round(APoints: TPointFList): TPointList;
var
  i: Integer;
begin
  SetLength(Result, Length(APoints));
  for i := 0 to High(APoints) do
    Result[i] := round(APoints[i]);
end;

function round(APoint: TPointF): TPoint;
begin
  Result.x := round(APoint.x);
  Result.y := round(APoint.y);
end;

const
  a = 2416;
  c = 374441;
  m = 1771875;

function rand(ARange: Integer; var seed: Integer): Integer;
begin
  seed := (a * seed + c) mod m;
  Result := seed mod ARange;
end;

function rand(AStart, AEnd: Integer; var seed: Integer): Integer;
begin
  seed := (a * seed + c) mod m;
  Result := AStart + (seed mod (AEnd - AStart));
end;

function RectF(ALeft, ATop, ARight, ABottom: float): TRectF;
begin
  with Result do
  begin
    Left := ALeft;
    Top := ATop;
    Right := ARight;
    Bottom := ABottom;
  end;
end;

function Point(X, Y: Int64): TPoint;
begin
  Point.x := X;
  Point.y := Y;
end;

function Point(X, Y: float): TPoint;
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

function LoadImage(AName: String; AType: TCtrlType): TBitmap;
var
  PNGImage: TPortableNetworkGraphic;
  CtrlDir: String;
begin
  case AType of
    ctButton: CtrlDir := 'Buttons';
    ctPanel: CtrlDir := 'Panels';
  end;
  PNGImage := TPortableNetworkGraphic.Create();
  CtrlDir := 'Images\' + CtrlDir + '\' + AName;
  PNGImage.LoadFromFile(CtrlDir);
  Result := TBitmap.Create();
  Result.Assign(PNGImage);
end;

{ TRectF }

class operator TRectF.Implicit(a: TRect): TRectF;
begin
  Result.Top := a.Top;
  Result.Left := a.Left;
  Result.Bottom := a.Bottom;
  Result.Right := a.Right;
end;

class operator TRectF.Implicit(a: TRectF): TRect;
begin
  Result.Top := round(a.Top);
  Result.Left := round(a.Left);
  Result.Bottom := round(a.Bottom);
  Result.Right := round(a.Right);
end;

end.
