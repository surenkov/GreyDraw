unit Transformations;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, Graphics, BGRABitmap, BGRABitmapTypes;

type
  TCtrlType = (ctButton, ctPanel);

  { TRectF }

  TRectF = record
    Left, Top, Right, Bottom: double;
    class operator Implicit(a: TRect): TRectF;
    class operator Implicit(a: TRectF): TRect;
  end;

  TOffset = TPoint;
  TPointList = array of TPoint;
  TPointFList = array of TPointF;

var
  Offset, CursorPos: TOffset;
  ViewPortCenter: TPointF;
  Scaling: single = 1;

function WorldToScreen(X, Y: single): TPointF; overload;
function WorldToScreen(APoint: TPointF): TPointF; overload;
function WorldToScreen(APoints: TPointFList): TPointFList; overload;

function ScreenToWorld(X, Y: integer): TPointF; overload;
function ScreenToWorld(APoint: TPoint): TPointF; overload;
function ScreenToWorld(APoints: TPointList): TPointFList; overload;

function round(APoints: TPointFList): TPointList; overload;
function round(APoint: TPointF): TPoint; overload;

function LoadImage(AName: string; AType: TCtrlType = ctButton): TBitmap;

implementation

function WorldToScreen(X, Y: single): TPointF;
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
  i: integer;
begin
  SetLength(Result, Length(APoints));
  for i := 0 to High(APoints) do
  begin
    Result[i] := WorldToScreen(APoints[i]);
  end;
end;

function ScreenToWorld(X, Y: integer): TPointF;
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
  i: integer;
begin
  SetLength(Result, Length(APoints));
  for i := 0 to High(APoints) do
  begin
    Result[i] := ScreenToWorld(APoints[i]);
  end;
end;

function round(APoints: TPointFList): TPointList;
var
  i: integer;
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

function LoadImage(AName: string; AType: TCtrlType): TBitmap;
var
  PNGImage: TPortableNetworkGraphic;
  CtrlDir: string;
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
