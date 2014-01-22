unit Transformations;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Math, BGRABitmapTypes, Utils;

function WorldToScreen(X, Y: float): TPointF; overload;
function WorldToScreen(APoint: TPointF): TPointF; overload;
function WorldToScreen(APoints: TPointFList): TPointFList; overload;

function ScreenToWorld(X, Y: Integer): TPointF; overload;
function ScreenToWorld(APoint: TPoint): TPointF; overload;
function ScreenToWorld(APoints: TPointList): TPointFList; overload;

implementation

function WorldToScreen(X, Y: float): TPointF;
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

end.
