unit Loaders;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Drawable, typinfo;

type
  TStringLoader = function(ASerialized: String): TFigure;
  TStringSaver = function(ADeserialized: TFigure): String;

function GreyLoadFromString(ASerialized: String): TFigure;
function GreySaveToString(ADeserialized: TFigure): String;

function CryoLoadFromString(ASerialized: String): TFigure;
function CryoSaveToString(ADeserialized: TFigure): String;

var
  FileName: String;


implementation

function GreyLoadFromString(ASerialized: String): TFigure;
begin

end;

function GreySaveToString(ADeserialized: TFigure): String;
begin

end;

function CryoLoadFromString(ASerialized: String): TFigure;

  function GetStr(AKey: String; var AStr: String): String;
  begin
    Result := Copy(AStr, 1, Pos(AKey, AStr) - 1);
    AStr   := Copy(AStr, Length(Result) + Length(AKey) + 1, Length(AStr) -
      Length(Result) - Length(AKey));
  end;

  function GetNum(AKey: String; var AStr: String): Double;
  begin
    Result := StrToFloat(GetStr(AKey, AStr));
  end;

var
  i:      Integer;
  PrList: TPropList;
  Ind:    Integer;
  X, Y:   Double;
  s:      String;
  Flag:   Boolean = False;
begin
  s    := Copy(ASerialized, 1, Pos('|||', ASerialized) - 1);
  ASerialized := Copy(ASerialized, Length(s) + 4, Length(ASerialized) - Length(S) - 3);
  for i := 0 to High(DrawObjectsClasses) do
    if s = Copy(DrawObjectsClasses[i].ClassName, 1, Length(s)) then
    begin
      Result := DrawObjectsClasses[i].Create;
      Flag   := True;
      break;
    end;
  if not Flag or (GetStr('::', ASerialized) <> 'Points') then
  begin
    Result := nil;
    exit;
  end;
  try
    Ind := round(GetNum('::', ASerialized));
    for i := 0 to Ind - 1 do
    begin
      X := GetNum('|', ASerialized);
      if i <> Ind - 1 then Y := GetNum('::', ASerialized)
      else
        Y := GetNum('|||', ASerialized);
      Result.AddPoint(FloatPoint(X, Y));
    end;
    GetProps(Result.ClassType, PrList);
    for i := 0 to High(PrList) do
    begin
      s := GetStr('::', ASerialized);
      if IsPublishedProp(Result, s) then SetPropValue(Result, s, GetStr('|||', ASerialized));
    end;
  except
    Result := nil;
    exit;
  end;
end;

function CryoSaveToString(ADeserialized: TFigure): String;
var
  i: Integer;
  PropList: TPropList;
  s: String;
begin
  Result := TClass(ADeserialized.ClassType).ClassName + '|||';

  Result += 'Points::' + IntToStr(ADeserialized.PointsCount + 1) + '::';
  for i := 0 to ADeserialized.PointsCount do
    Result += FloatToStr(ADeserialized.X) + '|' +
      FloatToStr(ADeserialized.Point[i].Y) + '::';
  Result   := Copy(Result, 1, Length(Result) - 2) + '|||';
  for i := GetPropList(ADeserialized.ClassType, @PropList) downto 0 do
  begin
    Result += PropList[i]^.Name + '::';
    s      := GetPropValue(ADeserialized, PropList[i]^.Name);
    Result += s + '|||';
  end;
end;

end.
