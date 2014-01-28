unit History;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Dialogs, Utils;

procedure HistoryClear;
procedure HistoryPush(AStr: String);
function HistoryStepBack: String;
function HistoryStepForward: String;

const
  HistoryLevel = 100;

var
  HistoryArray: array [0..HistoryLevel - 1] of String;

implementation

var
  HI: Cardinal = 0;
  HJ: Cardinal = 0;

procedure HistoryClear;
begin
  HI := 0;
  HJ := HI;
end;

procedure HistoryPush(AStr: String);
begin
  Inc(HI);
  HJ := HI;
  HistoryArray[abs(HI mod HistoryLevel)] := AStr;
end;

function HistoryStepBack: String;
begin
  if (HI > 0) and (HI mod HistoryLevel <> HJ mod HistoryLevel + 1) then
    Dec(HI);
  Result := HistoryArray[abs(HI mod HistoryLevel)];
  if HI = 0 then
    ChangeEvent(False);
end;

function HistoryStepForward: String;
begin
  if (HI mod High(HistoryArray) <> HJ mod HistoryLevel) then
    Inc(HI);
  Result := HistoryArray[abs(HI mod HistoryLevel)];
end;

initialization
  HistoryArray[0] := '{}';

end.
