unit Loaders;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Drawable, typinfo, fpjson, fpjsonrtti, jsonparser, Dialogs, History, Utils;

type
  TStringLoader = function(ASerialized: String): TFigure;
  TStringSaver = function(ADeserialized: TFigure): String;
  TFileLoader = procedure(var AFileName: String);
  TFileSaver = procedure(var AFileName: String);

  { TParserParam }

  TParserParam = class
    procedure GreyDraw(const AName: TJSONStringType; Item: TJSONData;
      Data: TObject; var Continue: Boolean);
    procedure CryoDraw(const AName: TJSONStringType; Item: TJSONData;
      Data: TObject; var Continue: Boolean);
  end;

procedure GreyDrawLoadFromText(AText: String; Replace: Boolean = False);
procedure GreyDrawLoadFromFile(var AFileName: String);
procedure GreyDrawSaveToFile(var AFileName: String);
function GreyDrawLoadFromString(ASerialized: String): TFigure;
function GreyDrawSaveToString(ADeserialized: TFigure): String;
function GreyDrawSaveToText: String;
function GreyDrawSaveToJSONData(ADeserialized: TFigure): TJSONObject;

procedure CryoDrawLoadFromFile(var AFileName: String);
procedure CryoDrawSaveToFile(var AFileName: String);
function CryoDrawLoadFromString(ASerialized: String): TFigure;
function CryoDrawSaveToString(ADeserialized: TFigure): String;

var
  FileName:    String;
  FileContent: String = '{}';
  CurrentMode: String = CurrentGDFSign;

implementation

uses MainForm, Graphics, Properties;

var
  Iterator: TParserParam;

procedure GreyDrawLoadFromText(AText: String; Replace: Boolean);
var
  JObject: TJSONData;
  i, size: Integer;
begin
  with TJSONParser.Create(AText) do
    try
      JObject := Parse;
    except
      ShowMessage('Файл поврежден или имеет неверный формат.');
      Destroy;
    end;
  if not Replace then
  begin
    size := Length(FiguresList);
    SetLength(FiguresList, JObject.Count + Length(FiguresList));
  end
  else
  begin
    size := 0;
    for i := 0 to High(FiguresList) do
      FiguresList[i].Free;
    SetLength(FiguresList, JObject.Count);
  end;
  for i := size to High(FiguresList) do
  begin
    FiguresList[i] := GreyDrawLoadFromString(JObject.Items[i - size].FormatJSON());
  end;
  JObject.Destroy;
  GreyDrawForm.ViewPort.Invalidate;
end;

procedure GreyDrawLoadFromFile(var AFileName: String);
var
  Content: String = '';
  CurrStr: String;
  JObject: TJSONObject;
  i:     Integer;
  FFile: Text;
  Fail:  Boolean = False;
begin
  if not FileExists(AFileName) then
    exit;
  AssignFile(FFile, AFileName);
  Reset(FFile);
  while not EOF(FFile) do
  begin
    ReadLn(FFile, CurrStr);
    Content := Concat(Content, #13#10, CurrStr);
  end;
  FileContent := Copy(Content, 0, Length(Content));
  with TJSONParser.Create(Content) do
    try
      try
        JObject := Parse as TJSONObject;
      except
        ShowMessage('Файл поврежден или имеет неверный формат.');
        Fail := True;
      end;
    finally
      Destroy;
    end;
  if not Fail then
    try
      if JObject.Get('sign', 'BlahBlahBlah') = CurrentGDFSign then
        JObject.Extract('sign')
      else
        raise Exception.Create('Файл поврежден или имеет неверный формат.');
      if JObject.Get('version', '0.0.0') <= CurrentGDFVersion then
        JObject.Extract('version')
      else
        raise Exception.Create('Формат файла несовместим с данной вресией редактора.');
      HistoryArray[0] := JObject.FormatJSON();
    except
      on e: Exception do
      begin
        ShowMessage(e.Message);
        JObject.Destroy;
        Fail := True;
      end;
    end;
  if not Fail then
  begin
    SetLength(FiguresList, JObject.Count);
    for i := 0 to JObject.Count - 1 do
    begin
      FiguresList[i].Free;
      FiguresList[i] := GreyDrawLoadFromString(JObject.Items[i].FormatJSON());
    end;
  end;
  GreyDrawForm.ViewPort.Invalidate;
  CloseFile(FFile);
end;

procedure GreyDrawSaveToFile(var AFileName: String);
var
  i:     Integer;
  JRoot: TJSONObject;
  FFile: Text;
begin
  if not FileExists(AFileName) then
    FileClose(FileCreate(AFileName));
  AssignFile(FFile, AFileName);
  Rewrite(FFile);
  JRoot := TJSONObject.Create;
  JRoot.Add('sign', CurrentGDFSign);
  JRoot.Add('version', CurrentGDFVersion);
  for i := 0 to High(FiguresList) do
    JRoot.Add(FiguresList[i].CreateUUID, GreyDrawSaveToJSONData(FiguresList[i]));
  Write(FFile, JRoot.FormatJSON());
  JRoot.Destroy;
  CloseFile(FFile);
end;

function GreyDrawLoadFromString(ASerialized: String): TFigure;
var
  JObject: TJSONObject;
begin
  with TJSONParser.Create(ASerialized) do
    try
      JObject := Parse as TJSONObject;
    finally
      Destroy;
    end;
  try
    Result := GetClass(String(JObject.Extract('Type').Value)).Create as TFigure;
    JObject.Iterate(@Iterator.GreyDraw, Result);
  except
    ShowMessage('Файл поврежден или имеет неверный формат.');
    JObject.Destroy;
    Result := nil;
  end;
end;

function GreyDrawSaveToString(ADeserialized: TFigure): String;
begin
  with GreyDrawSaveToJSONData(ADeserialized) do
    try
      Result := FormatJSON();
    finally
      Destroy;
    end;
end;

function GreyDrawSaveToText: String;
var
  i:     Integer;
  JRoot: TJSONObject;
begin
  JRoot := TJSONObject.Create;
  for i := 0 to High(FiguresList) do
    JRoot.Add(FiguresList[i].CreateUUID, GreyDrawSaveToJSONData(FiguresList[i]));
  Result := JRoot.FormatJSON();
  JRoot.Destroy;
end;

function GreyDrawSaveToJSONData(ADeserialized: TFigure): TJSONObject;
var
  i: Integer;
  JPoints, JPoint: TJSONArray;
begin
  with TJSONStreamer.Create(nil) do
    try
      Result  := ObjectToJSON(ADeserialized);
      JPoints := TJSONArray.Create;
      for i := 0 to ADeserialized.PointsCount do
      begin
        JPoint := TJSONArray.Create;
        JPoint.Add(ADeserialized.GetPoint(i).x);
        JPoint.Add(ADeserialized.GetPoint(i).y);
        JPoints.Add(JPoint);
      end;
      Result.Add('Type', ADeserialized.ClassName);
      Result.Add('Points', JPoints);
    finally
      Destroy;
    end;
end;

procedure CryoDrawLoadFromFile(var AFileName: String);
begin

end;

procedure CryoDrawSaveToFile(var AFileName: String);
begin

end;

function CryoDrawLoadFromString(ASerialized: String): TFigure;
begin

end;

function CryoDrawSaveToString(ADeserialized: TFigure): String;
begin

end;

{ TParserParam }

procedure TParserParam.GreyDraw(const AName: TJSONStringType;
  Item: TJSONData; Data: TObject; var Continue: Boolean);
var
  i: Integer;
begin
  case AName of
    'Points':
      for i := 0 to Item.Count - 1 do
        try
          (Data as TFigure).AddPoint(Double(Item.Items[i].Items[0].Value),
            Double(Item.Items[i].Items[1].Value));
        except
          ShowMessage('Формат изображения поврежден или неверен.');
        end;
    else
      if IsPublishedProp(Data, AName) then
        SetPropValue(Data, AName, Item.Value)
  end;
end;

procedure TParserParam.CryoDraw(const AName: TJSONStringType;
  Item: TJSONData; Data: TObject; var Continue: Boolean);
begin

end;

initialization
  Iterator := TParserParam.Create;

finalization
  Iterator.Destroy;

end.
