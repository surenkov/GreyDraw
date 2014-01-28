unit Loaders;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Drawable, Utils, typinfo, fpjson, fpjsonrtti,
  jsonparser, Dialogs, History, Graphics;

type

  { TFormat }

  TFormat = class(TPersistent)
  public
    procedure LoadFromFile(AFileName: String); virtual; abstract;
    procedure SaveToFile(AFileName: String); virtual; abstract;
    function FormatString: String; virtual; abstract;
    function TestSaveFile(AFileName: String): Boolean; virtual; abstract;
    function TestLoadFile(AFileName: String): Boolean; virtual; abstract;
  end;

  TFormatClass = class of TFormat;
  TFormatList = array of TFormat;

  { TGreyDrawFormat }

  TGreyDrawFormat = class(TFormat)
  public
    procedure LoadFromFile(AFileName: String); override;
    procedure SaveToFile(AFileName: String); override;
    function FormatString: String; override;
    function TestSaveFile(AFileName: String): Boolean; override;
    function TestLoadFile(AFileName: String): Boolean; override;

    class procedure LoadFromString(AString: String; Replace: Boolean);
    class function SaveToString: String;
    class function SerializeToJSONData(AFigure: TFigure): TJSONObject;
    class function DeserializeFromString(AString: String): TFigure;
  private
    procedure IterateObjects(const AName: TJSONStringType; Item: TJSONData;
      Data: TObject; var Continue: Boolean);
  end;

var
  FormatList:  TFormatList;
  FileName:    String = 'Untitled';
  FileContent: String = '{}';
  CurrentMode: String = GDFSign;

implementation

procedure RegisterFormat(AFormat: TFormatClass);
begin
  SetLength(FormatList, Length(FormatList) + 1);
  FormatList[High(FormatList)] := AFormat.Create;
  RegisterClass(AFormat);
end;

{ TGreyDrawFormat }

procedure TGreyDrawFormat.LoadFromFile(AFileName: String);
var
  Content: String = '';
  CurrStr: String;
  JObject: TJSONObject;
  i:     Integer;
  FFile: Text;
  Fail:  Boolean = False;
begin
  AssignFile(FFile, AFileName);
  Reset(FFile);
  while not EOF(FFile) do
  begin
    ReadLn(FFile, CurrStr);
    Content := Concat(Content, #13#10, CurrStr);
  end;
  with TJSONParser.Create(Content) do
    try
      JObject := Parse as TJSONObject;
    finally
      Destroy;
    end;
  JObject.Extract('sign');
  try
    if JObject.Get('version', '0.0.0') <= GDFVersion then
      JObject.Extract('version')
    else
      raise Exception.Create('Формат файла несовместим с данной вресией редактора.');
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
    HistoryArray[0] := JObject.FormatJSON();
    FileContent     := Copy(Content, 0, Length(Content));
    SetLength(FiguresList, JObject.Count);
    for i := 0 to JObject.Count - 1 do
    begin
      FiguresList[i].Free;
      FiguresList[i] := DeserializeFromString(JObject.Items[i].FormatJSON());
    end;
  end;
  ValidEvent;
  CloseFile(FFile);
end;

procedure TGreyDrawFormat.SaveToFile(AFileName: String);
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
  JRoot.Add('sign', GDFSign);
  JRoot.Add('version', GDFVersion);
  for i := 0 to High(FiguresList) do
    JRoot.Add(FiguresList[i].CreateUUID, SerializeToJSONData(FiguresList[i]));
  Write(FFile, JRoot.FormatJSON());
  JRoot.Destroy;
  CloseFile(FFile);
end;

function TGreyDrawFormat.FormatString: String;
begin
  Result := 'GreyDraw Image|*.gdr';
end;

function TGreyDrawFormat.TestSaveFile(AFileName: String): Boolean;
begin
  Result := AnsiLowerCase(ExtractFileExt(AFileName)) = '.gdr';
end;

function TGreyDrawFormat.TestLoadFile(AFileName: String): Boolean;
var
  JObject: TJSONObject;
  i:     Integer;
  FFile: Text;
  Content, CurrStr: String;
begin
  AssignFile(FFile, AFileName);
  Reset(FFile);
  while not EOF(FFile) do
  begin
    ReadLn(FFile, CurrStr);
    Content := Concat(Content, #13#10, CurrStr);
  end;
  CloseFile(FFile);
  Result := True;
  with TJSONParser.Create(Content) do
    try
      try
        JObject := Parse as TJSONObject;
      except
        ShowMessage('Файл поврежден или имеет неверный формат.');
        Result := False;
      end;
    finally
      Destroy;
    end;
  if not Result then
    Result := JObject.Get('sign', 'BlahBlahBlah') = GDFSign;
end;

class procedure TGreyDrawFormat.LoadFromString(AString: String; Replace: Boolean);
var
  JObject: TJSONData;
  i, size: Integer;
begin
  with TJSONParser.Create(AString) do
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
    FiguresList[i] := DeserializeFromString(JObject.Items[i - size].FormatJSON());
  end;
  JObject.Destroy;
  ValidEvent;
end;

class function TGreyDrawFormat.SaveToString: String;
var
  i:     Integer;
  JRoot: TJSONObject;
begin
  JRoot := TJSONObject.Create;
  for i := 0 to High(FiguresList) do
    JRoot.Add(FiguresList[i].CreateUUID, SerializeToJSONData(FiguresList[i]));
  Result := JRoot.FormatJSON();
  JRoot.Destroy;
end;

class function TGreyDrawFormat.SerializeToJSONData(AFigure: TFigure): TJSONObject;
var
  i: Integer;
  JPoints, JPoint: TJSONArray;
begin
  with TJSONStreamer.Create(nil) do
    try
      Result  := ObjectToJSON(AFigure);
      JPoints := TJSONArray.Create;
      for i := 0 to AFigure.PointsCount do
      begin
        JPoint := TJSONArray.Create;
        JPoint.Add(AFigure.GetPoint(i).x);
        JPoint.Add(AFigure.GetPoint(i).y);
        JPoints.Add(JPoint);
      end;
      Result.Add('Type', AFigure.ClassName);
      Result.Add('Points', JPoints);
    finally
      Destroy;
    end;
end;

class function TGreyDrawFormat.DeserializeFromString(AString: String): TFigure;
var
  JObject: TJSONObject;
begin
  with TJSONParser.Create(AString) do
    try
      JObject := Parse as TJSONObject;
    finally
      Destroy;
    end;
  try
    Result := GetClass(String(JObject.Extract('Type').Value)).Create as TFigure;
    JObject.Iterate(@IterateObjects, Result);
  except
    ShowMessage('Файл поврежден или имеет неверный формат.');
    JObject.Destroy;
    Result := nil;
  end;
end;

procedure TGreyDrawFormat.IterateObjects(const AName: TJSONStringType;
  Item: TJSONData; Data: TObject; var Continue: Boolean);
var
  i: Integer;
begin
  case AName of
    'Points':
      for i := 0 to Item.Count - 1 do
        try
          (Data as TFigure).AddPoint(
            Double(Item.Items[i].Items[0].Value),
            Double(Item.Items[i].Items[1].Value)
          );
        except
          ShowMessage('Формат изображения поврежден или неверен.');
        end;
    else
      if IsPublishedProp(Data, AName) then
        SetPropValue(Data, AName, Item.Value);
  end;
end;

initialization
  RegisterFormat(TGreyDrawFormat);

end.

