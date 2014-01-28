unit GDExport;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Drawable, Utils, BGRABitmap, Graphics, Math,
  JPEGForm, Controls, Forms, typinfo, Transformations;

type

  { TExporter }

  TExporter = class(TPersistent)
    class function TestFormat(AFileName: String): Boolean; virtual; abstract;
    class function ExportData(AFileNAme: String): Boolean; virtual; abstract;
    class function FormatString: String; virtual; abstract;
  end;

  TExporterClass = class of TExporter;
  TExporterList = array of TExporterClass;

  { TBMPExporter }

  TBMPExporter = class(TExporter)
    class function TestFormat(AFileName: String): Boolean; override;
    class function ExportData(AFileNAme: String): Boolean; override;
    class function FormatString: String; override;
  end;

  { TJpegExporter }

  TJpegExporter = class(TExporter)
    class function TestFormat(AFileName: String): Boolean; override;
    class function ExportData(AFileNAme: String): Boolean; override;
    class function FormatString: String; override;
  end;

  { TPNGExporter }

  TPNGExporter = class(TExporter)
    class function TestFormat(AFileName: String): Boolean; override;
    class function ExportData(AFileNAme: String): Boolean; override;
    class function FormatString: String; override;
  end;

  { TSVGExporter }

  TSVGExporter = class(TExporter)
    class function TestFormat(AFileName: String): Boolean; override;
    class function ExportData(AFileNAme: String): Boolean; override;
    class function FormatString: String; override;
  private
    class function FigureToSVGElement(AFigure: TFigure): String;
    class function PropertyToSVGAttribute(AFigure: TFigure; APropName: String): String;
  end;

procedure RegisterExporter(AExporter: TExporterClass);

var
  ExporterList: TExporterList;

implementation

var
  qual: TJPEGQualityRange;

procedure ChangeQuality(AQual: TJPEGQualityRange);
begin
  qual := AQual;
end;

function DrawOnBitmap: TBGRABitmap;
var
  i:   Integer;
  R:   TRect;
  scl: Double;
  off: TOffset;
begin
  R      := ScrollRect;
  Result := TBGRABitmap.Create(abs(R.Right - R.Left), abs(R.Bottom - R.Top));
  Result.Fill(clWhite);
  scl     := Scaling;
  off     := Offset;
  Scaling := 1;
  Offset  := Point(-R.Left, -R.Top);
  for i := 0 to High(FiguresList) do
    FiguresList[i].Draw(Result);
  Scaling := scl;
  Offset  := off;
end;

procedure RegisterExporter(AExporter: TExporterClass);
begin
  SetLength(ExporterList, Length(ExporterList) + 1);
  ExporterList[High(ExporterList)] := AExporter;
  RegisterClass(AExporter);
end;

{ TBMPExporter }

class function TBMPExporter.TestFormat(AFileName: String): Boolean;
begin
  Result := AnsiLowerCase(ExtractFileExt(AFileName)) = '.bmp';
end;

class function TBMPExporter.ExportData(AFileNAme: String): Boolean;
var
  bmp: TBitmap;
begin
  with DrawOnBitmap do
    try
      bmp := TBitmap.Create;
      try
        bmp.SetSize(Width, Height);
        bmp.Canvas.Brush.Color := clWhite;
        bmp.Canvas.FillRect(0, 0, Width, Height);
        Draw(bmp.Canvas, 0, 0, True);
        bmp.SaveToFile(AFileName);
      finally
        bmp.Free;
      end;
    finally
      Free;
    end;
end;

class function TBMPExporter.FormatString: String;
begin
  Result:= 'Windows Bitmap|*.bmp';
end;

{ TJpegExporter }

class function TJpegExporter.TestFormat(AFileName: String): Boolean;
begin
  Result := AnsiLowerCase(ExtractFileExt(AFileName)) = '.jpg';
end;

class function TJpegExporter.ExportData(AFileNAme: String): Boolean;
var
  jpg: TJPEGImage;
begin
  SetQuality := @ChangeQuality;
  with TJPEGQualitySelector.Create(Application) do
    try
      if ShowModal = mrOk then
        with DrawOnBitmap do
          try
            jpg := TJPEGImage.Create;
            jpg.SetSize(Width, Height);
            jpg.CompressionQuality := qual;
            jpg.Canvas.Brush.Color := clWhite;
            jpg.Canvas.FillRect(0, 0, Width, Height);
            Draw(jpg.Canvas, 0, 0, True);
            jpg.SaveToFile(AFileName);
            jpg.Free;
          finally
            Free;
          end;
    finally
      Free;
    end;
end;

class function TJpegExporter.FormatString: String;
begin
  Result := 'JPEG Image|*.jpg';
end;

{ TPNGExporter }

class function TPNGExporter.TestFormat(AFileName: String): Boolean;
begin
  Result := AnsiLowerCase(ExtractFileExt(AFileName)) = '.png';
end;

class function TPNGExporter.ExportData(AFileNAme: String): Boolean;
begin
  with DrawOnBitmap do
    try
      SaveToFile(AFileName);
    finally
      Free;
    end;
end;

class function TPNGExporter.FormatString: String;
begin
  Result := 'Portable Network Graphics|*.png';
end;

{ TSVGExporter }

const
  XMLDecl     = '<?xml version="1.0" encoding="UTF-8"?>';
  SVGSign     = '<!DOCTYPE svg PUBLIC "-//W3C//DTD SVG 1.1//EN" "http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd">';
  SVGRootOpen = '<svg version="1.1" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" x="0px" y="0px" width="%gpx" height="%gpx" xml:space="preserve">';
  SGRootClose = '</svg>';

class function TSVGExporter.TestFormat(AFileName: String): Boolean;
begin
  Result := AnsiLowerCase(ExtractFileExt(AFileName)) = '.svg';
end;

class function TSVGExporter.ExportData(AFileNAme: String): Boolean;
var
  SVGFile: Text;
  R: TRectF;
  i: Integer;
  s: Char;
begin
  R := ScrollRect;
  if not FileExists(AFileName) then
    FileClose(FileCreate(AFileName));
  AssignFile(SVGFile, AFileName);
  Rewrite(SVGFile);
  s := DefaultFormatSettings.DecimalSeparator;
  DefaultFormatSettings.DecimalSeparator := '.';
  WriteLn(SVGFile, XMLDecl);
  WriteLn(SVGFile, SVGSign);
  WriteLn(SVGFile, '<!-- Generator: ', GDEName, ' ', GDEVersion, ' -->');
  WriteLn(SVGFile, Format(SVGRootOpen, [R.Right - R.Left, R.Bottom - R.Top]));
  for i := 0 to High(FiguresList) do
    WriteLn(SVGFile, #9, FigureToSVGElement(FiguresList[i]));
  WriteLn(SVGFile, SGRootClose);
  DefaultFormatSettings.DecimalSeparator := s;
  CloseFile(SVGFile);
end;

class function TSVGExporter.FormatString: String;
begin
  Result := 'Scalable Vector Graphics|*.svg';
end;

class function TSVGExporter.FigureToSVGElement(AFigure: TFigure): String;
begin
  Result := '<';
  case AFigure.ClassName of
    'TEllipse': Result   := Concat(Result, 'ellipse');
    'TRectangle': Result := Concat(Result, 'rect');
    'TLine': Result      := Concat(Result, 'line');
    'TPolyline': Result  := Concat(Result, 'polyline');
    'TPolygon', 'TRegularPolygon': Result := Concat(Result, 'polygon');
    else
      exit('');
  end;
  if AFigure.InheritsFrom(TBrushFigure) then
    Result := Concat(Result, PropertyToSVGAttribute(AFigure, 'fill'))
  else
    Result := Concat(Result, ' fill="none"');
  if AFigure.InheritsFrom(TPenFigure) then
    Result := Concat(Result, PropertyToSVGAttribute(AFigure, 'stroke'),
      PropertyToSVGAttribute(AFigure, 'penstyle'));
  Result   := Concat(Result, PropertyToSVGAttribute(AFigure, 'points'));
  Result   := Concat(Result, ' />');
end;

class function TSVGExporter.PropertyToSVGAttribute(AFigure: TFigure;
  APropName: String): String;
var
  P: TPointFList;
  i: Integer;
  R: TRectF;
begin
  case APropName of
    'points':
    begin
      R := ScrollRect;
      case AFigure.ClassName of
        'TEllipse': Result   :=
            Format(' cx="%g" cy="%g" rx="%g" ry="%g"',
            [AFigure.GetPoint(0).x - R.Left, AFigure.GetPoint(0).y -
            R.Top, abs(AFigure.GetPoint(1).x - AFigure.GetPoint(0).x),
            abs(AFigure.GetPoint(1).y - AFigure.GetPoint(0).y)]);
        'TRectangle': Result :=
            Format(' x="%g" y="%g" width="%g" height="%g"',
            [min(AFigure.GetPoint(0).x, AFigure.GetPoint(1).x) -
            R.Left, min(AFigure.GetPoint(0).y, AFigure.GetPoint(1).y) -
            R.Top, abs(AFigure.GetPoint(1).x - AFigure.GetPoint(0).x) -
            R.Left, abs(AFigure.GetPoint(1).y - AFigure.GetPoint(0).y) - R.Top]);
        'TLine': Result      :=
            Format(' x1="%g" y1="%g" x2="%g" y2="%g"',
            [AFigure.GetPoint(0).x - R.Left, AFigure.GetPoint(0).y -
            R.Top, AFigure.GetPoint(1).x - R.Left, AFigure.GetPoint(1).y - R.Top]);
        'TPolyline', 'TPolygon':
        begin
          Result := ' points="';
          for i := 0 to AFigure.PointsCount do
            Result := Concat(Result,
              Format('%g,%g ', [AFigure.GetPoint(i).x - R.Left,
              AFigure.GetPoint(i).y - R.Top]));
          Result   := Copy(Result, 0, Length(Result) - 1);
          Result   := Concat(Result, '"');
        end;
        'TRegularPolygon':
        begin
          with AFigure as TRegularPolygon do
            P    := CreatePoints;
          Result := ' points="';
          for i := 0 to High(P) do
            Result := Concat(Result,
              Format('%g,%g ', [P[i].x - R.Left, P[i].y - R.Top]));
          Result := Copy(Result, 0, Length(Result) - 1);
          Result := Concat(Result, '"');
        end;
      end;
    end;
    'fill': with AFigure as TBrushFigure do
        Result := Format(' fill="#%6.6X" fill-opacity="%g"',
          [ColorToRGB(BrushColor), BrushOpacity / 255]);
    'stroke': with AFigure as TPenFigure do
        Result := Format(
          ' stroke="#%6.6X" stroke-width="%g" stroke-opacity="%g" stroke-linejoin="bevel" stroke-miterlimit="10"',
          [ColorToRGB(PenColor), PenSize, PenOpacity / 255]);
    'penstyle': with AFigure as TPenFigure do
        case PenStyle of
          psDot: Result     := Format(' stroke-dasharray="%g,%g"', [PenSize, PenSize]);
          psDash: Result    :=
              Format(' stroke-dasharray="%g,%g"', [PenSize * 2, PenSize]);
          psClear: Result   := Format(' stroke-opacity="%g"', [0]);
          psDashDot: Result :=
              Format(' stroke-dasharray="%g,%g,%g,%g"',
              [PenSize * 2, PenSize, PenSize, PenSize]);
          psDashDotDot: Result :=
              Format(' stroke-dasharray="%g,%g,%g,%g,%g,%g"',
              [PenSize * 3, PenSize, PenSize, PenSize, PenSize, PenSize]);
        end;
  end;
end;

initialization
  RegisterExporter(TBMPExporter);
  RegisterExporter(TPNGExporter);
  RegisterExporter(TJpegExporter);
  RegisterExporter(TSVGExporter);

end.
