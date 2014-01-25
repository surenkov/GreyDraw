unit GDExport;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Drawable, Utils, BGRABitmap, Graphics, Math,
  JPEGForm, Controls, Forms, typinfo;

type
  TExporter = function(AFileName: String): Integer;

function BMPExport(AFileName: String): Integer;
function JPEGExport(AFileName: String): Integer;
function PNGExport(AFileName: String): Integer;
function SVGExport(AFileName: String): Integer;

function ScrollRect: TRectF;

implementation

var
  qual: TJPEGQualityRange;

procedure ChangeQuality(AQual: TJPEGQualityRange);
begin
  qual := AQual;
end;

function BMPExport(AFileName: String): Integer;
var
  i:    Integer;
  R:    TRect;
  cnvs: TBGRABitmap;
  bmp:  TBitmap;
  scl:  Double;
  off:  TOffset;
begin
  R    := ScrollRect;
  cnvs := TBGRABitmap.Create(abs(R.Right - R.Left), abs(R.Bottom - R.Top));
  cnvs.Fill(clWhite);
  scl     := Scaling;
  off     := Offset;
  Scaling := 1;
  Offset  := Point(0 - R.Left, 0 - R.Top);
  for i := 0 to High(FiguresList) do
    FiguresList[i].Draw(cnvs);
  Scaling := scl;
  Offset  := off;
  bmp     := TBitmap.Create;
  bmp.SetSize(abs(R.Right - R.Left), abs(R.Bottom - R.Top));
  bmp.Canvas.Brush.Color := clWhite;
  bmp.Canvas.FillRect(R);
  cnvs.Draw(bmp.Canvas, 0, 0, True);
  cnvs.Free;
  bmp.SaveToFile(AFileName);
  bmp.Free;
end;

function JPEGExport(AFileName: String): Integer;
var
  i:    Integer;
  R:    TRect;
  cnvs: TBGRABitmap;
  jpg:  TJPEGImage;
  scl:  Double;
  off:  TOffset;
begin
  R    := ScrollRect;
  cnvs := TBGRABitmap.Create(abs(R.Right - R.Left), abs(R.Bottom - R.Top));
  cnvs.Fill(clWhite);
  scl     := Scaling;
  off     := Offset;
  Scaling := 1;
  Offset  := Point(-R.Left, -R.Top);
  for i := 0 to High(FiguresList) do
    FiguresList[i].Draw(cnvs);
  Scaling    := scl;
  Offset     := off;
  SetQuality := @ChangeQuality;
  with TJPEGQualitySelector.Create(Application) do
    try
      if ShowModal = mrOk then
      begin
        jpg := TJPEGImage.Create;
        jpg.SetSize(abs(R.Right - R.Left), abs(R.Bottom - R.Top));
        jpg.CompressionQuality := qual;
        jpg.Canvas.Brush.Color := clWhite;
        jpg.Canvas.FillRect(R);
        cnvs.Draw(jpg.Canvas, 0, 0, True);
        jpg.SaveToFile(AFileName);
        jpg.Free;
      end;
    finally
      cnvs.Free;
      Free;
    end;
end;

function PNGExport(AFileName: String): Integer;
var
  i:    Integer;
  R:    TRect;
  cnvs: TBGRABitmap;
  scl:  Double;
  off:  TOffset;
begin
  R      := ScrollRect;
  cnvs   := TBGRABitmap.Create(abs(R.Right - R.Left), abs(R.Bottom - R.Top));
  scl    := Scaling;
  off    := Offset;
  Scaling := 1;
  Offset := Point(-R.Left, -R.Top);
  for i := 0 to High(FiguresList) do
    FiguresList[i].Draw(cnvs);
  Scaling := scl;
  Offset  := off;
  cnvs.SaveToFile(AFileName);
  cnvs.Free;
end;

const
  XMLDecl     = '<?xml version="1.0" encoding="UTF-8"?>';
  SVGSign     =
    '<!DOCTYPE svg PUBLIC "-//W3C//DTD SVG 1.1//EN" "http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd">';
  SVGRootOpen =
    '<svg version="1.1" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" x="0" y="0" width="%g" height="%g" xml:space="preserve">';
  SGRootClose = '</svg>';


function PropertyToSVGAttribute(AFigure: TFigure; APropName: String): String;
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
            [AFigure.GetPoint(0).x - R.Left, AFigure.GetPoint(0).y - R.Top,
            abs(AFigure.GetPoint(1).x - AFigure.GetPoint(0).x),
            abs(AFigure.GetPoint(1).y - AFigure.GetPoint(0).y)]);
        'TRectangle': Result :=
            Format(' x="%g" y="%g" width="%g" height="%g"',
            [min(AFigure.GetPoint(0).x, AFigure.GetPoint(1).x) - R.Left,
            min(AFigure.GetPoint(0).y, AFigure.GetPoint(1).y) - R.Top,
            abs(AFigure.GetPoint(1).x - AFigure.GetPoint(0).x) - R.Left,
            abs(AFigure.GetPoint(1).y - AFigure.GetPoint(0).y) - R.Top]);
        'TLine': Result      :=
            Format(' x1="%g" y1="%g" x2="%g" y2="%g"', [AFigure.GetPoint(0).x - R.Left,
            AFigure.GetPoint(0).y - R.Top, AFigure.GetPoint(1).x - R.Left, AFigure.GetPoint(1).y - R.Top]);
        'TPolyline', 'TPolygon':
        begin
          Result := ' points="';
          for i := 0 to AFigure.PointsCount do
            Result := Concat(Result, Format('%g,%g ',
              [AFigure.GetPoint(i).x - R.Left, AFigure.GetPoint(i).y - R.Top]));
          Result   := Copy(Result, 0, Length(Result) - 1);
          Result   := Concat(Result, '"');
        end;
        'TRegularPolygon':
        begin
          with AFigure as TRegularPolygon do
            P    := CreatePoints;
          Result := ' points="';
          for i := 0 to High(P) do
            Result := Concat(Result, Format('%g,%g ', [P[i].x - R.Left, P[i].y - R.Top]));
          Result   := Copy(Result, 0, Length(Result) - 1);
          Result   := Concat(Result, '"');
        end;
      end;
    end;
    'fill': with AFigure as TBrushFigure do
      Result   := Format(' fill="#%6.6X" fill-opacity="%g"',
        [ColorToRGB(BrushColor), BrushOpacity / 255]);
    'stroke': with AFigure as TPenFigure do
      Result := Format(' stroke="#%6.6X" stroke-width="%g" stroke-opacity="%g"',
        [ColorToRGB(PenColor), PenSize, PenOpacity / 255]);
  end;
end;

function FigureToSVGElement(AFigure: TFigure): String;
begin
  Result := '<';
  case AFigure.ClassName of
    'TEllipse': Result   := Concat(Result, 'ellipse');
    'TRectangle': Result := Concat(Result, 'rect');
    'TLine': Result      := Concat(Result, 'line');
    'TPolyline': Result  := Concat(Result, 'polyline');
    'TPolygon', 'TRegularPolygon': Result := Concat(Result, 'polygon');
  end;
  if AFigure.InheritsFrom(TBrushFigure) then
    Result := Concat(Result, PropertyToSVGAttribute(AFigure, 'fill'))
  else
    Result := Concat(Result, ' fill="none"');
  if AFigure.InheritsFrom(TPenFigure) then
    Result := Concat(Result, PropertyToSVGAttribute(AFigure, 'stroke'));
  Result   := Concat(Result, PropertyToSVGAttribute(AFigure, 'points'));
  Result   := Concat(Result, ' />');
end;

function SVGExport(AFileName: String): Integer;
var
  SVGFile: Text;
  R: TRectF;
  i: Integer;
  s: char;
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
  WriteLn(SVGFile, Format(SVGRootOpen, [R.Right - R.Left, R.Bottom - R.Top]));
  for i := 0 to High(FiguresList) do
    WriteLn(SVGFile, FigureToSVGElement(FiguresList[i]));
  WriteLn(SVGFile, SGRootClose);
  DefaultFormatSettings.DecimalSeparator := s;
  CloseFile(SVGFile);
end;

function ScrollRect: TRectF;
var
  i:    Integer;
  Rect: TRectF;
begin
  if Length(FiguresList) > 0 then
    Result := FiguresList[0].WorldRect;
  for i := 0 to High(FiguresList) do
  begin
    Rect := FiguresList[i].WorldRect;
    with Result do
    begin
      Left   := Min(Left, Rect.Left);
      Right  := Max(Right, Rect.Right);
      Top    := Min(Top, Rect.Top);
      Bottom := Max(Bottom, Rect.Bottom);
    end;
  end;
end;

end.
