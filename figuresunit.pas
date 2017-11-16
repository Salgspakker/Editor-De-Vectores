unit figuresunit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, GraphMath, transformunit, Graphics;

  function CasePenStyle(Index: integer):TPenStyle;
  function CaseBrushStyle(Index: integer):TBrushStyle;

type

  TFigureClass = class of TFigure;

  TFigure = class
    FWidth:                integer;
    FPenStyle:             TPenStyle;
    FBrushStyle:           TBrushStyle;
    FAngle:                integer;
    Points:                array of TFloatPoint;
    FPenColor,FBrushColor: TColor;
    procedure Draw(Canvas: TCanvas); virtual; abstract;
  end;

  TPolyline = class(TFigure)
    procedure Draw(Canvas: TCanvas); override;
  end;

  TLine = class(TFigure)
    procedure Draw(Canvas: TCanvas); override;
  end;

  TRectangle = class(TFigure)
    procedure Draw(Canvas: TCanvas); override;
  end;

  TEllipse = class(TFigure)
    procedure Draw(Canvas: TCanvas); override;
  end;

  TTriangle = class(TFigure)
    procedure Draw(Canvas: TCanvas); override;
  end;

  TRoundRect = class(TFigure)
    procedure Draw(Canvas: TCanvas); override;
  end;

  TMagnifierFrame = class(TFigure)
    procedure Draw(Canvas: TCanvas); override;
  end;

  THandFigure = class(TFigure)
    procedure Draw(Canvas: TCanvas); override;
  end;

var
  Figures: array of TFigure;
  OffsetFirstPoint: TPoint;
  PenColor, BrushColor: TColor;

implementation

function CasePenStyle(Index: integer): TPenStyle;
begin
  case Index of
    0:Result:=psSolid;
    1:Result:=psDash;
    2:Result:=psDot;
    3:Result:=psDashDot;
    4:Result:=psDashDotDot;
  end;
end;

function CasePenStyleIndex(Style: TPenStyle): integer;
begin
  case Style of
    psSolid:Result     :=0;
    psDash:Result      :=1;
    psDot:Result       :=2;
    psDashDot:Result   :=3;
    psDashDotDot:Result:=4;
end;
end;

function CaseBrushStyle(Index: integer): TBrushStyle;
begin
  case Index of
    0:Result:=bsSolid;
    1:Result:=bsBDiagonal;
    2:Result:=bsDiagCross;
    3:Result:=bsVertical;
    4:Result:=bsCross;
    5:Result:=bsFDiagonal;
    6:Result:=bsHorizontal;
  end;
end;

function CaseBrushStyleIndex(BrushStyle: TBrushStyle): integer;
begin
  case BrushStyle of
    bsSolid: Result     :=0;
    bsBDiagonal:Result  :=1;
    bsDiagCross:Result  :=2;
    bsVertical:Result   :=3;
    bsCross:Result      :=4;
    bsFDiagonal:Result  :=5;
    bsHorizontal:Result :=6;
  end;
end;

procedure TPolyline.Draw(Canvas: TCanvas);
var i: integer;
begin
  Canvas.Pen.Style:=FPenStyle;
  Canvas.Pen.Width:=FWidth;
  Canvas.Pen.Color:=FPenColor;
  for i:=0 to high(Points)-1 do
  begin
    Canvas.Line(
      W2S(Points[i])  .x,
      W2S(Points[i])  .y,
      W2S(Points[i+1]).x,
      W2S(Points[i+1]).y);
  end;
end;

procedure THandFigure.Draw(Canvas: TCanvas);
begin
  Canvas.Pen.Color  :=clBlack;
  Canvas.Brush.Color:=clWhite;
  Canvas.Pen.Style  :=psSolid;
  Canvas.Brush.Style:=bsSolid;
  Canvas.Pen.Width  :=1;
  Canvas.Ellipse(
    W2S(Points[low(Points)]).x-5,
    W2S(Points[low(Points)]).y-5,
    W2S(Points[low(Points)]).x+5,
    W2S(Points[low(Points)]).y+5);
end;

procedure TMagnifierFrame.Draw(Canvas: TCanvas);
begin
  Canvas.Pen.Color  :=clBlack;
  Canvas.Brush.Color:=clWhite;
  Canvas.Pen.Style  :=psSolid;
  Canvas.Brush.Style:=bsSolid;
  Canvas.Pen.Width  :=1;
  Canvas.Frame(
    W2S(Points[low(Points)]) .x,
    W2S(Points[low(Points)]) .y,
    W2S(Points[high(Points)]).x,
    W2S(Points[high(Points)]).y);
end;

procedure TLine.Draw(Canvas: TCanvas);
begin
  Canvas.Pen.Style:=FPenStyle;
  Canvas.Pen.Width:=FWidth;
  Canvas.Pen.Color:=FPenColor;
  Canvas.Line(
    W2S(Points[low(Points)]) .x,
    W2S(Points[low(Points)]) .y,
    W2S(Points[high(Points)]).x,
    W2S(Points[high(Points)]).y);
end;

procedure TRectangle.Draw(Canvas: TCanvas);
begin
  Canvas.Pen.Style  :=FPenStyle;
  Canvas.Pen.Width  :=FWidth;
  Canvas.Pen.Color  :=FPenColor;
  Canvas.Brush.Style:=FBrushStyle;
  Canvas.Brush.Color:=FBrushColor;
  Canvas.Rectangle(
    W2S(Points[low(Points)]) .x,
    W2S(Points[low(Points)]) .y,
    W2S(Points[high(Points)]).x,
    W2S(Points[high(Points)]).y);
end;

procedure TRoundRect.Draw(Canvas: TCanvas);
begin
  Canvas.Pen.Style  :=FPenStyle;
  Canvas.Pen.Width  :=FWidth;
  Canvas.Pen.Color  :=FPenColor;
  Canvas.Brush.Style:=FBrushStyle;
  Canvas.Brush.Color:=FBrushColor;
  Canvas.RoundRect(
    W2S(Points[low(Points)]) .x,
    W2S(Points[low(Points)]) .y,
    W2S(Points[high(Points)]).x,
    W2S(Points[high(Points)]).y, FAngle, FAngle);
end;

procedure TEllipse.Draw(Canvas: TCanvas);
begin
  Canvas.Pen.Style  :=FPenStyle;
  Canvas.Pen.Width  := FWidth;
  Canvas.Pen.Color  :=FPenColor;
  Canvas.Brush.Style:=FBrushStyle;
  Canvas.Brush.Color:=FBrushColor;
  Canvas.Ellipse(
    W2S(Points[low(Points)]) .x,
    W2S(Points[low(Points)]) .y,
    W2S(Points[high(Points)]).x,
    W2S(Points[high(Points)]).y);
end;

procedure TTriangle.Draw(Canvas: TCanvas);
begin
  Canvas.Pen.Style  :=FPenStyle;
  Canvas.Pen.Width  :=FWidth;
  Canvas.Pen.Color  :=FPenColor;
  Canvas.Brush.Style:=FBrushStyle;
  Canvas.Brush.Color:=FBrushColor;
  Canvas.Polygon([
    W2S(Points[low(Points)]),
    W2S(Points[high(Points)]),
   (Point(W2S(Points[high(Points)]).x,
    W2S(Points[low(Points)]).y))]);
end;

end.


