unit transformunit;

{$mode objfpc}{$H+}

interface

uses
  Classes,GraphMath, SysUtils;

procedure ChangeScaling(AWidth, AHeight: integer; OldScale: double);
function  W2S(APoint: TFloatPoint): TPoint;
function  S2W(APoint: TPoint): TFloatPoint;
procedure SetOffset(APoint: TFloatPoint);
procedure SetMaxMinFloatPoints(APoint: TFloatPoint);
procedure RectScale(AHeight,AWidth:Integer;AMin,AMax:TFloatPoint);
procedure ToPointZoom(APoint: TFloatPoint);
procedure ScaleAll(AHeight,AWidth:Integer;AMin,AMax:TFloatPoint);

var
  PaintBoxSize: Tpoint;
  Scale: double;
  Offset: TPoint;
  MaxFloatPoint, MinFloatPoint: TFloatPoint;

implementation

function W2S(APoint: TFloatPoint): TPoint;
begin
  W2S.x:=round(APoint.x*Scale/100)-Offset.x;
  W2S.y:=round(APoint.y*Scale/100)-Offset.y;
end;

function S2W(APoint: TPoint): TFloatPoint;
begin
  S2W.x:=(APoint.x+Offset.x)/Scale*100;
  S2W.y:=(APoint.y+Offset.y)/Scale*100;
end;

procedure RectScale(AHeight,AWidth:Integer;AMin,AMax:TFloatPoint);
var oldScale: Double;
begin
  oldScale:=Scale;
  Scale:=Scale+50;
  if (Scale>1000) or (Scale<10)then
    begin
      Scale:=oldScale;
      exit;
    end;
  Offset.x:=round(AMin.X*Scale/100);
  Offset.y:=round(AMin.Y*Scale/100);
end;

procedure ScaleAll(AHeight,AWidth:Integer;AMin,AMax:TFloatPoint);
var
  OldScale: Double;
begin
  OldScale:=Scale;
  if (Awidth/(AMax.X-AMin.X))>(AHeight/(AMax.Y-AMin.Y)) then
    Scale:=AHeight/(AMax.Y-AMin.Y)*100
  else
    Scale:=AWidth/(AMax.X-AMin.X)*100;
  if (Scale > 1000) or (Scale<10) then
  begin
    Scale:=oldScale;
    exit;
  end;
  Offset.x:=round(AMin.X*Scale/100);
  Offset.y:=round(AMin.Y*Scale/100);
end;

procedure SetOffset (APoint: TFloatPoint);
begin
  Offset:=(APoint);
end;

procedure ChangeScaling(AWidth, AHeight:integer; OldScale:double);
begin
    Offset.x:=Offset.x+round(AWidth*(Scale-OldScale)/200);
    Offset.y:=Offset.y+round(AHeight*(Scale-OldScale)/200);
end;

procedure ToPointZoom(APoint: TFloatPoint);
begin
  begin
  Offset.x:=round(APoint.X*Scale/100-Apoint.x);
  Offset.y:=round(APoint.Y*Scale/100-APoint.y);
  end;
end;

procedure SetMaxMinFloatPoints (APoint: TFloatPoint);
begin
  if (APoint.x>MaxFloatPoint.x) then
     MaxFloatPoint.x:=APoint.x;
  if (APoint.y>MaxFloatPoint.y) then
     MaxFloatPoint.y:=APoint.y;
  if (APoint.x<MinFloatPoint.x) then
     MinFloatPoint.x:=APoint.x;
  if (APoint.y<MinFloatPoint.y) then
     MinFloatPoint.y:=APoint.y;
end;


initialization

  MinFloatPoint:=FloatPoint(0,0);
  MaxFloatPoint:=(PaintBoxSize);
  Offset:=FloatPoint(0,0);

end.


