unit mainunit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Menus, StdCtrls, GraphMath, Buttons, Spin, aboutunit,
  figuresunit, transformunit, toolsunit;

type

  { TMainForm }

  TMainForm = class(TForm)
  Panel1: TPanel;
  ShowAll: TBitBtn;
    MenuItemAbout: TMenuItem;
    MenuItemRef: TMenuItem;
    MenuItemExit: TMenuItem;
    MenuItemFile: TMenuItem;
    MainFormMainMenu: TMainMenu;
    PaintBox: TPaintBox;
    LSidePanel: TPanel;
    HorScrollBar: TScrollBar;
    ScaleEdit: TSpinEdit;
    StaticText3: TStaticText;
    VerScrollBar: TScrollBar;
    procedure ShowAllClick(Sender: TObject);
    procedure PaintBoxClick(Sender: TObject);
    procedure ToolBtnClick(ASender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure MenuItemAboutClick(Sender: TObject);
    procedure MenuItemExitClick(Sender: TObject);
    procedure PaintBoxMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBoxMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure PaintBoxMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBoxPaint(Sender: TObject);
    procedure ScaleEditChange(Sender: TObject);
      procedure DeletePropertyPanel;
    procedure CreatePropertyPanel;
    procedure ScrollBarScroll(Sender: TObject;
  ScrollCode: TScrollCode; var ScrollPos: Integer);
  private
    { private declarations }

  DWN, ScrollBool, RBtn: Boolean;

  public
    { public declarations }
  end;

var
  MainForm: TMainForm;
  OldPaintBoxSize: TPoint;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.MenuItemAboutClick(Sender: TObject);
begin
  AboutForm.show;
end;

procedure TMainform.CreatePropertyPanel;
var
  PropertyPanel:TPanel;
begin
  PropertyPanel        :=TPanel.Create(Mainform);
  PropertyPanel.Parent :=Panel1;
  PropertyPanel.Name   :='PropertyPanel';
  PropertyPanel.Caption:='';
  PropertyPanel.Width  :=300;
  PropertyPanel.Height :=79;
  PropertyPanel.align  :=alBottom;
  CTool.PropertiesCreate(PropertyPanel);
end;

procedure TMainform.DeletePropertyPanel;
begin
  if FindComponent('PropertyPanel')<>nil then
    begin
      FindComponent('PropertyPanel').Free;
    end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
var b: TSpeedButton;   i: integer;
begin
  scale                       :=100;
  InvalidateHandler           :=@Invalidate;
  CreatePanelHandler          :=@CreatePropertyPanel;
  DeletePanelHandler          :=@DeletePropertyPanel;
  transformunit.PaintBoxSize.x:=PaintBox.Width;
  transformunit.PaintBoxSize.y:=PaintBox.Height;
  for i:=high(ToolRegistry) downto 0 do
  begin
    b           :=TSpeedButton.Create(MainForm);
    b.Parent    :=MainForm.LSidePanel;
    b.name      :='ToolSpdBtn'+IntToStr(i+1);
    b.Glyph     :=ToolRegistry[i].Bitmap;
    b.Tag       :=Integer(i);
    b.Left      :=5+ (i mod 2)*40;
    b.Top       :=20+(i div 2)*40;
    b.Width     :=40;
    b.Height    :=40;
    b.GroupIndex:=1;
    b.Down      :=true;
    b.OnClick   :=@ToolBtnClick;
    b.color     :=clWhite;
  end;
  CTool           :=ToolRegistry[0];
  ScaleEdit.Value :=round(Scale);
  CreatePropertyPanel;
  Invalidate;
end;

procedure TMainForm.MenuItemExitClick(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TMainForm.PaintBoxMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  DWN:=True;
  CTool.FigureCreate(Point(X,Y));
    if (ssRight in Shift) then
      RBtn:=true;
  PaintBox.Invalidate;
end;

procedure TMainForm.PaintBoxMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if DWN then
    if ssLeft in Shift then
    begin
      CTool.AddPoint(Point(X,Y));
      PaintBox.Invalidate;
    end;
end;

procedure TMainForm.PaintBoxMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if (DWN) then
  begin
    DWN:=false;
    CTool.StopDraw(X,Y,PaintBox.Height, PaintBox.Width, RBtn);
    ScaleEdit.Value:=transformunit.Scale;
    RBtn:=false;
  end;
  Invalidate;
end;

procedure TMainForm.PaintBoxPaint(Sender: TObject);
var f: TFigure;
  MaxP, MinP: TFloatPoint;
begin
  PaintBox.Canvas.Brush.Color:=clWhite;
  PaintBox.canvas.FillRect(PaintBox.Canvas.ClipRect);
  for f in Figures do
    f.Draw(PaintBox.canvas);
 if MaxFloatPoint.X>PaintBox.Width then
    MaxP.X:=MaxFloatPoint.X
  else
    MaxP.X:=PaintBox.Width;
  if MaxFloatPoint.Y>PaintBox.Height then
    MaxP.Y:=MaxFloatPoint.Y
  else
    MaxP.Y:=PaintBox.Height;
  if MinFloatPoint.X<MinP.X then MinP.X:=MinFloatPoint.X;
  if MinFloatPoint.Y<MinP.Y then MinP.Y:=MinFloatPoint.Y;
  if Offset.x + PaintBox.Width>MaxP.X then
    MaxP.X:=Offset.x+PaintBox.Width;
  if Offset.x<MinP.X then
    MinP.X:=Offset.x;
  if Offset.y + PaintBox.Height>MaxP.Y then
    MaxP.Y:=Offset.y+PaintBox.Height;
  if Offset.y < MinP.Y then
    MinP.Y:=Offset.y;
  HorScrollBar.Min:=round(MinP.X*Scale/100);
  HorScrollBar.Max:=round(MaxP.X*Scale/100);
  VerScrollBar.Min:=round(MinP.Y*Scale/100);
  VerScrollBar.Max:=round(MaxP.Y*Scale/100);
  ScrollBool:=true;
  HorScrollBar.Position:=Offset.x;
  ScrollBool:=true;
  VerScrollBar.Position:=Offset.y;
end;

procedure TMainForm.ScaleEditChange(Sender: TObject);
var oldscale: double;
begin
  oldscale:=scale;
  Scale:=(ScalEedit.value);
  ChangeScaling(PaintBox.Width,PaintBox.Height, oldscale);
  Invalidate;
end;

procedure TMainForm.ToolBtnClick(ASender: TObject);
begin
  CTool:=ToolRegistry[(ASender as TSpeedButton).Tag];
  DeletePropertyPanel;
  CreatePropertyPanel;
end;

procedure TMainForm.PaintBoxClick(Sender: TObject);
begin

end;

procedure TMainForm.ShowAllClick(Sender: TObject);
begin
  ScaleAll(PaintBox.Height,PaintBox.Width,MinFloatPoint,MaxFloatPoint);
  ScaleEdit.Value:=round(Scale);
  Invalidate;
end;


procedure TMainForm.ScrollBarScroll(Sender: TObject;
  ScrollCode: TScrollCode; var ScrollPos: Integer);
begin
  if not ScrollBool then
  begin
    SetOffset(Point(HorScrollBar.Position,VerScrollBar.Position));
    Invalidate;
  end;
  ScrollBool:=false;
end;

initialization


end.

