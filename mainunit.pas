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
    procedure PaintBoxResize(Sender: TObject);
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
  PropertyPanel.Parent :=Mainform;
  PropertyPanel.Name   :='PropertyPanel';
  PropertyPanel.Caption:='';
  PropertyPanel.Width  :=300;
  PropertyPanel.Height :=50;
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
    b.Caption   :=ToolRegistry[i].name;
    b.Tag       :=Integer(i);
    b.Left      :=5;
    b.Top       :=20+i*32;
    b.Width     :=100;
    b.Height    :=32;
    b.GroupIndex:=1;
    b.Down      :=true;
    b.OnClick   :=@ToolBtnClick;
  end;
  CTool           := ToolRegistry[0];
  VerScrollBar.Max:=round(W2S(MaxFloatPoint).y);
  VerScrollBar.Min:=round(W2S(MinFloatPoint).y);
  HorScrollBar.Max:=round(W2S(MaxFloatPoint).x);
  HorScrollBar.Min:=round(W2S(MinFloatPoint).x);
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
  CTool.FigureCreate(CTool.FigureClass,Point(X,Y),PenColor,BrushColor);
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
begin
  PaintBox.Canvas.Brush.Color:=clWhite;
  PaintBox.canvas.FillRect(PaintBox.Canvas.ClipRect);
  for f in Figures do
    f.Draw(PaintBox.canvas);

  if round(MinFloatPoint.X*scale/100)<HorScrollBar.Min then
    HorScrollBar.Min:=round(MinFloatPoint.X*scale/100);
  if HorScrollBar.Max<round(MaxFloatPoint.X*scale/100) then
    HorScrollBar.Max:=round(MaxFloatPoint.X*scale/100);
  if round(MinFloatPoint.Y*scale/100)<VerScrollBar.Min then
    VerScrollBar.Min:=round(MinFloatPoint.Y*scale/100);
  if VerScrollBar.Max<round(MaxFloatPoint.Y*scale/100) then
    VerScrollBar.Max:=round(MaxFloatPoint.Y*scale/100);

  if Offset.x+PaintBox.Width>HorScrollBar.Max then
    HorScrollBar.Max:=Offset.x+PaintBox.Width;
  if Offset.x<HorScrollBar.Min then
    HorScrollBar.Min:=Offset.x;
  if Offset.y+PaintBox.Height>VerScrollBar.Max then
    VerScrollBar.Max:=Offset.y+PaintBox.Height;
  if Offset.y<VerScrollBar.Min then
    VerScrollBar.Min:=Offset.y;

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

procedure TMainForm.PaintBoxResize(Sender: TObject);
begin
  if (OldPaintBoxSize <> Point(0,0)) then
  begin
    Offset.x:=round(Offset.x-(PaintBox.Width-OldPaintBoxSize.x) div 2);
    Offset.y:=round(Offset.y-(PaintBox.Height-OldPaintBoxSize.y) div 2);
  end;
  transformunit.PaintBoxSize.x:=PaintBox.Width;
  transformunit.PaintBoxSize.y:=PaintBox.Height;
  OldPaintBoxSize.x:=PaintBox.Width;
  OldPaintBoxSize.y:=PaintBox.Height;
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

