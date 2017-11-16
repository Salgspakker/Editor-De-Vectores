unit aboutunit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls;

type

  { TAboutForm }

  TAboutForm = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    LabelMain: TLabel;
    Panel1: TPanel;
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  AboutForm: TAboutForm;

implementation

{$R *.lfm}

{ TAboutForm }

procedure TAboutForm.FormCreate(Sender: TObject);
begin
  LabelMain.font.size:=10;
  LabelMain.font.color:=ClPurple;
  LabelMain.caption:=
  'Позволяет:'                          +#10#13+
  '- Рисовать прямые и ломанные линии;' +#10#13+
  '- Рисовать окружности;'              +#10#13+
  '- Рисовать прямоугольники;'          +#10#13+
  '- Рисовать кистью;'                  +#10#13+
  '- Рисовать треугольники.';
end;

end.

