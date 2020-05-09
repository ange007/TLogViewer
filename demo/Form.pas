unit Form;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Objects,
  FMX.Layouts, LogViewer, FMX.Controls.Presentation, FMX.StdCtrls, Fmx.Memo,
  FMX.Menus;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    lgm1: TLogViewer;
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.Button1Click(Sender: TObject);
var
  i: Integer;
begin
  lgm1.BeginUpdate;
  try
    for i := 0 to 100000 do lgm1.Add('Item ' + i.ToString);
  finally
    lgm1.EndUpdate;
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  lgm1.Add('Check on GitHub', 'https://github.com');
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  lgm1.Clear;
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  lgm1.CopyToClipboard;
end;

procedure TForm1.Button5Click(Sender: TObject);
var
  i: Integer;
  Color: TAlphaColorRec;
  Styles: TFontStyles;
begin
  lgm1.BeginUpdate;
  try
    for i := 0 to 5000 do
    begin
      Color.R := Random(256);
      Color.G := Random(256);
      Color.B := Random(256);
      Color.A := 255;

      case Random(5) of
        1: Styles := [TFontStyle.fsUnderline];
        2: Styles := [TFontStyle.fsBold];
        3: Styles := [TFontStyle.fsUnderline];
        4: Styles := [TFontStyle.fsItalic]
        else Styles := [];
      end;

      lgm1.Add('Item ' + i.ToString, '', TAlphaColor(Color), Styles);
    end;
  finally
    lgm1.EndUpdate;
  end;
end;

end.
