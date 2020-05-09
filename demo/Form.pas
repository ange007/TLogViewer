unit Form;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Objects,
  FMX.Layouts, LogViewer, FMX.Controls.Presentation, FMX.StdCtrls, Fmx.Memo,
  FMX.Menus;

type
  TForm1 = class(TForm)
    btn_add_many_lines: TButton;
    btn_add_line: TButton;
    btn_clear: TButton;
    btn_copy: TButton;
    btn_add_random_lines: TButton;
    lgm1: TLogViewer;
    lyt2: TLayout;
    procedure btn_add_lineClick(Sender: TObject);
    procedure btn_clearClick(Sender: TObject);
    procedure btn_copyClick(Sender: TObject);
    procedure btn_add_random_linesClick(Sender: TObject);
    procedure btn_add_many_linesClick(Sender: TObject);
  private
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.btn_add_many_linesClick(Sender: TObject);
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

procedure TForm1.btn_add_lineClick(Sender: TObject);
begin
  lgm1.Add('Check on GitHub', 'https://github.com/ange007/TLogViewer');
end;

procedure TForm1.btn_clearClick(Sender: TObject);
begin
  lgm1.Clear;
end;

procedure TForm1.btn_copyClick(Sender: TObject);
begin
  lgm1.CopyToClipboard;
end;

procedure TForm1.btn_add_random_linesClick(Sender: TObject);
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

      {Long line}
      if Random(5) = 3 then lgm1.Add('Long Long Long Long Long Long Long Long Long Long Long Long Long Long Long '
                                      + 'Long Long Long Long Long Long Long Long Long Long Long Long Long Long Long '
                                      + 'Long Long Long Long Long Long Long Long Long Long Long Long Long Long Long', '', TAlphaColor(Color), Styles)
      {Line wrap}
      else if Random(5) = 2 then lgm1.Add('1. Wrap Line'
                                          + #13#10 + '2. Wrap Line', '', TAlphaColor(Color), Styles)
      {Standart Line}
      else lgm1.Add('Item ' + i.ToString, '', TAlphaColor(Color), Styles);
    end;
  finally
    lgm1.EndUpdate;
  end;
end;

end.
