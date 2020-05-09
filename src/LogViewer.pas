unit LogViewer;

{
  FMX Component

  Original idea:
    http://fire-monkey.ru/topic/5426-tmemo-%D0%B4%D0%BB%D1%8F-%D0%B2%D1%8B%D0%B2%D0%BE%D0%B4%D0%B0-%D0%BB%D0%BE%D0%B3%D0%B0-%D1%82%D0%BE%D1%80%D0%BC%D0%BE%D0%B7%D0%B8%D1%82
    Nick Peterson (http://fire-monkey.ru/profile/5417-nick-peterson/)
    slav_z (http://fire-monkey.ru/profile/4697-slav_z/)

  Component Author: Vladimir B. (https://github.com/ange007/TLogViewer)
  Version: 1.0.5
}

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.UIConsts, System.Classes,
  System.Math, System.RTTI, System.Threading, FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms,
  FMX.Objects, FMX.Layouts, FMX.platform, FMX.Text, FMX.TextLayout,
  FMX.Menus,

  {$IF DEFINED(LINUX) or DEFINED(MACOS)}
  POSIX.Stdlib
  {$ELSE}
  Winapi.Windows, Winapi.ShellApi
  {$ENDIF};

type
  PLogItem = ^RLogItem;
  RLogItem = record
    Msg: string;
    URL: string;
    Color: TAlphaColor;
    Style: TFontStyles;
  end;

  TAccessControl = class(TControl);

  TLogViewer = class(TStyledControl)
  private
    FScrollBox: TFramedVertScrollBox;
    FTextLayout: TText;
    FActionsMenu: TPopupMenu;
    FMouseIsDown: Boolean;
    {}
    procedure DoChange;
    procedure DoTextChange;
    procedure RepaintItems(const OldItemIndex, OldToItemIndex: Integer);
    {}
    function GetCount: Integer;
    {}
    procedure SetItemHeight(Value: Single);
    procedure SetItemIndex(Value: Integer);
    procedure SetToItemIndex(Value: Integer);
    {}
    function GetItemText(AIndex: Integer): string;
    function GetItemIndexText: string;
    function GetSelectedText: string;
    function GetIsTracking: Boolean;
    {}
    procedure ExtendSelection(Y: Single);
    procedure DoSelectTimer(Sender: TObject);
    {}
    procedure CreateDefaultMenu;
    {}
    procedure OnTextKeyDown(Sender: TObject; var Key: Word; var KeyChar: WideChar; Shift: TShiftState);
    procedure OnTextKeyUp(Sender: TObject; var Key: Word; var KeyChar: WideChar; Shift: TShiftState);
    procedure OnTextPainting(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
    procedure OnTextMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure OnTextMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
    procedure OnTextMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure OnTextMouseLeave(Sender: TObject);
    procedure OnTextMouseDblClick(Sender: TObject);
    {}
    procedure OnTextActionCopyClick(Sender: TObject);
    procedure OnTextActionSelectAllClick(Sender: TObject);
  protected
    FItems: array of PLogItem;
    FItemHeight: Single;
    FItemIndex: Integer;
    FToItemIndex: Integer;
    FLineLength: Integer;
    FItemIndexColor: TAlphaColor;
    FReleasedItems: Boolean;
    FAniCalculationsStarting: Boolean;
    FSelectTimer: TTimer;
    FMovingDirection: Integer;
    FMaxCount: Integer;
    FReleaseCount: Integer;
    FAutoScroll: Boolean;
    FOnChange: TNotifyEvent;
    {}
    function GetDefaultStyleLookupName: string; override;
    function GetStyleObject: TFmxObject; override;
    procedure ApplyStyle; override;
    procedure FreeStyle; override;
    {Menu}
    function ShowContextMenu(const ScreenPosition: TPointF): Boolean; override;
    {}
    procedure Paint; override;
    procedure DoEndUpdate; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    {}
    procedure Clear;
    procedure Add(const AText: string; AURL: string = ''; const AColor: TAlphaColor = claNull; const AStyle: TFontStyles = []);
    procedure AddStrings(const AStrings: TStrings);
    function GetText: string;
    {}
    procedure ScrollTo(ToItemIndex: Integer);
    procedure CopyToClipboard;
    {Custom Properties}
    property ItemText[Index: Integer]: string read GetItemText;
    property ItemIndexText: string read GetItemIndexText;
    property SelectedText: string read GetSelectedText;
    property Count: Integer read GetCount;
    property IsTracking: Boolean read GetIsTracking;
  published
    property Align;
    property ClipChildren default False;
    property ClipParent default False;
    property Cursor default crDefault;
    property DragMode default TDragMode.dmManual;
    property Enabled;
    property Height;
    property HelpContext;
    property HelpKeyword;
    property HelpType;
    property Padding;
    property Opacity;
    property Margins;
    property PopupMenu;
    property Position;
    property RotationAngle;
    property RotationCenter;
    property Scale;
    property Size;
    property StyleLookup;
    property TabOrder;
    property Visible stored VisibleStored;
    property Width;
    {Custom Properties}
    property ItemIndex: Integer read FItemIndex write SetItemIndex;
    property ToItemIndex: Integer read FToItemIndex write SetToItemIndex;
    property ItemHeight: Single read FItemHeight write SetItemHeight;
    property ItemIndexColor: TAlphaColor read FItemIndexColor write FItemIndexColor;
    property LineLength: Integer read FLineLength write FLineLength;
    {}
    property AutoScroll: Boolean read FAutoScroll write FAutoScroll default True;
    property ReleasedItems: Boolean read FReleasedItems write FReleasedItems;
    property MaxCount: Integer read FMaxCount write FMaxCount;
    property ReleaseCount: Integer read FReleaseCount write FReleaseCount;
    {}
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

procedure Register;

implementation

uses
  FMX.Styles;

{$R TLogViewer.res}
{$R TLogViewer.dres}

const
  CRLF = #13#10;

procedure Register;
begin
  RegisterComponents('Other', [TLogViewer]);
end;

function IsCaptured(FmxObject: TFmxObject): Boolean;
begin
  Result := Assigned(FmxObject)
            and Assigned(FmxObject.Root)
            and Assigned(FmxObject.Root.Captured)
            and (FmxObject.Root.Captured.GetObject = FmxObject);
end;

procedure AddToList(var L: string; S, V: string);
begin
  if (L <> '') and (V <> '') then L := L + S + V
  else if V <> '' then L := V;
end;

function ShellComand(const arg1, arg2, arg3: string): Integer;
begin
  {$IF DEFINED(MACOS)}
  Result := _system(PAnsiChar(AnsiString(arg1 + ' ' + arg2 + ' ' + arg3)));
  {$ELSEIF Defined(IOS)}
  Result := SharedApplication.OpenURL(StrToNSUrl(URL));
  {$ELSEIF DEFINED(LINUX)}
  Result := _system(MarshaledAString(UTF8String(arg1 + ' ' + arg2 + ' ' + arg3)));
  {$ELSEIF Defined(ANDROID)}
  Intent := TJIntent.Create;
  Intent.setAction(TJIntent.JavaClass.ACTION_VIEW);
  Intent.setData(StrToJURI(URL));
  Result := TAndroidHelper.Activity.startActivity(Intent);
  {$ELSE}
  Result := ShellExecute(0, PChar(arg1), PChar(arg2), PChar(arg3), nil, SW_SHOWNORMAL);
  {$ENDIF}
end;

{

}

constructor TLogViewer.Create(AOwner: TComponent);
begin
  inherited;

  {Control Options}
  Width := 320;
  Height := 240;
  Size.PlatformDefault := False;

  {Block Insert Controls}
  SetAcceptsControls(False);
  //CanFocus := True;
  //AutoCapture := True;
  //Stored := True;
  //Locked := True;

  {}
  FMouseIsDown := False;

  {Default Options}
  FAutoScroll := True;
  FReleasedItems := True;
  FItemIndex := -1;
  FToItemIndex := -1;
  FItemHeight := 20;
  FItemIndexColor := claSilver;
  FMaxCount := 20000;
  FReleaseCount := 15000;
  FLineLength := 150;

  {First ItemHeight check}
  TThread.CreateAnonymousThread(procedure
  begin
    Sleep(10);
    FItemHeight := Round(Canvas.TextHeight('A') + 4);
  end)
  .Start;

  {Selection timer}
  FSelectTimer := TTimer.Create(Self);
  with FSelectTimer do
  begin
    Interval := 100;
    Enabled := False;
    OnTimer := DoSelectTimer;
  end;
end;

destructor TLogViewer.Destroy;
begin
  FreeAndNil(FSelectTimer);
  if Assigned(FActionsMenu) then FreeAndNil(FActionsMenu);

  {}
  SetLength(FItems, 0);

  inherited;
end;

{
  Style
}

function TLogViewer.GetStyleObject: TFmxObject;
begin
  if StyleLookup = '' then Result := TFmxObject(TStyleStreaming.LoadFromResource(HInstance, GetDefaultStyleLookupName, RT_RCDATA))
  else Result := inherited GetStyleObject;
end;

function TLogViewer.GetDefaultStyleLookupName: string;
begin
  Result := 'LogViewerStyle';
end;

procedure TLogViewer.ApplyStyle;
begin
  inherited ApplyStyle;

  {}
  if FindStyleResource<TFramedVertScrollBox>('LogViewerScrollBox', FScrollBox) then
  begin
    FScrollBox.AniCalculations.Animation := False;
    FScrollBox.CanFocus := False;
  end;

  {}
  if FindStyleResource<TText>('LogViewerText', FTextLayout) then
  begin
    FTextLayout.Parent := FScrollBox;
    FTextLayout.CanFocus := True;

    with FTextLayout do
    begin
      OnKeyDown := OnTextKeyDown;
      OnKeyUp := OnTextKeyUp;
      OnMouseDown := OnTextMouseDown;
      OnMouseMove := OnTextMouseMove;
      OnMouseUp := OnTextMouseUp;
      OnMouseLeave := OnTextMouseLeave;
      OnDblClick := OnTextMouseDblClick;
      OnPainting := OnTextPainting;
    end;
  end;
end;

procedure TLogViewer.FreeStyle;
begin
  FTextLayout := nil;
  FScrollBox := nil;

  inherited;
end;

procedure TLogViewer.Paint;
begin
  inherited;
end;

procedure TLogViewer.DoEndUpdate;
begin
  inherited;

  DoTextChange;
end;

{

}

function TLogViewer.ShowContextMenu(const ScreenPosition: TPointF): Boolean;
begin
  Result := inherited;

  if not (Result) then
  begin
    if not (Assigned(FActionsMenu)) then CreateDefaultMenu;

    FActionsMenu.PopupComponent := Self;
    FActionsMenu.Popup(Round(ScreenPosition.X), Round(ScreenPosition.Y));
  end;
end;

procedure TLogViewer.CreateDefaultMenu;
var
  item: TMenuItem;
begin
  FActionsMenu := TPopupMenu.Create(Self);

  with FActionsMenu do
  begin
    item := TMenuItem.Create(FActionsMenu);
    with item do
    begin
      Parent := FActionsMenu;
      Text := 'Select All';
      OnClick := OnTextActionSelectAllClick;
    end;

    AddObject(item);

    item := TMenuItem.Create(FActionsMenu);
    with item do
    begin
      Parent := FActionsMenu;
      Text := '-';
    end;

    AddObject(item);

    item := TMenuItem.Create(FActionsMenu);
    with item do
    begin
      Parent := FActionsMenu;
      Text := 'Copy';
      OnClick := OnTextActionCopyClick;
    end;

    AddObject(item);
  end;
end;

procedure TLogViewer.OnTextActionCopyClick(Sender: TObject);
begin
  CopyToClipboard;
end;

procedure TLogViewer.OnTextActionSelectAllClick(Sender: TObject);
begin
  ItemIndex := 0;
  ToItemIndex := Count - 1;
end;

{

}

procedure TLogViewer.DoTextChange;
begin
  if not (Assigned(FTextLayout)) then Exit;
  
  FTextLayout.Height := (Count + 1) * FItemHeight;
  if FAutoScroll then ScrollTo(Count);
end;


procedure TLogViewer.Clear;
begin
  ItemIndex := -1;
  SetLength(FItems, 0);

  DoTextChange;
end;

function TLogViewer.GetText: string;
var
  S: PLogItem;
begin
  Result := '';
  for S in FItems do AddToList(Result, CRLF, S.Msg);
end;

procedure TLogViewer.RepaintItems(const oldItemIndex, oldToItemIndex: Integer);
var
  ARect: TRectF;
begin
  ARect := FTextLayout.AbsoluteRect;
  ARect.Bottom := ARect.Top + MaxIntValue([oldItemIndex, oldToItemIndex, ItemIndex, ToItemIndex]) * (FItemHeight * 2);
  ARect.Top := ARect.Top + MinIntValue([oldItemIndex, oldToItemIndex, ItemIndex, ToItemIndex]) * FItemHeight;

  TAccessControl(FTextLayout).RepaintRect(ARect);
end;

procedure TLogViewer.SetItemIndex(Value: Integer);
var
  oldItemIndex, oldToItemIndex: Integer;
begin
  if (FItemIndex <> Value) or (ToItemIndex <> Value) then
  begin
    oldItemIndex := FItemIndex;
    oldToItemIndex := FToItemIndex;

    FItemIndex := Value;
    FToItemIndex := Value;

    RepaintItems(oldItemIndex, oldToItemIndex);
    DoChange;
  end;
end;

procedure TLogViewer.SetToItemIndex(Value: Integer);
var
  oldToItemIndex: Integer;
begin
  if FToItemIndex = Value then Exit;

  oldToItemIndex := FToItemIndex;
  FToItemIndex := Value;

  RepaintItems(ItemIndex, oldToItemIndex);
  DoChange;
end;

function TLogViewer.GetCount: Integer;
begin
  Result := Length(FItems);
end;

procedure TLogViewer.SetItemHeight(Value: Single);
begin
  FItemHeight := Value;
  DoTextChange;
end;

procedure TLogViewer.DoChange;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

function TLogViewer.GetItemText(AIndex: Integer): string;
begin
  Result := FItems[AIndex].Msg;
end;

function TLogViewer.GetItemIndexText: string;
begin
  if ItemIndex <> -1 then Result := ItemText[ItemIndex]
  else Result := '';
end;

function TLogViewer.GetSelectedText: string;
var
  i: Integer;
begin
  Result := '';
  for i := Max(0, Min(ItemIndex, ToItemIndex)) to Max(ItemIndex, ToItemIndex) do AddToList(Result, CRLF, ItemText[i]);
end;

procedure TLogViewer.Add(const AText: string; AURL: string = ''; const AColor: TAlphaColor = claNull; const AStyle: TFontStyles = []);
var
  msg: string;
  logItem: PLogItem;
begin
  if FReleasedItems then
  begin
    if Count > FMaxCount then Delete(FItems, 0, FReleaseCount);
  end;

  for msg in AText.Split([CRLF]) do
  begin
    New(logItem);
    logItem.Msg := msg;
    logItem.URL := AURL;
    logItem.Color := AColor;
    logItem.Style := AStyle;

    FItems := FItems + [logItem];
  end;

  if not (IsUpdating) then DoTextChange;
end;

procedure TLogViewer.AddStrings(const AStrings: TStrings);
var
  i: Integer;
begin
  BeginUpdate;
  try
    for i := 0 to AStrings.Count - 1 do Add(AStrings[i], '');
  finally
    EndUpdate;
  end;
end;

function TLogViewer.GetIsTracking: Boolean;
begin
  Result := IsCaptured(FTextLayout);
end;

procedure TLogViewer.ScrollTo(ToItemIndex: Integer);
begin
  if not (IsTracking) then FScrollBox.ViewportPosition := PointF(0, ToItemIndex * FItemHeight);
end;

procedure TLogViewer.CopyToClipboard;
var
  ClipService: IFMXClipboardService;
begin
  if (ItemIndexText <> '')
    and TPlatformServices.Current.SupportsPlatformService(IFMXClipboardService, ClipService) then ClipService.SetClipboard(SelectedText);
end;

procedure TLogViewer.ExtendSelection(Y: Single);
var
  newItemIndex: Integer;
begin
  Y := Max(Y, FScrollBox.ViewportPosition.Y);
  Y := Min(Y, FScrollBox.ViewportPosition.Y + FScrollBox.Height);

  newItemIndex := Trunc(Y / FItemHeight);
  if InRange(newItemIndex, 0, Count - 1) then ToItemIndex := newItemIndex;
end;

procedure TLogViewer.DoSelectTimer(Sender: TObject);
begin
  FScrollBox.ScrollBy(0, FMovingDirection * FItemHeight);

  if FMovingDirection > 0 then ExtendSelection(0)
  else ExtendSelection(FScrollBox.ContentBounds.Bottom);
end;

{
  Keyboard Events
}

procedure TLogViewer.OnTextKeyDown(Sender: TObject; var Key: Word; var KeyChar: WideChar; Shift: TShiftState);
var
  newItemIndex: Integer;
begin
  if Key in [vkUp, vkDown] then
  begin
    newItemIndex := ItemIndex;

    if Key = vkUp then
    begin
      newItemIndex := ItemIndex - 1;
      FMovingDirection := 1;
    end
    else if Key = vkDown then
    begin
      newItemIndex := ItemIndex + 1;
      FMovingDirection := -1;
    end
    else FMovingDirection := 0;

    if InRange(newItemIndex, 0, Count - 1) then
    begin
      ItemIndex := newItemIndex;
      FScrollBox.ScrollBy(0, FMovingDirection * FItemHeight);
    end;

    // if (ssShift in Shift) then FSelectTimer.Enabled := True;
  end;
end;

procedure TLogViewer.OnTextKeyUp(Sender: TObject; var Key: Word; var KeyChar: WideChar; Shift: TShiftState);
begin
  FSelectTimer.Enabled := False;
end;

{
  Mouse Events
}

procedure TLogViewer.OnTextMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
var
  newItemIndex: Integer;
begin
  if Button = TMouseButton.mbLeft then
  begin
    FMouseIsDown := True;
    newItemIndex := Trunc(Y / FItemHeight);

    if InRange(newItemIndex, 0, Count - 1) then
    begin
       if (ssShift in Shift) then ToItemIndex := newItemIndex
       else ItemIndex := newItemIndex;
    end;
  end;
end;

procedure TLogViewer.OnTextMouseLeave(Sender: TObject);
begin
  FMouseIsDown := False;
  FSelectTimer.Enabled := False;
end;

procedure TLogViewer.OnTextMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
begin
  if not (FMouseIsDown) then Exit;

  {Range Select}
  if (ssLeft in Shift) then ExtendSelection(Y);

  {Move Direction}
  if (Y < FScrollBox.ViewportPosition.Y) then FMovingDirection := 1
  else if (Y > FScrollBox.ViewportPosition.Y + FScrollBox.Height) then FMovingDirection := -1
  else FMovingDirection := 0;

  {Start timer or not}
  FSelectTimer.Enabled := FMovingDirection <> 0;
end;

procedure TLogViewer.OnTextMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  FMouseIsDown := False;
  FSelectTimer.Enabled := False;

  {Show Meni}
  if Button = TMouseButton.mbRight then
  begin
    ShowContextMenu(Screen.MousePos);
  end;
end;

procedure TLogViewer.OnTextMouseDblClick(Sender: TObject);
begin
  if not InRange(ItemIndex, 0, Count - 1) then Exit;

  if FItems[ItemIndex].URL <> '' then ShellComand('open', FItems[ItemIndex].URL, '');
end;

procedure TLogViewer.OnTextPainting(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
var
  i: Integer;
  msg: string;
  textRect, controlRect: TRectF;
begin
  Canvas.Fill.Kind := TBrushKind.Solid;
  Canvas.Font.Assign(FTextLayout.Font);

  textRect := ARect;
  textRect.Height := FItemHeight;

  controlRect := ARect;
  controlRect.Top := FScrollBox.ViewportPosition.Y;
  controlRect.Height := FScrollBox.Height;

  {Draw Items}
  for i := 0 to Count - 1 do
  begin
    {Draw Text Line}
    if controlRect.IntersectsWith(textRect) then
    begin
      {Background}
      Canvas.Fill.Color := ItemIndexColor;
      if InRange(i, ItemIndex, ToItemIndex)
        or InRange(i, ToItemIndex, ItemIndex) then Canvas.FillRect(textRect, 0, 0, AllCorners, FTextLayout.Opacity);

      {Color}
      if FItems[i].Color <> claNull then Canvas.Fill.Color := FItems[i].Color
      else Canvas.Fill.Color := FTextLayout.TextSettings.FontColor;

      {Style}
      if FItems[i].Style <> [] then Canvas.Font.Style := FItems[i].Style
      else Canvas.Font.Style := FTextLayout.TextSettings.Font.Style;

      {Cropp string}
      msg := FItems[i].Msg;
      if Length(msg) > FLineLength then
      begin
        Delete(msg, FLineLength, Length(msg));
        msg := msg + '...';
      end;

      {Write Text}
      Canvas.FillText(textRect, msg, False, FTextLayout.Opacity, [], FTextLayout.TextSettings.HorzAlign, FTextLayout.TextSettings.VertAlign);
    end;

    textRect.Offset(0, textRect.Height);
  end;
end;

initialization
  RegisterFMXClasses([TLogViewer]);
end.
