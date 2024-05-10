unit Pocket;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Buttons, Graphics, Dialogs, ExtCtrls, Controls, fgl;

type

  { CStateNotify }

  TStateNotifyKind = (snNone, snProcess, snFailed, snSuccess, snCancel);

  CStateNotify = class(TCustomPanel)
  private
    FStateNotifyKind: TStateNotifyKind;
    FTimer: TTimer;
    FShowTime: Cardinal;
    procedure SetStateNotifyKind(Value: TStateNotifyKind);
    procedure PaintState(Sender: TObject);
    procedure TimerTicker(Sender: TObject);
  public
    constructor Create(TheOwner: TComponent); override;
  published
    property State: TStateNotifyKind read FStateNotifyKind write SetStateNotifyKind default snNone;
    property ShowTime: Cardinal read FShowTime write FShowTime default 1000;
  end;

  { Связи }

  (*
    Provoking - вызывающий элемент
    Invoking - вызываемый элемен
  *)

  { CBonds }

  CBonds = Class(TObject)
    private
      function GetCount: Integer;
    public
      arProvoking: array of Integer;
      arInvoking: array of Integer;
      procedure Add(Provoking, Invoking: Integer);
      procedure DeleteWhereProvoking(Provoking: Integer);
      procedure DeleteWhereInvoking(Invoking: Integer);
      function GetInvokingWhereProvoking(Provoking: Integer): Integer;
      function GetProvokingWhereInvoking(Invoking: Integer): Integer;
      function IndexOfProvoking(Provoking: Integer): Integer;
      function IndexOfInvoking(Invoking: Integer): Integer;
      procedure SwapWhereProvoking(ProvokingFirst, ProvokingTwo: Integer);
      procedure UpdateWhereProvoking(oldProvoking, newProvoking: Integer);
      procedure UpdateWhereInvoking(oldInvoking, newInvoking: Integer);
      procedure TwiceUpdateWhereProvoking(oldProvokingFirst, newProvokingFirst,
                                    oldProvokingTwo, newProvokingTwo: Integer);
      function InProvoking(Number: Integer): Boolean;
      function InInvoking(Number: Integer): Boolean;
      function InBonds(Number: Integer): Boolean; overload;
      function InBonds(Provoking, Invoking: Integer): Boolean; overload;
      procedure Clear;
    published
      property Count: Integer read GetCount;
  end;

  { CScenario }

  CScenario = Class(TObject)
    public
      Name: String;
      FilePath: String;
      AudioFileNames: TStringList;
      AudioFilePaths: TStringList;
      VideoFileNames: TStringList;
      VideoFilePaths: TStringList;
      Bonds: CBonds;
      constructor Create;
  end;

  CScenarioList = specialize TFPGObjectList<CScenario>;

  procedure setGlyphSpeedButton(SpeedButton: TSpeedButton; Path: String); overload;
  procedure setGlyphSpeedButton(BitBtn: TBitBtn; Path: String); overload;

implementation

procedure setGlyphSpeedButton(SpeedButton: TSpeedButton; Path: String); overload;
var Picture: TPicture;
begin
  try
    Picture := TPicture.Create;
    Picture.LoadFromFile(Path);
    SpeedButton.Glyph.Assign(Picture.Bitmap);
  finally
    Picture.Destroy;
  end;
end;

procedure setGlyphSpeedButton(BitBtn: TBitBtn; Path: String); overload;
var Picture: TPicture;
begin
  try
    Picture := TPicture.Create;
    Picture.LoadFromFile(Path);
    BitBtn.Glyph.Assign(Picture.Bitmap);
  finally
    Picture.Destroy;
  end;
end;

{ CScenario }

constructor CScenario.Create;
begin
  inherited Create;
  Name := '';
  FilePath := '';
  AudioFileNames := TStringList.Create;
  AudioFilePaths := TStringList.Create;
  VideoFileNames := TStringList.Create;
  VideoFilePaths := TStringList.Create;
  Bonds := CBonds.Create;
end;

{ CStateNotify }

procedure CStateNotify.SetStateNotifyKind(Value: TStateNotifyKind);
begin
  FStateNotifyKind := Value;
  case FStateNotifyKind of
    snNone:
      begin
        Self.Visible := False;
      end;
    snProcess: Self.Visible := True;
    snFailed, snSuccess, snCancel:
      begin
        Self.Visible := True;
        FTimer.Interval := ShowTime;
        FTimer.Enabled := True;
      end;
  end;
  Self.Repaint;
end;

procedure CStateNotify.PaintState(Sender: TObject);
var
  text_width, text_height, text_x, text_y: Integer;
begin
  Self.Canvas.Pen.Style := psSolid;
  Self.Canvas.Brush.Style := bsSolid;
  Self.Canvas.Pen.Color := clDefault;
  Self.Canvas.Brush.Color := clDefault;
  case FStateNotifyKind of
    snNone:
      begin
        Self.Canvas.Clear;
      end;
    snProcess:
      begin
        Self.Canvas.Clear;
        text_width := Self.Canvas.TextWidth('Сохранение...');
        text_height := Self.Canvas.TextHeight('Сохранение...');
        text_x := Round(Self.Width/2) - Round(text_width/2);
        text_y := Round(Self.Height/2) - Round(text_height/2);
        Self.Canvas.TextOut(text_x, text_y, 'Сохранение...');
      end;
    snFailed:
      begin
        Self.Canvas.Clear;
        text_width := Self.Canvas.TextWidth('Ошибка при сохранении');
        text_height := Self.Canvas.TextHeight('Ошибка при сохранении');
        text_x := Round(Self.Width/2) - Round(text_width/2);
        text_y := Round(Self.Height/2) - Round(text_height/2);
        Self.Canvas.TextOut(text_x, text_y, 'Ошибка при сохранении');
      end;
    snSuccess:
      begin
        Self.Canvas.Clear;
        text_width := Self.Canvas.TextWidth('Успешно сохранено');
        text_height := Self.Canvas.TextHeight('Успешно сохранено');
        text_x := Round(Self.Width/2) - Round(text_width/2);
        text_y := Round(Self.Height/2) - Round(text_height/2);
        Self.Canvas.TextOut(text_x, text_y, 'Успешно сохранено');
        Self.Canvas.Pen.Style := psSolid;
        Self.Canvas.Brush.Style := bsSolid;
        Self.Canvas.Pen.Color := clGreen;
        Self.Canvas.Brush.Color := clGreen;
      end;
    snCancel:
      begin
        Self.Canvas.Clear;
        text_width := Self.Canvas.TextWidth('Сохранение отменено');
        text_height := Self.Canvas.TextHeight('Сохранение отменено');
        text_x := Round(Self.Width/2) - Round(text_width/2);
        text_y := Round(Self.Height/2) - Round(text_height/2);
        Self.Canvas.TextOut(text_x, text_y, 'Сохранение отменено');
      end;
  end;
end;

procedure CStateNotify.TimerTicker(Sender: TObject);
begin
  FTimer.Enabled := False;
  State := snNone;
end;

constructor CStateNotify.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  Self.OnPaint := @PaintState;
  Self.Visible := False;
  FTimer := TTimer.Create(Self);
  FTimer.Enabled := False;
  FTimer.OnTimer := @TimerTicker;
end;

{ CBonds }

function CBonds.GetCount: Integer;
begin
  Result := Length(arProvoking);
end;

procedure CBonds.Add(Provoking, Invoking: Integer);
begin
  SetLength(arProvoking, Count+1);
  arProvoking[Count-1] := Provoking;
  SetLength(arInvoking, Count);
  arInvoking[Count-1] := Invoking;
end;

procedure CBonds.DeleteWhereProvoking(Provoking: Integer);
var i, t : Integer;
begin
  for i := 0 to (Count - 1) do
    begin
      if arProvoking[i] = Provoking then
        begin
          if Count = 1 then
            begin
              SetLength(arProvoking, 0);
              SetLength(arInvoking, 0);
              Exit;
            end;

          for t := i to (Count-2) do
            begin
              arProvoking[t] := arProvoking[t+1];
              arInvoking[t] := arInvoking[t+1];
            end;
          SetLength(arProvoking, Count-1);
          SetLength(arInvoking, Count);
        end;
    end;
end;

procedure CBonds.DeleteWhereInvoking(Invoking: Integer);
var i, t : Integer;
begin
  for i := 0 to (Count - 1) do
    begin
      if arInvoking[i] = Invoking then
        begin
          if Count = 1 then
            begin
              SetLength(arProvoking, 0);
              SetLength(arInvoking, 0);
              Exit;
            end;

          for t := i to (Count-2) do
            begin
              arProvoking[t] := arProvoking[t+1];
              arInvoking[t] := arInvoking[t+1];
              SetLength(arProvoking, Count-1);
              SetLength(arInvoking, Count);
            end;
        end;
    end;
end;

function CBonds.GetInvokingWhereProvoking(Provoking: Integer): Integer;
begin
  if IndexOfProvoking(Provoking) = -1 then
    Exit(-1);

  Result := arInvoking[IndexOfProvoking(Provoking)];
end;

function CBonds.GetProvokingWhereInvoking(Invoking: Integer): Integer;
begin
  if IndexOfInvoking(Invoking) = -1 then
    Exit(-1);

  Result := arProvoking[IndexOfInvoking(Invoking)];
end;

function CBonds.IndexOfProvoking(Provoking: Integer): Integer;
var i: Integer;
begin
  Result := -1;

  if Count = 0 then
    Exit;

  for i := 0 to Count-1 do
    if Provoking = arProvoking[i] then
      Exit(i);
end;

function CBonds.IndexOfInvoking(Invoking: Integer): Integer;
var i: Integer;
begin
  Result := -1;

  if Count = 0 then
    Exit;

  for i := 0 to Count-1 do
    if Invoking = arInvoking[i] then
      Exit(i);
end;

procedure CBonds.SwapWhereProvoking(ProvokingFirst, ProvokingTwo: Integer);
var i: Integer;
begin
  if Count = 0 then
    Exit;

  for i := 0 to Count-1 do
    begin
      if arProvoking[i] = ProvokingFirst then
         arProvoking[i] := ProvokingTwo
      else if arProvoking[i] = ProvokingTwo then
         arProvoking[i] := ProvokingFirst;
    end;
end;

procedure CBonds.UpdateWhereProvoking(oldProvoking, newProvoking: Integer);
var i: Integer;
begin
  if Count = 0 then
    Exit;

  for i := 0 to Count-1 do
    if arProvoking[i] = oldProvoking then
         arProvoking[i] := newProvoking;
end;

procedure CBonds.UpdateWhereInvoking(oldInvoking, newInvoking: Integer);
var i: Integer;
begin
  if Count = 0 then
    Exit;

  for i := 0 to Count-1 do
    if arInvoking[i] = oldInvoking then
         arInvoking[i] := newInvoking;
end;

procedure CBonds.TwiceUpdateWhereProvoking(oldProvokingFirst, newProvokingFirst,
  oldProvokingTwo, newProvokingTwo: Integer);
var i: Integer;
begin
  if Count = 0 then
    Exit;

  for i := 0 to Count-1 do
    if arProvoking[i] = oldProvokingFirst then
      arProvoking[i] := newProvokingFirst
    else if arProvoking[i] = oldProvokingTwo then
      arProvoking[i] := newProvokingTwo;

end;

function CBonds.InProvoking(Number: Integer): Boolean;
var i: Integer;
begin
  Result := False;
  for i in arProvoking do
    if i = Number then
    begin
      Exit(True);
    end;
end;

function CBonds.InInvoking(Number: Integer): Boolean;
var i: Integer;
begin
  Result := False;
  for i in arInvoking do
    if i = Number then
    begin
      Exit(True);
    end;
end;

function CBonds.InBonds(Number: Integer): Boolean;
begin
  Result := InProvoking(Number) or InInvoking(Number);
end;

function CBonds.InBonds(Provoking, Invoking: Integer): Boolean;
begin
  Result := InProvoking(Provoking) and InInvoking(Invoking);
end;

procedure CBonds.Clear;
begin
  SetLength(arProvoking, 0);
  SetLength(arInvoking, 0);
end;

end.

