unit Pocket;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Buttons, Graphics, Dialogs;

type

  { CBonds }

  (*
    Provoking - вызывающий элемент
    Invoking - вызываемый элемен
  *)

  CBonds = Class(TObject)
    private
      function GetCount: Integer;
    public
      arProvoking: array of Integer;
      arInvoking: array of Integer;
      procedure Add(Provoking, Invoking: Integer);
      procedure DeleteWhereProvoking(Provoking: Integer);
      procedure DeleteWhereInvoking(Provoking: Integer);
      function GetInvokingWhereProvoking(Provoking: Integer): Integer;
      function IndexOfProvoking(Provoking: Integer): Integer;
      function IndexOfInvoking(Invoking: Integer): Integer;
      procedure UpdateWhereProvoking(oldProvoking, newProvoking: Integer);
      procedure UpdateWhereInvoking(oldInvoking, newInvoking: Integer);
      function InProvoking(Number: Integer): Boolean;
      function InInvoking(Number: Integer): Boolean;
      function InBonds(Number: Integer): Boolean; overload;
      function InBonds(Provoking, Invoking: Integer): Boolean; overload;
      procedure Clear;
    published
      property Count: Integer read GetCount;
  end;

  { CBondsArray }

  CBondsArray = Class(TObject)
    private
      FBondsArray: Array of CBonds;
    public
      procedure Add(Bonds: CBonds);
      procedure Delete(Position: CBonds);
      procedure Move(ABonds: CBonds; APosition: Integer);
  end;

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

{ CBondsArray }

procedure CBondsArray.Add(Bonds: CBonds);
begin

end;

procedure CBondsArray.Delete(Position: CBonds);
begin

end;

procedure CBondsArray.Move(ABonds: CBonds; APosition: Integer);
begin

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
              arProvoking[t] := arProvoking[t+1];
              SetLength(arProvoking, Count-1);
              SetLength(arInvoking, Count);
            end;
        end;
    end;
end;

procedure CBonds.DeleteWhereInvoking(Provoking: Integer);
var i : Integer;
begin

end;

function CBonds.GetInvokingWhereProvoking(Provoking: Integer): Integer;
begin
  Result := arInvoking[IndexOfProvoking(Provoking)];
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

