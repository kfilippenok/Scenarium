{*******************************************************}
{                                                       }
{       Scenarium                                       }
{       Copyright (c) 2024 Kirill Filippenok            }
{       Apache License 2.0                              }
{                                                       }
{*******************************************************}

unit Settings;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls,
  StdCtrls;

type

  { TfSettings }

  TfSettings = class(TForm)
    chkboxAdaptivePanels: TCheckBox;
    chkboxTips: TCheckBox;
    pgcntrlSettings: TPageControl;
    radgrVisibleVariants: TRadioGroup;
    tabDisplay: TTabSheet;
    tabAdditional: TTabSheet;
    procedure chkboxAdaptivePanelsChange(Sender: TObject);
    procedure chkboxTipsChange(Sender: TObject);
    procedure radgrVisibleVariantsSelectionChanged(Sender: TObject);
  private

  public

  end;

var
  fSettings: TfSettings;

implementation

{$R *.lfm}

uses Main;

{ TfSettings }

procedure TfSettings.radgrVisibleVariantsSelectionChanged(Sender: TObject);
begin
    if radgrVisibleVariants.ItemIndex = 0 then
    begin
      fMain.panVideo.Visible := False;
      fMain.Splitter.Visible := False;

      fMain.panAudio.Align := (alClient);
      fMain.panAudio.Visible := True;
    end
    else if radgrVisibleVariants.ItemIndex = 1 then
    begin
      fMain.panAudio.Visible := False;
      fMain.Splitter.Visible := False;

      fMain.panVideo.Align := (alClient);
      fMain.panVideo.Visible := True;
    end
    else if radgrVisibleVariants.ItemIndex = 2 then
    begin
      with fMain.panAudio do
      begin
        Align := (alLeft);
        AnchorToNeighbour(akRight, 0, fMain.Splitter);
      end;
      with fMain.panVideo do
      begin
        Align := (alRight);
        AnchorToNeighbour(akLeft, 0, fMain.Splitter);
      end;
      fMain.panAudio.Visible := True;
      fMain.Splitter.Visible := True;
      fMain.panVideo.Visible := True;
    end;
end;

procedure TfSettings.chkboxTipsChange(Sender: TObject);
begin
  if (Sender as TCheckBox).Checked then
  begin
    fMain.ShowHint := True;
  end
  else
  begin
    fMain.ShowHint := False;
  end;
end;

procedure TfSettings.chkboxAdaptivePanelsChange(Sender: TObject);
begin
  if chkboxAdaptivePanels.Checked then
    Main.glAdaptivePanels := True
  else
    Main.glAdaptivePanels := False;
end;

end.

