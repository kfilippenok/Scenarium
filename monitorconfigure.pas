unit MonitorConfigure;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
  // Project Units
  PLaybackVideo, Pocket;

type

  { TfMonitorConfigure }

  TfMonitorConfigure = class(TForm)
    panMonitors: TPanel;
    panUpdateMonitorsList: TPanel;
    sbtnUpdateMonitorsList: TSpeedButton;
    procedure FormCreate(Sender: TObject);
    procedure UpdateMonitorsList(Sender: TObject);
  private
    procedure MonitorClick(Sender: TObject);
    procedure ClearMonitorsList;
    procedure PrintMonitorsList;
    procedure DetectAndAutoChoosePlaybackMonitor();
  public

  end;

var
  fMonitorConfigure: TfMonitorConfigure;
  PlaybackMonitor: Integer = -1;

implementation

{$R *.lfm}

{ TfMonitorConfigure }

procedure TfMonitorConfigure.FormCreate(Sender: TObject);
begin
  // Если при запуске приложения доступно больше 1 монитора,
  // то свободный будет выбран по умолчанию.
  DetectAndAutoChoosePlaybackMonitor();

  PrintMonitorsList();
end;

procedure TfMonitorConfigure.UpdateMonitorsList(Sender: TObject);
begin
  Screen.UpdateMonitors();
  DetectAndAutoChoosePlaybackMonitor();
  ClearMonitorsList();
  PrintMonitorsList();
end;

procedure TfMonitorConfigure.MonitorClick(Sender: TObject);
var LMonitor: TSpeedButton;
    LMonitorIndex: Integer;
begin
  LMonitor := TSpeedButton.Create(panMonitors);
  LMonitor := (Sender as TSpeedButton);
  LMonitorIndex := StrToInt(LMonitor.Name[Length(LMonitor.Name)]);
  if Screen.Monitors[LMonitorIndex-1].MonitorNum = fMonitorConfigure.Monitor.MonitorNum then
    begin
      QuestionDlg('Предупреждение!',
        'Скорее всего вы ошибочно выбрали данный экран. Окно воспроизведения перекроет окно управления. Так нельзя!', mtInformation, [mrCancel, 'Ок', 'IsDefault'], '');
      Exit();
    end
  else
    begin
      PlaybackMonitor := LMonitorIndex;
      fPLaybackVideo.BoundsRect := Screen.Monitors[PlaybackMonitor-1].BoundsRect;
      UpdateMonitorsList(Sender);
    end;
end;

procedure TfMonitorConfigure.ClearMonitorsList;
var icmp: Byte;
begin;
  for icmp := 0 to panMonitors.ComponentCount-1 do
  begin
      panMonitors.Components[0].Free;
  end;
end;

procedure TfMonitorConfigure.PrintMonitorsList;
var SpeedButton: TSpeedButton;
    i: Byte;
begin
  for i := 1 to Screen.MonitorCount do
  begin
    SpeedButton := TSpeedButton.Create(panMonitors);
    SpeedButton.Parent := panMonitors;
    SpeedButton.Top := Trunc(panMonitors.Height/2)-50;
    SpeedButton.Left := i*100;
    SpeedButton.Name := 'imgMonitor' + IntToStr(i);
    SpeedButton.Width := 100;
    SpeedButton.Height := 100;
    SpeedButton.Flat := True;
    SpeedButton.OnClick := @MonitorClick;

    if PlaybackMonitor = i
      then setGlyphSpeedButton(SpeedButton, 'icons\monitor_active.png')
      else setGlyphSpeedButton(SpeedButton, 'icons\monitor_inactive.png');
  end;
end;

procedure TfMonitorConfigure.DetectAndAutoChoosePlaybackMonitor;
begin
  if Screen.MonitorCount > 1 then
    begin
      if Screen.Monitors[0].MonitorNum <> fMonitorConfigure.Monitor.MonitorNum then
        PlaybackMonitor := 1
      else if Screen.Monitors[1].MonitorNum <> fMonitorConfigure.Monitor.MonitorNum then
        PlaybackMonitor := 2;

      fPLaybackVideo.BoundsRect := Screen.Monitors[PlaybackMonitor-1].BoundsRect;
    end;
end;

end.

