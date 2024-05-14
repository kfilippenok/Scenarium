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
  PlaybackMonitor := LMonitorIndex;
  fPLaybackVideo.BoundsRect := Screen.Monitors[PlaybackMonitor].BoundsRect;
  UpdateMonitorsList(Sender);
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
  for i := 0 to Screen.MonitorCount-1 do
  begin
    SpeedButton := TSpeedButton.Create(panMonitors);
    SpeedButton.Parent := panMonitors;
    SpeedButton.Top := Trunc(panMonitors.Height/2)-50;
    SpeedButton.Left := (i+1)*100;
    SpeedButton.Name := 'imgMonitor' + IntToStr(i);
    SpeedButton.Width := 100;
    SpeedButton.Height := 100;
    SpeedButton.Flat := True;
    SpeedButton.OnClick := @MonitorClick;

    if PlaybackMonitor = i then
      setGlyphSpeedButton(SpeedButton, 'icons' + PathDelim +'monitor_active.png')
    else if i = fMonitorConfigure.Monitor.MonitorNum then
      begin
        SpeedButton.Enabled := False;
        setGlyphSpeedButton(SpeedButton, 'icons' + PathDelim +'monitor_disable.png')
      end
    else
      setGlyphSpeedButton(SpeedButton, 'icons' + PathDelim +'monitor_inactive.png');
  end;
end;

procedure TfMonitorConfigure.DetectAndAutoChoosePlaybackMonitor;
begin
  if Screen.MonitorCount > 1 then
    begin
      if Screen.Monitors[0].MonitorNum <> fMonitorConfigure.Monitor.MonitorNum then
        PlaybackMonitor := 0
      else if Screen.Monitors[1].MonitorNum <> fMonitorConfigure.Monitor.MonitorNum then
        PlaybackMonitor := 1;

      fPLaybackVideo.BoundsRect := Screen.Monitors[PlaybackMonitor].BoundsRect;
    end;
end;

end.

