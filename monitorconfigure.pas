{*******************************************************}
{                                                       }
{       Scenarium                                       }
{       Copyright (c) 2024 Kirill Filippenok            }
{       Apache License 2.0                              }
{                                                       }
{*******************************************************}

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
    panUpdateMonitorsList: TPanel;
    sbtnUpdateMonitorsList: TSpeedButton;
    scrboxMonitors: TScrollBox;
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
  LMonitor := (Sender as TSpeedButton);
  LMonitorIndex := StrToInt(LMonitor.Name[Length(LMonitor.Name)]);
  if PlaybackMonitor = LMonitorIndex then
    begin
      PlaybackMonitor := -1;
      fPLaybackVideo.Visible := False;
    end
  else
    begin
      PlaybackMonitor := LMonitorIndex;
      fPLaybackVideo.BoundsRect := Screen.Monitors[PlaybackMonitor].BoundsRect;
    end;

  UpdateMonitorsList(Sender);
end;

procedure TfMonitorConfigure.ClearMonitorsList;
var icmp: Byte;
begin;
  for icmp := 0 to scrboxMonitors.ComponentCount-1 do
  begin
      scrboxMonitors.Components[0].Free;
  end;
end;

procedure TfMonitorConfigure.PrintMonitorsList;
var SpeedButton: TSpeedButton;
    i: Byte;
    OffsetBorder: Integer = 50;
    OffsetElement: Integer = 10;
begin
  for i := 0 to Screen.MonitorCount-1 do
  begin
    SpeedButton := TSpeedButton.Create(scrboxMonitors);
    SpeedButton.Parent := scrboxMonitors;
    SpeedButton.Top := Trunc(scrboxMonitors.Height/2)-50;
    if i <> 0 then
      SpeedButton.Left := OffsetElement;
    SpeedButton.Left := SpeedButton.Left + OffsetBorder + i*100;
    SpeedButton.Name := 'imgMonitor' + IntToStr(i);
    SpeedButton.Width := 100;
    SpeedButton.Height := 100;
    SpeedButton.Flat := True;
    SpeedButton.OnClick := @MonitorClick;

    if PlaybackMonitor = i then
      setGlyphSpeedButton(SpeedButton, Application.Location + 'icons' + PathDelim +'monitor_active.png')
    else if i = fMonitorConfigure.Monitor.MonitorNum then
      begin
        SpeedButton.Enabled := False;
        setGlyphSpeedButton(SpeedButton, Application.Location + 'icons' + PathDelim +'monitor_disable.png')
      end
    else
      setGlyphSpeedButton(SpeedButton, Application.Location + 'icons' + PathDelim +'monitor_inactive.png');
  end;
end;

procedure TfMonitorConfigure.DetectAndAutoChoosePlaybackMonitor;
begin
  if PlaybackMonitor <> -1 then Exit;

  PlaybackMonitor := -1;

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

