program Scenarium;

{$mode ObjFPC} {$LONGSTRINGS ON}

uses
 {$IFDEF UNIX}
  cthreads,
   {$ENDIF} {$IFDEF HASAMIGA}
  athreads,
   {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,
  Main,
  AboutProject,
  PlaybackVideo,
  PlaybackAudio,
  Settings,
  MonitorConfigure, MPVBasePlayer,
  MPVClient,
  MPVConst,
  MPVNode,
  MPVRender,
  MPVRenderGL,
  MPVStreamCB,
  MPVTrack;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TfMain, fMain);
  Application.CreateForm(TfAboutProject, fAboutProject);
  Application.CreateForm(TfPlaybackVideo, fPlaybackVideo);
  Application.CreateForm(TfSettings, fSettings);
  Application.CreateForm(TfPlaybackAudio, fPlaybackAudio);
  Application.CreateForm(TfMonitorConfigure, fMonitorConfigure);
  Application.Run;
end.
