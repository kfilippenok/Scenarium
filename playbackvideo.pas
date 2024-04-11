unit PlaybackVideo;

{$mode ObjFPC} {$LONGSTRINGS ON} {$RANGECHECKS ON} {$WRITEABLECONST OFF}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, MPVBasePlayer;

type

  { TfPlaybackVideo }

  TfPlaybackVideo = class(TForm)
    panMPVHandle: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private

  public
    videoPlayer: TMPVBasePlayer;
  end;

var
  fPlaybackVideo: TfPlaybackVideo;

implementation

{$R *.lfm}

{ TfPlaybackVideo }

procedure TfPlaybackVideo.FormCreate(Sender: TObject);
begin
  if not MPVLibLoaded('') then
  begin
    ShowMessage('Failed to load MPV dll');
    Application.Terminate;
  end;

  FreeAndNil(videoPlayer);
  videoPlayer := TMPVBasePlayer.Create;
  videoPlayer.InitPlayer(IntToStr(panMPVHandle.Handle), '', '', '');
end;

procedure TfPlaybackVideo.FormDestroy(Sender: TObject);
begin
  FreeAndNil(videoPlayer);
end;

end.

