{*******************************************************}
{                                                       }
{       Scenarium                                       }
{       Copyright (c) 2024 Kirill Filippenok            }
{       Apache License 2.0                              }
{                                                       }
{*******************************************************}

unit PlaybackAudio;

{$mode ObjFPC} {$LONGSTRINGS ON} {$RANGECHECKS ON} {$WRITEABLECONST OFF}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  // libMPVDelphi
  MPVBasePlayer;

type

  { TfPlaybackAudio }

  TfPlaybackAudio = class(TForm)
    panMPVHandle: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private

  public
    audioPlayer: TMPVBasePlayer;
  end;

var
  fPlaybackAudio: TfPlaybackAudio;

implementation

{$R *.lfm}

{ TfPlaybackAudio }

procedure TfPlaybackAudio.FormCreate(Sender: TObject);
begin
  if not MPVLibLoaded('') then
  begin
    ShowMessage('Failed to load MPV dll');
    Application.Terminate;
  end;

  FreeAndNil(audioPlayer);
  audioPlayer := TMPVBasePlayer.Create;
  audioPlayer.InitPlayer(IntToStr(panMPVHandle.Handle), '', '', '');
end;

procedure TfPlaybackAudio.FormDestroy(Sender: TObject);
begin
  FreeAndNil(audioPlayer);
end;

end.

