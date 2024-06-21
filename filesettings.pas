unit FileSettings;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons, Pocket;

type

  { TfFileSettings }

  TfFileSettings = class(TForm)
    bbtnSave: TBitBtn;
    bbtnClose: TBitBtn;
    edtOldFilePath: TEdit;
    edtNewFilePath: TEdit;
    OpenDialog: TOpenDialog;
    sbtnOpenFile: TSpeedButton;
    procedure bbtnCloseClick(Sender: TObject);
    procedure bbtnSaveClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure sbtnOpenFileClick(Sender: TObject);
  private
    Orig_FilePath, Orig_FileName, New_FilePath, New_FileName: String;
  public

  end;

var
  fFileSettings: TfFileSettings;

implementation

uses Main;

{$R *.lfm}

{ TfFileSettings }

procedure TfFileSettings.bbtnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TfFileSettings.bbtnSaveClick(Sender: TObject);
var currScenario: CScenario = NIL;
    ItemIndexAudio, ItemIndexVideo: Integer;
begin
  try
    currScenario := Main.ScenarioList.Items[Main.fMain.TabControl.TabIndex];

    case identifyFileType(Orig_FileName) of
      ftAudio:
        begin
          ItemIndexAudio := Main.fMain.clboxAudioPlaylist.ItemIndex;

          if ItemIndexAudio = glCurrentAudioItemIndex then
            ShowMessage('Изменение отклонено. Выбранный трек для редактироdания сейчас проигрывается.')
          else
            begin
              currScenario.AudioFilePaths.Strings[ItemIndexAudio] := New_FilePath;
              currScenario.AudioFileNames.Strings[ItemIndexAudio] := New_FileName;
              Main.fMain.clboxAudioPlaylist.Items := currScenario.AudioFileNames;
            end;
        end;
      ftVideo, ftImage:
        begin
          ItemIndexVideo := Main.fMain.clboxVideoPlaylist.ItemIndex;
          if ItemIndexVideo = glCurrentVideoItemIndex then
            ShowMessage('Изменение отклонено. Выбранный трек для редактироdания сейчас проигрывается.')
          else
            begin
              currScenario.VideoFilePaths.Strings[ItemIndexVideo] := New_FilePath;
              currScenario.VideoFileNames.Strings[ItemIndexVideo] := New_FileName;
              Main.fMain.clboxVideoPlaylist.Items := currScenario.VideoFileNames;
            end;
        end
    end;
  finally
    edtNewFilePath.Text := '';
    Close;
  end;
end;

procedure TfFileSettings.FormShow(Sender: TObject);
begin

end;

procedure TfFileSettings.sbtnOpenFileClick(Sender: TObject);
begin
  Orig_FilePath := edtOldFilePath.Text;
  Orig_FileName := ExtractFileName(Orig_FilePath);
  if OpenDialog.Execute then
    begin
      New_FilePath := OpenDialog.FileName;
      New_FileName := ExtractFileName(New_FilePath);
      case identifyFileType(Orig_FileName) of
        ftAudio:
          if isAudio(New_FileName) then
            begin
              edtNewFilePath.Text := New_FilePath;
              bbtnSave.Enabled := True;
            end
          else
            begin
              ShowMessage('Выбранный файл не является аудиофайлом. Изменение отклонено.');
            end;
        ftVideo, ftImage:
          if isVideo(New_FileName) or isImage(New_FileName) then
            begin
              edtNewFilePath.Text := New_FilePath;
              bbtnSave.Enabled := True;
            end
          else
            begin
              ShowMessage('Выбранный файл не является видеофайлом. Изменение отклонено.');
            end;
        ftUnknown: ShowMessage('Тип оригинального файла неизвестен.');
      end;
    end;
end;

end.

