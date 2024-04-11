unit Main;

{$mode ObjFPC}{$LONGSTRINGS ON}{$RANGECHECKS ON}{$WRITEABLECONST OFF}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, ExtCtrls,
  CheckLst, Buttons, StdCtrls, ComCtrls, Types, LCLType, lazUTF8, Themes,
  Math,
  // JSON
  fpjson, jsonparser,
  // Files
  FileUtil,
  // Project Units
  AboutProject, PlaybackVideo, PlaybackAudio, MonitorConfigure, Settings,
  Pocket,
  // libMPVDelphi
  MPVBasePlayer;


type

  { TfMain }

  TfMain = class(TForm)
    lblCurrentVideoItem: TLabel;
    lblVideoTimeTotal: TLabel;
    lblVideoTimeCurrent: TLabel;
    lblCurrentAudioItem: TLabel;
    miAudioDeleteBond: TMenuItem;
    miAudioAddBond: TMenuItem;
    miVideoAddBond: TMenuItem;
    miAudioDelete: TMenuItem;
    muVideoDelete: TMenuItem;
    ppmnAudioPlaylist: TPopupMenu;
    ppmnVideoPlaylist: TPopupMenu;
    { Вкладки }
    TabControl: TTabControl;

    { Меню }
    MainMenu: TMainMenu;
    ScenarioNew: TMenuItem;
    ScenarioOpen: TMenuItem;
    ScenarioSaveAs: TMenuItem;
    ScenarioSave: TMenuItem;
    Scenario: TMenuItem;
    ScenarioExit: TMenuItem;
    Settings: TMenuItem;
    Info: TMenuItem;
    InfoAboutProgram: TMenuItem;

    { Аудио панель }
    panAudio: TPanel;
    panAudioControlsTop: TPanel;
    panAudioControlsBottom: TPanel;
    // Работа со списком
    clboxAudioPlaylist: TCheckListBox;
    sbtnAudioAdd: TSpeedButton;
    sbtnAudioSubtract: TSpeedButton;
    sbtnAudioClear: TSpeedButton;
    // Воспроизведение
    sbtnAudioPlay: TSpeedButton;
    sbtnAudioPause: TSpeedButton;
    sbtnAudioStop: TSpeedButton;
    trbarAudioTime: TTrackBar;
    lblAudioTimeCurrent: TLabel;
    lblAudioTimeTotal: TLabel;
    // Громкость
    sbtnAudioVolume: TSpeedButton;
    trbarAudioVolume: TTrackBar;
    // Таймер
    TimerAudio: TTimer;

    { Разделитель }
    Splitter: TSplitter;

    { Видео панель }
    panVideo: TPanel;
    panVideoControlsTop: TPanel;
    panVideoControlsBottom: TPanel;
    // Работа со списком
    clboxVideoPlaylist: TCheckListBox;
    sbtnVideoAdd: TSpeedButton;
    sbtnVideoSubtract: TSpeedButton;
    sbtnVideoClear: TSpeedButton;
    // Воспроизведение
    sbtnVideoPause: TSpeedButton;
    sbtnVideoPlay: TSpeedButton;
    sbtnVideoStop: TSpeedButton;
    trbarVideoTime: TTrackBar;
    // Громкость
    sbtnVideoVolume: TSpeedButton;
    trbarVideoVolume: TTrackBar;
    // Таймер
    TimerVideo: TTimer;
    // Монитор
    sbtnFullyDisplay: TSpeedButton;
    sbtnMonitorConfigure: TSpeedButton;

    { Диалоги }
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;

    procedure clboxAudioPlaylistClick(Sender: TObject);
    procedure clboxAudioPlaylistDblClick(Sender: TObject);
    procedure clboxVideoPlaylistClick(Sender: TObject);
    procedure clboxVideoPlaylistDblClick(Sender: TObject);
    procedure clboxVideoPlaylistItemClick(Sender: TObject; Index: integer);
    procedure clboxVideoPlaylistKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure clboxVideoPlaylistMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure AddBond(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of string);
    procedure FormResize(Sender: TObject);
    procedure miAudioDeleteBondClick(Sender: TObject);
    procedure miAudioDeleteClick(Sender: TObject);
    procedure muVideoDeleteClick(Sender: TObject);
    procedure sbtnVideoVolumeClick(Sender: TObject);
    procedure sbtnVideoPlayClick(Sender: TObject);

    { Меню }
    procedure ScenarioNewClick(Sender: TObject);
    procedure ScenarioOpenClick(Sender: TObject);
    procedure ScenarioSaveClick(Sender: TObject);
    procedure ScenarioSaveAsClick(Sender: TObject);
    procedure ScenarioExitClick(Sender: TObject);
    procedure SettingsClick(Sender: TObject);
    procedure InfoAboutProgramClick(Sender: TObject);

    { Список файлов }
    procedure clboxAudioPlaylistDrawItem(Control: TWinControl;
      Index: integer; ARect: TRect; State: TOwnerDrawState);          (* Аудио *)
    procedure sbtnAudioAddClick(Sender: TObject);
    procedure sbtnAudioSubtractClick(Sender: TObject);
    procedure sbtnAudioClearClick(Sender: TObject);
    procedure clboxVideoPlaylistDrawItem(Control: TWinControl;
      Index: integer; ARect: TRect; State: TOwnerDrawState);          (* Видео *)
    procedure sbtnVideoSubtractClick(Sender: TObject);
    procedure sbtnVideoClearClick(Sender: TObject);

    { Воспроизведение }
    procedure sbtnAudioPlayClick(Sender: TObject);
    procedure MediaPause(Sender: TObject);
    procedure MediaStop(Sender: TObject);
    procedure TabControlChange(Sender: TObject);
    procedure TabControlChanging(Sender: TObject; var AllowChange: Boolean);
    procedure trbarAudioTimeChange(Sender: TObject);
    procedure trbarAudioTimeMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure trbarAudioTimeMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);

    { Окно воспроизведения (Видео) }
    procedure sbtnFullyDisplayClick(Sender: TObject);
    procedure sbtnMonitorConfigureClick(Sender: TObject);

    { Громкость (Аудио) }
    procedure sbtnAudioVolumeClick(Sender: TObject);
    procedure trbarAudioVolumeChange(Sender: TObject);
    procedure trbarAudioVolumeMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);

    { Таймер }
    procedure TimerMediaTimer(Sender: TObject);
    procedure trbarVideoTimeChange(Sender: TObject);
    procedure trbarVideoTimeMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure trbarVideoTimeMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure trbarVideoVolumeChange(Sender: TObject);
    procedure trbarVideoVolumeMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
  private
    procedure setFullyDisplay(ShowPlayback: Boolean);
    procedure resetTime(Player: string);
    procedure SetElementGlyphs;
    procedure LoadScenarioFromJSON(FilePath: String);
    procedure BondButtonClick(Sender: TObject);
    function isAudio(const FileName: String): Boolean;
    function isVideo(const FileName: String): Boolean;
    function isImage(const FileName: String): Boolean;
  public

  end;

var
  fMain: TfMain;
  glAudioMute: boolean;
  glVideoMute: boolean;
  glVideoTrackRewinding: boolean = False;
  glAudioTrackRewinding: boolean = False;
  glCurrentAudioItem: string;
  glCurrentAudioItemIndex: Integer;
  glCurrentVideoItem: string;
  glCurrentVideoItemIndex: Integer;
  glAudioFileNames: TStringList;
  glAudioFilePaths: TStringList;
  glVideoFileNames: TStringList;
  glVideoFilePaths: TStringList;
  glScenarioFileNames: TStringList;
  glScenarioFilePaths: TStringList;
  glBonds: CBonds;
  glAdaptivePanels: Boolean = False;

implementation

{$R *.lfm}

function SecsToTimeStr(time_secs: Integer): String;
var
  secs, mins: integer;
begin
  mins := time_secs div 60;
  secs := time_secs mod 60;
  if mins < 10 then
    Result := '0'
  else
    Result := '';
  Result := Result + IntToStr(mins);
  Result := Result + ':';
  if secs < 10 then
    Result := Result + '0';
  Result := Result + IntToStr(secs);
end;

procedure TfMain.BondButtonClick(Sender: TObject);
begin
  ShowMessage(
    clboxVideoPlaylist.Items.Strings[
      glBonds.GetInvokingWhereProvoking(
        StrToInt(
          (Sender as TBitBtn).Name[Length((Sender as TBitBtn).Name)]
        )
      )
    ]
  );
end;

function TfMain.isAudio(const FileName: String): Boolean;
begin
  Result := False;

  case UpperCase(ExtractFileExt(FileName)) of
    '.AAC' : Result := True;
    '.FLAC' : Result := True;
    '.MP3' : Result := True;
    '.OGG' : Result := True;
    '.OPUS' : Result := True;
    '.VOC' : Result := True;
    '.WFP' : Result := True;
  end;
end;

function TfMain.isVideo(const FileName: String): Boolean;
begin
  Result := False;

  case UpperCase(ExtractFileExt(FileName)) of
    '.AVI' : Result := True;
    '.AVC' : Result := True;
    '.BDMV' : Result := True;
    '.H264' : Result := True;
    '.M2V' : Result := True;
    '.M4S' : Result := True;
    '.MJPEG' : Result := True;
    '.MKV' : Result := True;
    '.MOV' : Result := True;
    '.MP4' : Result := True;
    '.MP5' : Result := True;
    '.MPEG' : Result := True;
    '.MPV' : Result := True;
    '.SRT' : Result := True;
    '.STR' : Result := True;
    '.VID' : Result := True;
    '.WEBM' : Result := True;
    '.WLMP' : Result := True;
    '.WMV' : Result := True;
    '.XVID' : Result := True;
  end;
end;

function TfMain.isImage(const FileName: String): Boolean;
begin
  Result := False;

  case UpperCase(ExtractFileExt(FileName)) of
    '.BMP' : Result := True;
    '.GIF' : Result := True;
    '.HDR' : Result := True;
    '.HEIC' : Result := True;
    '.HEIF' : Result := True;
    '.ICO' : Result := True;
    '.JPG' : Result := True;
    '.JPEG' : Result := True;
    '.PNG' : Result := True;
    '.RAW' : Result := True;
    '.RPF' : Result := True;
    '.SVG' : Result := True;
    '.WEBP' : Result := True;
  end;
end;

{ TfMain }

procedure TfMain.FormCreate(Sender: TObject);
begin
  SetElementGlyphs();

  { #todo 1 -okfilippenok -cDoubleBuffered : Выяснить работает ли DoubleBuffered }
  // DoubleBuffered
  fMain.DoubleBuffered := True;
  panAudio.DoubleBuffered:=True;
  panVideo.DoubleBuffered:=True;

  // Состояние звука (Аудио)
  glAudioMute := False;

  // Воспроизводимый на данный момент файл (Аудио)
  glCurrentAudioItem := '';
  glCurrentAudioItemIndex := -1;

  // Время устанавливается в нулевое положение (Аудио)
  resetTime('AudioPlayer');

  // Иницализация хранилищ для списков имён и их путей (Аудио)
  glAudioFileNames := TStringList.Create;
  glAudioFilePaths := TStringList.Create;
  // Заполняем список (Аудио)
  clboxAudioPlaylist.Items := glAudioFileNames;

  // Создаём списки сценариев
  glScenarioFileNames := TStringList.Create;
  glScenarioFilePaths := TStringList.Create;

  // Вкладки
  TabControl.Tabs.Add('Default');
  glScenarioFileNames.Add('Default');
  glScenarioFilePaths.Add('');

  // Списки зависимостей
  glBonds := CBonds.Create;

  // Состояние звука (Видео)
  glVideoMute := False;

  // Воспроизводимый на данный момент файл (Видео)
  glCurrentVideoItem := '';
  glCurrentVideoItemIndex := -1;

  // Время устанавливается в нулевое положение (Видео)
  resetTime('VideoPlayer');

  // Иницализация хранилищ для списков имён и их путей (Видео)
  glVideoFileNames := TStringList.Create;
  glVideoFilePaths := TStringList.Create;
  // Заполняем список (Видео)
  clboxVideoPlaylist.Items := glVideoFileNames;

end;

procedure TfMain.clboxAudioPlaylistDblClick(Sender: TObject);
var
  i: word;
  IndexBond: Integer;
begin
  with fPlaybackAudio do
  begin
    if (fPlaybackAudio.audioPlayer = nil) then Exit;
    if (clboxAudioPlaylist.Count = 0) then Exit;

    glCurrentAudioItem := glAudioFilePaths[clboxAudioPlaylist.ItemIndex];
    glCurrentAudioItemIndex := clboxAudioPlaylist.ItemIndex;
    lblCurrentAudioItem.Caption := ExtractFileName(glCurrentAudioItem);
    audioPlayer.OpenFile(glCurrentAudioItem);

    { #todo 1 -okfilippenok -cBonds : Доделать одновременный запуск }

    { Связи }
    IndexBond := glBonds.IndexOfProvoking(clboxAudioPlaylist.ItemIndex);
    if IndexBond <> -1 then
      begin
        clboxVideoPlaylist.OnDblClick(clboxVideoPlaylist);
      end;
    { Связи }

    // Время устанавливается в нулевое положение
    resetTime('AudioPlayer');
    TimerAudio.Enabled := True;

    clboxAudioPlaylist.Repaint;

    // Изменение состояния элементов воспроизведения
    setGlyphSpeedButton(sbtnAudioStop, 'icons' + PathDelim + 'stop.png');
    setGlyphSpeedButton(sbtnAudioPause, 'icons' + PathDelim + 'pause.png');
    setGlyphSpeedButton(sbtnAudioPlay, 'icons' + PathDelim + 'play.png');
  end;
end;

procedure TfMain.clboxVideoPlaylistClick(Sender: TObject);
begin

  //ShowMessageFmt('ItemIndex: %d', [clboxVideoPlaylist.ItemIndex]);
end;

procedure TfMain.clboxAudioPlaylistClick(Sender: TObject);
begin
  // !? Выбран ли элемент
  if clboxAudioPlaylist.ItemIndex = -1 then
    Exit;

  miAudioDeleteBond.Visible := False;

  // !? Если ли в связях
  if not(glBonds.InProvoking(clboxAudioPlaylist.ItemIndex)) then
    Exit;

  miAudioDeleteBond.Visible := True;

  { #todo 1 -okfilippenok -cBonds : Доделать выделение по клику }
  clboxVideoPlaylist.ClearSelection;
  clboxVideoPlaylist.Selected[glBonds.GetInvokingWhereProvoking(clboxAudioPlaylist.ItemIndex)] := True;
end;

procedure TfMain.clboxVideoPlaylistDblClick(Sender: TObject);
var
  i: word;
begin
  with fPlaybackVideo do
  begin
    if (fPlaybackVideo.videoPlayer = nil) then Exit;
    if (clboxVideoPlaylist.Count = 0) then Exit;

    glCurrentVideoItem := glVideoFilePaths[clboxVideoPlaylist.ItemIndex];
    glCurrentVideoItemIndex := clboxVideoPlaylist.ItemIndex;
    lblCurrentVideoItem.Caption := ExtractFileName(glCurrentVideoItem);
    VideoPlayer.OpenFile(glCurrentVideoItem);

    // Время устанавливается в нулевое положение
    resetTime('VideoPlayer');
    TimerVideo.Enabled := True;

    clboxVideoPlaylist.Repaint;

    // Изменение состояния элементов воспроизведения
    setGlyphSpeedButton(sbtnVideoStop, 'icons' + PathDelim + 'stop.png');
    setGlyphSpeedButton(sbtnVideoPause, 'icons' + PathDelim + 'pause.png');
    setGlyphSpeedButton(sbtnVideoPlay, 'icons' + PathDelim + 'play.png');

    setFullyDisplay(True);
  end;
end;

procedure TfMain.clboxVideoPlaylistItemClick(Sender: TObject; Index: integer);
begin
  clboxVideoPlaylist.Invalidate;
end;

procedure TfMain.clboxVideoPlaylistKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  clboxVideoPlaylist.Invalidate;
end;

procedure TfMain.clboxVideoPlaylistMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  clboxVideoPlaylist.Invalidate;
end;

procedure TfMain.FormDestroy(Sender: TObject);
var i: Integer;
    bbtnBond: TBitBtn;
begin
  FreeAndNil(glAudioFileNames);
  FreeAndNil(glAudioFilePaths);
  FreeAndNil(glVideoFileNames);
  FreeAndNil(glVideoFilePaths);
  FreeAndNil(glScenarioFileNames);
  FreeAndNil(glScenarioFilePaths);
  if glBonds.Count <> 0 then
    begin
      for i := 0 to glBonds.Count - 1 do
        begin
          if fMain.FindComponent('bbtnBondVideo' + IntToStr(glBonds.arProvoking[i])) <> NIL then
            begin
              bbtnBond := (fMain.FindComponent('bbtnBondVideo' + IntToStr(glBonds.arProvoking[i])) as TBitBtn);
              FreeAndNil(bbtnBond);
            end;
        end;
    end;
  FreeAndNil(glBonds);
  FreeAndNil(glBonds);
end;

procedure TfMain.AddBond(Sender: TObject);
var bbtnFindedVideoBond: TBitBtn;
begin
  if clboxAudioPlaylist.ItemIndex = -1 then
  begin
    ShowMessage('Аудио не выбрано.');
    Exit;
  end;
  if clboxVideoPlaylist.ItemIndex = -1 then
  begin
    ShowMessage('Видео не выбрано.');
    Exit;
  end;

  if glBonds.InProvoking(clboxAudioPlaylist.ItemIndex) then
    begin
      glBonds.DeleteWhereProvoking(clboxAudioPlaylist.ItemIndex);
    end;

  glBonds.Add(clboxAudioPlaylist.ItemIndex, clboxVideoPlaylist.ItemIndex);

  if fMain.FindComponent('bbtnBondVideo' + IntToStr(clboxAudioPlaylist.ItemIndex)) <> NIL then
    begin
      bbtnFindedVideoBond := (fMain.FindComponent('bbtnBondVideo' + IntToStr(clboxAudioPlaylist.ItemIndex)) as TBitBtn);
      bbtnFindedVideoBond.Caption := IntToStr(glBonds.arInvoking[glBonds.IndexOfProvoking(clboxAudioPlaylist.ItemIndex)]);
    end;

  miAudioDeleteBond.Visible := True;

  clboxAudioPlaylist.Repaint;
end;

procedure TfMain.FormDropFiles(Sender: TObject; const FileNames: array of string
  );
const  mrAudio = 21;
       mrVideo = 22;
       mrSkeep = 23;
var i: Word;
begin
  for i := 0 to Length(FileNames)-1 do
    begin
      if isAudio(FileNames[i]) then
        begin
          glAudioFilePaths.Add(FileNames[i]);
          glAudioFileNames.Add(ExtractFileName(FileNames[i]));
        end
      else if isVideo(FileNames[i]) or isImage(FileNames[i]) then
        begin
          glVideoFilePaths.Add(FileNames[i]);
          glVideoFileNames.Add(ExtractFileName(FileNames[i]));
        end
      else
        begin
          case QuestionDlg('Неизвестный тип файла', 'Тип ' + ExtractFileExt(FileNames[i]) + ' файла ' + ExtractFileName(FileNames[i]) + ' неизвестен. Как воспринимать файл?', mtCustom, [mrAudio, 'Аудио', mrVideo, 'Видео', mrSkeep, 'Пропустить'], '') of
            mrAudio:
              begin
                glAudioFilePaths.Add(FileNames[i]);
                glAudioFileNames.Add(ExtractFileName(FileNames[i]));
              end;
            mrVideo:
              begin
                glVideoFilePaths.Add(FileNames[i]);
                glVideoFileNames.Add(ExtractFileName(FileNames[i]));
              end;
          end;
        end;
    end;

  clboxAudioPlaylist.Items := glAudioFileNames;
  clboxVideoPlaylist.Items := glVideoFileNames;
end;

procedure TfMain.FormResize(Sender: TObject);

  procedure setRowView;
  begin
    panAudio.Anchors := [];
    panAudio.Visible := False;
    panVideo.Anchors := [];
    panVideo.Visible := False;

    Splitter.Anchors := [];
    Splitter.AnchorParallel(akTop, 0, TabControl);
    Splitter.AnchorParallel(akBottom, 0, fMain);
    Splitter.ResizeAnchor := akLeft;
    Splitter.Width := 5;
    Splitter.SetSplitterPosition(fMain.Width div 2 - Splitter.Width div 2);

    panAudio.AnchorToNeighbour(akTop, 0, TabControl);
    panAudio.AnchorParallel(akLeft, 0, fMain);
    panAudio.AnchorParallel(akBottom, 0, fMain);
    panAudio.AnchorToNeighbour(akRight, 0, Splitter);
    panAudio.Visible := True;

    panVideo.AnchorToNeighbour(akTop, 0, TabControl);
    panVideo.AnchorParallel(akRight, 0, fMain);
    panVideo.AnchorParallel(akBottom, 0, fMain);
    panVideo.AnchorToNeighbour(akLeft, 0, Splitter);
    panVideo.Visible := True;
  end;

  procedure setColumnView;
  begin
    panAudio.Anchors := [];
    panAudio.Visible := False;
    panVideo.Anchors := [];
    panVideo.Visible := False;

    Splitter.Anchors := [];
    Splitter.AnchorParallel(akLeft, 0, fMain);
    Splitter.AnchorParallel(akRight, 0, fMain);
    Splitter.ResizeAnchor := akTop;
    Splitter.Height := 5;
    Splitter.SetSplitterPosition((fMain.Height div 2 - Splitter.Height div 2) + TabControl.Height);

    panAudio.AnchorToNeighbour(akTop, 0, TabControl);
    panAudio.AnchorParallel(akLeft, 0, fMain);
    panAudio.AnchorToNeighbour(akBottom, 0, Splitter);
    panAudio.AnchorParallel(akRight, 0, fMain);
    panAudio.Visible := True;

    panVideo.AnchorToNeighbour(akTop, 0, Splitter);
    panVideo.AnchorParallel(akRight, 0, fMain);
    panVideo.AnchorParallel(akBottom, 0, fMain);
    panVideo.AnchorParallel(akLeft, 0, fMain);
    panVideo.Visible := True;
  end;

begin
  if not(glAdaptivePanels) then
    Exit;

  if fMain.Height > 500 then
    begin
      setColumnView;
    end
  else
    begin
      setRowView;
    end;
end;

procedure TfMain.miAudioDeleteBondClick(Sender: TObject);
var bbtnFindedVideoBond: TBitBtn;
begin
  glBonds.DeleteWhereProvoking(clboxAudioPlaylist.ItemIndex);

  if fMain.FindComponent('bbtnBondVideo' + IntToStr(clboxAudioPlaylist.ItemIndex)) <> NIL then
    begin
      bbtnFindedVideoBond := (fMain.FindComponent('bbtnBondVideo' + IntToStr(clboxAudioPlaylist.ItemIndex)) as TBitBtn);
      FreeAndNil(bbtnFindedVideoBond);
    end;

  clboxAudioPlaylist.Repaint;
end;

procedure TfMain.miAudioDeleteClick(Sender: TObject);
begin
  miAudioDeleteBondClick(Self);
  sbtnAudioSubtract.Click;
end;

procedure TfMain.muVideoDeleteClick(Sender: TObject);
begin
  sbtnVideoSubtract.Click;
end;

procedure TfMain.clboxVideoPlaylistDrawItem(Control: TWinControl;
  Index: integer; ARect: TRect; State: TOwnerDrawState);
var
  savedItem: string;
  cSize: TSize;
  details: TThemedElementDetails;
  CheckBoxState: TThemedButton;
  ReceivedState: TOwnerDrawState absolute State;
begin
  with clboxVideoPlaylist do
  begin
    // Забираем текст текущего элемента в переменную,
    // для дальнейшей возможной модификации
    savedItem := Items[Index];

    // Фоновый цвет файлов
    if ExtractFileExt(clboxVideoPlaylist.Items[Index]) = '.mp4' then
      Canvas.Brush.Color := RGBToColor(222, 218, 244)
    else
      Canvas.Brush.Color := RGBToColor(192, 248, 191);

    if (Index = glCurrentVideoItemIndex) and (clboxVideoPlaylist.Items[Index] = lblCurrentVideoItem.Caption) then
    begin
      Canvas.Brush.Color := RGBToColor(146, 146, 146);
      Canvas.Font.Color := clWhite;
    end;

    // Выделенная строка
    if (odSelected in ReceivedState) then
    begin
      if ExtractFileExt(clboxVideoPlaylist.Items[Index]) = '.mp4' then
        Canvas.Brush.Color := RGBToColor(144, 89, 223)
      else
        Canvas.Brush.Color := RGBToColor(32, 204, 29);
      Canvas.Font.Color := clWhite;
    end;

    // Готовим холст к отрисовке
    Canvas.FillRect(ARect);

    // Отрисовываем текст с заданными параметрами
    Canvas.TextOut(ARect.Left + 23, (ARect.Top + ARect.Height div 4), savedItem);

    { Связи }{
    if glBondsVideo.IndexOf(savedItem) <> -1 then
      Canvas.TextOut(ARect.Left + 250, (ARect.Top + ARect.Height div 4), 'Cвязь: '+IntToStr(glBondsVideo.IndexOf(savedItem)));
    }{ Связи }

    { Номер элемента }
    Canvas.TextOut(ARect.Left + 5, (ARect.Top + ARect.Height div 4), IntToStr(Index));
    { Номер элемента }

    { Checkbox }{
    // Проверка состояния CheckBox'а
    if Checked[Index] then CheckBoxState := tbCheckBoxCheckedNormal
    else
      CheckBoxState := tbCheckBoxUncheckedNormal;

    // Настраиваем детали CheckBox'а
    Details := ThemeServices.GetElementDetails(CheckBoxState);
    cSize := ThemeServices.GetDetailSize(Details);
    with ARect do
    begin
      Left := 3;
      Top := Trunc((ARect.Top + ARect.Bottom - cSize.cy) / 2);
      ARect := Bounds(Left, Top, cSize.cx, cSize.cy);
    end;

    // Вставляем CheckBox
    ThemeServices.DrawElement(Canvas.Handle, Details, ARect, nil);
    }{ Checkbox }
  end;
end;

procedure TfMain.sbtnVideoVolumeClick(Sender: TObject);
begin
  glVideoMute := not (glVideoMute);

  if glVideoMute then
  begin
    setGlyphSpeedButton(sbtnVideoVolume, 'icons' + PathDelim + 'speaker_off.png');
    fPlaybackVideo.videoPlayer.SetMute(True);
  end
  else
  begin
    if trbarVideoVolume.Position < 33 then
      setGlyphSpeedButton(sbtnVideoVolume, 'icons' + PathDelim + 'speaker_0.png')
    else if trbarVideoVolume.Position < 66 then
      setGlyphSpeedButton(sbtnVideoVolume, 'icons' + PathDelim + 'speaker_1.png')
    else
      setGlyphSpeedButton(sbtnVideoVolume, 'icons' + PathDelim + 'speaker_2.png');

    setGlyphSpeedButton(sbtnVideoVolume, 'icons' + PathDelim + 'speaker_2.png');
    fPlaybackVideo.videoPlayer.SetMute(False);
    // На тот случай, если ползунок был изменен пока не было звука
    fPlaybackVideo.videoPlayer.SetVolume(double(trbarVideoVolume.Position));
  end;
end;

procedure TfMain.sbtnVideoClearClick(Sender: TObject);
var
  i: integer;
begin
  i := 0;
  while i <> glVideoFileNames.Count do
  begin
    if glVideoFileNames.Strings[i] <> glCurrentVideoItem then
    begin
      glVideoFileNames.Delete(i);
      glVideoFilePaths.Delete(i);
    end
    else
      i += 1;
  end;
  clboxVideoPlaylist.Items := glVideoFileNames;
end;

procedure TfMain.clboxAudioPlaylistDrawItem(Control: TWinControl;
  Index: integer; ARect: TRect; State: TOwnerDrawState);
var
  savedItem: string;
  cSize: TSize;
  details: TThemedElementDetails;
  CheckBoxState: TThemedButton;
  ReceivedState: TOwnerDrawState absolute State;
  BondRect: TRect;
  bbtnVideoBond: TBitBtn;
  bbtnFindedVideoBond: TBitBtn;
begin
  with clboxAudioPlaylist do
  begin
    // Забираем текст текущего элемента в переменную,
    // для дальнейшей возможной модификации
    savedItem := Items[Index];

    // Присваиваем нужный цвет фону элементам
    Canvas.Brush.Color := RGBToColor(244, 218, 218);

    if (Index = glCurrentAudioItemIndex) and (clboxAudioPlaylist.Items[Index] = lblCurrentAudioItem.Caption) then
    begin
      Canvas.Brush.Color := RGBToColor(146, 146, 146);
      Canvas.Font.Color := clWhite;
    end;

    // Выделенная строка
    if (odSelected in ReceivedState) then
    begin
      Canvas.Brush.Color := RGBToColor(255, 92, 92);
      Canvas.Font.Color := clWhite;
    end;

    // Готовим холст к отрисовке
    Canvas.FillRect(ARect);

    // Отрисовываем текст с заданными параметрами
    Canvas.TextOut(ARect.Left + 23, (ARect.Top + ARect.Height div 4), savedItem);

    { Связи }
    if glBonds.IndexOfProvoking(Index) <> -1 then
    begin
      if fMain.FindComponent('bbtnBondVideo' + IntToStr(Index)) <> NIL then
        begin
          bbtnFindedVideoBond := (fMain.FindComponent('bbtnBondVideo' + IntToStr(Index)) as TBitBtn);
          bbtnFindedVideoBond.Left := ARect.Right - 54;
          bbtnFindedVideoBond.Top := ARect.Top + 5;
        end
      else
        begin
          //ShowMessage('bbtnBondVideo' + IntToStr(Index) + ' is created');
          bbtnVideoBond := TBitBtn.Create(fMain);
          setGlyphSpeedButton(bbtnVideoBond, 'icons'+PathDelim+'video.png');
          bbtnVideoBond.Parent := clboxAudioPlaylist;
          bbtnVideoBond.Name := 'bbtnBondVideo' + IntToStr(Index);
          bbtnVideoBond.Height := 30;
          bbtnVideoBond.Width := 44;
          bbtnVideoBond.Caption := IntToStr(glBonds.GetInvokingWhereProvoking(Index));
          bbtnVideoBond.Top := ARect.Top + 5;
          bbtnVideoBond.Left := ARect.Right - 54;
          bbtnVideoBond.OnClick := @BondButtonClick;
        end;
    end;
    { Связи }

    { Номер элемента }
    Canvas.TextOut(ARect.Left + 5, (ARect.Top + ARect.Height div 4), IntToStr(Index));
    { Номер элемента }

    { Checkbox }{
    // Проверка состояния CheckBox'а
    if Checked[Index] then
      CheckBoxState := tbCheckBoxCheckedNormal
    else
      CheckBoxState := tbCheckBoxUncheckedNormal;

    // Настраиваем детали CheckBox'а
    Details := ThemeServices.GetElementDetails(CheckBoxState);
    cSize := ThemeServices.GetDetailSize(Details);
    with ARect do
    begin
      Left := 3;
      Top := Trunc((ARect.Top + ARect.Bottom - cSize.cy) / 2);
      ARect := Bounds(Left, Top, cSize.cx, cSize.cy);
    end;

    // Вставляем CheckBox
    ThemeServices.DrawElement(Canvas.Handle, Details, ARect, nil);
    }{ Checkbox }
  end;
end;

procedure TfMain.sbtnVideoSubtractClick(Sender: TObject);
var
  i, del_count: word;
begin
  i := 0;
  del_count := 0;
  while i < clboxVideoPlaylist.Count do
  begin
    if clboxVideoPlaylist.Selected[i] then
    begin
      if clboxVideoPlaylist.Items[i] <> glCurrentAudioItem then
      begin
        glVideoFileNames.Delete(i - del_count);
        glVideoFilePaths.Delete(i - del_count);
        del_count += 1;
      end;
    end;
    i += 1;
  end;
  clboxVideoPlaylist.Items := glVideoFileNames;
end;

procedure TfMain.sbtnVideoPlayClick(Sender: TObject);
begin
  with fPlaybackVideo do
  begin
    if (videoPlayer = nil) then Exit;
    if (clboxVideoPlaylist.Count = 0) then Exit;

    videoPlayer.Resume;
    // Включаем таймер
    TimerVideo.Enabled := True;

    // Переотрисовка списка для корректного отображения
    // проиграиваемого элемента
    clboxVideoPlaylist.Repaint;

    // Включаем таймер
    TimerVideo.Enabled := True;

    // Изменение состояния элементов воспроизведения
    setGlyphSpeedButton(sbtnVideoStop, 'icons' + PathDelim + 'stop.png');
    setGlyphSpeedButton(sbtnVideoPause, 'icons' + PathDelim + 'pause.png');
    setGlyphSpeedButton(sbtnVideoPlay, 'icons' + PathDelim + 'play_active.png');
  end;
end;



{ Меню }

procedure TfMain.ScenarioNewClick(Sender: TObject);
begin
  TabControl.Tabs.Add('Untitled');
  TabControl.TabIndex := TabControl.Tabs.Count-1;
  glScenarioFileNames.Add('Untitled');
  glScenarioFilePaths.Add('');

  glAudioFilePaths.Clear();
  glAudioFileNames.Clear();
  glVideoFilePaths.Clear();
  glVideoFileNames.Clear();
  clboxAudioPlaylist.Items := glAudioFilePaths;
  clboxVideoPlaylist.Items := glAudioFilePaths;

  // Очищаем сценарный файл в диалоге
  SaveDialog.FileName := '';
end;

procedure TfMain.LoadScenarioFromJSON(FilePath: String);
var
  jdScenario: TJSONData = NIL;
  jaAudioFilePaths, jaVideoFilePaths: TJSONArray;
  i: Integer;
begin
  // Читаем из файла
  jdScenario := GetJSON(ReadFileToString(FilePath));
  // Получаем массив JSON со списком файлов
  jaAudioFilePaths := (jdScenario.FindPath('AudioFilePaths') as TJSONArray); (* Аудио *)
  jaVideoFilePaths := (jdScenario.FindPath('VideoFilePaths') as TJSONArray); (* Видео *)

  // Очищяем список файлов (Аудио)
  glAudioFilePaths.Clear;
  glAudioFileNames.Clear;
  // Заполняем список файлов (Аудио)
  if jaAudioFilePaths.Count > 0 then
    for i := 0 to jaAudioFilePaths.Count-1 do
    begin
      if FileExists(jaAudioFilePaths.Strings[i]) then
      begin
        glAudioFilePaths.Add(jaAudioFilePaths.Strings[i]);
        glAudioFileNames.Add(ExtractFileName(jaAudioFilePaths.Strings[i]));
      end;
    end;
  clboxAudioPlaylist.Items := glAudioFileNames;

  // Очищяем список файлов (Видео)
  glVideoFilePaths.Clear;
  glVideoFileNames.Clear;
  // Заполняем список файлов (Видео)
  if jaVideoFilePaths.Count > 0 then
    for i := 0 to jaVideoFilePaths.Count-1 do
    begin
      if FileExists(jaVideoFilePaths.Strings[i]) then
      begin
        glVideoFilePaths.Add(jaVideoFilePaths.Strings[i]);
        glVideoFileNames.Add(ExtractFileName(jaVideoFilePaths.Strings[i]));
      end;
    end;
  clboxVideoPlaylist.Items := glVideoFileNames;

  FreeAndNil(jdScenario);
end;

procedure TfMain.ScenarioOpenClick(Sender: TObject);
begin
  OpenDialog.FileName := '';
  OpenDialog.Title := 'Открыть сценарий';
  OpenDialog.DefaultExt := '.json';
  OpenDialog.Filter := 'JSON|*.json|Все файлы|*.*|';
  if OpenDialog.Execute then
  begin
    //FileStream := TFileStream.Create(OpenDialog.FileName, fmOpenRead);
    //FreeAndNil(FileStream);
    TabControl.Tabs.Add(OpenDialog.FileName);
    TabControl.TabIndex := TabControl.Tabs.Count-1;
    LoadScenarioFromJSON(OpenDialog.FileName);

    // Копируем имя файла в диалог SaveDialog,
    // чтобы потом знать, куда сохранять:
    SaveDialog.FileName := OpenDialog.FileName;
  end;
end;

procedure TfMain.ScenarioSaveClick(Sender: TObject);
var jObject: TJSONObject;
    jarArrayAudio, jarArrayVideo, jarAudioBondsProvoking, jarAudioBondsInvoking: TJSONArray;
    i: Word;
    StringList: TStringList;
begin
  // Если имя файла был открыт ранее
  if SaveDialog.FileName <> '' then
  begin
    jObject := TJSONObject.Create;

    jarArrayAudio := TJSONArray.Create;
    if glAudioFilePaths.Count > 0 then
    for i := 0 to glAudioFilePaths.Count-1 do
    begin
      jarArrayAudio.Add(glAudioFilePaths.Strings[i]);
    end;
    jObject.Add('AudioFilePaths', jarArrayAudio);

    jarAudioBondsProvoking := TJSONArray.Create;
    if glBonds.Count > 0 then
    for i := 0 to glBonds.Count-1 do
    begin
      jarAudioBondsProvoking.Add(glBonds.arProvoking[i]);
    end;
    jObject.Add('AudioBondsProvoking', jarAudioBondsProvoking);

    jarAudioBondsInvoking := TJSONArray.Create;
    if glBonds.Count > 0 then
    for i := 0 to glBonds.Count-1 do
    begin
      jarAudioBondsInvoking.Add(glBonds.arInvoking[i]);
    end;
    jObject.Add('AudioBondsInvoking', jarAudioBondsInvoking);

    jarArrayVideo := TJSONArray.Create;
    if glVideoFilePaths.Count > 0 then
    for i := 0 to glVideoFilePaths.Count-1 do
    begin
      jarArrayVideo.Add(glVideoFilePaths.Strings[i]);
    end;
    jObject.Add('VideoFilePaths', jarArrayVideo);

    StringList := TStringList.Create;
    StringList.Add(jObject.FormatJSON);
    StringList.SaveToFile(UTF8ToSys(SaveDialog.FileName));

    FreeAndNil(jObject);
    FreeAndNil(StringList);
  end
  // Если имя файл не существует
  else
    ScenarioSaveAsClick(Sender);
end;

procedure TfMain.ScenarioSaveAsClick(Sender: TObject);
var JSONObject: TJSONObject;
    JSONArrayAudio, JSONArrayVideo: TJSONArray;
    i: Word;
    StringList: TStringList;
begin
  SaveDialog.Title := 'Сохранить сценарий как';
  SaveDialog.DefaultExt := '.json';
  SaveDialog.Filter := 'JSON|*.json|Все файлы|*.*|';
  if SaveDialog.Execute then
  begin
    JSONObject := TJSONObject.Create;

    JSONArrayAudio := TJSONArray.Create;
    if glAudioFilePaths.Count > 0 then
    for i := 0 to glAudioFilePaths.Count-1 do
    begin
      JSONArrayAudio.Add(glAudioFilePaths.Strings[i]);
    end;
    JSONObject.Add('AudioFilePaths', JSONArrayAudio);

    JSONArrayVideo := TJSONArray.Create;
    if glVideoFilePaths.Count > 0 then
    for i := 0 to glVideoFilePaths.Count-1 do
    begin
      JSONArrayVideo.Add(glVideoFilePaths.Strings[i]);
    end;
    JSONObject.Add('VideoFilePaths', JSONArrayVideo);

    StringList := TStringList.Create;
    StringList.Add(JSONObject.FormatJSON);
    StringList.SaveToFile(Utf8ToSys(SaveDialog.FileName));

    TabControl.Tabs.Strings[TabControl.TabIndex] := SaveDialog.FileName;

    FreeAndNil(JSONObject);
    FreeAndNil(StringList);
  end;
end;

procedure TfMain.ScenarioExitClick(Sender: TObject);
begin
  Close;
end;

procedure TfMain.SettingsClick(Sender: TObject);
begin
  fSettings.Show;
end;

procedure TfMain.InfoAboutProgramClick(Sender: TObject);
begin
  fAboutProject.Show;
end;


{ Список элементов }

procedure TfMain.sbtnAudioAddClick(Sender: TObject);
var
  FilePath: string;
begin
  OpenDialog.Title := 'Добавить элемент';
  OpenDialog.DefaultExt := '';
  OpenDialog.Filter := 'Media|*.mp4; *.mp3; *.jpg; *.jpeg; *.png|All files|*.*';
  if OpenDialog.Execute then
  begin
    for FilePath in OpenDialog.Files do
    begin
      if Sender = sbtnAudioAdd then
      begin
        glAudioFilePaths.Add(FilePath);
        glAudioFileNames.Add(ExtractFileName(FilePath));
      end;
      if Sender = sbtnVideoAdd then
      begin
        glVideoFilePaths.Add(FilePath);
        glVideoFileNames.Add(ExtractFileName(FilePath));
      end;
    end;

    if Sender = sbtnAudioAdd then clboxAudioPlaylist.Items := glAudioFileNames;
    if Sender = sbtnVideoAdd then clboxVideoPlaylist.Items := glVideoFileNames;
  end;
end;

procedure TfMain.sbtnAudioSubtractClick(Sender: TObject);
var
  i, del_count: word;
begin
  i := 0;
  del_count := 0;
  while i < clboxAudioPlaylist.Count do
  begin
    if clboxAudioPlaylist.Selected[i] then
    begin
      if clboxAudioPlaylist.Items[i] <> glCurrentAudioItem then
      begin
        glAudioFileNames.Delete(i - del_count);
        glAudioFilePaths.Delete(i - del_count);
        del_count += 1;
      end;
    end;
    i += 1;
  end;
  clboxAudioPlaylist.Items := glAudioFileNames;
end;

procedure TfMain.sbtnAudioClearClick(Sender: TObject);
var
  i: integer;
begin
  i := 0;
  while i <> glAudioFileNames.Count do
  begin
    if glAudioFileNames.Strings[i] <> glCurrentAudioItem then
    begin
      glAudioFileNames.Delete(i);
      glAudioFilePaths.Delete(i);
    end
    else
      i += 1;
  end;
  clboxAudioPlaylist.Items := glAudioFileNames;
end;


{ Воспроизведение }

procedure TfMain.sbtnAudioPlayClick(Sender: TObject);
begin
  with fPlaybackAudio do
  begin
    if (audioPlayer = nil) then Exit;
    if (clboxAudioPlaylist.Count = 0) then Exit;

    audioPlayer.Resume;
    // Включаем таймер
    TimerAudio.Enabled := True;

    // Переотрисовка списка для корректного отображения
    // проиграиваемого элемента
    clboxAudioPlaylist.Repaint;

    // Включаем таймер
    TimerAudio.Enabled := True;

    // Изменение состояния элементов воспроизведения
    setGlyphSpeedButton(sbtnAudioStop, 'icons' + PathDelim + 'stop.png');
    setGlyphSpeedButton(sbtnAudioPause, 'icons' + PathDelim + 'pause.png');
    setGlyphSpeedButton(sbtnAudioPlay, 'icons' + PathDelim + 'play_active.png');
  end;
end;

procedure TfMain.MediaPause(Sender: TObject);
begin
  if Sender = sbtnAudioPause then
  begin
    with fPlaybackAudio do
    begin
      if (audioPlayer = nil) then Exit;

      audioPlayer.Pause;
    end;
    TimerAudio.Enabled := False;

    // Изменение состояния элементов воспроизведения
    setGlyphSpeedButton(sbtnAudioStop, 'icons' + PathDelim + 'stop.png');
    setGlyphSpeedButton(sbtnAudioPlay, 'icons' + PathDelim + 'play.png');
    setGlyphSpeedButton(sbtnAudioPause, 'icons' + PathDelim + 'pause_active.png');
  end
  else if Sender = sbtnVideoPause then
  begin
    with fPlaybackVideo do
    begin
      if (videoPlayer = nil) then Exit;

      videoPlayer.Pause;
    end;
    TimerVideo.Enabled := False;

    // Изменение состояния элементов воспроизведения
    setGlyphSpeedButton(sbtnVideoStop, 'icons' + PathDelim + 'stop.png');
    setGlyphSpeedButton(sbtnVideoPlay, 'icons' + PathDelim + 'play.png');
    setGlyphSpeedButton(sbtnVideoPause, 'icons' + PathDelim + 'pause_active.png');
  end;
end;

procedure TfMain.MediaStop(Sender: TObject);
var
  i: word;
begin
  if Sender = sbtnAudioStop then
  begin
    if (fPlaybackAudio.audioPlayer = nil) then Exit;

    fPlaybackAudio.audioPlayer.Stop;

    // Изменение состояния элементов воспроизведения
    setGlyphSpeedButton(sbtnAudioPause, 'icons' + PathDelim + 'pause.png');
    setGlyphSpeedButton(sbtnAudioPlay, 'icons' + PathDelim + 'play.png');
    setGlyphSpeedButton(sbtnAudioStop, 'icons' + PathDelim + 'stop_active.png');

    // "Проигриваемый элемент" очищается (Аудио)
    glCurrentAudioItem := '';
    glCurrentAudioItemIndex := -1;
    lblCurrentAudioItem.Caption := '';
    // Список элементов переотрисовывается (Аудио)
    clboxAudioPlaylist.Repaint;
    // Выключаем таймер (Аудио)
    TimerAudio.Enabled := False;

    // Время устанавливается в нулевое положение (Аудио)
    resetTime('AudioPlayer');
  end
  else if Sender = sbtnVideoStop then
  begin
    if (fPlaybackVideo.VideoPlayer = nil) then Exit;

    // Выключаем окно воспроизведения
    setFullyDisplay(False);

    // Выключаем воспроизведение видео
    fPlaybackVideo.videoPlayer.Stop;

    // Изменение состояния элементов воспроизведения
    setGlyphSpeedButton(sbtnVideoPause, 'icons' + PathDelim + 'pause.png');
    setGlyphSpeedButton(sbtnVideoPlay, 'icons' + PathDelim + 'play.png');
    setGlyphSpeedButton(sbtnVideoStop, 'icons' + PathDelim + 'stop_active.png');

    // "Проигриваемый элемент" очищается (Видео)
    glCurrentVideoItem := '';
    glCurrentVideoItemIndex := -1;
    lblCurrentVideoItem.Caption := '';
    // Список элементов переотрисовывается (Видео)
    clboxVideoPlaylist.Invalidate;
    // Выключаем таймер (Видео)
    TimerVideo.Enabled := False;

    // Время устанавливается в нулевое положение (Видео)
    resetTime('VideoPlayer');
  end;
end;

procedure TfMain.TabControlChange(Sender: TObject);
var i: Integer;
    LBitBtn: TBitBtn;
begin
  if FileExists(TabControl.Tabs.Strings[TabControl.TabIndex]) then
    LoadScenarioFromJSON(TabControl.Tabs.Strings[TabControl.TabIndex])
  else
    begin
      glAudioFileNames.Clear;
      glAudioFilePaths.Clear;
      glVideoFileNames.Clear;
      glVideoFilePaths.Clear;
      clboxAudioPlaylist.Clear;
      clboxVideoPlaylist.Clear;
      for i:= 0 to glBonds.Count - 1 do
        begin
          if fMain.FindComponent('bbtnBondVideo' + IntToStr(glBonds.arProvoking[i])) <> NIL then
            begin
              LBitBtn := (fMain.FindComponent('bbtnBondVideo' + IntToStr(glBonds.arProvoking[i])) as TBitBtn);
              FreeAndNil(LBitBtn);
            end;
        end;
      glBonds.Clear;
    end;
end;

procedure TfMain.TabControlChanging(Sender: TObject; var AllowChange: Boolean);
begin
  if FileExists(TabControl.Tabs.Strings[TabControl.TabIndex]) then
  begin
    SaveDialog.FileName := TabControl.Tabs.Strings[TabControl.TabIndex];
    ScenarioSaveClick(Self);
  end;
end;

procedure TfMain.sbtnMonitorConfigureClick(Sender: TObject);
begin
  fMonitorConfigure.ShowModal;
end;

procedure TfMain.trbarAudioTimeChange(Sender: TObject);
begin
  lblAudioTimeCurrent.Caption := SecsToTimeStr(trbarAudioTime.Position);
  if trbarAudioTime.Position = trbarAudioTime.Max then sbtnAudioStop.Click;
end;

procedure TfMain.trbarAudioTimeMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  // Пользователь начал выбирать время
  glAudioTrackRewinding := True;
end;

procedure TfMain.trbarAudioTimeMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  // Пользователь выбрал нужное время
  trbarAudioTime.SelEnd := trbarAudioTime.Position;
  // Ставим прогресс на позицию метки
  fPlaybackAudio.audioPlayer.Seek(double(trbarAudioTime.Position), False);
  // Ставим на выбранную позицию
  TimerAudio.Enabled := True; // Включаем таймер
  glAudioTrackRewinding := False;
  // Трек больше не перематывается
end;


{ Окно воспроизведения }

procedure TfMain.sbtnFullyDisplayClick(Sender: TObject);
begin
  setFullyDisplay(Not(fPlaybackVideo.Visible));
end;


{ Громкость }

// Кнопка

procedure TfMain.sbtnAudioVolumeClick(Sender: TObject);
begin
  glAudioMute := not (glAudioMute);

  if glAudioMute then
  begin
    setGlyphSpeedButton(sbtnAudioVolume, 'icons' + PathDelim + 'speaker_off.png');
    fPlaybackAudio.audioPlayer.SetMute(True);
  end
  else
  begin
    if trbarAudioVolume.Position < 33 then
      setGlyphSpeedButton(sbtnAudioVolume, 'icons' + PathDelim + 'speaker_0.png')
    else if trbarAudioVolume.Position < 66 then
      setGlyphSpeedButton(sbtnAudioVolume, 'icons' + PathDelim + 'speaker_1.png')
    else
      setGlyphSpeedButton(sbtnAudioVolume, 'icons' + PathDelim + 'speaker_2.png');

    fPlaybackAudio.audioPlayer.SetMute(False);
    // На тот случай, если ползунок был изменен пока не было звука
    fPlaybackAudio.audioPlayer.SetVolume(double(trbarAudioVolume.Position));
  end;
end;

// Ползунок

procedure TfMain.trbarAudioVolumeChange(Sender: TObject);
begin
  trbarAudioVolume.SelEnd := trbarAudioVolume.Position;

  if not (glAudioMute) then
  begin
    if trbarAudioVolume.Position < 33 then
      setGlyphSpeedButton(sbtnAudioVolume, 'icons' + PathDelim + 'speaker_0.png')
    else if trbarAudioVolume.Position < 66 then
      setGlyphSpeedButton(sbtnAudioVolume, 'icons' + PathDelim + 'speaker_1.png')
    else
      setGlyphSpeedButton(sbtnAudioVolume, 'icons' + PathDelim + 'speaker_2.png');

    fPlaybackAudio.audioPlayer.SetVolume(double(trbarAudioVolume.Position));
  end;
end;

procedure TfMain.trbarAudioVolumeMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  if not (glAudioMute) then
  begin
    fPlaybackAudio.audioPlayer.SetVolume(double(trbarAudioVolume.Position));
  end;
end;

{ Таймер }

procedure TfMain.TimerMediaTimer(Sender: TObject);
begin
  if Sender = TimerAudio then
  begin
    // Длительность трека (Обновляется)
    trbarAudioTime.Max := Math.Ceil(fPlaybackAudio.audioPlayer.TotalSeconds);
    lblAudioTimeTotal.Caption := SecsToTimeStr(trbarAudioTime.Max);
    // Если трек не перематывается, то позиция метки повышается
    if not (glAudioTrackRewinding) then
      trbarAudioTime.Position := trbarAudioTime.Position + 1;
    // Прогресс текущего воспроизведения (Повышается)
    trbarAudioTime.SelEnd := trbarAudioTime.SelEnd + 1;
  end
  else if Sender = TimerVideo then
  begin
    // Длительность трека (Обновляется)
    trbarVideoTime.Max := Math.Ceil(fPlaybackVideo.videoPlayer.TotalSeconds);
    lblVideoTimeTotal.Caption := SecsToTimeStr(trbarVideoTime.Max);
    // Если трек не перематывается, то позиция метки повышается
    if not (glVideoTrackRewinding) then
      trbarVideoTime.Position := trbarVideoTime.Position + 1;
    // Прогресс текущего воспроизведения (Повышается)
    trbarVideoTime.SelEnd := trbarVideoTime.SelEnd + 1;
  end;
end;


{ #todo 1 -cВремя : Перевести код связанный со временем на тип данных времени в FreePascal }
procedure TfMain.trbarVideoTimeChange(Sender: TObject);
begin
  lblVideoTimeCurrent.Caption := SecsToTimeStr(trbarVideoTime.Position);
  if trbarVideoTime.Position = trbarVideoTime.Max then sbtnVideoStop.Click;
end;

procedure TfMain.trbarVideoTimeMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  // Пользователь начал выбирать время
  glVideoTrackRewinding := True;
end;

procedure TfMain.trbarVideoTimeMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  // Пользователь выбрал нужное время
  trbarVideoTime.SelEnd := trbarVideoTime.Position;
  // Ставим прогресс на позицию метки
  fPlaybackVideo.videoPlayer.Seek(double(trbarVideoTime.Position), False);
  // Ставим на выбранную позицию
  TimerVideo.Enabled := True; // Включаем таймер
  glVideoTrackRewinding := False;
  // Трек больше не перематывается
end;

procedure TfMain.trbarVideoVolumeChange(Sender: TObject);
begin
  trbarVideoVolume.SelEnd := trbarVideoVolume.Position;

  if not (glVideoMute) then
  begin
    if trbarVideoVolume.Position < 33 then
      setGlyphSpeedButton(sbtnVideoVolume, 'icons' + PathDelim + 'speaker_0.png')
    else if trbarVideoVolume.Position < 66 then
      setGlyphSpeedButton(sbtnVideoVolume, 'icons' + PathDelim + 'speaker_1.png')
    else
      setGlyphSpeedButton(sbtnVideoVolume, 'icons' + PathDelim + 'speaker_2.png');

    fPlaybackVideo.videoPlayer.SetVolume(double(trbarVideoVolume.Position));
  end;
end;

procedure TfMain.trbarVideoVolumeMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  if not (glVideoMute) then
    fPlaybackVideo.videoPlayer.SetVolume(double(trbarVideoVolume.Position));
end;

procedure TfMain.setFullyDisplay(ShowPlayback: Boolean);
begin
  // Проверка на выбранный монитор
  if MonitorConfigure.PlaybackMonitor = -1 then Exit;

  if ShowPlayback then
  begin
    fPLaybackVideo.Visible := True;
    setGlyphSpeedButton(sbtnFullyDisplay, 'icons' + PathDelim + 'share_screen_off.png');
  end
  else
  begin
    fPLaybackVideo.Visible := False;
    setGlyphSpeedButton(sbtnFullyDisplay, 'icons' + PathDelim + 'share_screen_on.png')
  end;
end;


{ Прочее }

procedure TfMain.resetTime(Player: string);
begin
  if LowerCase(Player) = LowerCase('AudioPlayer') then
  begin
    // Время устанавливается в нулевое положение
    trbarAudioTime.SelEnd := 0;
    trbarAudioTime.Position := 0;
    lblAudioTimeTotal.Caption := '00:00';
    lblAudioTimeCurrent.Caption := '00:00';
  end
  else if LowerCase(Player) = LowerCase('VideoPlayer') then
  begin
    // Время устанавливается в нулевое положение
    trbarVideoTime.SelEnd := 0;
    trbarVideoTime.Position := 0;
    lblVideoTimeTotal.Caption := '00:00';
    lblVideoTimeCurrent.Caption := '00:00';
  end;
end;

procedure TfMain.SetElementGlyphs;
begin
  // Аудио панель
  setGlyphSpeedButton(sbtnAudioAdd, 'icons' + PathDelim + 'add.png');
  setGlyphSpeedButton(sbtnAudioSubtract, 'icons' + PathDelim + 'subtract.png');
  setGlyphSpeedButton(sbtnAudioPlay, 'icons' + PathDelim + 'play.png');
  setGlyphSpeedButton(sbtnAudioPause, 'icons' + PathDelim + 'pause.png');
  setGlyphSpeedButton(sbtnAudioStop, 'icons' + PathDelim + 'stop.png');
  setGlyphSpeedButton(sbtnAudioClear, 'icons' + PathDelim + 'clear.png');
  setGlyphSpeedButton(sbtnAudioVolume, 'icons' + PathDelim + 'speaker_2.png');
  // Видео панель
  setGlyphSpeedButton(sbtnVideoAdd, 'icons' + PathDelim + 'add.png');
  setGlyphSpeedButton(sbtnVideoSubtract, 'icons' + PathDelim + 'subtract.png');
  setGlyphSpeedButton(sbtnVideoPlay, 'icons' + PathDelim + 'play.png');
  setGlyphSpeedButton(sbtnVideoPause, 'icons' + PathDelim + 'pause.png');
  setGlyphSpeedButton(sbtnVideoStop, 'icons' + PathDelim + 'stop.png');
  setGlyphSpeedButton(sbtnVideoClear, 'icons' + PathDelim + 'clear.png');
  setGlyphSpeedButton(sbtnVideoVolume, 'icons' + PathDelim + 'speaker_2.png');
  setGlyphSpeedButton(sbtnFullyDisplay, 'icons' + PathDelim + 'share_screen_on.png');
  setGlyphSpeedButton(sbtnMonitorConfigure, 'icons' + PathDelim +
    'share_screen_settings.png');
end;

end.
