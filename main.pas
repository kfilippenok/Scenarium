{*******************************************************}
{                                                       }
{       Scenarium                                       }
{       Copyright (c) 2024 Kirill Filippenok            }
{       Apache License 2.0                              }
{                                                       }
{*******************************************************}

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
  Pocket, UniqueInstance, FileSettings,
  // libMPVDelphi
  MPVBasePlayer;


type

  { TfMain }

  TfMain = class(TForm)
    lblCurrentVideoItem: TLabel;
    lblVideoTimeTotal: TLabel;
    lblVideoTimeCurrent: TLabel;
    lblCurrentAudioItem: TLabel;
    miVideoSettings: TMenuItem;
    miAudioSettings: TMenuItem;
    miVideoReloadPlaylist: TMenuItem;
    miAudioReloadPlaylist: TMenuItem;
    miScenarioReload: TMenuItem;
    miVideoDeleteBonds: TMenuItem;
    miCloseTab: TMenuItem;
    miAudioDeleteBond: TMenuItem;
    miAudioAddBond: TMenuItem;
    miVideoAddBond: TMenuItem;
    miAudioDelete: TMenuItem;
    miVideoDelete: TMenuItem;
    ppmnTabControl: TPopupMenu;
    ppmnAudioPlaylist: TPopupMenu;
    ppmnVideoPlaylist: TPopupMenu;
    sbtnAudioLinkControlTrackBar: TSpeedButton;
    sbtnAudioRepeat: TSpeedButton;
    sbtnVideoLinkControlTrackBar: TSpeedButton;
    sbtnVideoRepeat: TSpeedButton;
    sbtnAudioLinkControlButtons: TSpeedButton;
    sbtnVideoLinkControlButtons: TSpeedButton;
    { Вкладки }
    TabControl: TTabControl;

    { Меню }
    MainMenu: TMainMenu;
    miScenarioNew: TMenuItem;
    miScenarioOpen: TMenuItem;
    miScenarioSaveAs: TMenuItem;
    miScenarioSave: TMenuItem;
    miScenario: TMenuItem;
    miScenarioExit: TMenuItem;
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
    UniqueInstance: TUniqueInstance;

    procedure clboxAudioPlaylistClick(Sender: TObject);
    procedure clboxAudioPlaylistDblClick(Sender: TObject);
    procedure clboxAudioPlaylistDragDrop(Sender, Source: TObject; X, Y: Integer
      );
    procedure clboxAudioPlaylistDragOver(Sender, Source: TObject; X,
      Y: Integer; State: TDragState; var Accept: Boolean);
    procedure clboxVideoPlaylistClick(Sender: TObject);
    procedure clboxVideoPlaylistDblClick(Sender: TObject);
    procedure clboxVideoPlaylistDragDrop(Sender, Source: TObject; X, Y: Integer
      );
    procedure clboxVideoPlaylistDragOver(Sender, Source: TObject; X,
      Y: Integer; State: TDragState; var Accept: Boolean);
    procedure clboxVideoPlaylistItemClick(Sender: TObject; Index: integer);
    procedure clboxVideoPlaylistKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure clboxVideoPlaylistMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure clboxVideoPlaylistMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure AddBond(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of string);
    procedure FormResize(Sender: TObject);
    procedure miAudioDeleteBondClick(Sender: TObject);
    procedure miAudioDeleteClick(Sender: TObject);
    procedure miAudioSettingsClick(Sender: TObject);
    procedure miCloseTabClick(Sender: TObject);
    procedure miScenarioReloadClick(Sender: TObject);
    procedure miVideoDeleteBondsClick(Sender: TObject);
    procedure miVideoDeleteClick(Sender: TObject);
    procedure miVideoSettingsClick(Sender: TObject);
    procedure panAudioControlsTopMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure ppmnAudioPlaylistPopup(Sender: TObject);
    procedure ppmnTabControlPopup(Sender: TObject);
    procedure ppmnVideoPlaylistPopup(Sender: TObject);
    procedure sbtnAudioLinkControlTrackBarClick(Sender: TObject);
    procedure sbtnAudioPauseClick(Sender: TObject);
    procedure sbtnAudioStopClick(Sender: TObject);
    procedure sbtnAudioStopMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure sbtnVideoAddClick(Sender: TObject);
    procedure sbtnVideoLinkControlButtonsClick(Sender: TObject);
    procedure sbtnVideoLinkControlTrackBarClick(Sender: TObject);
    procedure sbtnVideoRepeatClick(Sender: TObject);
    procedure sbtnVideoStopMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure sbtnVideoVolumeClick(Sender: TObject);
    procedure sbtnVideoPlayClick(Sender: TObject);

    { Меню }
    procedure miScenarioNewClick(Sender: TObject);
    procedure miScenarioOpenClick(Sender: TObject);
    procedure miScenarioSaveClick(Sender: TObject);
    procedure miScenarioSaveAsClick(Sender: TObject);
    procedure miScenarioExitClick(Sender: TObject);
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
    procedure sbtnVideoPauseClick(Sender: TObject);
    procedure sbtnVideoStopClick(Sender: TObject);
    procedure sbtnAudioRepeatClick(Sender: TObject);
    procedure sbtnAudioLinkControlButtonsClick(Sender: TObject);
    procedure TabControlChange(Sender: TObject);
    procedure TabControlChanging(Sender: TObject; var AllowChange: Boolean);
    procedure TabControlDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure TabControlDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure TabControlMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure trbarAudioTimeChange(Sender: TObject);
    procedure trbarAudioTimeMouseLeave(Sender: TObject);
    procedure trbarAudioTimeMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
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
    procedure trbarVideoTimeMouseLeave(Sender: TObject);
    procedure trbarVideoTimeMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure trbarVideoTimeMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure trbarVideoVolumeChange(Sender: TObject);
    procedure trbarVideoVolumeMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure UniqueInstanceOtherInstance(Sender: TObject; ParamCount: Integer;
      const Parameters: array of String);
  private
    procedure OpenAndPlayAudio(AItemIndex: Integer);
    procedure OpenAndPlayVideo(AItemIndex: Integer);
    procedure openScenario(const ScenarioFilePath: String);
    procedure setFullyDisplay(ShowPlayback: Boolean);
    procedure resetTime(Player: string);
    procedure SetElementGlyphs;
    procedure LoadScenarioFromJSON(FilePath: String);
    procedure BondButtonClick(Sender: TObject);
  public

  end;

var
  fMain: TfMain;
  glAudioMute: boolean;
  glVideoMute: boolean;
  glVideoTrackRewinding: boolean = False;
  glAudioTrackRewinding: boolean = False;
  glAudioTrackRepeat: boolean = False;
  glVideoTrackRepeat: boolean = False;
  glCurrentAudioItem: string;
  glCurrentAudioItemIndex: Integer;
  glCurrentVideoItem: string;
  glCurrentVideoItemIndex: Integer;
  glCurrentPlaylist: CScenario = NIL;
  ScenarioList: CScenarioList;
  glAdaptivePanels: Boolean = True;
  StateNotify: CStateNotify;
  MousePos_TabControl: TPoint;
  Popuped_TabControl: Boolean = False;
  NewTab_Count: Integer = 0;
  LinkPlaybackControlButtons: Boolean = False;
  LinkPlaybackControlTrackBar: Boolean = False;

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
    IntToStr(
      ScenarioList.Items[TabControl.TabIndex].Bonds.GetInvokingWhereProvoking(
        StrToInt(
          (Sender as TBitBtn).Name[Length((Sender as TBitBtn).Name)]
        )
      )
    )
    + ' - ' +
    clboxVideoPlaylist.Items.Strings[
      ScenarioList.Items[TabControl.TabIndex].Bonds.GetInvokingWhereProvoking(
        StrToInt(
          (Sender as TBitBtn).Name[Length((Sender as TBitBtn).Name)]
        )
      )
    ]
  );
end;

{ TfMain }

procedure TfMain.FormCreate(Sender: TObject);
begin
  SetElementGlyphs();

  //ShowMessageFmt('%d', [ParamCount]);

  // Двойная буферизация
  fMain.DoubleBuffered := True;
  panAudio.DoubleBuffered:=True;
  panVideo.DoubleBuffered:=True;

  // Сценарий
  ScenarioList := CScenarioList.Create(True);

  // Вкладки
  ScenarioList.Add(CScenario.Create);
  ScenarioList.Items[0].Name := 'Default';
  TabControl.Tabs.Add(ScenarioList.Items[0].Name);

  // Заполняем список треков
  clboxAudioPlaylist.Items := ScenarioList.Items[0].AudioFileNames; // Аудио
  clboxVideoPlaylist.Items := ScenarioList.Items[0].VideoFileNames; // Видео

  if ParamCount > 0 then
    if FileExists(ParamStr(1)) then
      openScenario(ParamStr(1));

  { По умолчанию звук включен }
  glAudioMute := False; // Аудио
  glVideoMute := False; // Видео

  { Воспроизводимый на данный момент файл }
  glCurrentAudioItem := '';        // Аудио
  glCurrentAudioItemIndex := -1;
  glCurrentVideoItem := '';        // Видео
  glCurrentVideoItemIndex := -1;

  { Время устанавливается в нулевое положение }
  resetTime('AudioPlayer');  // Аудио
  resetTime('VideoPlayer');  // Видео

  // Оповещение о состоянии работы с файлом
  StateNotify := CStateNotify.Create(fMain);
  StateNotify.Parent := fMain;
  StateNotify.Width := 250;
  StateNotify.Height := 50;
  StateNotify.Anchors := [];
  StateNotify.AnchorParallel(akLeft, 20, fMain);
  StateNotify.AnchorParallel(akBottom, 60, fMain);
  StateNotify.Visible := False;
  StateNotify.BorderStyle := bsNone;
  StateNotify.BevelInner := bvNone;
  StateNotify.BevelOuter := bvNone;
  StateNotify.ShowTime := 2000;
  StateNotify.BringToFront;
end;

procedure TfMain.OpenAndPlayAudio(AItemIndex: Integer);
var IndexBond: Integer;
begin
  with fPlaybackAudio do
  begin
    glCurrentAudioItem := glCurrentPlaylist.AudioFilePaths[AItemIndex];
    glCurrentAudioItemIndex := AItemIndex;
    lblCurrentAudioItem.Caption := ExtractFileName(glCurrentAudioItem);
    audioPlayer.OpenFile(glCurrentAudioItem);

    // Время устанавливается в нулевое положение
    resetTime('AudioPlayer');
    TimerAudio.Enabled := True;

    clboxAudioPlaylist.Repaint;

    { Связи }
    IndexBond := glCurrentPlaylist.Bonds.IndexOfProvoking(AItemIndex);
    if IndexBond <> -1 then
      begin
        LinkPlaybackControlButtons := True;
        setGlyphSpeedButton(sbtnAudioLinkControlButtons, Application.Location + 'icons' + PathDelim + 'link_on.png');
        setGlyphSpeedButton(sbtnVideoLinkControlButtons, Application.Location + 'icons' + PathDelim + 'link_on.png');

        OpenAndPlayVideo(glCurrentPlaylist.Bonds.GetInvokingWhereProvoking(AItemIndex));
      end
    else
      begin
        if LinkPlaybackControlButtons = True then
          sbtnAudioLinkControlButtonsClick(fMain);
        if LinkPlaybackControlTrackBar = True then
          sbtnAudioLinkControlTrackBarClick(fMain);
      end;
    { Связи }

    // Изменение состояния элементов воспроизведения
    setGlyphSpeedButton(sbtnAudioStop, Application.Location + 'icons' + PathDelim + 'stop.png');
    setGlyphSpeedButton(sbtnAudioPause, Application.Location + 'icons' + PathDelim + 'pause.png');
    setGlyphSpeedButton(sbtnAudioPlay, Application.Location + 'icons' + PathDelim + 'play.png');
  end;
end;

procedure TfMain.clboxAudioPlaylistDblClick(Sender: TObject);
var
  i: word;
  IndexBond: Integer;
begin
  if (fPlaybackAudio.audioPlayer = nil) then Exit;
  if (clboxAudioPlaylist.Count = 0) then Exit;
  if ScenarioList.Items[TabControl.TabIndex].MissingFiles.IndexOf(ScenarioList.Items[TabControl.TabIndex].AudioFilePaths.Strings[clboxAudioPlaylist.ItemIndex]) <> -1 then
    begin
      ShowMessage('Файл по пути ' + ScenarioList.Items[TabControl.TabIndex].AudioFilePaths.Strings[clboxAudioPlaylist.ItemIndex] + ' отсутствует.');
      Exit;
    end;

  glCurrentPlaylist := ScenarioList.Items[TabControl.TabIndex];
  OpenAndPlayAudio(clboxAudioPlaylist.ItemIndex);
end;

procedure TfMain.clboxAudioPlaylistDragDrop(Sender, Source: TObject; X,
  Y: Integer);
var clbox : TCheckListBox;
    oldIndex, newIndex, i: Integer;
    LBitBtn: TBitBtn;
begin
  if clboxAudioPlaylist.Count = 0 then
    Exit;

  clbox := TCheckListBox(Sender);
  newIndex := clbox.GetIndexAtXY(X, Y);
  oldIndex := clboxAudioPlaylist.ItemIndex;

  if newIndex = -1 then                        // Если перенесли на пустое место
    newIndex := clboxAudioPlaylist.Count-1;

  if newIndex = oldIndex then // Проверка на смену позиции
    Exit;

  if (Sender = Source) then
    begin
      clboxAudioPlaylist.Items.Move(oldIndex, newIndex); // Передвигаем элемент
      ScenarioList.Items[TabControl.TabIndex].AudioFileNames.Move(oldIndex, newIndex);
      ScenarioList.Items[TabControl.TabIndex].AudioFilePaths.Move(oldIndex, newIndex);

      if oldIndex > newIndex then
        begin
          ScenarioList.Items[TabControl.TabIndex].Bonds.UpdateWhereProvoking(oldIndex, -1);
          if fMain.FindComponent('bbtnBondVideo' + IntToStr(oldIndex)) <> NIL then
            begin
              LBitBtn := ((fMain.FindComponent('bbtnBondVideo' + IntToStr(oldIndex))) as TBitBtn);
              FreeAndNil(LBitBtn);
            end;
          for i := oldIndex-1 downto newIndex do
            begin
              if i = glCurrentAudioItemIndex then
                glCurrentAudioItemIndex := i+1;
              if fMain.FindComponent('bbtnBondVideo' + IntToStr(i)) <> NIL then
              begin
                LBitBtn := ((fMain.FindComponent('bbtnBondVideo' + IntToStr(i))) as TBitBtn);
                FreeAndNil(LBitBtn);
              end;
              ScenarioList.Items[TabControl.TabIndex].Bonds.UpdateWhereProvoking(i, i+1);
            end;
          ScenarioList.Items[TabControl.TabIndex].Bonds.UpdateWhereProvoking(-1, newIndex);
          if oldIndex = glCurrentAudioItemIndex then
            glCurrentAudioItemIndex := newIndex;
        end
      else
        begin
          ScenarioList.Items[TabControl.TabIndex].Bonds.UpdateWhereProvoking(oldIndex, -1);
          if fMain.FindComponent('bbtnBondVideo' + IntToStr(oldIndex)) <> NIL then
            begin
              LBitBtn := ((fMain.FindComponent('bbtnBondVideo' + IntToStr(oldIndex))) as TBitBtn);
              FreeAndNil(LBitBtn);
            end;
          for i := oldIndex+1 to newIndex do
            begin
              if i = glCurrentAudioItemIndex then
                glCurrentAudioItemIndex := i-1;
              if fMain.FindComponent('bbtnBondVideo' + IntToStr(i)) <> NIL then
              begin
                LBitBtn := ((fMain.FindComponent('bbtnBondVideo' + IntToStr(i))) as TBitBtn);
                FreeAndNil(LBitBtn);
              end;
              ScenarioList.Items[TabControl.TabIndex].Bonds.UpdateWhereProvoking(i, i-1);
            end;
          ScenarioList.Items[TabControl.TabIndex].Bonds.UpdateWhereProvoking(-1, newIndex);
          if oldIndex = glCurrentAudioItemIndex then
            glCurrentAudioItemIndex := newIndex;
        end;

      clboxAudioPlaylist.ItemIndex := newIndex;
      clboxAudioPlaylist.ClearSelection;
      clboxAudioPlaylist.Selected[newIndex] := True;
    end;
end;

procedure TfMain.clboxAudioPlaylistDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
begin
  if clboxAudioPlaylist.Count > 0 then
    Accept := True;
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

  // !? Если ли в связях
  if not(ScenarioList.Items[TabControl.TabIndex].Bonds.InProvoking(clboxAudioPlaylist.ItemIndex)) then
    Exit;

  clboxVideoPlaylist.ClearSelection;
  clboxVideoPlaylist.Selected[ScenarioList.Items[TabControl.TabIndex].Bonds.GetInvokingWhereProvoking(clboxAudioPlaylist.ItemIndex)] := True;
end;

procedure TfMain.OpenAndPlayVideo(AItemIndex: Integer);
begin
  with fPlaybackVideo do
  begin
    glCurrentVideoItem := glCurrentPlaylist.VideoFilePaths[AItemIndex];
    glCurrentVideoItemIndex := AItemIndex;
    lblCurrentVideoItem.Caption := ExtractFileName(glCurrentVideoItem);
    VideoPlayer.OpenFile(glCurrentVideoItem);

    // Время устанавливается в нулевое положение
    resetTime('VideoPlayer');
    TimerVideo.Enabled := True;

    clboxVideoPlaylist.Repaint;

    // Изменение состояния элементов воспроизведения
    setGlyphSpeedButton(sbtnVideoStop, Application.Location + 'icons' + PathDelim + 'stop.png');
    setGlyphSpeedButton(sbtnVideoPause, Application.Location + 'icons' + PathDelim + 'pause.png');
    setGlyphSpeedButton(sbtnVideoPlay, Application.Location + 'icons' + PathDelim + 'play.png');

    setFullyDisplay(True);
  end;
end;

procedure TfMain.openScenario(const ScenarioFilePath: String);
var i: Integer;
    bbtnBond: TBitBtn;
begin
  try
    if ScenarioList.Items[TabControl.TabIndex].Bonds.Count <> 0 then
      begin
        for i := 0 to ScenarioList.Items[TabControl.TabIndex].Bonds.Count - 1 do
          begin
            if fMain.FindComponent('bbtnBondVideo' + IntToStr(ScenarioList.Items[TabControl.TabIndex].Bonds.arProvoking[i])) <> NIL then
              begin
                bbtnBond := (fMain.FindComponent('bbtnBondVideo' + IntToStr(ScenarioList.Items[TabControl.TabIndex].Bonds.arProvoking[i])) as TBitBtn);
                FreeAndNil(bbtnBond);
              end;
          end;
      end;
    ScenarioList.Add(CScenario.Create);
    ScenarioList.Items[TabControl.Tabs.Count].FilePath := ScenarioFilePath;
    ScenarioList.Items[TabControl.Tabs.Count].Name := ExtractFileName(ScenarioFilePath);
    TabControl.Tabs.Add(ScenarioList.Items[TabControl.Tabs.Count].Name);
    TabControl.TabIndex := TabControl.Tabs.Count-1;
    LoadScenarioFromJSON(ScenarioFilePath);
    if fMain.CanFocus then
      fMain.SetFocus;
  except
    on E: Exception do begin
      if ScenarioList.Count > 1 then
        begin
          FreeAndNil(ScenarioList.Items[ScenarioList.Count-1].AudioFileNames);
          FreeAndNil(ScenarioList.Items[ScenarioList.Count-1].AudioFilePaths);
          FreeAndNil(ScenarioList.Items[ScenarioList.Count-1].VideoFileNames);
          FreeAndNil(ScenarioList.Items[ScenarioList.Count-1].VideoFilePaths);
          FreeAndNil(ScenarioList.Items[ScenarioList.Count-1].MissingFiles);
          if ScenarioList.Items[ScenarioList.Count-1].Bonds.Count <> 0 then
            begin
              for i := 0 to ScenarioList.Items[ScenarioList.Count-1].Bonds.Count - 1 do
                begin
                  if fMain.FindComponent('bbtnBondVideo' + IntToStr(ScenarioList.Items[TabControl.TabIndex].Bonds.arProvoking[i])) <> NIL then
                    begin
                      bbtnBond := (fMain.FindComponent('bbtnBondVideo' + IntToStr(ScenarioList.Items[TabControl.TabIndex].Bonds.arProvoking[i])) as TBitBtn);
                      FreeAndNil(bbtnBond);
                    end;
                end;
            end;
          FreeAndNil(ScenarioList.Items[TabControl.TabIndex].Bonds);

         end;
       ScenarioList.Remove(ScenarioList.Items[TabControl.TabIndex]);
       TabControl.Tabs.Delete(TabControl.TabIndex);
       ShowMessage(ScenarioFilePath + ' не является файлом сценария!');
    end;
  end;
end;

procedure TfMain.clboxVideoPlaylistDblClick(Sender: TObject);
var
  i: word;
begin
  with fPlaybackVideo do
  begin
    if (fPlaybackVideo.videoPlayer = nil) then Exit;
    if (clboxVideoPlaylist.Count = 0) then Exit;
    if ScenarioList.Items[TabControl.TabIndex].MissingFiles.IndexOf(ScenarioList.Items[TabControl.TabIndex].VideoFilePaths.Strings[clboxVideoPlaylist.ItemIndex]) <> -1 then
      begin
        ShowMessage('Файл по пути ' + ScenarioList.Items[TabControl.TabIndex].VideoFilePaths.Strings[clboxVideoPlaylist.ItemIndex] + ' отсутствует.');
        Exit;
      end;

    if LinkPlaybackControlButtons = True then
      sbtnAudioLinkControlButtonsClick(fMain);
    if LinkPlaybackControlTrackBar = True then
      sbtnAudioLinkControlTrackBarClick(fMain);

    glCurrentPlaylist := ScenarioList.Items[TabControl.TabIndex];
    OpenAndPlayVideo(clboxVideoPlaylist.ItemIndex);
  end;
end;

procedure TfMain.clboxVideoPlaylistDragDrop(Sender, Source: TObject; X,
  Y: Integer);
var clbox : TCheckListBox;
    oldIndex, newIndex, i: Integer;
    LBitBtn: TBitBtn;
begin
  if clboxVideoPlaylist.Count = 0 then
    Exit;

  clbox := TCheckListBox(Sender);
  newIndex := clbox.GetIndexAtXY(X, Y);
  oldIndex := clboxVideoPlaylist.ItemIndex;

  if newIndex = -1 then                      // Если перенесли на пустое место
    newIndex := clboxVideoPlaylist.Count-1;

  if newIndex = oldIndex then // Проверка на смену позиции
    Exit;

  if (Sender = Source) then // Если перемещение происходит внутри компонента
    begin
      clboxVideoPlaylist.Items.Move(oldIndex, newIndex); // Передвигаем крайние
      ScenarioList.Items[TabControl.TabIndex].VideoFileNames.Move(oldIndex, newIndex);
      ScenarioList.Items[TabControl.TabIndex].VideoFilePaths.Move(oldIndex, newIndex);

      if oldIndex > newIndex then
        begin
          if i = glCurrentAudioItemIndex then
                glCurrentAudioItemIndex := i-1;
          ScenarioList.Items[TabControl.TabIndex].Bonds.UpdateWhereInvoking(oldIndex, -1);
          for i := oldIndex-1 downto newIndex do
            begin
              ScenarioList.Items[TabControl.TabIndex].Bonds.UpdateWhereInvoking(i, i+1);
            end;
          ScenarioList.Items[TabControl.TabIndex].Bonds.UpdateWhereInvoking(-1, newIndex);
          if oldIndex = glCurrentAudioItemIndex then
            glCurrentAudioItemIndex := newIndex;
        end
      else
        begin
          if i = glCurrentAudioItemIndex then
                glCurrentAudioItemIndex := i-1;
          ScenarioList.Items[TabControl.TabIndex].Bonds.UpdateWhereInvoking(oldIndex, -1);
          for i := oldIndex+1 to newIndex do
            begin
              ScenarioList.Items[TabControl.TabIndex].Bonds.UpdateWhereInvoking(i, i-1);
            end;
          ScenarioList.Items[TabControl.TabIndex].Bonds.UpdateWhereInvoking(-1, newIndex);
          if oldIndex = glCurrentAudioItemIndex then
            glCurrentAudioItemIndex := newIndex;
        end;

      clboxAudioPlaylist.Repaint;
      clboxVideoPlaylist.ItemIndex := newIndex;
      clboxVideoPlaylist.ClearSelection;
      clboxVideoPlaylist.Selected[newIndex] := True;
    end;
end;

procedure TfMain.clboxVideoPlaylistDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
begin
  if clboxVideoPlaylist.Count > 0 then
    Accept := True;
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

procedure TfMain.clboxVideoPlaylistMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  if glVideoTrackRewinding = True then
    trbarVideoTimeMouseLeave(Sender);
end;

procedure TfMain.FormDestroy(Sender: TObject);
var i, iscn: Integer;
    bbtnBond: TBitBtn;
    Scenario: CScenario;
begin
  if ScenarioList.Items[TabControl.TabIndex].Bonds.Count <> 0 then
    begin
      for i := 0 to ScenarioList.Items[TabControl.TabIndex].Bonds.Count - 1 do
        begin
          if fMain.FindComponent('bbtnBondVideo' + IntToStr(ScenarioList.Items[TabControl.TabIndex].Bonds.arProvoking[i])) <> NIL then
            begin
              bbtnBond := (fMain.FindComponent('bbtnBondVideo' + IntToStr(ScenarioList.Items[TabControl.TabIndex].Bonds.arProvoking[i])) as TBitBtn);
              FreeAndNil(bbtnBond);
            end;
        end;
    end;
  FreeAndNil(ScenarioList);
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

  if ScenarioList.Items[TabControl.TabIndex].Bonds.InProvoking(clboxAudioPlaylist.ItemIndex) then
    begin
      ScenarioList.Items[TabControl.TabIndex].Bonds.DeleteWhereProvoking(clboxAudioPlaylist.ItemIndex);
    end;

  ScenarioList.Items[TabControl.TabIndex].Bonds.Add(clboxAudioPlaylist.ItemIndex, clboxVideoPlaylist.ItemIndex);

  if fMain.FindComponent('bbtnBondVideo' + IntToStr(clboxAudioPlaylist.ItemIndex)) <> NIL then
    begin
      bbtnFindedVideoBond := (fMain.FindComponent('bbtnBondVideo' + IntToStr(clboxAudioPlaylist.ItemIndex)) as TBitBtn);
      bbtnFindedVideoBond.Caption := IntToStr(ScenarioList.Items[TabControl.TabIndex].Bonds.arInvoking[ScenarioList.Items[TabControl.TabIndex].Bonds.IndexOfProvoking(clboxAudioPlaylist.ItemIndex)]);
    end;

  clboxAudioPlaylist.Repaint;
end;

procedure TfMain.FormDropFiles(Sender: TObject; const FileNames: array of string
  );
const  mrAudio = 21;
       mrVideo = 22;
       mrSkeep = 23;
       mrSkeepAll = 24;
var i: Word;
    SkeepAll: Boolean = False;
begin
  for i := 0 to Length(FileNames)-1 do
    begin
      case identifyFileType(FileNames[i]) of
        ftAudio:
          begin
            ScenarioList.Items[TabControl.TabIndex].AudioFilePaths.Add(FileNames[i]);
            ScenarioList.Items[TabControl.TabIndex].AudioFileNames.Add(ExtractFileName(FileNames[i]));
          end;
        ftVideo, ftImage:
          begin
            ScenarioList.Items[TabControl.TabIndex].VideoFilePaths.Add(FileNames[i]);
            ScenarioList.Items[TabControl.TabIndex].VideoFileNames.Add(ExtractFileName(FileNames[i]));
          end;
        ftUnknown:
          begin
            if Not(SkeepAll) then
              case QuestionDlg('Неизвестный тип файла', 'Тип ' + ExtractFileExt(FileNames[i]) + ' файла ' + ExtractFileName(FileNames[i]) + ' неизвестен. Как воспринимать файл?', mtCustom, [mrAudio, 'Аудио', mrVideo, 'Видео', mrSkeep, 'Пропустить', mrSkeepAll, 'Пропустить все'], '') of
                mrAudio:
                  begin
                    ScenarioList.Items[TabControl.TabIndex].AudioFilePaths.Add(FileNames[i]);
                    ScenarioList.Items[TabControl.TabIndex].AudioFileNames.Add(ExtractFileName(FileNames[i]));
                  end;
                mrVideo:
                  begin
                    ScenarioList.Items[TabControl.TabIndex].VideoFilePaths.Add(FileNames[i]);
                    ScenarioList.Items[TabControl.TabIndex].VideoFileNames.Add(ExtractFileName(FileNames[i]));
                  end;
                mrSkeepAll:
                  SkeepAll := True;
              end;
            end;
      end;
    end;

  clboxAudioPlaylist.Items := ScenarioList.Items[TabControl.TabIndex].AudioFileNames;
  clboxVideoPlaylist.Items := ScenarioList.Items[TabControl.TabIndex].VideoFileNames;
end;

procedure TfMain.FormResize(Sender: TObject);

  procedure setRowView;
  begin
    panAudio.Anchors := [];
    panAudio.Visible := False;
    panVideo.Anchors := [];
    panVideo.Visible := False;

    Splitter.Anchors := [];
    Splitter.AnchorToNeighbour(akTop, 0, TabControl);
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

  if (fMain.Height / fMain.Width) >= 1 then
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
  ScenarioList.Items[TabControl.TabIndex].Bonds.DeleteWhereProvoking(clboxAudioPlaylist.ItemIndex);

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

procedure TfMain.miAudioSettingsClick(Sender: TObject);
var AudioExtensions: String = '';
    i: Integer;
begin
  FileSettings.fFileSettings.edtOldFilePath.Text := ScenarioList.Items[TabControl.TabIndex].AudioFilePaths.Strings[clboxAudioPlaylist.ItemIndex];
  for i := Low(Pocket.arrAudioExtensions) to High(Pocket.arrAudioExtensions) do
    AudioExtensions := AudioExtensions + '*' + arrAudioExtensions[i] + ';';
  FileSettings.fFileSettings.OpenDialog.Filter := 'Audio|' + AudioExtensions + '|All files|*.*';
  FileSettings.fFileSettings.ShowModal;
end;

procedure TfMain.miCloseTabClick(Sender: TObject);
var Scenario: CScenario;
    i, choose_index: Integer;
    bbtnBond: TBitBtn;
begin
  if Popuped_TabControl then
    begin
      choose_index := TabControl.IndexOfTabAt(MousePos_TabControl.X, MousePos_TabControl.Y);
      if choose_index = -1 then
        choose_index := TabControl.TabIndex;
      Popuped_TabControl := False;
    end
  else
    choose_index := TabControl.TabIndex;
  Scenario := ScenarioList.Extract(ScenarioList.Items[choose_index]);
  TabControl.Tabs.Delete(choose_index);
  FreeAndNil(Scenario.AudioFileNames);
  FreeAndNil(Scenario.AudioFilePaths);
  FreeAndNil(Scenario.VideoFileNames);
  FreeAndNil(Scenario.VideoFilePaths);
  if Scenario.Bonds.Count <> 0 then
    begin
      for i := 0 to Scenario.Bonds.Count - 1 do
        begin
          if fMain.FindComponent('bbtnBondVideo' + IntToStr(Scenario.Bonds.arProvoking[i])) <> NIL then
            begin
              bbtnBond := (fMain.FindComponent('bbtnBondVideo' + IntToStr(Scenario.Bonds.arProvoking[i])) as TBitBtn);
              FreeAndNil(bbtnBond);
            end;
        end;
    end;
  FreeAndNil(Scenario.Bonds);
  FreeAndNil(Scenario);
  clboxAudioPlaylist.Clear;
  clboxVideoPlaylist.Clear;
  clboxAudioPlaylist.Items := ScenarioList.Items[TabControl.TabIndex].AudioFileNames;
  clboxVideoPlaylist.Items := ScenarioList.Items[TabControl.TabIndex].VideoFileNames;
end;

procedure TfMain.miScenarioReloadClick(Sender: TObject);
var i, AudioItemIndex, VideoItemIndex: Integer;
    LBitBtn: TBitBtn;
begin
  if ScenarioList.Items[TabControl.TabIndex].FilePath = '' then
    begin
      ScenarioList.Items[TabControl.TabIndex].MissingFiles.Clear;
      for i := 0 to clboxAudioPlaylist.Count-1 do
        if Not(FileExists(ScenarioList.Items[TabControl.TabIndex].AudioFilePaths.Strings[i])) then
          begin
            ScenarioList.Items[TabControl.TabIndex].MissingFiles.Add(ScenarioList.Items[TabControl.TabIndex].AudioFilePaths.Strings[i]);
            ShowMessage(ScenarioList.Items[TabControl.TabIndex].AudioFilePaths.Strings[i]);
          end;
      for i := 0 to clboxVideoPlaylist.Count-1 do
        if Not(FileExists(ScenarioList.Items[TabControl.TabIndex].VideoFilePaths.Strings[i])) then
          ScenarioList.Items[TabControl.TabIndex].MissingFiles.Add(ScenarioList.Items[TabControl.TabIndex].VideoFilePaths.Strings[i]);
      clboxAudioPlaylist.Repaint;
      clboxVideoPlaylist.Repaint;
      StateNotify.State := snReload;
    end
  else
    begin
      for i:= 0 to ScenarioList.Items[TabControl.TabIndex].Bonds.Count - 1 do
        begin
          if fMain.FindComponent('bbtnBondVideo' + IntToStr(ScenarioList.Items[TabControl.TabIndex].Bonds.arProvoking[i])) <> NIL then
            begin
              LBitBtn := (fMain.FindComponent('bbtnBondVideo' + IntToStr(ScenarioList.Items[TabControl.TabIndex].Bonds.arProvoking[i])) as TBitBtn);
              FreeAndNil(LBitBtn);
            end;
        end;
      AudioItemIndex := clboxAudioPlaylist.ItemIndex;
      VideoItemIndex := clboxVideoPlaylist.ItemIndex;
      ScenarioList.Items[TabControl.TabIndex].AudioFileNames.Clear;
      ScenarioList.Items[TabControl.TabIndex].AudioFilePaths.Clear;
      ScenarioList.Items[TabControl.TabIndex].VideoFileNames.Clear;
      ScenarioList.Items[TabControl.TabIndex].VideoFilePaths.Clear;
      ScenarioList.Items[TabControl.TabIndex].MissingFiles.Clear;
      ScenarioList.Items[TabControl.TabIndex].Bonds.Clear;
      LoadScenarioFromJSON(ScenarioList.Items[TabControl.TabIndex].FilePath);
      clboxAudioPlaylist.ItemIndex := AudioItemIndex;
      clboxVideoPlaylist.ItemIndex := VideoItemIndex;
      StateNotify.State := snReload;
    end;
end;

procedure TfMain.miVideoDeleteBondsClick(Sender: TObject);
var i, tmpProvoking: Integer;
    bbtnFindedVideoBond: TBitBtn;
begin
  i := 0;
  while i < ScenarioList.Items[TabControl.TabIndex].Bonds.Count do
    if ScenarioList.Items[TabControl.TabIndex].Bonds.arInvoking[i] = clboxVideoPlaylist.ItemIndex then
      begin
        tmpProvoking := ScenarioList.Items[TabControl.TabIndex].Bonds.arProvoking[i];
        ScenarioList.Items[TabControl.TabIndex].Bonds.DeleteWhereProvoking(tmpProvoking);
        if fMain.FindComponent('bbtnBondVideo' + IntToStr(tmpProvoking)) <> NIL then
          begin
            bbtnFindedVideoBond := (fMain.FindComponent('bbtnBondVideo' + IntToStr(tmpProvoking)) as TBitBtn);
            FreeAndNil(bbtnFindedVideoBond);
          end;
      end
    else
      Inc(i);

  clboxAudioPlaylist.Repaint;
end;

procedure TfMain.ppmnTabControlPopup(Sender: TObject);
begin
  miCloseTab.Enabled := TabControl.Tabs.Count > 1;

  Popuped_TabControl := True;
end;

procedure TfMain.ppmnVideoPlaylistPopup(Sender: TObject);
begin
  if clboxVideoPlaylist.Count <> 0 then
    begin
      miVideoDelete.Enabled := True;
      if clboxVideoPlaylist.ItemIndex <> -1
        then
          begin
            miVideoAddBond.Enabled := True;
            if clboxVideoPlaylist.ItemIndex <> glCurrentVideoItemIndex then
              miVideoSettings.Enabled := True
            else
              miVideoSettings.Enabled := False;
          end
        else
          begin
            miVideoAddBond.Enabled := False;
            miVideoSettings.Enabled := False;
          end;
      if ScenarioList.Items[TabControl.TabIndex].Bonds.InInvoking(clboxVideoPlaylist.ItemIndex)
        then miVideoDeleteBonds.Enabled := True
        else miVideoDeleteBonds.Enabled := False;
    end
  else
    begin
      miVideoDelete.Enabled := False;
      miVideoAddBond.Enabled := False;
      miVideoDeleteBonds.Enabled := False;
      miVideoSettings.Enabled := False;
    end;
end;

procedure TfMain.sbtnAudioLinkControlTrackBarClick(Sender: TObject);
begin
  LinkPlaybackControlTrackBar := Not(LinkPlaybackControlTrackBar);

  if LinkPlaybackControlTrackBar then
    begin
      setGlyphSpeedButton(sbtnAudioLinkControlTrackBar, Application.Location + 'icons' + PathDelim + 'link_on.png');
      setGlyphSpeedButton(sbtnVideoLinkControlTrackBar, Application.Location + 'icons' + PathDelim + 'link_on.png');
    end
  else
    begin
      setGlyphSpeedButton(sbtnAudioLinkControlTrackBar, Application.Location + 'icons' + PathDelim + 'link_off.png');
      setGlyphSpeedButton(sbtnVideoLinkControlTrackBar, Application.Location + 'icons' + PathDelim + 'link_off.png');
    end;
end;

procedure TfMain.sbtnAudioPauseClick(Sender: TObject);
begin
  if (glCurrentAudioItem = '') then Exit;

  with fPlaybackAudio do
  begin
    if (audioPlayer = nil) then Exit;

    audioPlayer.Pause;
  end;
  TimerAudio.Enabled := False;

  // Изменение состояния элементов воспроизведения
  setGlyphSpeedButton(sbtnAudioStop, Application.Location + 'icons' + PathDelim + 'stop.png');
  setGlyphSpeedButton(sbtnAudioPlay, Application.Location + 'icons' + PathDelim + 'play.png');
  setGlyphSpeedButton(sbtnAudioPause, Application.Location + 'icons' + PathDelim + 'pause_active.png');

  if LinkPlaybackControlButtons then
    begin
      LinkPlaybackControlButtons := False;
      sbtnVideoPauseClick(Self);
      LinkPlaybackControlButtons := True;
    end;
end;

procedure TfMain.sbtnAudioStopClick(Sender: TObject);
begin
  if (fPlaybackAudio.audioPlayer = nil) then Exit;
  if (glCurrentAudioItem = '') then Exit;

  if Not(glAudioTrackRepeat) then
    begin
      fPlaybackAudio.audioPlayer.Stop;

      // Изменение состояния элементов воспроизведения
      setGlyphSpeedButton(sbtnAudioPause, Application.Location + 'icons' + PathDelim + 'pause.png');
      setGlyphSpeedButton(sbtnAudioPlay, Application.Location + 'icons' + PathDelim + 'play.png');
      setGlyphSpeedButton(sbtnAudioStop, Application.Location + 'icons' + PathDelim + 'stop.png');

      // "Проигриваемый элемент" очищается (Аудио)
      glCurrentPlaylist := NIL;
      glCurrentAudioItem := '';
      glCurrentAudioItemIndex := -1;
      lblCurrentAudioItem.Caption := '';
      // Список элементов переотрисовывается (Аудио)
      clboxAudioPlaylist.Repaint;
      // Выключаем таймер (Аудио)
      TimerAudio.Enabled := False;

      // Время устанавливается в нулевое положение (Аудио)
      resetTime('AudioPlayer');

      if LinkPlaybackControlButtons then
        begin
          LinkPlaybackControlButtons := False;
          if glVideoTrackRepeat then
            begin
              sbtnVideoRepeat.Click;
            end;
          sbtnVideoStopClick(Self);
          LinkPlaybackControlButtons := True;
          sbtnAudioLinkControlButtonsClick(Sender);
        end;
      if LinkPlaybackControlTrackBar then
        sbtnAudioLinkControlTrackBarClick(Sender);
    end
  else
    begin
      TimerAudio.Enabled := False;
      OpenAndPlayAudio(glCurrentAudioItemIndex);
    end;
end;

procedure TfMain.sbtnAudioStopMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if (fPlaybackAudio.audioPlayer = nil) then Exit;
  if (glCurrentAudioItem = '') then Exit;

  // Изменение состояния элементов воспроизведения
  setGlyphSpeedButton(sbtnAudioPause, Application.Location + 'icons' + PathDelim + 'pause.png');
  setGlyphSpeedButton(sbtnAudioPlay, Application.Location + 'icons' + PathDelim + 'play.png');
  setGlyphSpeedButton(sbtnAudioStop, Application.Location + 'icons' + PathDelim + 'stop_active.png');
end;

procedure TfMain.sbtnVideoAddClick(Sender: TObject);
var
  FilePath: string;
  ImageExtensions, VideoExtensions: String;
  i: Integer;
begin
  OpenDialog.Title := 'Добавить элемент';
  OpenDialog.DefaultExt := '';

  ImageExtensions := ''; VideoExtensions := '';
  for i := Low(Pocket.arrVideoExtensions) to High(Pocket.arrVideoExtensions) do
    VideoExtensions := VideoExtensions + '*' + arrVideoExtensions[i] + ';';
  for i := Low(Pocket.arrImageExtensions) to High(Pocket.arrImageExtensions) do
    ImageExtensions := ImageExtensions + '*' + Pocket.arrImageExtensions[i] + ';';

  OpenDialog.Filter := 'Video and Images|' + VideoExtensions + ImageExtensions + '|' + 'Video|' + VideoExtensions + '|' + 'Image|' + ImageExtensions + '|' + 'All files|*.*';
  if OpenDialog.Execute then
  begin
    for FilePath in OpenDialog.Files do
    begin
      ScenarioList.Items[TabControl.TabIndex].VideoFilePaths.Add(FilePath);
      ScenarioList.Items[TabControl.TabIndex].VideoFileNames.Add(ExtractFileName(FilePath));
    end;

    clboxVideoPlaylist.Items := ScenarioList.Items[TabControl.TabIndex].VideoFileNames;
  end;
end;

procedure TfMain.sbtnVideoLinkControlButtonsClick(Sender: TObject);
begin
  LinkPlaybackControlButtons := Not(LinkPlaybackControlButtons);

  if LinkPlaybackControlButtons then
    begin
      setGlyphSpeedButton(sbtnAudioLinkControlButtons, Application.Location + 'icons' + PathDelim + 'link_on.png');
      setGlyphSpeedButton(sbtnVideoLinkControlButtons, Application.Location + 'icons' + PathDelim + 'link_on.png');
    end
  else
    begin
      setGlyphSpeedButton(sbtnAudioLinkControlButtons, Application.Location + 'icons' + PathDelim + 'link_off.png');
      setGlyphSpeedButton(sbtnVideoLinkControlButtons, Application.Location + 'icons' + PathDelim + 'link_off.png');
    end;
end;

procedure TfMain.sbtnVideoLinkControlTrackBarClick(Sender: TObject);
begin
  LinkPlaybackControlTrackBar := Not(LinkPlaybackControlTrackBar);

  if LinkPlaybackControlTrackBar then
    begin
      setGlyphSpeedButton(sbtnAudioLinkControlTrackBar, Application.Location + 'icons' + PathDelim + 'link_on.png');
      setGlyphSpeedButton(sbtnVideoLinkControlTrackBar, Application.Location + 'icons' + PathDelim + 'link_on.png');
    end
  else
    begin
      setGlyphSpeedButton(sbtnAudioLinkControlTrackBar, Application.Location + 'icons' + PathDelim + 'link_off.png');
      setGlyphSpeedButton(sbtnVideoLinkControlTrackBar, Application.Location + 'icons' + PathDelim + 'link_off.png');
    end;
end;

procedure TfMain.miVideoDeleteClick(Sender: TObject);
begin
  miVideoDeleteBondsClick(Self);
  sbtnVideoSubtract.Click;
end;

procedure TfMain.miVideoSettingsClick(Sender: TObject);
var i: Integer;
    ImageExtensions, VideoExtensions: String;
begin
  FileSettings.fFileSettings.edtOldFilePath.Text := ScenarioList.Items[TabControl.TabIndex].VideoFilePaths.Strings[clboxVideoPlaylist.ItemIndex];

  ImageExtensions := ''; VideoExtensions := '';
  for i := Low(Pocket.arrVideoExtensions) to High(Pocket.arrVideoExtensions) do
    VideoExtensions := VideoExtensions + '*' + Pocket.arrVideoExtensions[i] + ';';
  for i := Low(Pocket.arrImageExtensions) to High(Pocket.arrImageExtensions) do
    ImageExtensions := ImageExtensions + '*' + Pocket.arrImageExtensions[i] + ';';

  FileSettings.fFileSettings.OpenDialog.Filter := 'Video and Images|' + VideoExtensions + ImageExtensions + '|' + 'Video|' + VideoExtensions + '|' + 'Image|' + ImageExtensions + '|' + 'All files|*.*';

  FileSettings.fFileSettings.ShowModal;
end;

procedure TfMain.panAudioControlsTopMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  if glAudioTrackRewinding = True then
    trbarAudioTimeMouseLeave(Sender);
end;

procedure TfMain.ppmnAudioPlaylistPopup(Sender: TObject);
begin
  if clboxAudioPlaylist.Count <> 0 then
    begin
      miAudioDelete.Enabled := True;
      if clboxVideoPlaylist.ItemIndex <> -1
        then
          begin
            miAudioAddBond.Enabled := True;
            if clboxAudioPlaylist.ItemIndex <> glCurrentAudioItemIndex then
              miAudioSettings.Enabled := True
            else
              miAudioSettings.Enabled := False;
          end
        else
          begin
            miAudioAddBond.Enabled := False;
            miAudioSettings.Enabled := False;
          end;
      if ScenarioList.Items[TabControl.TabIndex].Bonds.InProvoking(clboxAudioPlaylist.ItemIndex)
        then miAudioDeleteBond.Enabled := True
        else miAudioDeleteBond.Enabled := False;
    end
  else
    begin
      miAudioDelete.Enabled := False;
      miAudioAddBond.Enabled := False;
      miAudioDeleteBond.Enabled := False;
      miAudioSettings.Enabled := False;
    end;
end;

procedure TfMain.sbtnVideoRepeatClick(Sender: TObject);
begin
  glVideoTrackRepeat := Not(glVideoTrackRepeat);

  if glVideoTrackRepeat = True then
    setGlyphSpeedButton(sbtnVideoRepeat, Application.Location + 'icons' + PathDelim + 'repeat_on.png')
  else
    setGlyphSpeedButton(sbtnVideoRepeat, Application.Location + 'icons' + PathDelim + 'repeat_off.png');
end;

procedure TfMain.sbtnVideoStopMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  setGlyphSpeedButton(sbtnVideoPause, Application.Location + 'icons' + PathDelim + 'pause.png');
  setGlyphSpeedButton(sbtnVideoPlay, Application.Location + 'icons' + PathDelim + 'play.png');
  setGlyphSpeedButton(sbtnVideoStop, Application.Location + 'icons' + PathDelim + 'stop_active.png');
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
    // Если файл отсутствует
    if ScenarioList.Items[TabControl.TabIndex].MissingFiles.IndexOf(ScenarioList.Items[TabControl.TabIndex].VideoFilePaths.Strings[Index]) <> -1 then
      begin
        if Not(odSelected in ReceivedState) then
          begin
            Canvas.Brush.Color := RGBToColor(244, 234, 218);
            Canvas.Font.Color := clBlack;
          end
        else
          begin
            Canvas.Brush.Color := RGBToColor(255, 203, 92);
            Canvas.Font.Color := clBlack;
          end;
      end
    else if (odSelected in ReceivedState) then   // Выбранный элемент
      begin
        if IsVideo(clboxVideoPlaylist.Items[Index]) then
          Canvas.Brush.Color := RGBToColor(144, 89, 223)
        else
          Canvas.Brush.Color := RGBToColor(32, 204, 29);
        Canvas.Font.Color := clWhite;
      end
    else                                         // Проигрываемый
      begin
        if (ScenarioList.Items[TabControl.TabIndex] = glCurrentPlaylist)
        and ((Index = glCurrentVideoItemIndex) and (clboxVideoPlaylist.Items[Index] = lblCurrentVideoItem.Caption)) then
          begin
                Canvas.Brush.Color := RGBToColor(146, 146, 146);
                Canvas.Font.Color := clWhite;
          end
        else
          begin                                      // Обычный
            if IsVideo(clboxVideoPlaylist.Items[Index]) then
             Canvas.Brush.Color := RGBToColor(222, 218, 244)
            else
             Canvas.Brush.Color := RGBToColor(192, 248, 191);
          end;
      end;


    // Готовим холст к отрисовке
    Canvas.FillRect(ARect);

    // Отрисовываем текст с заданными параметрами
    Canvas.TextOut(ARect.Left + 23, (ARect.Top + ARect.Height div 4), savedItem);

    { Связи }{
    if ScenarioList.Items[TabControl.TabIndex].BondsVideo.IndexOf(savedItem) <> -1 then
      Canvas.TextOut(ARect.Left + 250, (ARect.Top + ARect.Height div 4), 'Cвязь: '+IntToStr(ScenarioList.Items[TabControl.TabIndex].BondsVideo.IndexOf(savedItem)));
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
      setGlyphSpeedButton(sbtnVideoVolume, Application.Location + 'icons' + PathDelim + 'speaker_0.png')
    else if trbarVideoVolume.Position < 66 then
      setGlyphSpeedButton(sbtnVideoVolume, Application.Location + 'icons' + PathDelim + 'speaker_1.png')
    else
      setGlyphSpeedButton(sbtnVideoVolume, Application.Location + 'icons' + PathDelim + 'speaker_2.png');

    setGlyphSpeedButton(sbtnVideoVolume, Application.Location + 'icons' + PathDelim + 'speaker_2.png');
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
  while i <> ScenarioList.Items[TabControl.TabIndex].VideoFileNames.Count do
  begin
    if ScenarioList.Items[TabControl.TabIndex].VideoFileNames.Strings[i] <> glCurrentVideoItem then
    begin
      ScenarioList.Items[TabControl.TabIndex].VideoFileNames.Delete(i);
      ScenarioList.Items[TabControl.TabIndex].VideoFilePaths.Delete(i);
    end
    else
      i += 1;
  end;
  clboxVideoPlaylist.Items := ScenarioList.Items[TabControl.TabIndex].VideoFileNames;
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
  InvokingIndex: Integer;
begin
  with clboxAudioPlaylist do
  begin
    // Забираем текст текущего элемента в переменную,
    // для дальнейшей возможной модификации
    savedItem := Items[Index];

    // Присваиваем нужный цвет фону элементам
    Canvas.Brush.Color := RGBToColor(244, 218, 218);

    // Если файл отсутствует
    if ScenarioList.Items[TabControl.TabIndex].MissingFiles.IndexOf(ScenarioList.Items[TabControl.TabIndex].AudioFilePaths.Strings[Index]) <> -1 then
      begin
        if Not(odSelected in ReceivedState) then
          begin
            Canvas.Brush.Color := RGBToColor(244, 234, 218);
            Canvas.Font.Color := clBlack;
          end
        else
          begin
            Canvas.Brush.Color := RGBToColor(255, 203, 92);
            Canvas.Font.Color := clBlack;
          end;
      end
    else if (odSelected in ReceivedState) then   // Выделенная строка
      begin
        Canvas.Brush.Color := RGBToColor(255, 92, 92);
        Canvas.Font.Color := clWhite;
      end
    else if ScenarioList.Items[TabControl.TabIndex] = glCurrentPlaylist then
      if (Index = glCurrentAudioItemIndex) and (clboxAudioPlaylist.Items[Index] = lblCurrentAudioItem.Caption) then
        begin
          Canvas.Brush.Color := RGBToColor(146, 146, 146);
          Canvas.Font.Color := clWhite;
        end;

    // Готовим холст к отрисовке
    Canvas.FillRect(ARect);

    // Отрисовываем текст с заданными параметрами
    Canvas.TextOut(ARect.Left + 23, (ARect.Top + ARect.Height div 4), savedItem);

    { Номер элемента }
    Canvas.TextOut(ARect.Left + 5, (ARect.Top + ARect.Height div 4), IntToStr(Index));
    { Номер элемента }

    { Связи }
    if ScenarioList.Items[TabControl.TabIndex].Bonds.IndexOfProvoking(Index) <> -1 then
    begin
      if fMain.FindComponent('bbtnBondVideo' + IntToStr(Index)) <> NIL then
        begin
          bbtnFindedVideoBond := (fMain.FindComponent('bbtnBondVideo' + IntToStr(Index)) as TBitBtn);
          bbtnFindedVideoBond.Left := ARect.Right - 54;
          bbtnFindedVideoBond.Top := ARect.Top + 5;
          bbtnFindedVideoBond.Caption := IntToStr(ScenarioList.Items[TabControl.TabIndex].Bonds.GetInvokingWhereProvoking(Index));
          bbtnFindedVideoBond.Repaint;
        end
      else
        begin
          bbtnVideoBond := TBitBtn.Create(fMain);
          InvokingIndex := ScenarioList.Items[TabControl.TabIndex].Bonds.GetInvokingWhereProvoking(Index);
          if IsVideo(ScenarioList.Items[TabControl.TabIndex].VideoFileNames.Strings[InvokingIndex])
            then setGlyphSpeedButton(bbtnVideoBond, Application.Location + 'icons'+PathDelim+'video.png')
            else setGlyphSpeedButton(bbtnVideoBond, Application.Location + 'icons'+PathDelim+'image.png');
          bbtnVideoBond.Parent := clboxAudioPlaylist;
          bbtnVideoBond.Name := 'bbtnBondVideo' + IntToStr(Index);
          bbtnVideoBond.Height := 30;
          bbtnVideoBond.Width := 44;
          bbtnVideoBond.Caption := IntToStr(ScenarioList.Items[TabControl.TabIndex].Bonds.GetInvokingWhereProvoking(Index));
          bbtnVideoBond.Top := ARect.Top + 5;
          bbtnVideoBond.Left := ARect.Right - 54;
          bbtnVideoBond.OnClick := @BondButtonClick;
        end;
    end;
    { Связи }

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
  i, t, tmp_count: Integer;
  bbtnFindedVideoBond: TBitBtn;
begin
  tmp_count := clboxVideoPlaylist.Count-1;
  i := tmp_count;
  while i >= 0 do
  begin
    if clboxVideoPlaylist.Selected[i] then
    begin
      if clboxVideoPlaylist.Items[i] <> glCurrentVideoItem then
      begin
        { Связи }
        while ScenarioList.Items[TabControl.TabIndex].Bonds.InProvoking(ScenarioList.Items[TabControl.TabIndex].Bonds.GetProvokingWhereInvoking(i)) do
          begin
            // Сначала очищаем кнопку
            if fMain.FindComponent('bbtnBondVideo' + IntToStr(ScenarioList.Items[TabControl.TabIndex].Bonds.GetProvokingWhereInvoking(i))) <> NIL then
              begin
                bbtnFindedVideoBond := (fMain.FindComponent('bbtnBondVideo' + IntToStr(ScenarioList.Items[TabControl.TabIndex].Bonds.GetProvokingWhereInvoking(i))) as TBitBtn);
                FreeAndNil(bbtnFindedVideoBond);
                // Затем очищаем саму связь
                ScenarioList.Items[TabControl.TabIndex].Bonds.DeleteWhereProvoking(ScenarioList.Items[TabControl.TabIndex].Bonds.GetProvokingWhereInvoking(i));
              end;
          end;
        { Связи }

        ScenarioList.Items[TabControl.TabIndex].VideoFileNames.Delete(i);
        ScenarioList.Items[TabControl.TabIndex].VideoFilePaths.Delete(i);

        { Связи }
        // Обновляем индексы по вызываемым
        for t := i to clboxVideoPlaylist.Count-1 do
        begin
          ScenarioList.Items[TabControl.TabIndex].Bonds.UpdateWhereInvoking(t, t-1);
        end;
        { Связи }
      end;
    end;
    Dec(i);
  end;
  clboxAudioPlaylist.Repaint;
  clboxVideoPlaylist.Items := ScenarioList.Items[TabControl.TabIndex].VideoFileNames;
end;

procedure TfMain.sbtnVideoPlayClick(Sender: TObject);
begin
  with fPlaybackVideo do
  begin
    if (videoPlayer = nil) then Exit;
    if (glCurrentVideoItem = '') then Exit;

    videoPlayer.Resume;

    // Включаем таймер
    TimerVideo.Enabled := True;

    // Переотрисовка списка для корректного отображения
    // проиграиваемого элемента
    clboxVideoPlaylist.Repaint;

    // Изменение состояния элементов воспроизведения
    setGlyphSpeedButton(sbtnVideoStop, Application.Location + 'icons' + PathDelim + 'stop.png');
    setGlyphSpeedButton(sbtnVideoPause, Application.Location + 'icons' + PathDelim + 'pause.png');
    setGlyphSpeedButton(sbtnVideoPlay, Application.Location + 'icons' + PathDelim + 'play_active.png');

    if LinkPlaybackControlButtons then
    begin
      LinkPlaybackControlButtons := False;
      sbtnAudioPlayClick(Self);
      LinkPlaybackControlButtons := True;
    end;
  end;
end;



{ Меню }

procedure TfMain.miScenarioNewClick(Sender: TObject);
var i: Integer;
    LBitBtn: TBitBtn;
begin
  ScenarioList.Add(CScenario.Create);
  ScenarioList.Items[ScenarioList.Count-1].Name := 'Untitled' + IntToStr(NewTab_Count);
  Inc(NewTab_Count);
  TabControl.Tabs.Add(ScenarioList.Items[ScenarioList.Count-1].Name);
  for i:= 0 to ScenarioList.Items[TabControl.TabIndex].Bonds.Count - 1 do
    begin
      if fMain.FindComponent('bbtnBondVideo' + IntToStr(ScenarioList.Items[TabControl.TabIndex].Bonds.arProvoking[i])) <> NIL then
        begin
          LBitBtn := (fMain.FindComponent('bbtnBondVideo' + IntToStr(ScenarioList.Items[TabControl.TabIndex].Bonds.arProvoking[i])) as TBitBtn);
          FreeAndNil(LBitBtn);
        end;
    end;
  TabControl.TabIndex := TabControl.Tabs.Count-1;

  ScenarioList.Items[TabControl.TabIndex].AudioFilePaths.Clear();
  ScenarioList.Items[TabControl.TabIndex].AudioFileNames.Clear();
  ScenarioList.Items[TabControl.TabIndex].VideoFilePaths.Clear();
  ScenarioList.Items[TabControl.TabIndex].VideoFileNames.Clear();
  clboxAudioPlaylist.Items := ScenarioList.Items[TabControl.TabIndex].AudioFilePaths;
  clboxVideoPlaylist.Items := ScenarioList.Items[TabControl.TabIndex].AudioFilePaths;
end;

procedure TfMain.LoadScenarioFromJSON(FilePath: String);
var
  jdScenario: TJSONData = NIL;
  jaAudioFilePaths, jaVideoFilePaths, jaAudioBondsProvoking, jaAudioBondsInvoking: TJSONArray;
  i: Integer;
begin
  // Читаем из файла
  jdScenario := GetJSON(ReadFileToString(FilePath));
  // Получаем массив JSON со списком файлов
  jaAudioFilePaths := (jdScenario.FindPath('AudioFilePaths') as TJSONArray); (* Аудио *)
  jaAudioBondsProvoking := (jdScenario.FindPath('AudioBondsProvoking') as TJSONArray); (* Связи *)
  jaAudioBondsInvoking := (jdScenario.FindPath('AudioBondsInvoking') as TJSONArray);   (* Связи *)
  jaVideoFilePaths := (jdScenario.FindPath('VideoFilePaths') as TJSONArray); (* Видео *)

  // Заполняем список файлов (Аудио)
  if jaAudioFilePaths.Count > 0 then
    for i := 0 to jaAudioFilePaths.Count-1 do
    begin
      ScenarioList.Items[TabControl.TabIndex].AudioFilePaths.Add(jaAudioFilePaths.Strings[i]);
      ScenarioList.Items[TabControl.TabIndex].AudioFileNames.Add(ExtractFileName(jaAudioFilePaths.Strings[i]));
      if Not(FileExists(jaAudioFilePaths.Strings[i])) then
      begin
        ScenarioList.Items[TabControl.TabIndex].MissingFiles.Add(jaAudioFilePaths.Strings[i]);
      end;
    end;
  clboxAudioPlaylist.Items := ScenarioList.Items[TabControl.TabIndex].AudioFileNames;

  // Заполняем список (Связи)
  if jaAudioBondsProvoking.Count > 0 then
    for i := 0 to jaAudioBondsProvoking.Count-1 do
    begin
      ScenarioList.Items[TabControl.TabIndex].Bonds.Add(jaAudioBondsProvoking.Integers[i], jaAudioBondsInvoking.Integers[i]);
    end;

  // Заполняем список файлов (Видео)
  if jaVideoFilePaths.Count > 0 then
    for i := 0 to jaVideoFilePaths.Count-1 do
    begin
      if FileExists(jaVideoFilePaths.Strings[i]) then
      begin
        ScenarioList.Items[TabControl.TabIndex].VideoFilePaths.Add(jaVideoFilePaths.Strings[i]);
        ScenarioList.Items[TabControl.TabIndex].VideoFileNames.Add(ExtractFileName(jaVideoFilePaths.Strings[i]));
        if Not(FileExists(jaVideoFilePaths.Strings[i])) then
        begin
          ScenarioList.Items[TabControl.TabIndex].MissingFiles.Add(jaVideoFilePaths.Strings[i]);
        end;
      end;
    end;
  clboxVideoPlaylist.Items := ScenarioList.Items[TabControl.TabIndex].VideoFileNames;

  FreeAndNil(jdScenario);
end;

procedure TfMain.miScenarioOpenClick(Sender: TObject);
var i: Integer;
    LBitBtn: TBitBtn;
    LAllowChange: Boolean = True;
begin
  OpenDialog.FileName := '';
  OpenDialog.Title := 'Открыть сценарий';
  OpenDialog.DefaultExt := '.json';
  OpenDialog.Filter := 'Scenarium File|*.scnr|Все файлы|*.*|';
  if OpenDialog.Execute then
  begin
    for i:= 0 to ScenarioList.Count-1 do
      if OpenDialog.FileName = ScenarioList.Items[i].FilePath then
        begin
          TabControlChanging(Sender, LAllowChange);
          TabControl.TabIndex := i;
          TabControlChange(Sender);
          Exit;
        end;

    openScenario(OpenDialog.FileName);
  end;
end;

procedure TfMain.miScenarioSaveClick(Sender: TObject);
var jObject: TJSONObject;
    jarArrayAudio, jarArrayVideo, jarAudioBondsProvoking, jarAudioBondsInvoking: TJSONArray;
    i: Word;
    StringList: TStringList;
begin
  // Если имя файла был открыт ранее
  if ScenarioList.Items[TabControl.TabIndex].FilePath <> '' then
  begin
    StateNotify.State := snNone;
    StateNotify.State := snProcess;
    jObject := TJSONObject.Create;

    jarArrayAudio := TJSONArray.Create;
    if ScenarioList.Items[TabControl.TabIndex].AudioFilePaths.Count > 0 then
    for i := 0 to ScenarioList.Items[TabControl.TabIndex].AudioFilePaths.Count-1 do
    begin
      jarArrayAudio.Add(ScenarioList.Items[TabControl.TabIndex].AudioFilePaths.Strings[i]);
    end;
    jObject.Add('AudioFilePaths', jarArrayAudio);

    jarAudioBondsProvoking := TJSONArray.Create;
    if ScenarioList.Items[TabControl.TabIndex].Bonds.Count > 0 then
    for i := 0 to ScenarioList.Items[TabControl.TabIndex].Bonds.Count-1 do
    begin
      jarAudioBondsProvoking.Add(ScenarioList.Items[TabControl.TabIndex].Bonds.arProvoking[i]);
    end;
    jObject.Add('AudioBondsProvoking', jarAudioBondsProvoking);

    jarAudioBondsInvoking := TJSONArray.Create;
    if ScenarioList.Items[TabControl.TabIndex].Bonds.Count > 0 then
    for i := 0 to ScenarioList.Items[TabControl.TabIndex].Bonds.Count-1 do
    begin
      jarAudioBondsInvoking.Add(ScenarioList.Items[TabControl.TabIndex].Bonds.arInvoking[i]);
    end;
    jObject.Add('AudioBondsInvoking', jarAudioBondsInvoking);

    jarArrayVideo := TJSONArray.Create;
    if ScenarioList.Items[TabControl.TabIndex].VideoFilePaths.Count > 0 then
    for i := 0 to ScenarioList.Items[TabControl.TabIndex].VideoFilePaths.Count-1 do
    begin
      jarArrayVideo.Add(ScenarioList.Items[TabControl.TabIndex].VideoFilePaths.Strings[i]);
    end;
    jObject.Add('VideoFilePaths', jarArrayVideo);

    StringList := TStringList.Create;
    StringList.Add(jObject.FormatJSON);
    StringList.SaveToFile(UTF8ToSys(ScenarioList.Items[TabControl.TabIndex].FilePath));

    FreeAndNil(jObject);
    FreeAndNil(StringList);

    // Оповещение о выполненном сохранении
    StateNotify.State := snSuccess;
  end
  // Если имя файл не существует
  else
    miScenarioSaveAsClick(Sender);
end;

procedure TfMain.miScenarioSaveAsClick(Sender: TObject);
var jObject: TJSONObject;
    jarAudio, jarVideo, jarAudioBondsProvoking, jarAudioBondsInvoking: TJSONArray;
    i: Word;
    StringList: TStringList;
begin
  SaveDialog.Title := 'Сохранить сценарий как';
  SaveDialog.DefaultExt := '.json';
  SaveDialog.Filter := 'Scenarium File|*.scnr|Все файлы|*.*|';
  StateNotify.State := snNone;
  StateNotify.State := snProcess;
  if SaveDialog.Execute then
  begin
    jObject := TJSONObject.Create;

    jarAudio := TJSONArray.Create;
    if ScenarioList.Items[TabControl.TabIndex].AudioFilePaths.Count > 0 then
    for i := 0 to ScenarioList.Items[TabControl.TabIndex].AudioFilePaths.Count-1 do
    begin
      jarAudio.Add(ScenarioList.Items[TabControl.TabIndex].AudioFilePaths.Strings[i]);
    end;
    jObject.Add('AudioFilePaths', jarAudio);

    jarAudioBondsProvoking := TJSONArray.Create;
    if ScenarioList.Items[TabControl.TabIndex].Bonds.Count > 0 then
    for i := 0 to ScenarioList.Items[TabControl.TabIndex].Bonds.Count-1 do
    begin
      jarAudioBondsProvoking.Add(ScenarioList.Items[TabControl.TabIndex].Bonds.arProvoking[i]);
    end;
    jObject.Add('AudioBondsProvoking', jarAudioBondsProvoking);

    jarAudioBondsInvoking := TJSONArray.Create;
    if ScenarioList.Items[TabControl.TabIndex].Bonds.Count > 0 then
    for i := 0 to ScenarioList.Items[TabControl.TabIndex].Bonds.Count-1 do
    begin
      jarAudioBondsInvoking.Add(ScenarioList.Items[TabControl.TabIndex].Bonds.arInvoking[i]);
    end;
    jObject.Add('AudioBondsInvoking', jarAudioBondsInvoking);

    jarVideo := TJSONArray.Create;
    if ScenarioList.Items[TabControl.TabIndex].VideoFilePaths.Count > 0 then
    for i := 0 to ScenarioList.Items[TabControl.TabIndex].VideoFilePaths.Count-1 do
    begin
      jarVideo.Add(ScenarioList.Items[TabControl.TabIndex].VideoFilePaths.Strings[i]);
    end;
    jObject.Add('VideoFilePaths', jarVideo);

    StringList := TStringList.Create;
    StringList.Add(jObject.FormatJSON);
    StringList.SaveToFile(Utf8ToSys(SaveDialog.FileName));

    ScenarioList.Items[TabControl.TabIndex].FilePath := SaveDialog.FileName;
    ScenarioList.Items[TabControl.TabIndex].Name := ExtractFileName(SaveDialog.FileName);
    TabControl.Tabs.Strings[TabControl.TabIndex] := ScenarioList.Items[TabControl.TabIndex].Name;

    FreeAndNil(jObject);
    FreeAndNil(StringList);
    StateNotify.State := snSuccess;
  end else
    StateNotify.State := snCancel;
end;

procedure TfMain.miScenarioExitClick(Sender: TObject);
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
  AudioExtensions: String = '';
  i: Integer;
begin
  OpenDialog.Title := 'Добавить элемент';
  OpenDialog.DefaultExt := '';
  for i := Low(arrAudioExtensions) to High(arrAudioExtensions) do
    AudioExtensions := AudioExtensions + '*' + arrAudioExtensions[i] + ';';
  OpenDialog.Filter := 'Audio|' + AudioExtensions + '|All files|*.*';
  if OpenDialog.Execute then
  begin
    for FilePath in OpenDialog.Files do
    begin
      ScenarioList.Items[TabControl.TabIndex].AudioFilePaths.Add(FilePath);
      ScenarioList.Items[TabControl.TabIndex].AudioFileNames.Add(ExtractFileName(FilePath));
    end;

    clboxAudioPlaylist.Items := ScenarioList.Items[TabControl.TabIndex].AudioFileNames;
  end;
end;

procedure TfMain.sbtnAudioSubtractClick(Sender: TObject);
var
  bbtnFindedVideoBond: TBitBtn;
  i, tmp_count, t: Integer;
begin
  tmp_count := clboxAudioPlaylist.Count-1;
  i := tmp_count;
  while i >= 0 do
  begin
    if clboxAudioPlaylist.Selected[i] then
    begin
      if clboxAudioPlaylist.Items[i] <> glCurrentAudioItem then
      begin
        { Связи }
        // Сначала очищаем кнопку
        if fMain.FindComponent('bbtnBondVideo' + IntToStr(i)) <> NIL then
          begin
            bbtnFindedVideoBond := (fMain.FindComponent('bbtnBondVideo' + IntToStr(i)) as TBitBtn);
            FreeAndNil(bbtnFindedVideoBond);
            // Затем очищаем саму связь
            ScenarioList.Items[TabControl.TabIndex].Bonds.DeleteWhereProvoking(i);
          end;
        { Связи }
        ScenarioList.Items[TabControl.TabIndex].AudioFileNames.Delete(i);
        ScenarioList.Items[TabControl.TabIndex].AudioFilePaths.Delete(i);
        { Связи }
        // Обновляем индексы в вызывающих
        for t := i to clboxAudioPlaylist.Count-1 do
        begin
          // Очищаем старые кнопки
          if fMain.FindComponent('bbtnBondVideo' + IntToStr(t)) <> NIL then
          begin
            bbtnFindedVideoBond := (fMain.FindComponent('bbtnBondVideo' + IntToStr(t)) as TBitBtn);
            FreeAndNil(bbtnFindedVideoBond);
          end;
          ScenarioList.Items[TabControl.TabIndex].Bonds.UpdateWhereProvoking(t, t-1);
        end;
        { Связи }
      end;
    end;
    Dec(i);
  end;
  clboxAudioPlaylist.Items := ScenarioList.Items[TabControl.TabIndex].AudioFileNames;
end;

procedure TfMain.sbtnAudioClearClick(Sender: TObject);
var
  i: integer;
begin
  i := 0;
  while i <> ScenarioList.Items[TabControl.TabIndex].AudioFileNames.Count do
  begin
    if ScenarioList.Items[TabControl.TabIndex].AudioFileNames.Strings[i] <> glCurrentAudioItem then
    begin
      ScenarioList.Items[TabControl.TabIndex].AudioFileNames.Delete(i);
      ScenarioList.Items[TabControl.TabIndex].AudioFilePaths.Delete(i);
    end
    else
      i += 1;
  end;
  clboxAudioPlaylist.Items := ScenarioList.Items[TabControl.TabIndex].AudioFileNames;
end;


{ Воспроизведение }

procedure TfMain.sbtnAudioPlayClick(Sender: TObject);
begin
  with fPlaybackAudio do
  begin
    if (audioPlayer = nil) then Exit;
    if (glCurrentAudioItem = '') then Exit;

    audioPlayer.Resume;
    // Включаем таймер
    TimerAudio.Enabled := True;

    // Переотрисовка списка для корректного отображения
    // проиграиваемого элемента
    clboxAudioPlaylist.Repaint;

    // Изменение состояния элементов воспроизведения
    setGlyphSpeedButton(sbtnAudioStop, Application.Location + 'icons' + PathDelim + 'stop.png');
    setGlyphSpeedButton(sbtnAudioPause, Application.Location + 'icons' + PathDelim + 'pause.png');
    setGlyphSpeedButton(sbtnAudioPlay, Application.Location + 'icons' + PathDelim + 'play_active.png');

    if LinkPlaybackControlButtons then
      begin
        LinkPlaybackControlButtons := False;
        sbtnVideoPlayClick(Self);
        LinkPlaybackControlButtons := True;
      end;
  end;
end;

procedure TfMain.sbtnVideoPauseClick(Sender: TObject);
begin
  if (glCurrentVideoItem = '') then Exit;

  with fPlaybackVideo do
  begin
    if (videoPlayer = nil) then Exit;

    videoPlayer.Pause;

  end;
  TimerVideo.Enabled := False;

  // Изменение состояния элементов воспроизведения
  setGlyphSpeedButton(sbtnVideoStop, Application.Location + 'icons' + PathDelim + 'stop.png');
  setGlyphSpeedButton(sbtnVideoPlay, Application.Location + 'icons' + PathDelim + 'play.png');
  setGlyphSpeedButton(sbtnVideoPause, Application.Location + 'icons' + PathDelim + 'pause_active.png');

  if LinkPlaybackControlButtons then
    begin
      LinkPlaybackControlButtons := False;
      sbtnAudioPauseClick(Self);
      LinkPlaybackControlButtons := True;
    end;
end;

procedure TfMain.sbtnVideoStopClick(Sender: TObject);
begin
    if (fPlaybackVideo.VideoPlayer = nil) then Exit;
    if (glCurrentVideoItem = '') then Exit;

    if Not(glVideoTrackRepeat) then
      begin
        // Выключаем окно воспроизведения
        if Not(glAudioTrackRepeat) then
          setFullyDisplay(False);

        // Выключаем воспроизведение видео
        fPlaybackVideo.videoPlayer.Stop;

        // Изменение состояния элементов воспроизведения
        setGlyphSpeedButton(sbtnVideoPause, Application.Location + 'icons' + PathDelim + 'pause.png');
        setGlyphSpeedButton(sbtnVideoPlay, Application.Location + 'icons' + PathDelim + 'play.png');
        setGlyphSpeedButton(sbtnVideoStop, Application.Location + 'icons' + PathDelim + 'stop.png');
        if not(glAudioTrackRepeat) then
          begin
            // "Проигриваемый элемент" очищается (Видео)
            glCurrentPlaylist := NIL;
            glCurrentVideoItem := '';
            glCurrentVideoItemIndex := -1;
            lblCurrentVideoItem.Caption := '';
            // Список элементов переотрисовывается (Видео)
            clboxVideoPlaylist.Invalidate;
            // Выключаем таймер (Видео)
            TimerVideo.Enabled := False;
            resetTime('VideoPlayer');
          end;

        if LinkPlaybackControlButtons then
        begin
          if glAudioTrackRepeat then
            begin
              TimerVideo.Enabled := False;
              sbtnAudioStopClick(Sender);
            end
          else
            begin
              LinkPlaybackControlButtons := False;
              LinkPlaybackControlTrackBar := False;
              sbtnAudioStopClick(Self);
              LinkPlaybackControlButtons := True;
              LinkPlaybackControlTrackBar := True;
              sbtnVideoLinkControlButtonsClick(Sender);
            end;
        end;
        if LinkPlaybackControlTrackBar then
          sbtnVideoLinkControlTrackBarClick(Sender);
      end
    else
      begin
        // Выключаем таймер (Видео)
        TimerVideo.Enabled := False;

        OpenAndPlayVideo(glCurrentVideoItemIndex);
      end;
end;

procedure TfMain.sbtnAudioRepeatClick(Sender: TObject);
begin
  glAudioTrackRepeat := Not(glAudioTrackRepeat);

  if glAudioTrackRepeat = True then
    setGlyphSpeedButton(sbtnAudioRepeat, Application.Location + 'icons' + PathDelim + 'repeat_on.png')
  else
    setGlyphSpeedButton(sbtnAudioRepeat, Application.Location + 'icons' + PathDelim + 'repeat_off.png');
end;

procedure TfMain.sbtnAudioLinkControlButtonsClick(Sender: TObject);
begin
  LinkPlaybackControlButtons := Not(LinkPlaybackControlButtons);

  if LinkPlaybackControlButtons then
    begin
      setGlyphSpeedButton(sbtnAudioLinkControlButtons, Application.Location + 'icons' + PathDelim + 'link_on.png');
      setGlyphSpeedButton(sbtnVideoLinkControlButtons, Application.Location + 'icons' + PathDelim + 'link_on.png');
    end
  else
    begin
      setGlyphSpeedButton(sbtnAudioLinkControlButtons, Application.Location + 'icons' + PathDelim + 'link_off.png');
      setGlyphSpeedButton(sbtnVideoLinkControlButtons, Application.Location + 'icons' + PathDelim + 'link_off.png');
    end;
end;

procedure TfMain.TabControlChange(Sender: TObject);
begin
  clboxAudioPlaylist.Clear;
  clboxVideoPlaylist.Clear;
  clboxAudioPlaylist.Items := ScenarioList.Items[TabControl.TabIndex].AudioFileNames;
  clboxVideoPlaylist.Items := ScenarioList.Items[TabControl.TabIndex].VideoFileNames;
end;

procedure TfMain.TabControlChanging(Sender: TObject; var AllowChange: Boolean);
var i: Integer;
    LBitBtn: TBitBtn;
begin
  for i:= 0 to ScenarioList.Items[TabControl.TabIndex].Bonds.Count - 1 do
    begin
      if fMain.FindComponent('bbtnBondVideo' + IntToStr(ScenarioList.Items[TabControl.TabIndex].Bonds.arProvoking[i])) <> NIL then
        begin
          LBitBtn := (fMain.FindComponent('bbtnBondVideo' + IntToStr(ScenarioList.Items[TabControl.TabIndex].Bonds.arProvoking[i])) as TBitBtn);
          FreeAndNil(LBitBtn);
        end;
    end;
end;

procedure TfMain.TabControlDragDrop(Sender, Source: TObject; X, Y: Integer);
var
  tc     : TTabControl;
  newTabIndex : Integer;
begin
  tc := TTabControl(Sender);
  newTabIndex := tc.IndexOfTabAt(X, Y);

  if Source = Sender then begin
    if (newTabIndex <> tc.TabIndex) then
    begin
      if newTabIndex <> -1 then // С вкладки на вкладку
        begin
          ScenarioList.Move(tc.TabIndex, newTabIndex);
          tc.Tabs.Move(tc.TabIndex, newTabIndex);
          tc.TabIndex := newTabIndex;
        end
      else // С вкладки на пустое место
        begin
          ScenarioList.Move(tc.TabIndex, tc.Tabs.Count-1);
          tc.Tabs.Move(tc.TabIndex, tc.Tabs.Count-1);
          tc.TabIndex := tc.Tabs.Count-1;
        end;
    end;
  end;
end;

procedure TfMain.TabControlDragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
  Accept := True;
end;

procedure TfMain.TabControlMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  MousePos_TabControl.X := X;
  MousePos_TabControl.Y := Y;
end;

procedure TfMain.sbtnMonitorConfigureClick(Sender: TObject);
begin
  fMonitorConfigure.ShowModal;
end;

procedure TfMain.trbarAudioTimeChange(Sender: TObject);
begin
  lblAudioTimeCurrent.Caption := SecsToTimeStr(trbarAudioTime.Position);
  if trbarAudioTime.SelEnd = trbarAudioTime.Max-1
    then sbtnAudioStop.Click;
end;

procedure TfMain.trbarAudioTimeMouseLeave(Sender: TObject);
begin
  with trbarAudioTime do
      Position := SelEnd;
  glAudioTrackRewinding := False;

  if LinkPlaybackControlTrackBar then
    begin
      If IsImage(ExtractFileName(glCurrentVideoItem)) then
        Exit;
      glVideoTrackRewinding := False;
      trbarVideoTime.Position := trbarVideoTime.SelEnd;
    end;
end;

procedure TfMain.trbarAudioTimeMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  glAudioTrackRewinding := True;
  with trbarAudioTime do
      Position := Round((Max - Min) / Width * X) + Min;

  if LinkPlaybackControlTrackBar then
    begin
      If IsImage(ExtractFileName(glCurrentVideoItem)) then
        Exit;
      glVideoTrackRewinding := True;
      if trbarAudioTime.Position > trbarVideoTime.Max
        then trbarVideoTime.Position := trbarVideoTime.Max
        else trbarVideoTime.Position := trbarAudioTime.Position;
    end;

end;

procedure TfMain.trbarAudioTimeMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  // Пользователь выбрал нужное время
  trbarAudioTime.SelEnd := trbarAudioTime.Position;
  // Ставим прогресс на позицию метки
  fPlaybackAudio.audioPlayer.Seek(double(trbarAudioTime.Position), False);
  // Ставим на выбранную позицию
  glAudioTrackRewinding := False;
  TimerAudio.Enabled := True; // Включаем таймер
  // Трек больше не перематывается

  if LinkPlaybackControlTrackBar then
    begin
      If IsImage(ExtractFileName(glCurrentVideoItem)) then
        Exit;
      if trbarVideoTime.Position = trbarVideoTime.Max
        then sbtnAudioStopClick(Self)
        else
          begin
            LinkPlaybackControlTrackBar := False;
            If Not(IsImage(glCurrentVideoItem)) then
              trbarVideoTimeMouseUp(Sender, Button, Shift, X, Y);
            LinkPlaybackControlTrackBar := True;
          end;
    end;
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
    setGlyphSpeedButton(sbtnAudioVolume, Application.Location + 'icons' + PathDelim + 'speaker_off.png');
    fPlaybackAudio.audioPlayer.SetMute(True);
  end
  else
  begin
    if trbarAudioVolume.Position < 33 then
      setGlyphSpeedButton(sbtnAudioVolume, Application.Location + 'icons' + PathDelim + 'speaker_0.png')
    else if trbarAudioVolume.Position < 66 then
      setGlyphSpeedButton(sbtnAudioVolume, Application.Location + 'icons' + PathDelim + 'speaker_1.png')
    else
      setGlyphSpeedButton(sbtnAudioVolume, Application.Location + 'icons' + PathDelim + 'speaker_2.png');

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
      setGlyphSpeedButton(sbtnAudioVolume, Application.Location + 'icons' + PathDelim + 'speaker_0.png')
    else if trbarAudioVolume.Position < 66 then
      setGlyphSpeedButton(sbtnAudioVolume, Application.Location + 'icons' + PathDelim + 'speaker_1.png')
    else
      setGlyphSpeedButton(sbtnAudioVolume, Application.Location + 'icons' + PathDelim + 'speaker_2.png');

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
  if trbarVideoTime.SelEnd = trbarVideoTime.Max-1 then
    sbtnVideoStop.Click;
end;

procedure TfMain.trbarVideoTimeMouseLeave(Sender: TObject);
begin
  glVideoTrackRewinding := False;
  with trbarVideoTime do
    Position := SelEnd;

  if LinkPlaybackControlTrackBar then
    begin
      If IsImage(ExtractFileName(glCurrentVideoItem)) then
        Exit;
      glAudioTrackRewinding := False;
      trbarAudioTime.Position := trbarAudioTime.SelEnd;
    end;
end;

procedure TfMain.trbarVideoTimeMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  // Пользователь начал выбирать время
  glVideoTrackRewinding := True;

  with trbarVideoTime do
    Position := Round((Max - Min) / Width * X) + Min;

  if LinkPlaybackControlTrackBar then
    begin
      If IsImage(ExtractFileName(glCurrentVideoItem)) then
        Exit;
      glAudioTrackRewinding := True;
      if trbarVideoTime.Position > trbarAudioTime.Max
        then trbarAudioTime.Position := trbarAudioTime.Max
        else trbarAudioTime.Position := trbarVideoTime.Position;
    end;
end;

procedure TfMain.trbarVideoTimeMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  // Пользователь выбрал нужное время
  if trbarVideoTime.Position = trbarVideoTime.Max then
    trbarVideoTime.Position := trbarVideoTime.Position - 1;
  trbarVideoTime.SelEnd := trbarVideoTime.Position;
  // Ставим прогресс на позицию метки
  fPlaybackVideo.videoPlayer.Seek(double(trbarVideoTime.Position), False);
  // Ставим на выбранную позицию
  glVideoTrackRewinding := False;
  TimerVideo.Enabled := True; // Включаем таймер

  if LinkPlaybackControlTrackBar then
    begin
      If IsImage(ExtractFileName(glCurrentVideoItem)) then
        Exit;
      if trbarAudioTime.Position = trbarAudioTime.Max
        then sbtnVideoStopClick(Self)
        else
          begin
            LinkPlaybackControlTrackBar := False;
            trbarAudioTimeMouseUp(Sender, Button, Shift, X, Y);
            LinkPlaybackControlTrackBar := True;
          end;
    end;
end;

procedure TfMain.trbarVideoVolumeChange(Sender: TObject);
begin
  trbarVideoVolume.SelEnd := trbarVideoVolume.Position;

  if not (glVideoMute) then
  begin
    if trbarVideoVolume.Position < 33 then
      setGlyphSpeedButton(sbtnVideoVolume, Application.Location + 'icons' + PathDelim + 'speaker_0.png')
    else if trbarVideoVolume.Position < 66 then
      setGlyphSpeedButton(sbtnVideoVolume, Application.Location + 'icons' + PathDelim + 'speaker_1.png')
    else
      setGlyphSpeedButton(sbtnVideoVolume, Application.Location + 'icons' + PathDelim + 'speaker_2.png');

    fPlaybackVideo.videoPlayer.SetVolume(double(trbarVideoVolume.Position));
  end;
end;

procedure TfMain.trbarVideoVolumeMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  if not (glVideoMute) then
    fPlaybackVideo.videoPlayer.SetVolume(double(trbarVideoVolume.Position));
end;

procedure TfMain.UniqueInstanceOtherInstance(Sender: TObject;
  ParamCount: Integer; const Parameters: array of String);
begin
  if ParamCount > 0 then
    if FileExists(Parameters[0]) then
      openScenario(Parameters[0]);
end;

procedure TfMain.setFullyDisplay(ShowPlayback: Boolean);
begin
  // Проверка на выбранный монитор
  if MonitorConfigure.PlaybackMonitor = -1 then
    begin
      if fPLaybackVideo.Visible then
        fPLaybackVideo.Visible := False;
      Exit;
    end;

  if ShowPlayback then
  begin
    fPLaybackVideo.Visible := True;
    setGlyphSpeedButton(sbtnFullyDisplay, Application.Location + 'icons' + PathDelim + 'share_screen_off.png');
  end
  else
  begin
    fPLaybackVideo.Visible := False;
    setGlyphSpeedButton(sbtnFullyDisplay, Application.Location + 'icons' + PathDelim + 'share_screen_on.png')
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
    trbarAudioTime.Min := 0;
    trbarAudioTime.Max := 0;
    lblAudioTimeTotal.Caption := '00:00';
    lblAudioTimeCurrent.Caption := '00:00';
  end
  else if LowerCase(Player) = LowerCase('VideoPlayer') then
  begin
    // Время устанавливается в нулевое положение
    trbarVideoTime.SelEnd := 0;
    trbarVideoTime.Position := 0;
    trbarVideoTime.Min := 0;
    trbarVideoTime.Max := 0;
    lblVideoTimeTotal.Caption := '00:00';
    lblVideoTimeCurrent.Caption := '00:00';
  end;
end;

procedure TfMain.SetElementGlyphs;
begin
  // Аудио панель
  setGlyphSpeedButton(sbtnAudioAdd, Application.Location + 'icons' + PathDelim + 'add.png');
  setGlyphSpeedButton(sbtnAudioSubtract, Application.Location + 'icons' + PathDelim + 'subtract.png');
  setGlyphSpeedButton(sbtnAudioPlay, Application.Location + 'icons' + PathDelim + 'play.png');
  setGlyphSpeedButton(sbtnAudioPause, Application.Location + 'icons' + PathDelim + 'pause.png');
  setGlyphSpeedButton(sbtnAudioStop, Application.Location + 'icons' + PathDelim + 'stop.png');
  setGlyphSpeedButton(sbtnAudioClear, Application.Location + 'icons' + PathDelim + 'clear.png');
  setGlyphSpeedButton(sbtnAudioVolume, Application.Location + 'icons' + PathDelim + 'speaker_2.png');
  setGlyphSpeedButton(sbtnAudioRepeat, Application.Location + 'icons' + PathDelim + 'repeat_off.png');
  // Видео панель
  setGlyphSpeedButton(sbtnVideoAdd, Application.Location + 'icons' + PathDelim + 'add.png');
  setGlyphSpeedButton(sbtnVideoSubtract, Application.Location + 'icons' + PathDelim + 'subtract.png');
  setGlyphSpeedButton(sbtnVideoPlay, Application.Location + 'icons' + PathDelim + 'play.png');
  setGlyphSpeedButton(sbtnVideoPause, Application.Location + 'icons' + PathDelim + 'pause.png');
  setGlyphSpeedButton(sbtnVideoStop, Application.Location + 'icons' + PathDelim + 'stop.png');
  setGlyphSpeedButton(sbtnVideoClear, Application.Location + 'icons' + PathDelim + 'clear.png');
  setGlyphSpeedButton(sbtnVideoVolume, Application.Location + 'icons' + PathDelim + 'speaker_2.png');
  setGlyphSpeedButton(sbtnFullyDisplay, Application.Location + 'icons' + PathDelim + 'share_screen_on.png');
  setGlyphSpeedButton(sbtnMonitorConfigure, Application.Location + 'icons' + PathDelim +
    'share_screen_settings.png');
  setGlyphSpeedButton(sbtnVideoRepeat, Application.Location + 'icons' + PathDelim + 'repeat_off.png');
end;

end.
