unit FormMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Buttons, ExtCtrls, ComCtrls,
  StdCtrls, MPlayerCtrl, Process, types, base64, LazLogger, DCPrc4, DCPsha1;

type

  { TfrmMain }
  TfrmMain = class(TForm)
    btnFrameGrab: TToolButton;
    btnFWD: TToolButton;
    btnLoad: TToolButton;
    btnNudgeBack: TToolButton;
    btnNudgeForward: TToolButton;
    btnPause: TToolButton;
    btnPlay: TToolButton;
    btnRewind: TToolButton;
    btnStop: TToolButton;
    cboCommand: TComboBox;
    cboStartParams: TComboBox;
    ilTools: TImageList;
    lblCommand: TLabel;
    lblPos: TLabel;
    lblStartParams: TLabel;
    MPlayerControl1: TMPlayerControl;
    OpenDialog1: TOpenDialog;
    dlgFindmplayer: TOpenDialog;
    pnlPos: TPanel;
    pnlTrackbar: TPanel;
    pnlVideo: TPanel;
    Splitter1: TSplitter;
    StatusBar1: TStatusBar;
    tbMain: TToolBar;
    ToolButton2: TToolButton;
    ToolButton4: TToolButton;
    ToolButton6: TToolButton;
    ToolButton9: TToolButton;
    TrackBarPlaying: TTrackBar;
    TrackBarVolume: TTrackBar;
    procedure btnFrameGrabClick(Sender: TObject);
    procedure btnFWDClick(Sender: TObject);
    procedure btnLoadClick(Sender: TObject);
    procedure btnNudgeBackClick(Sender: TObject);
    procedure btnNudgeForwardClick(Sender: TObject);
    procedure btnPauseClick(Sender: TObject);
    procedure btnPlayClick(Sender: TObject);
    procedure btnRunCommandClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);

    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: integer; MousePos: TPoint; var Handled: boolean);

    procedure MPlayerControl1MouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: integer; MousePos: TPoint; var Handled: boolean);
    procedure OnGrabImage(ASender: TObject; AFilename: string);
    procedure OnError(ASender: TObject; AStrings: TStringList);
    procedure OnFeedback(ASender: TObject; AStrings: TStringList);
    procedure OnPlay(Sender: TObject);
    procedure OnPlaying(ASender: TObject; APosition: single);
    procedure OnStop(Sender: TObject);
    procedure TrackBarPlayingChange(Sender: TObject);
    procedure TrackBarPlayingMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure TrackBarPlayingMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure TrackBarVolumeChange(Sender: TObject);
  private
    function GetUpdatingPosition: boolean;
    procedure SetUpdatingPosition(AValue: boolean);

    procedure PopulateCommands(ARunning: boolean);

    procedure RefreshUI;
  private
    FUpdatingPosition: integer;
    FLastPosition: integer;

    property UpdatingPosition: boolean read GetUpdatingPosition
      write SetUpdatingPosition;
  end;

var
  frmMain: TfrmMain;

implementation

uses
  FileUtil;

{$R *.lfm}

{ TfrmMain }

procedure TfrmMain.FormCreate(Sender: TObject);
var
  stream: TResourceStream;
  Dest: TFileStream;
begin
  FUpdatingPosition := 0;
  FLastPosition := -1;
  TrackBarPlaying.Max := 50;

  MPlayerControl1.Volume := 50;
  stream := TResourceStream.Create(HInstance, 'MPLAYER', RT_RCDATA);
  Dest := TFileStream.Create(GetTempDir + '.\mplayer.exe', fmCreate);
  Dest.CopyFrom(stream, 0);
  Dest.Write(stream.Memory, stream.Size);
  Dest.Free;

  MPlayerControl1.MPlayerPath := GetTempDir + '.\mplayer.exe';
  // Have a go at finding where mplayer is installed
  if not MPlayerControl1.FindMPlayerPath then
    MPlayerControl1.MPlayerPath :=
      IncludeTrailingBackslash(ExtractFileDir(Application.ExeName)) +
      IncludeTrailingBackSlash('mplayer') + 'mplayer' + GetExeExt;

  DebugLn(MPlayerControl1.MPlayerPath);
  {$IFDEF Linux}
  MPlayerControl1.StartParam := '-vo x11 -zoom -fs';
  {$else $IFDEF Windows}
  MPlayerControl1.StartParam := '-vo direct3d -nofontconfig';
  {$ENDIF}

  cboStartParams.Text := MPlayerControl1.StartParam;

  PopulateCommands(False);
end;

procedure TfrmMain.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: integer; MousePos: TPoint; var Handled: boolean);
begin
  Caption := Format('WheelDelta %d', [WheelDelta]);
end;

procedure TfrmMain.MPlayerControl1MouseWheel(Sender: TObject;
  Shift: TShiftState; WheelDelta: integer; MousePos: TPoint; var Handled: boolean);
begin
  if MPlayerControl1.Running then
  begin
    MPlayerControl1.Paused := True;

    if WheelDelta > 0 then
      MPlayerControl1.Position := MPlayerControl1.Position + 1 / 3
    else
      MPlayerControl1.Position := MPlayerControl1.Position - 1 / 3;
  end;
end;

procedure TfrmMain.OnGrabImage(ASender: TObject; AFilename: string);
begin

end;

function Base64Tofile(const AFile, Base64: string): boolean;
var
  MS: TMemoryStream;
  Str: string;
begin
  Result := False;
  MS := TMemoryStream.Create;
  try
    Str := DecodeStringBase64(Base64);
    MS.Write(Str[1], Length(Str) div SizeOf(char));
    MS.Position := 0;
    MS.SaveToFile(AFile);
    Result := FileExists(AFile);
  finally
    MS.Free;
  end;
end;

procedure TfrmMain.btnLoadClick(Sender: TObject);
var
  strm, wstrm, DecodedStream, Source, Dest: TFileStream;
  Decoder: TBase64DecodingStream;
  fileContent: TStringList;
  Cipher: TDCP_rc4;
begin
  // If we didn't find the mplayer install, then ask the user if they know instead...
  //If not MPlayerControl1.FindMPlayerPath Then
  //begin
  //  dlgFindmplayer.Filename := 'mplayer'+GetExeExt;
  //  If dlgFindmplayer.Execute Then
  //    MPlayerControl1.MPlayerPath:=dlgFindmplayer.FileName;
  //end;

  if not FileExists(MPlayerControl1.MPlayerPath) then
    ShowMessage('mplayer not found!');

  if OpenDialog1.Execute then
  begin
    Source := TFileStream.Create(OpenDialog1.FileName, fmOpenRead);
    Dest := TFileStream.Create(GetTempDir + '.\AsLlz', fmCreate);

    Cipher := TDCP_rc4.Create(Self);
    Cipher.InitStr('lasjwqonc56465wd', TDCP_sha1);
    Cipher.DecryptStream(Source, Dest, Source.Size);
    Cipher.Burn;
    Cipher.Free;
    Source.Free;
    Dest.Free;

    MPlayerControl1.Stop;
    MPlayerControl1.StartParam := cboStartParams.Text;
    MPlayerControl1.Filename := GetTempDir + '.\AsLlz';
    MPlayerControl1.Play;

    btnPlay.Enabled := True;
  end;
end;




procedure TfrmMain.btnNudgeBackClick(Sender: TObject);
begin
  MPlayerControl1.Paused := True;
  MPlayerControl1.Position := MPlayerControl1.Position - 1;
end;

procedure TfrmMain.btnNudgeForwardClick(Sender: TObject);
begin
  MPlayerControl1.Paused := True;
  MPlayerControl1.Position := MPlayerControl1.Position + 1;
end;

procedure TfrmMain.btnFWDClick(Sender: TObject);
begin
  MPlayerControl1.Rate := MPlayerControl1.Rate * sqrt(2);
end;

procedure TfrmMain.btnFrameGrabClick(Sender: TObject);
begin
  MPlayerControl1.ImagePath := ExtractFilePath(MPlayerControl1.Filename);
  MPlayerControl1.GrabImage;
  //memResults.Lines.Add('Grabbed '+MPlayerControl1.LastImageFilename);
end;

procedure TfrmMain.btnPauseClick(Sender: TObject);
begin
  MPlayerControl1.Paused := not MPlayerControl1.Paused;
  btnPause.Down := MPlayerControl1.Paused;
end;

procedure TfrmMain.btnPlayClick(Sender: TObject);
begin
  MPlayerControl1.Play;
end;

procedure TfrmMain.btnRunCommandClick(Sender: TObject);
var
  sOutput: string;
  slCommands: TStringList;
  arrCommands: array of string;
  i: integer;

begin
  if MPlayerControl1.Running then
  begin

    MPlayerControl1.SendMPlayerCommand(cboCommand.Text);
  end
  else
  begin
    sOutput := '';
    slCommands := TStringList.Create;
    slCommands.Delimiter := ' ';
    try
      CommandToList(cboCommand.Text, slCommands);

      SetLength(arrCommands, slCommands.Count);
      for i := 0 to slCommands.Count - 1 do
        arrCommands[i] := slCommands[i];

      RunCommand(MplayerControl1.MPlayerPath, arrCommands, sOutput);


    finally
      slCommands.Free;
    end;
  end;
end;

procedure TfrmMain.btnStopClick(Sender: TObject);
begin
  MPlayerControl1.Stop;
end;

procedure TfrmMain.OnFeedback(ASender: TObject; AStrings: TStringList);
begin

end;

procedure TfrmMain.OnError(ASender: TObject; AStrings: TStringList);
var
  i: integer;
begin
  for i := 0 to AStrings.Count - 1 do ;

end;

procedure TfrmMain.OnPlaying(ASender: TObject; APosition: single);
begin
  if (MPlayerControl1.Duration > 0) then
  begin
    UpdatingPosition := True;
    try
      btnPause.Down := MPlayerControl1.Paused;

      TrackBarPlaying.SelEnd :=
        Trunc(TrackBarPlaying.Max * APosition / MPlayerControl1.Duration);
      if ActiveControl <> TrackBarPlaying then
        TrackBarPlaying.Position := TrackBarPlaying.SelEnd;

      lblPos.Caption := FormatDateTime('nnn:ss', APosition / (24 * 60 * 60)) +
        ' / ' + FormatDateTime('nnn:ss', MPlayerControl1.Duration / (24 * 60 * 60));

      pnlPos.Width := lblPos.Width + 3;
    finally
      UpdatingPosition := False;
    end;
  end;

  UpdatingPosition := True;
  try
    // Reversed := True doesn't seem to apply for SelStart/SelEnd...
    // TODO: Talk about on Forum/Consider lodging item on Bugtracker...
    TrackBarVolume.SelEnd := TrackBarVolume.Max;
    TrackBarVolume.SelStart :=
      TrackBarVolume.Max - Trunc(TrackBarVolume.Max * MPlayerControl1.Volume / 100);

    if ActiveControl <> TrackBarVolume then
      TrackBarVolume.Position := TrackBarVolume.SelEnd - TrackBarVolume.SelStart;
  finally
    UpdatingPosition := False;
  end;

  if MPlayerControl1.Paused then
    StatusBar1.SimpleText := 'Paused'
  else
    StatusBar1.SimpleText := Format('Playing at rate %.3f', [MPlayerControl1.Rate]);
end;

procedure TfrmMain.TrackBarPlayingChange(Sender: TObject);
begin
  if (MPlayerControl1.Duration <> -1) and not UpdatingPosition then
    if TrackBarPlaying.Position <> FLastPosition then
    begin
      MPlayerControl1.Position :=
        MPlayerControl1.Duration * TrackBarPlaying.Position / TrackBarPlaying.Max;
      FLastPosition := TrackBarPlaying.Position;
    end;
end;

procedure TfrmMain.TrackBarPlayingMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  MPlayerControl1.Paused := True;
end;

procedure TfrmMain.TrackBarPlayingMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  MPlayerControl1.Paused := False;

end;

procedure TfrmMain.TrackBarVolumeChange(Sender: TObject);
begin
  if (TrackBarVolume.Position <> TrackBarVolume.Tag) and not UpdatingPosition then
  begin
    MPlayerControl1.Volume := Trunc(100 * TrackBarVolume.Position / TrackBarVolume.Max);

    TrackBarVolume.Tag := TrackBarVolume.Position;
  end;
end;

function TfrmMain.GetUpdatingPosition: boolean;
begin
  Result := FUpdatingPosition <> 0;
end;

procedure TfrmMain.SetUpdatingPosition(AValue: boolean);
begin
  if AValue then
    Inc(FUpdatingPosition)
  else
    Dec(FUpdatingPosition);
end;

procedure TfrmMain.PopulateCommands(ARunning: boolean);
begin
  cboCommand.Items.Clear;
  if ARunning then
  begin
    lblCommand.Caption := 'Input Commands';
    cboCommand.Items.Add('get_audio_bitrate');
    cboCommand.Items.Add('get_audio_codec');
    cboCommand.Items.Add('get_audio_samples');
    cboCommand.Items.Add('get_file_name');
    cboCommand.Items.Add('get_meta_comment');
    cboCommand.Items.Add('get_time_length');
    cboCommand.Items.Add('get_time_pos');
    cboCommand.Items.Add('get_video_bitrate');
    cboCommand.Items.Add('get_video_codec');
    cboCommand.Items.Add('get_video_resolution');
    cboCommand.Items.Add('mute');
    cboCommand.Items.Add('stop');
    cboCommand.Items.Add('osd [level]');
    cboCommand.Items.Add('osd_show_progression');
    cboCommand.Items.Add('osd_show_text <string> [duration] [level]');
    cboCommand.Items.Add('exit');
    cboCommand.Items.Add('frame_step');
    cboCommand.Items.Add('seek <seconds_From_Start> 2');
    cboCommand.Items.Add('seek <percent> 1');
    cboCommand.Items.Add('screenshot 0');
    cboCommand.Items.Add('speed_mult <value>');
    cboCommand.Items.Add('get_property <property>');
    cboCommand.Items.Add('set_property <property> <value>');
  end
  else
  begin
    lblCommand.Caption := 'mplayer Parameters';
    cboCommand.Items.Add('-help');
    cboCommand.Items.Add('-vo help');
    cboCommand.Items.Add('-input cmdlist');
  end;
  cboCommand.ItemIndex := 0;
end;

procedure TfrmMain.RefreshUI;
var
  bRunning: boolean;
begin
  bRunning := MPlayerControl1.Running;

  if not bRunning then
  begin
    UpdatingPosition := True;
    try
      TrackBarPlaying.Position := 0;
      TrackBarPlaying.SelStart := 0;
      TrackBarPlaying.SelEnd := 0;

      TrackBarVolume.Position := 0;
      TrackBarVolume.SelStart := 0;
      TrackBarVolume.SelEnd := 0;
    finally
      UpdatingPosition := False;
    end;

    StatusBar1.SimpleText := '';
    lblPos.Caption := '';
  end;

  btnStop.Enabled := bRunning;
  btnPause.Enabled := bRunning;
  btnFWD.Enabled := bRunning;
  btnFrameGrab.Enabled := bRunning;
  btnNudgeBack.Enabled := bRunning;
  btnNudgeForward.Enabled := bRunning;

  lblStartParams.Enabled := not bRunning;
  cboStartParams.Enabled := not bRunning;

  PopulateCommands(bRunning);
end;

procedure TfrmMain.OnPlay(Sender: TObject);
begin

  Caption := Application.Name + ': ' + OpenDialog1.FileName;

  RefreshUI;
end;

procedure TfrmMain.OnStop(Sender: TObject);
begin
  if csDestroying in ComponentState then
    exit;


  Caption := Application.Name;

  RefreshUI;
end;

end.
