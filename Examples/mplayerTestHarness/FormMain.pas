Unit FormMain;

{$mode objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Buttons, ExtCtrls, ComCtrls,
  StdCtrls, MPlayerCtrl;

Type

  { TfrmMain }

  TfrmMain = Class(TForm)
    btnRunCommand: TButton;
    cboCommand: TComboBox;
    ilTools: TImageList;
    lblPos: TLabel;
    memResults: TMemo;
    MPlayerControl1: TMPlayerControl;
    OpenDialog1: TOpenDialog;
    pnlTrackbar: TPanel;
    pnlPos: TPanel;
    pnlCommands: TPanel;
    pnlFeedback: TPanel;
    pnlVideo: TPanel;
    Splitter1: TSplitter;
    StatusBar1: TStatusBar;
    tbMain: TToolBar;
    btnLoad: TToolButton;
    btnFrameGrab: TToolButton;
    btnNudgeBack: TToolButton;
    ToolButton2: TToolButton;
    btnPlay: TToolButton;
    btnStop: TToolButton;
    btnPause: TToolButton;
    btnNudgeForward: TToolButton;
    ToolButton4: TToolButton;
    ToolButton6: TToolButton;
    btnRewind: TToolButton;
    btnFWD: TToolButton;
    ToolButton9: TToolButton;
    TrackBarPlaying: TTrackBar;
    TrackBarVolume: TTrackBar;
    Procedure btnLoadClick(Sender: TObject);
    procedure btnPauseClick(Sender: TObject);
    procedure btnPlayClick(Sender: TObject);
    Procedure btnRunCommandClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
    Procedure OnError(ASender: TObject; AStrings: TStringList);
    Procedure OnFeedback(ASender: TObject; AStrings: TStringList);
    Procedure OnPlay(Sender: TObject);
    Procedure OnPlaying(ASender: TObject; APosition: Single);
    Procedure OnStop(Sender: TObject);
    Procedure TrackBarPlayingChange(Sender: TObject);

      procedure TrackBarPlayingMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

        procedure TrackBarPlayingMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure TrackBarVolumeChange(Sender: TObject);
  Private
    Function GetUpdatingPosition: Boolean;
    Procedure SetUpdatingPosition(AValue: Boolean);
  Private
    FUpdatingPosition: Integer;
    FLastPosition: Integer;

    Property UpdatingPosition: Boolean read GetUpdatingPosition write SetUpdatingPosition;
  End;

Var
  frmMain: TfrmMain;

Implementation

{$R *.lfm}

{ TfrmMain }

Procedure TfrmMain.FormCreate(Sender: TObject);
Begin
  FUpdatingPosition := 0;
  FLastPosition := -1;
  TrackBarPlaying.Max := 50;

  MPlayerControl1.OnFeedback := @OnFeedback;
  MPlayerControl1.OnError := @OnError;
  MPlayerControl1.OnPlaying := @OnPlaying;
  MPlayerControl1.OnPlay := @OnPlay;
  MPlayerControl1.OnStop := @OnStop;

  MPlayerControl1.Volume := 50;

  //MPlayerControl1.MPlayerPath := 'B:\Code\Compile\mplayer-svn-37216\mplayer.exe';
  MPlayerControl1.MPlayerPath := IncludeTrailingBackslash(ExtractFileDir(Application.ExeName))+
    IncludeTrailingBackslash('..')+
    IncludeTrailingBackslash('mplayer')+
    'mplayer.exe';

  //MPlayerControl1.StartParam := '-vf screenshot';
End;

Procedure TfrmMain.btnLoadClick(Sender: TObject);
Begin
  If OpenDialog1.Execute Then
  Begin
    MPlayerControl1.Stop;
    memResults.Lines.Clear;
    MPlayerControl1.Filename := OpenDialog1.Filename;
    MPlayerControl1.Play;
  End;
End;

procedure TfrmMain.btnPauseClick(Sender: TObject);
begin
  MPlayerControl1.Paused := Not MPlayerControl1.Paused;
  btnPause.Down := MPlayerControl1.Paused;
end;

procedure TfrmMain.btnPlayClick(Sender: TObject);
begin
  MPlayerControl1.Play;
end;

Procedure TfrmMain.btnRunCommandClick(Sender: TObject);
Begin
  memResults.Lines.Add(cboCommand.Text);
  MPlayerControl1.SendMPlayerCommand(cboCommand.Text);
End;

procedure TfrmMain.btnStopClick(Sender: TObject);
begin
  MPlayerControl1.Stop;
end;

Procedure TfrmMain.OnFeedback(ASender: TObject; AStrings: TStringList);
Begin
  memResults.Lines.AddStrings(AStrings);

  memResults.SelStart := Length(memResults.Text);
  //memResults.SelLength := 0;
End;

Procedure TfrmMain.OnError(ASender: TObject; AStrings: TStringList);
Var
  i: Integer;
Begin
  For i := 0 To AStrings.Count - 1 Do
    memResults.Lines.Add(' Err: ' + AStrings[i]);
End;

Procedure TfrmMain.OnPlaying(ASender: TObject; APosition: Single);
Begin
  If (MPlayerControl1.Duration <> -1) Then
  Begin
    UpdatingPosition:=True;
    Try
      btnPause.Down := MPlayerControl1.Paused;

      TrackBarPlaying.SelEnd := Trunc(TrackBarPlaying.Max * APosition / MPlayerControl1.Duration);
      If ActiveControl<>TrackBarPlaying Then
        TrackBarPlaying.Position := TrackBarPlaying.SelEnd;

      lblPos.Caption := FormatDateTime('nnn:ss', APosition / (24 * 60 * 60)) + ' / '+
                        FormatDateTime('nnn:ss', MPlayerControl1.Duration / (24 * 60 * 60));

      pnlPos.Width := lblPos.Width + 3;

      // Reversed := True doesn't seem to apply for SelStart/SelEnd...
      // TODO: Talk about on Forum/Consider lodging item on Bugtracker...
      TrackBarVolume.SelEnd := TrackBarVolume.Max;
      TrackBarVolume.SelStart := TrackBarVolume.Max - Trunc(TrackBarVolume.Max * MPlayerControl1.Volume / 100);

      If ActiveControl<>TrackBarVolume Then
        TrackBarVolume.Position := TrackBarVolume.SelEnd - TrackBarVolume.SelStart;
    finally
      UpdatingPosition:=False;
    end;
  End;
End;

Procedure TfrmMain.TrackBarPlayingChange(Sender: TObject);
Begin
  If (MPlayerControl1.Duration <> -1) And Not UpdatingPosition Then
    If TrackBarPlaying.Position <> FLastPosition Then
    Begin
      MPlayerControl1.Position := MPlayerControl1.Duration * TrackBarPlaying.Position / TrackBarPlaying.Max;
      FLastPosition := TrackBarPlaying.Position;
    End;
End;

procedure TfrmMain.TrackBarPlayingMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  MPlayerControl1.Paused := True;
end;

procedure TfrmMain.TrackBarPlayingMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  MPlayerControl1.Paused := False;
end;

procedure TfrmMain.TrackBarVolumeChange(Sender: TObject);
begin
  If (TrackBarVolume.Position<>TrackBarVolume.Tag) And Not UpdatingPosition Then
  begin
    MPlayerControl1.Volume := Trunc(100 * TrackBarVolume.Position / TrackBarVolume.Max);

    TrackBarVolume.Tag := TrackBarVolume.Position;
  end;
end;

Function TfrmMain.GetUpdatingPosition: Boolean;
Begin
  Result := FUpdatingPosition <> 0;
End;

Procedure TfrmMain.SetUpdatingPosition(AValue: Boolean);
Begin
  If AValue Then
    Inc(FUpdatingPosition)
  Else
    Dec(FUpdatingPosition);
End;

Procedure TfrmMain.OnPlay(Sender: TObject);
Begin
  memResults.Lines.Add('OnPlay message received');
  StatusBar1.SimpleText := 'Playing '+MPlayerControl1.Filename;

  btnStop.Enabled := MPlayerControl1.Running;
  btnPause.Enabled := MPlayerControl1.Running;
End;

Procedure TfrmMain.OnStop(Sender: TObject);
Begin
  If csDestroying In ComponentState Then
    exit;

  memResults.Lines.Add('OnStop message received');
  StatusBar1.SimpleText := '';

  UpdatingPosition := True;
  Try
    TrackBarPlaying.Position := 0;
    TrackBarPlaying.SelStart := 0;
  Finally
    UpdatingPosition := False;
  End;

  btnStop.Enabled := MPlayerControl1.Running;
  btnPause.Enabled := MPlayerControl1.Running;

  lblPos.Caption := '';
End;

End.
