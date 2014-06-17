Unit FormMain;

{$mode objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Buttons, ExtCtrls, ComCtrls,
  StdCtrls, MPlayerCtrl;

Type

  { TfrmMain }

  TfrmMain = Class(TForm)
    btnInvalid: TBitBtn;
    btnLoad: TBitBtn;
    btnRunCommand: TButton;
    cboCommand: TComboBox;
    lblPos: TLabel;
    memResults: TMemo;
    MPlayerControl1: TMPlayerControl;
    OpenDialog1: TOpenDialog;
    Panel2: TPanel;
    Panel3: TPanel;
    pnlCommands: TPanel;
    pnlControl: TPanel;
    pnlToolbar: TPanel;
    pnlVideo: TPanel;
    Splitter1: TSplitter;
    TrackBar1: TTrackBar;
    Procedure btnInvalidClick(Sender: TObject);
    Procedure btnLoadClick(Sender: TObject);
    Procedure btnRunCommandClick(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
    Procedure OnError(ASender: TObject; AStrings: TStringList);
    Procedure OnFeedback(ASender: TObject; AStrings: TStringList);
    Procedure OnPlay(Sender: TObject);
    Procedure OnPlaying(ASender: TObject; APosition: Single);
    Procedure OnStop(Sender: TObject);
    Procedure TrackBar1Change(Sender: TObject);
  End;

Var
  frmMain: TfrmMain;

Implementation

{$R *.lfm}

{ TfrmMain }

Procedure TfrmMain.FormCreate(Sender: TObject);
Begin
  TrackBar1.Max := 100;

  MPlayerControl1.OnFeedback := @OnFeedback;
  MPlayerControl1.OnError := @OnError;
  MPlayerControl1.OnPlaying := @OnPlaying;
  MPlayerControl1.OnPlay := @OnPlay;
  MPlayerControl1.OnStop := @OnStop;

  MPlayerControl1.MPlayerPath := 'B:\Code\Compile\mplayer-svn-37216\mplayer.exe';
  MPlayerControl1.StartParam := '-vf screenshot';
End;

Procedure TfrmMain.btnLoadClick(Sender: TObject);
Begin
  If OpenDialog1.Execute Then
  Begin
    MPlayerControl1.Stop;
    memResults.Lines.Clear;
    MPlayerControl1.Filename := '"' + OpenDialog1.Filename + '"';
    MPlayerControl1.Play;
  End;
End;

Procedure TfrmMain.btnInvalidClick(Sender: TObject);
Begin
  MPlayerControl1.Filename :=
    'B:\Code\Compile\Test Data\6GIRBC1EDC4_OA_FL_2012-R~001_12-06-07_01-50-56_1.pkt';
  MPlayerControl1.Play;
End;

Procedure TfrmMain.btnRunCommandClick(Sender: TObject);
Begin
  MPlayerControl1.SendMPlayerCommand(cboCommand.Text);
End;

Procedure TfrmMain.OnFeedback(ASender: TObject; AStrings: TStringList);
Begin
  memResults.Lines.AddStrings(AStrings);

  memResults.SelStart:=Length(memResults.Text);
  memResults.SelLength:=0;
End;

Procedure TfrmMain.OnError(ASender: TObject; AStrings: TStringList);
Var
  i: Integer;
Begin
  memResults.Lines.Add('Error');
  memResults.Lines.Add('-----');
  For i := 0 To AStrings.Count - 1 Do
    memResults.Lines.Add('   ' + AStrings[i]);
End;

Procedure TfrmMain.OnPlaying(ASender: TObject; APosition: Single);
Begin
  If (MPlayerControl1.Duration <> -1) Then
  Begin
    Trackbar1.Tag := 100;
    Try
      Trackbar1.Position := Trunc(100 * APosition / MPlayerControl1.Duration);

      lblPos.Caption := Format('%.1f / %.1f', [APosition, MPlayerControl1.Duration]);
    Finally
      Trackbar1.Tag := 0;
    End;
  End;
End;

Procedure TfrmMain.TrackBar1Change(Sender: TObject);
Begin
  If (TrackBar1.Tag = 0) And (MPlayerControl1.Duration <> -1) Then
    MPlayerControl1.Position := MPlayerControl1.Duration * TrackBar1.Position / 100;
End;

Procedure TfrmMain.OnPlay(Sender: TObject);
Begin
  pnlVideo.Visible := True;
End;

Procedure TfrmMain.OnStop(Sender: TObject);
Begin
  pnlVideo.Visible := False;

  Trackbar1.Tag := 100;
  Try
    Trackbar1.Position := 0;
  Finally
    Trackbar1.Tag := 0;
  End;

  lblPos.Caption := '';
End;

End.
