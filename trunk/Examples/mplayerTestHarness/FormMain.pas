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
    pnlTrackbar: TPanel;
    Panel3: TPanel;
    pnlCommands: TPanel;
    pnlFeedback: TPanel;
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
  private
    function GetUpdatingPosition: Boolean;
    procedure SetUpdatingPosition(AValue: Boolean);
  private
    FUpdatingPosition : Integer;
    FLastPosition : Integer;

    Property UpdatingPosition : Boolean Read GetUpdatingPosition Write SetUpdatingPosition;
  End;

Var
  frmMain: TfrmMain;

Implementation

{$R *.lfm}

{ TfrmMain }

procedure TfrmMain.FormCreate(Sender: TObject);
Begin
  FUpdatingPosition := 0;
  FLastPosition := -1;
  TrackBar1.Max := 100;

  MPlayerControl1.OnFeedback := @OnFeedback;
  MPlayerControl1.OnError := @OnError;
  MPlayerControl1.OnPlaying := @OnPlaying;
  MPlayerControl1.OnPlay := @OnPlay;
  MPlayerControl1.OnStop := @OnStop;

  MPlayerControl1.MPlayerPath := 'B:\Code\Compile\mplayer-svn-37216\mplayer.exe';
  MPlayerControl1.StartParam := '-vf screenshot';
End;

procedure TfrmMain.btnLoadClick(Sender: TObject);
Begin
  If OpenDialog1.Execute Then
  Begin
    MPlayerControl1.Stop;
    memResults.Lines.Clear;
    MPlayerControl1.Filename := '"' + OpenDialog1.Filename + '"';
    MPlayerControl1.Play;
  End;
End;

procedure TfrmMain.btnInvalidClick(Sender: TObject);
Begin
  MPlayerControl1.Filename :=
    'B:\Code\Compile\Test Data\6GIRBC1EDC4_OA_FL_2012-R~001_12-06-07_01-50-56_1.pkt';
  MPlayerControl1.Play;
End;

procedure TfrmMain.btnRunCommandClick(Sender: TObject);
Begin
  MPlayerControl1.SendMPlayerCommand(cboCommand.Text);
End;

procedure TfrmMain.OnFeedback(ASender: TObject; AStrings: TStringList);
Begin
  memResults.Lines.AddStrings(AStrings);

  memResults.SelStart:=Length(memResults.Text);
  memResults.SelLength:=0;
End;

procedure TfrmMain.OnError(ASender: TObject; AStrings: TStringList);
Var
  i: Integer;
Begin
  For i := 0 To AStrings.Count - 1 Do
    memResults.Lines.Add(' Err: ' + AStrings[i]);
End;

procedure TfrmMain.OnPlaying(ASender: TObject; APosition: Single);
Begin
  If (MPlayerControl1.Duration <> -1) Then
  Begin
    // Ignore TrackBar1.OnChange as it's us that's changing it
    UpdatingPosition := True;
    Try
      Trackbar1.Position := Trunc(100 * APosition / MPlayerControl1.Duration);

      lblPos.Caption := Format('%.1f / %.1f', [APosition, MPlayerControl1.Duration]);
    Finally
      UpdatingPosition := False;
    End;
  End;
End;

procedure TfrmMain.TrackBar1Change(Sender: TObject);
Begin
  If (Not UpdatingPosition) And (MPlayerControl1.Duration <> -1) Then
  begin
    If Trackbar1.Position<>FLastPosition Then
    begin
      UpdatingPosition := True;
      Try
        MPlayerControl1.Position := MPlayerControl1.Duration * TrackBar1.Position / 100;
        FLastPosition := TrackBar1.Position;
      finally
        UpdatingPosition := False;
      end;
    end;
  end;
End;

function TfrmMain.GetUpdatingPosition: Boolean;
begin
  Result := FUpdatingPosition<>0;
end;

procedure TfrmMain.SetUpdatingPosition(AValue: Boolean);
begin
  If AValue Then
    Inc(FUpdatingPosition)
  Else
    Dec(FUpdatingPosition);
end;

procedure TfrmMain.OnPlay(Sender: TObject);
Begin
  memResults.Lines.Add('OnPlay message received');
End;

procedure TfrmMain.OnStop(Sender: TObject);
Begin
  If csDestroying in ComponentState Then
    exit;

  memResults.Lines.Add('OnStop message received');

  UpdatingPosition:=True;
  Try
    Trackbar1.Position := 0;
  Finally
    UpdatingPosition:=False;
  End;

  lblPos.Caption := '';
End;

End.
