{ LCL control for playing videos using mplayer under gtk2

  Copyright (C) 2009 Mattias Gaertner mattias@freepascal.org

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

Changes:
  2014-03-24  Changes for Microsoft Windows Compatibility and added Events
              for Mouse Actions/ Michael Koecher aka six1

  2014-06-21  Added OnFeedback/OnError events
              Added OnPlay, OnStop, OnPlaying events
              Expanded Timer code to track state of Player
              Added pause_keep_force to all commands
                (requesting state information was enough to resume paused video)
              Added Duration, Position
              Replaced StrToCmdParam with AnsiQuotedStr in Play
                (StrToCmdParam didn't work under Windows - wrapped filename in ', Windows needed ")
              Persisted FCanvas outside of IDE to prevent painting issues when no file playing
                by Mike Thompson
TODO
  2014-06-21
              EXTENSIVE TESTING UNDER LINUX
              Consider descending control from TGraphicControl (instead of creating FCanvas)
              Add Rate(dRate)
              Add StepForward(increment), Stepback(increment)
              Add FrameGrab (and OnFrameGrab)
              Hide PlayerProcess (OnFeedback/OnError events + Running property
                means there is no reason for this to be exposed...)
              Find out if AnsiQuotedStr breaks unicode filenames
              Find out if AnsiQuotedStr works under Linux (files with spaces or " in filename)
              set Volume on Play
              Find out what happens if Volume <0 or >100

}
Unit MPlayerCtrl;

{$mode objfpc}{$H+}

{$ifdef Linux}
 {$ifndef LCLgtk2}
 {$error this unit only supports LCL under gtk2}
 {$endif}
{$endif}

Interface

Uses
  Classes, SysUtils, Controls, WSLCLClasses, LCLProc, LCLType, InterfaceBase,
  LResources, LMessages, Graphics, ExtCtrls, FileUtil, Process, UTF8Process,
  LazFileUtils
  {$ifdef Linux}
  , gtk2int, gtk2, glib2, gdk2x, Gtk2WSControls, GTK2Proc, Gtk2Def
  {$endif}  ;

Type

  { TCustomMPlayerControl }

  TOnFeedback = Procedure(ASender: TObject; AStrings: TStringList) Of Object;
  TOnError = Procedure(ASender: TObject; AStrings: TStringList) Of Object;
  TOnPlaying = Procedure(ASender: TObject; APosition: Single) Of Object;

  TCustomMPlayerControl = Class(TWinControl)
  Private
    FCanvas: TCanvas;
    FDuration: Single;
    FFilename: String;
    FLastPosition: String;
    FLoop: Integer;
    FMPlayerPath: String;
    FOnError: TOnError;
    FOnFeedback: TOnFeedback;
    FOnPlay: TNotifyEvent;
    FOnPlaying: TOnPlaying;
    FOnStop: TNotifyEvent;
    FOutList: TStringList;
    FPaused: Boolean;
    FPlayerProcess: TProcessUTF8;
    FRequestVolume: Boolean;
    FStartParam: String;
    FTimer: TTimer;
    FVolume: Integer;
    Function GetPosition: Single;
    Procedure SetFilename(Const AValue: String);
    Procedure SetLoop(Const AValue: Integer);
    Procedure SetMPlayerPath(Const AValue: String);
    Procedure SetPaused(Const AValue: Boolean);
    Procedure SetPosition(AValue: Single);
    Procedure SetVolume(Const AValue: Integer);
    Procedure SetStartParam(Const AValue: String);
    Procedure TimerEvent(Sender: TObject);
  Protected
    Procedure WMPaint(Var Message: TLMPaint); Message LM_PAINT;
    Procedure WMSize(Var Message: TLMSize); Message LM_SIZE;

    Function DoCommand(ACommand, AResultIdentifier: String): String;
  Public
    Constructor Create(TheOwner: TComponent); Override;
    Destructor Destroy; Override;
    Procedure SendMPlayerCommand(Cmd: String); // see: mplayer -input cmdlist and http://www.mplayerhq.hu/DOCS/tech/slave.txt
    Function Running: Boolean;
    Procedure Play;
    Procedure Stop;
    Function Playing: Boolean;
    Procedure Invalidate; Override;
    Procedure EraseBackground(DC: HDC); Override;
  Public
    Property Filename: String read FFilename write SetFilename;
    Property StartParam: String read FStartParam write SetStartParam;
    Property MPlayerPath: String read FMPlayerPath write SetMPlayerPath;
    Property PlayerProcess: TProcessUTF8 read FPlayerProcess;
    Property Paused: Boolean read FPaused write SetPaused;
    Property Loop: Integer read FLoop write SetLoop; // -1 no, 0 forever, 1 once, 2 twice, ...
    Property Volume: Integer read FVolume write SetVolume;

    Property Duration: Single read FDuration; // seconds
    Property Position: Single read GetPosition write SetPosition; // seconds

    Property OnFeedback: TOnFeedback read FOnFeedback write FOnFeedback;
    Property OnError: TOnError read FOnError write FOnError;
    Property OnPlaying: TOnPlaying read FOnPlaying write FOnPlaying;
    Property OnPlay: TNotifyEvent read FOnPlay write FOnPlay;
    Property OnStop: TNotifyEvent read FOnStop write FOnStop;
  End;

  TMPlayerControl = Class(TCustomMPlayerControl)
  Published
    Property Align;
    Property Anchors;
    Property BorderSpacing;
    Property Enabled;
    Property Filename;
    Property Loop;
    Property OnChangeBounds;
    Property OnConstrainedResize;
    Property OnResize;
    Property OnClick;
    Property OnMouseUp;
    Property OnMouseDown;
    Property Visible;
    Property Volume;     // 0 to 100
    Property OnFeedback; // Provides standard console output from mplayer
    Property OnError;    // Provides stderr console output from mplayer
    Property OnPlaying;  // When not paused, an event every 250ms to 500ms with Position
    Property OnPlay;     // Sent when mplayer initialised with video file
    Property OnStop;     // Sent sometime (approx 250ms) after mplayer finishes
  End;

  { TWSMPlayerControl }

  {$ifdef Linux}
  TWSMPlayerControl = Class(TGtk2WSWinControl)
  Published
    Class Function CreateHandle(Const AWinControl: TWinControl;
      Const AParams: TCreateParams): HWND; Override;
    Class Procedure DestroyHandle(Const AWinControl: TWinControl); Override;
  End;

  {$endif}

Procedure Register;

Implementation

Procedure Register;
Begin
  RegisterComponents('Multimedia', [TMPlayerControl]);
End;

Function ExtractAfter(AInput, AIdentifier: String): String; Inline;
Begin
  AInput := Lowercase(AInput);
  AIdentifier := Lowercase(AIdentifier);

  Result := Copy(AInput, Length(AIdentifier) + 1, Length(AInput) - Length(AIdentifier));
End;

{ TCustomMPlayerControl }

Constructor TCustomMPlayerControl.Create(TheOwner: TComponent);
Begin
  Inherited Create(TheOwner);
  ControlStyle := ControlStyle - [csSetCaption];
  FCanvas := TControlCanvas.Create;
  TControlCanvas(FCanvas).Control := Self;
  SetInitialBounds(0, 0, 160, 90);

  FOutlist := TStringList.Create;

  FMPlayerPath := 'mplayer' + GetExeExt;

  FTimer := TTimer.Create(Self);
  FTimer.Enabled := False;
  FTimer.Interval := 250;
  FTimer.OnTimer := @TimerEvent;
End;

Destructor TCustomMPlayerControl.Destroy;
Begin
  Stop;
  FreeAndNil(FCanvas);
  FreeAndNil(FTimer);
  FreeAndNil(FOutList);
  Inherited Destroy;
End;

Procedure TCustomMPlayerControl.WMPaint(Var Message: TLMPaint);
Begin
  Include(FControlState, csCustomPaint);
  Inherited WMPaint(Message);
  If (csDesigning In ComponentState) And (FCanvas <> nil) Then
    With FCanvas Do
    Begin
      If Message.DC <> 0 Then
        Handle := Message.DC;
      Brush.Color := clLtGray;
      Pen.Color := clRed;
      Rectangle(0, 0, Self.Width - 1, Self.Height - 1);
      MoveTo(0, 0);
      LineTo(Self.Width, Self.Height);
      MoveTo(0, Self.Height);
      LineTo(Self.Width, 0);
      If Message.DC <> 0 Then
        Handle := 0;
    End;
  Exclude(FControlState, csCustomPaint);
End;

Procedure TCustomMPlayerControl.WMSize(Var Message: TLMSize);
Begin
  If (Message.SizeType And Size_SourceIsInterface) > 0 Then
    DoOnResize;
End;

Procedure TCustomMPlayerControl.Invalidate;
Begin
  If csCustomPaint In FControlState Then
    exit;
  Inherited Invalidate;
End;

Procedure TCustomMPlayerControl.EraseBackground(DC: HDC);
Begin
  If (Not Running) And (FCanvas <> nil) Then
    With FCanvas Do
    Begin
      If DC <> 0 Then
        Handle := DC;
      Brush.Color := clLtGray;
      Rectangle(0, 0, Self.Width, Self.Height);
      If DC <> 0 Then
        Handle := 0;
    End;
  // else
  // everything is painted, so erasing the background is not needed
End;

Procedure TCustomMPlayerControl.Play;
Var
  ExePath: String;
  CurWindowID: PtrUInt;
Begin
  If (csDesigning In ComponentState) Then
    exit;

  If Running And Paused Then
  Begin
    Paused := False;
    exit;
  End;

  If Playing Then
    exit;

  {$IFDEF Linux}
  If (Not HandleAllocated) Then
    exit;
  DebugLn(['TCustomMPlayerControl.Play ']);
  {$endif}

  If FPlayerProcess <> nil Then
    FreeAndNil(FPlayerProcess);
//    raise Exception.Create('TCustomMPlayerControl.Play FPlayerProcess still exists');

  If MPlayerPath = '' Then
    MPlayerPath := 'mplayer' + GetExeExt;
  ExePath := MPlayerPath;
  If Not FilenameIsAbsolute(ExePath) Then
    ExePath := FindDefaultExecutablePath(ExePath);
  If Not FileExistsUTF8(ExePath) Then
    Raise Exception.Create(MPlayerPath + ' not found');

  {$IFDEF Linux}
  CurWindowID := GDK_WINDOW_XWINDOW({%H-}PGtkWidget(PtrUInt(Handle))^.window);
  {$else}
  CurWindowID := Handle;
  {$ENDIF}

  FPlayerProcess := TProcessUTF8.Create(Self);
  FPlayerProcess.Options := FPlayerProcess.Options + [poUsePipes, poNoConsole];

  { -quiet              : supress most messages
    -really-quiet       : DONT USE: causes the video player to not connect to -wid.  Odd...
    -msglevel global=6  : required for EOF signal when playing stops
    -wid                : sets Window ID
    -noconfig all       : stop mplayer from reading commands from a text file }
  FPlayerProcess.CommandLine := ExePath + ' -slave -quiet -noconfig all -wid ' +
    IntToStr(CurWindowID) + ' ' + StartParam + ' ' + AnsiQuotedStr(Filename, '"');

  DebugLn(['TCustomMPlayerControl.Play ', FPlayerProcess.CommandLine]);

  If assigned(FOnFeedback) Then
  Begin
    FOutlist.Clear;
    FOutlist.Add(FPlayerProcess.CommandLine);
    FOutlist.Add('');
    FonFeedback(Self, FOutlist);
  End;

  FPlayerProcess.Execute;

  // Inject a request for Duration
  FDuration := -1;
  SendMPlayerCommand('get_time_length');
  FRequestVolume := True;

  // Start the timer that handles feedback from mplayer
  FTimer.Enabled := True;
End;

Procedure TCustomMPlayerControl.Stop;
Begin
  If FPlayerProcess = nil Then
    exit;

  FPaused := False;
  FDuration := -1;
  FTimer.Enabled := False;

  SendMPlayerCommand('quit');

  If Assigned(FOnStop) Then
    FOnStop(Self);

  FreeAndNil(FPlayerProcess);

  // repaint the control
  Invalidate;
End;

Procedure TCustomMPlayerControl.TimerEvent(Sender: TObject);
Var
  ErrList: TStringList;
  dPosition: Single;
  i: Integer;
  sTemp: String;
  bFoundPosition: Boolean;
  iPosEquals: SizeInt;
  sValue: String;
  sProperty: String;

Begin
  If Running Then
  Begin
    // Inject a request for current position
    bFoundPosition := False;
    If Assigned(FOnPlaying) And Not FPaused Then
      SendMPlayerCommand('get_time_pos');

    If FRequestVolume Then
      SendMPlayerCommand('get_property volume');

    If FPlayerProcess.Output.NumBytesAvailable > 0 Then
    Begin
      FOutList.LoadFromStream(FPlayerProcess.Output);

      // Look for responses to injected commands...
      // or for standard commands
      For i := FOutList.Count - 1 Downto 0 Do
      Begin
        sTemp := Lowercase(FOutList[i]);

        // Property Requested are provided in the format
        // ANS_PropertyName=Value

        If Pos('ans_', sTemp) = 1 Then
        Begin
          iPosEquals := Pos('=', sTemp);

          If iPosEquals > 1 Then
          Begin
            sValue := Copy(sTemp, iPosEquals + 1, Length(sTemp) - iPosEquals);
            sProperty := Copy(sTemp, 5, iPosEquals - 5);

            If (FDuration = -1) And (sProperty = 'length') Then
            Begin
              FDuration := StrToFloatDef(sValue, -1);

              // clear this response from the queue
              FOutList.Delete(i);
            End
            Else If Assigned(FOnPlaying) And (Not bFoundPosition) And
              (sProperty = 'time_position') Then
            Begin
              // Are we paused by any chance?
              If sValue = FLastPosition Then
                SendMPlayerCommand('get_property pause');

              FLastPosition := sValue;

              dPosition := StrToFloatDef(sValue, 0);

              // Don't remove any further ANS_Time_Positions, they're not ours...
              bFoundPosition := True;

              // Send the message
              FOnPlaying(Self, dPosition);

              // clear this response from the queue
              FOutList.Delete(i);
            End
            Else If FRequestVolume And (sProperty = 'volume') Then
            Begin
              FVolume := Trunc(0.5 + StrToFloatDef(sValue, 100));
              FRequestVolume := False;

              // clear this response from the queue
              FOutList.Delete(i);
            End
            Else If (sProperty = 'pause') Then
              FPaused := (sValue = 'yes');
          End;

        End
        Else If Assigned(FOnPlay) And (sTemp = 'starting playback...') Then
          FOnPlay(Self);
      End;

      If Assigned(FOnFeedback) And (FOutlist.Count > 0) Then
        FOnFeedback(Self, FOutlist);
    End;

    If FPlayerProcess.StdErr.NumBytesAvailable > 0 Then
    Begin
      ErrList := TStringList.Create;
      Try
        ErrList.LoadFromStream(FPlayerProcess.Stderr);

        If Assigned(FOnError) Then
          FOnError(Self, ErrList);
      Finally
        ErrList.Free;
      End;
    End;
  End
  Else
    Stop;
End;

Function TCustomMPlayerControl.DoCommand(ACommand, AResultIdentifier: String): String;
Var
  i: Integer;
Begin
  If Not Running Then
    Exit;

  // Pause the timer
  FTimer.Enabled := False;

  // Clear the output queue
  TimerEvent(Self);

  SendMPlayerCommand(ACommand);

  // Now *immediately* read the output results.
  // this will have problems if mplayer takes
  // a while to execute this command...

  // Read the result
  FOutList.LoadFromStream(FPlayerProcess.Output);

  // Find our reply
  i := 0;
  While (i < FOutList.Count) And (Pos(AResultIdentifier, FOutlist[i]) <> 1) Do
    Inc(i);

  If (i <> FOutList.Count) Then
    Result := ExtractAfter(FOutList[i], AResultIdentifier);

  // Ensure any feedback we accidently intercepted get's processed
  If Assigned(FOnFeedback) And (FOutlist.Count > 0) Then
    FOnFeedback(Self, FOutlist);

  // Resume the timer
  FTimer.Enabled := True;
End;

Procedure TCustomMPlayerControl.SendMPlayerCommand(Cmd: String);
Begin
  If Cmd = '' Then
    exit;
  If Not Running Then
    exit;
  If Pos('paus', Lowercase(Cmd)) <> 1 Then
    Cmd := 'pausing_keep_force ' + Cmd;
  If Cmd[length(Cmd)] <> LineEnding Then
    Cmd := Cmd + LineEnding;

  FPlayerProcess.Input.Write(Cmd[1], length(Cmd));
End;

Procedure TCustomMPlayerControl.SetStartParam(Const AValue: String);
Begin
  If FStartParam = AValue Then
    exit;
  FStartParam := AValue;
End;

Procedure TCustomMPlayerControl.SetFilename(Const AValue: String);
Begin
  If FFilename = AValue Then
    exit;
  FFilename := AValue;
  If Running Then
    SendMPlayerCommand('loadfile ' + StrToCmdLineParam(Filename));
End;

Function TCustomMPlayerControl.GetPosition: Single;
Begin
  Result := 0;

  If Not Running Then
    exit;

  Result := StrToFloatDef(DoCommand('get_time_pos', 'ans_time_position='), 0);
End;

Procedure TCustomMPlayerControl.SetLoop(Const AValue: Integer);
Begin
  If FLoop = AValue Then
    exit;
  FLoop := AValue;
  If Running Then
    SendMPlayerCommand('loop ' + IntToStr(FLoop));
End;

Procedure TCustomMPlayerControl.SetMPlayerPath(Const AValue: String);
Begin
  If FMPlayerPath = AValue Then
    exit;
  FMPlayerPath := AValue;
End;

Procedure TCustomMPlayerControl.SetPaused(Const AValue: Boolean);
Begin
  If FPaused = AValue Then
    exit;
  If Running Then
  Begin
    FPaused := AValue;
    SendMPlayerCommand('pause');
  End;
End;

Procedure TCustomMPlayerControl.SetPosition(AValue: Single);
Begin
  If Running Then
    SendMPlayerCommand(Format('pausing_keep seek %.3f 2', [AValue]));
End;

Procedure TCustomMPlayerControl.SetVolume(Const AValue: Integer);
Begin
  If FVolume = AValue Then
    exit;
  FVolume := AValue;
  If Running Then
  Begin
    SendMPlayerCommand('volume ' + IntToStr(FVolume) + ' 1');
    FRequestVolume := True;
  End;
End;

Function TCustomMPlayerControl.Running: Boolean;
Begin
  Result := (FPlayerProcess <> nil) And FPlayerProcess.Running;
End;

Function TCustomMPlayerControl.Playing: Boolean;
Begin
  Result := Running And (Not Paused);
End;

{$ifdef Linux}
function MPLayerWidgetDestroyCB(Widget: PGtkWidget; {%H-}data: gPointer): GBoolean; cdecl;
begin
  FreeWidgetInfo(Widget); // created in TWSMPlayerControl.CreateHandle
  Result:=false;
end;

{ TWSMPlayerControl }

class function TWSMPlayerControl.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): HWND;
var
  NewWidget: PGtkWidget;
  WidgetInfo: PWidgetInfo;
  Allocation: TGTKAllocation;
begin
  if csDesigning in AWinControl.ComponentState then
    Result:=inherited CreateHandle(AWinControl,AParams)
  else begin
    NewWidget:=gtk_event_box_new;

    WidgetInfo := GetWidgetInfo(NewWidget,true); // destroyed in MPLayerWidgetDestroyCB
    WidgetInfo^.LCLObject := AWinControl;
    WidgetInfo^.Style := AParams.Style;
    WidgetInfo^.ExStyle := AParams.ExStyle;
    WidgetInfo^.WndProc := {%H-}PtrUInt(AParams.WindowClass.lpfnWndProc);

    // set allocation
    Allocation.X := AParams.X;
    Allocation.Y := AParams.Y;
    Allocation.Width := AParams.Width;
    Allocation.Height := AParams.Height;
    gtk_widget_size_allocate(NewWidget, @Allocation);

    if csDesigning in AWinControl.ComponentState then begin
      // at designtime setup normal handlers
      TGtk2WidgetSet(WidgetSet).FinishCreateHandle(AWinControl,NewWidget,AParams);
    end else begin
      // at runtime
      g_signal_connect(GPointer(NewWidget), 'destroy',
                       TGTKSignalFunc(@MPLayerWidgetDestroyCB), WidgetInfo);
    end;
    Result:=HWND({%H-}PtrUInt(Pointer(NewWidget)));
    DebugLn(['TWSMPlayerControl.CreateHandle ',dbgs(NewWidget)]);
  end;
end;

class procedure TWSMPlayerControl.DestroyHandle(const AWinControl: TWinControl
  );
begin
  inherited DestroyHandle(AWinControl);
end;

initialization
  RegisterWSComponent(TCustomMPlayerControl,TWSMPlayerControl);
  {$I mplayerctrl.lrs}

{$else ifwindows}

initialization
  {$I mplayerctrl.lrs}

{$endif}

End.
