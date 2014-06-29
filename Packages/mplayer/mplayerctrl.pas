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
              Added pausing_keep_force to all commands
                - simply requesting state information was enough to resume paused video
                - adding pausing_keep was insufficent
              Added Duration, Position
              Replaced StrToCmdParam with AnsiQuotedStr in Play
                - StrToCmdParam didn't work under Windows - wrapped filename in ', Windows needed "
              Persisted FCanvas outside of IDE to prevent painting issues when no file playing
              / Mike Thompson

  2014-06-24  Added FindMPlayerPath (Refactored code from Play)
  2014-06-28  Extended FindMPlayer to also look for mplayer in a subfolder of the exe
              Fixed painting issues when playing audio files (introduces a flicker on
                resize when playing video :-( )...
              Fixed repeated requests for volume in files that don't support volme
              Changed TProcessUTF8 population code in .Play from .CommandLine to
                use .Executable & .Parameters
                - incidently removed the need to use AnsiQuotedStr around Filename under Windows
              Added Rate (Fast Forward only, mplayer doesn't support rewind)
              Only request position updates every ON_PLAYING_INTERVAL
              Set Volume on Play
              Added GrabImage and OnGrabImage (delay before mplayer grabs image)
                - doesn't work well with some renderers (-glnosw for instance,
                  also inconsistently on -vo X11)
                - Capturing failed attempts in code will be hard, for now I'll
                  just ensure this is documented on the wiki (recommend -vo direct3d under win)
              / Mike Thompson

TODO
              EXTENSIVE TESTING UNDER LINUX
                - Tested under Linus Mint 16 (MATE) with mplayer installed (not mplayer2)
              Consider descending control from TGraphicControl (instead of creating FCanvas)
              Add StepForward(increment), Stepback(increment)
              Hide PlayerProcess (OnFeedback/OnError events + Running property
                means there is no reason for this to be exposed... (speak to mattias/six1 first)
              Fix repeated requests for Pause in TimerEvent (Use DoCommand)
              Change existing commands (ie "volume") to their set_property equivalent
              Position isn't working for some videos (they have an embedded start_time.
                existing code assumes start_time is 0 for all files)
                - Investigate switching over to position by percent instead,
                  or calculate start time from stream information and subtract
                  that from broadcast position
NOTES
  2014-06-29  TProcessUTF8 is a thin wrapper over TProcess.  TProcess on Windows
                is not unicode aware, so there is currently an issue playing unicode
                filenames under windows.
                No easy apparent solution other than upgrading TProcess (win\process.inc).
}
unit MPlayerCtrl;

{$mode objfpc}{$H+}

{$ifdef Linux}
 {$ifndef LCLgtk2}
 {$error this unit only supports LCL under gtk2}
 {$endif}
{$endif}

interface

uses
  Classes, SysUtils, Controls, WSLCLClasses, LCLProc, LCLType, InterfaceBase,
  LResources, LMessages, Graphics, ExtCtrls, FileUtil, Process, UTF8Process,
  LazFileUtils
  {$ifdef Linux}
  , gtk2int, gtk2, glib2, gdk2x, Gtk2WSControls, GTK2Proc, Gtk2Def
  {$endif}
  ;

type

  { TCustomMPlayerControl }
  
  TOnFeedback = procedure(ASender: TObject; AStrings: TStringList) of object;
  TOnError = procedure(ASender: TObject; AStrings: TStringList) of object;
  TOnPlaying = procedure(ASender: TObject; APosition: single) of object;
  TOnGrabImage = Procedure(ASender: TObject; AFilename: String) of object;

  TCustomMPlayerControl = class(TWinControl)
  private
    FFilename: string;
    FImagePath: string;
    FLastImageFilename: String;
    FOnGrabImage: TOnGrabImage;
    FRate: single;
    FStartParam:string;
    FLoop: integer;
    FMPlayerPath: string;
    FPaused: boolean;
    FPlayerProcess: TProcessUTF8;
    FTimer: TTimer;
    FVolume: integer;
    FCanvas: TCanvas;
    FLastPosition: string;
    FRequestingPosition: boolean;
    FLastTimer: TDateTime;
    FRequestVolume: boolean;
    FDuration: single;
    FOnError: TOnError;
    FOnFeedback: TOnFeedback;
    FOnPlay: TNotifyEvent;
    FOnPlaying: TOnPlaying;
    FOnStop: TNotifyEvent;
    FOutList: TStringList;
    function GetPosition: single;
    function GetRate: single;
    procedure SetImagePath(AValue: string);
    procedure SetPosition(AValue: single);
    procedure SetFilename(const AValue: string);
    procedure SetLoop(const AValue: integer);
    procedure SetMPlayerPath(const AValue: string);
    procedure SetPaused(const AValue: boolean);
    procedure SetRate(AValue: single);
    procedure SetVolume(const AValue: integer);
    procedure SetStartParam(const AValue: string);
    procedure TimerEvent(Sender: TObject);
  protected
    procedure WMPaint(var Message: TLMPaint); message LM_PAINT;
    procedure WMSize(var Message: TLMSize); message LM_SIZE;
	
    function DoCommand(ACommand, AResultIdentifier: string): string;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure SendMPlayerCommand(Cmd: string); // see: mplayer -input cmdlist and http://www.mplayerhq.hu/DOCS/tech/slave.txt
    function Running: boolean;
    procedure Play;
    procedure Stop;
    function Playing: boolean;
    procedure Invalidate; override;
    procedure EraseBackground(DC: HDC); override;
  public
    function FindMPlayerPath : Boolean;

    procedure GrabImage;
    property LastImageFilename: String read FLastImageFilename;

    property Filename: string read FFilename write SetFilename;
    property StartParam: string read FStartParam write SetStartParam;
    property MPlayerPath: string read FMPlayerPath write SetMPlayerPath;
    property PlayerProcess: TProcessUTF8 read fPlayerProcess;
    property Paused: boolean read FPaused write SetPaused;
    property Loop: integer read FLoop write SetLoop; // -1 no, 0 forever, 1 once, 2 twice, ...
    property Volume: integer read FVolume write SetVolume;

    property ImagePath: string read FImagePath write SetImagePath;

    property Rate: single read GetRate write SetRate; // mplayer only supports 0.1 to 100
    property Duration: single read FDuration; // seconds
    property Position: single read GetPosition write SetPosition; // seconds

    property OnFeedback: TOnFeedback read FOnFeedback write FOnFeedback;
    property OnError: TOnError read FOnError write FOnError;
    property OnPlaying: TOnPlaying read FOnPlaying write FOnPlaying;
    property OnPlay: TNotifyEvent read FOnPlay write FOnPlay;
    property OnStop: TNotifyEvent read FOnStop write FOnStop;
    Property OnGrabImage: TOnGrabImage read FOnGrabImage write FOnGrabImage;
  end;

  TMPlayerControl = class(TCustomMPlayerControl)
  published
    property Align;
    property Anchors;
    property BorderSpacing;
    property Enabled;
    property Filename;
    property Loop;
    property OnChangeBounds;
    property OnConstrainedResize;
    property OnResize;
    property OnClick;
    property OnMouseUp;
    property OnMouseDown;
    property Visible;
    property Volume;     // 0 to 100
    property OnFeedback; // Provides standard console output from mplayer
    property OnError;    // Provides stderr console output from mplayer
    property OnPlaying;  // When not paused, an event every 250ms to 500ms with Position
    property OnPlay;     // Sent when mplayer initialised with video file
    property OnStop;     // Sent sometime (approx 250ms) after mplayer finishes
    property OnGrabImage; // Fired when mplayer reports the filename
  end;

  { TWSMPlayerControl }

  {$ifdef Linux}
  TWSMPlayerControl = class(TGtk2WSWinControl)
  published
    class function CreateHandle(const AWinControl: TWinControl;
                                const AParams: TCreateParams): HWND; override;
    class procedure DestroyHandle(const AWinControl: TWinControl); override;
  end;
  {$endif}

Const
  ON_PLAYING_INTERVAL = 500 / (24*60*60*1000);

procedure Register;

implementation

Uses
  Forms;

procedure Register;
begin
  RegisterComponents('Multimedia',[TMPlayerControl]);
end;

// returns the value from "ANS_PropertyName=Value" strings
function ExtractAfter(AInput, AIdentifier: string): string; inline;
begin
  AInput := Lowercase(AInput);
  AIdentifier := Lowercase(AIdentifier);

  Result := Copy(AInput, Length(AIdentifier) + 1, Length(AInput) - Length(AIdentifier));
end;

{ TCustomMPlayerControl }

procedure TCustomMPlayerControl.TimerEvent(Sender: TObject);
var
  ErrList: TStringList;
  dPosition: single;
  i: integer;
  sTemp: string;
  iPosEquals: SizeInt;
  sValue: string;
  sProperty: string;
  iError: Integer;

begin
  if FPlayerProcess<>nil then
  begin
    If Running And ((Now-FLastTimer)>ON_PLAYING_INTERVAL) Then
    begin
      // Inject a request for current position
      if Assigned(FOnPlaying) and not FPaused then
      begin
        SendMPlayerCommand('get_time_pos');
        FRequestingPosition := True;
      end;

      FLastTimer := Now;
    end;

    // Inject a request for Volume level
    if Running And FRequestVolume then
      SendMPlayerCommand('get_property volume');

    if FPlayerProcess.Output.NumBytesAvailable > 0 then
    begin
      FOutList.LoadFromStream(FPlayerProcess.Output);

      // Look for responses to injected commands...
      // or for standard commands
      for i := FOutList.Count - 1 downto 0 do
      begin
        sTemp := Lowercase(FOutList[i]);

        // Property Requested are provided in the format
        // ANS_PropertyName=Value
        if Pos('ans_', sTemp) = 1 then
        begin
          iPosEquals := Pos('=', sTemp);

          if iPosEquals > 1 then
          begin
            sValue := Copy(sTemp, iPosEquals + 1, Length(sTemp) - iPosEquals);
            sProperty := Copy(sTemp, 5, iPosEquals - 5);

            if (FDuration = -1) and (sProperty = 'length') then
            begin
              FDuration := StrToFloatDef(sValue, -1);

              // clear this response from the queue
              FOutList.Delete(i);
            end
            else if Assigned(FOnPlaying) and (FRequestingPosition) and
              (sProperty = 'time_position') then
            begin
              // Are we paused by any chance?
              if sValue = FLastPosition then
                SendMPlayerCommand('get_property pause');

              FLastPosition := sValue;

              dPosition := StrToFloatDef(sValue, 0);

              // Don't remove any further ANS_Time_Positions, they're not ours...
              FRequestingPosition := False;

              // Send the message
              FOnPlaying(Self, dPosition);

              // clear this response from the queue
              FOutList.Delete(i);
            end
            else if {FRequestVolume And }(sProperty = 'volume') then
            begin
              FVolume := Trunc(0.5 + StrToFloatDef(sValue, 100));
              FRequestVolume := False;

              // clear this response from the queue
              FOutList.Delete(i);
            end
            else if (sProperty = 'pause') then
              FPaused := (sValue = 'yes');
          end;

        end
        else if Assigned(FOnPlay) and (sTemp = 'starting playback...') then
          FOnPlay(Self)
        else if (Pos('*** screenshot', sTemp)=1) Then
        begin
          //  result looks like *** screenshot 'shot0002.png' ***
          FLastImageFilename:=IncludeTrailingBackslash(GetCurrentDirUTF8) + Copy(sTemp, 17, Pos('.', sTemp)-17+4);

          if assigned(FOnGrabImage) And FileExistsUTF8(FLastImageFilename) then
            FOnGrabImage(Self, FLastImageFilename);

          // clear this response from the queue
          FOutList.Delete(i);
        end
        else if sTemp='sending vfctrl_screenshot!' then
          FOutList.Delete(i);
      end;

      if Assigned(FOnFeedback) and (FOutlist.Count > 0) then
        FOnFeedback(Self, FOutlist);
    end;

    if FPlayerProcess.StdErr.NumBytesAvailable > 0 then
    begin
      ErrList := TStringList.Create;
      try
        ErrList.LoadFromStream(FPlayerProcess.Stderr);

        // Catch error retrieving volume
        If FRequestVolume Then
        begin
          iError := ErrList.IndexOf('Failed to get value of property ''volume''.');
          If iError<>-1 Then
          begin
            Errlist.Delete(iError);

            // Prevent further requests for volume
            FVolume := 0;
            FRequestVolume := False;
          end;
        end;

        if Assigned(FOnError) then
          FOnError(Self, ErrList);
      finally
        ErrList.Free;
      end;
    end;
  end;

  If not Running Then
    Stop;
end;

procedure TCustomMPlayerControl.WMPaint(var Message: TLMPaint);
begin
  Include(FControlState, csCustomPaint);
  inherited WMPaint(Message);
  if (csDesigning in ComponentState) and (FCanvas<>nil) then begin
    with FCanvas do begin
      if Message.DC <> 0 then
        Handle := Message.DC;
      Brush.Color:=clLtGray;
      Pen.Color:=clRed;
      Rectangle(0,0,Self.Width-1,Self.Height-1);
      MoveTo(0,0);
      LineTo(Self.Width,Self.Height);
      MoveTo(0,Self.Height);
      LineTo(Self.Width,0);
      if Message.DC <> 0 then
        Handle := 0;
    end;
  end;
  Exclude(FControlState, csCustomPaint);
end;

procedure TCustomMPlayerControl.WMSize(var Message: TLMSize);
begin
  if (Message.SizeType and Size_SourceIsInterface)>0 then
    DoOnResize;
end;


procedure TCustomMPlayerControl.SetStartParam(const AValue: string);
begin
  if FStartParam=AValue then exit;
  FStartParam:=AValue;
end;

procedure TCustomMPlayerControl.SetFilename(const AValue: string);
  // Copied from win\process.inc
  Function MaybeQuoteIfNotQuoted(Const S : String) : String;
  begin
    If (Pos(' ',S)<>0) and (pos('"',S)=0) then
      Result:='"'+S+'"'
    else
       Result:=S;
  end;
begin
  if FFilename=AValue then exit;
  FFilename:=AValue;
  if Running then
    SendMPlayerCommand('loadfile '+MaybeQuoteIfNotQuoted(Filename));
end;

procedure TCustomMPlayerControl.SetLoop(const AValue: integer);
begin
  if FLoop=AValue then exit;
  FLoop:=AValue;
  if Running then
    SendMPlayerCommand('loop '+IntToStr(FLoop));
end;

procedure TCustomMPlayerControl.SetMPlayerPath(const AValue: string);
begin
  if FMPlayerPath=AValue then exit;
  FMPlayerPath:=AValue;
end;

procedure TCustomMPlayerControl.SetPaused(const AValue: boolean);
begin
  if FPaused=AValue then exit;
  if Running then begin
    FPaused:=AValue;
    SendMPlayerCommand('pause');
  end;
end;

procedure TCustomMPlayerControl.SetRate(AValue: single);
begin
  if FRate=AValue then Exit;
  if (FRate<0.1) or (FRate>100) then Exit;
  if Running then begin
    FRate:=AValue;
    SendMPlayerCommand(Format('set_property speed %.3f', [FRate]));
  end;
end;

procedure TCustomMPlayerControl.SetVolume(const AValue: integer);
begin
  if FVolume=AValue then exit;
  FVolume:=AValue;
  if Running then
  begin
    SendMPlayerCommand('volume ' + IntToStr(FVolume) + ' 1');
    FRequestVolume := True;
  end;
end;

constructor TCustomMPlayerControl.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  ControlStyle:=ControlStyle-[csSetCaption];
  FCanvas := TControlCanvas.Create;
  TControlCanvas(FCanvas).Control := Self;
  SetInitialBounds(0, 0, 160, 90);

  FOutlist := TStringList.Create;

  FMPlayerPath := 'mplayer' + GetExeExt;

  FTimer := TTimer.Create(Self);
  FTimer.Enabled := False;
  FTimer.Interval := 250;
  FTimer.OnTimer := @TimerEvent;
end;

destructor TCustomMPlayerControl.Destroy;
begin
  Stop;
  FreeAndNil(FCanvas);
  FreeAndNil(FTimer);
  FreeAndNil(FOutList);
  inherited Destroy;
end;

procedure TCustomMPlayerControl.SendMPlayerCommand(Cmd: string);
begin
  if Cmd='' then exit;
  if not Running then exit;

  if Pos('paus', Lowercase(Cmd)) <> 1 then
    Cmd := 'pausing_keep_force ' + Cmd;
  if Cmd[length(Cmd)] <> LineEnding then
    Cmd := Cmd + LineEnding;

  FPlayerProcess.Input.Write(Cmd[1], length(Cmd));
end;

function TCustomMPlayerControl.Running: boolean;
begin
  Result:=(fPlayerProcess<>nil) and fPlayerProcess.Running;
end;

function TCustomMPlayerControl.FindMPlayerPath: Boolean;
var
  ExePath: string;
  MPlayerExe: String;
begin
  result := FileExistsUTF8(FMPlayerPath);

  If not result then
  begin
    MPlayerExe:='mplayer'+GetExeExt;
    if FMPlayerPath='' then
      FMPlayerPath:=MPlayerExe;
    ExePath:=FMPlayerPath;
    // Is mplayer installed in the environment path?
    if not FilenameIsAbsolute(ExePath) then
      ExePath:=FindDefaultExecutablePath(ExePath);
    // is mplayer in a folder under the application folder?
    if Not FileExistsUTF8(ExePath) then
      ExePath := IncludeTrailingBackSlash(ExtractFileDir(Application.ExeName))+
        IncludeTrailingBackslash('mplayer') + MPlayerExe;
    // did we find it?
    if FileExistsUTF8(ExePath) then
    begin
      FMPlayerPath:=ExePath;
      result := true;
    end;
  end;
end;

procedure TCustomMPlayerControl.GrabImage;
begin
  if Running then
    SendMPlayerCommand('screenshot 0')
end;

procedure TCustomMPlayerControl.Play;
var
  CurWindowID: PtrUInt;
  slStartParams : TStringList;
begin
  if (csDesigning in ComponentState) then exit;

  if Running and Paused then begin
    Paused:=false;
    exit;
  end;

  if Playing then
  begin
    if FRate>1 Then
      Rate := 1;

    exit;
  end;

  {$IFDEF Linux}
  if (not HandleAllocated) then exit;
  DebugLn(['TCustomMPlayerControl.Play ']);
  {$endif}

  if fPlayerProcess<>nil then
    FreeAndNil(fPlayerProcess);
//    raise Exception.Create('TCustomMPlayerControl.Play fPlayerProcess still exists');

  if not FindMPlayerPath then
    raise Exception.Create(MPlayerPath+' not found');

  {$IFDEF Linux}
    CurWindowID := GDK_WINDOW_XWINDOW({%H-}PGtkWidget(PtrUInt(Handle))^.window);
  {$else}
    CurWindowID := Handle;
  {$ENDIF}

  FPlayerProcess := TProcessUTF8.Create(Self);
  FPlayerProcess.Options := FPlayerProcess.Options + [poUsePipes, poNoConsole];

  // -slave              : allow us to control mplayer
  // -quiet              : supress most messages
  // -really-quiet       : DONT USE: causes the video player to not connect to -wid.  Odd...
  // -msglevel global=6  : required for EOF signal when playing stops
  // -wid                : sets Window ID (display video in our control)
  // -noconfig all       : stop mplayer from reading commands from a text file
  // -vf screenshot      : Allow frame grab
  // -zoom -fs           : Unsure:  Only perceptible difference is background drawn black not green
  // -vo direct3d        : uses Direct3D renderer (recommended under windows)
  // -vo gl_nosw         : uses OpenGL no software renderer
  FPlayerProcess.Executable:=FMPlayerPath;
  FPlayerProcess.Parameters.Add('-slave');
  FPlayerProcess.Parameters.Add('-quiet');
  FPlayerProcess.Parameters.Add('-vf');
  FPlayerProcess.Parameters.Add('screenshot');
  FPlayerProcess.Parameters.Add('-wid');
  FPlayerProcess.Parameters.Add(IntToStr(CurWindowID));
  slStartParams := TStringList.Create;
  Try
    CommandToList(StartParam, slStartParams);
    FPlayerProcess.Parameters.AddStrings(slStartParams);
  finally
    slStartParams.Free;
  end;
  FPlayerProcess.Parameters.Add(FFilename);

  FPlayerProcess.Parameters.Delimiter:=' ';
  DebugLn(['TCustomMPlayerControl.Play ', FPlayerProcess.Parameters.DelimitedText]);

  // Normally I'd be careful to only use FOutList in the
  // Timer event, but here I'm confident the timer isn't running...
  if assigned(FOnFeedback) then
  begin
    FOutlist.Clear;
    FOutlist.Add(FPlayerProcess.Executable + ' ' + FPlayerProcess.Parameters.DelimitedText);
    FOutlist.Add('');
    FonFeedback(Self, FOutlist);
  end;

  // Populate defaults
  FDuration := -1;
  FRate := 1;

  FPlayerProcess.Execute;

  // Get/Set Initial State
  SendMPlayerCommand('get_time_length');
  SendMPlayerCommand('volume ' + IntToStr(FVolume) + ' 1');
  FRequestVolume := True;   // Confirm set volume worked...

  // Start the timer that handles feedback from mplayer
  FTimer.Enabled := True;
end;

procedure TCustomMPlayerControl.Stop;
begin
  if FPlayerProcess = nil then
    exit;

  DebugLn(Format('ExitStatus=%d', [fPlayerProcess.ExitStatus]));
  FPaused := False;
  FDuration := -1;
  FTimer.Enabled := False;

  SendMPlayerCommand('quit');

  FreeAndNil(FPlayerProcess);

  if Assigned(FOnStop) then
    FOnStop(Self);

  // repaint the control
  Refresh;
end;

function TCustomMPlayerControl.Playing: boolean;
begin
  Result := Running and (not Paused);
end;

procedure TCustomMPlayerControl.Invalidate;
begin
  if csCustomPaint in FControlState then exit;
  inherited Invalidate;
end;

procedure TCustomMPlayerControl.EraseBackground(DC: HDC);
begin
  if (FCanvas <> nil) then
    with FCanvas do
    begin
      if DC <> 0 then
        Handle := DC;
      Brush.Color := clLtGray;
      Rectangle(0, 0, Self.Width, Self.Height);
      if DC <> 0 then
        Handle := 0;
    end;
end;

// Allows this control to inject commands without the results
// being exposed to end users of this control (other than via
// public interface)
// DoCommand is actually written for get_property XXX calls
function TCustomMPlayerControl.DoCommand(ACommand, AResultIdentifier: string): string;
var
  i: integer;
  slTemp: TStringList;
begin
  if not Running then
    Exit;

  // Pause the timer
  FTimer.Enabled := False;

  // Clear existing mplayer console output
  TimerEvent(Self);

  SendMPlayerCommand(ACommand);

  // Now *immediately* read the output results.
  // this may have problems if mplayer takes
  // a while to execute this command, but outside intilisation
  // this doesn't appear to be the case...

  if FPlayerProcess.Output.NumBytesAvailable > 0 then
  begin
    slTemp := TStringList.Create;
    try
      // Read the result
      slTemp.LoadFromStream(FPlayerProcess.Output);

      // Find our reply
      i := 0;
      while (i < slTemp.Count) and (Pos(AResultIdentifier, slTemp[i]) <> 1) do
        Inc(i);

      if (i <> slTemp.Count) then
      begin
        Result := ExtractAfter(slTemp[i], AResultIdentifier);

        // Hide our feedback from the outer app
        slTemp.Delete(i);
      end;

      // Ensure any feedback we accidently intercepted gets processed
      if Assigned(FOnFeedback) and (slTemp.Count > 0) then
        FOnFeedback(Self, slTemp);
    finally
      slTemp.Free;
    end;
  end
  Else
    DebugLn('TCustomMPlayerControl.DoCommand - missed response');

  // Resume the timer
  FTimer.Enabled := True;
end;

function TCustomMPlayerControl.GetPosition: single;
begin
  Result := 0;

  if not Running then
    exit;

  Result := StrToFloatDef(DoCommand('get_time_pos', 'ans_time_position='), 0);
end;

function TCustomMPlayerControl.GetRate: single;
begin
  Result := FRate;

  //If not Running Then
  //  Result := FRate
  //Else
  //  Result := StrToFloatDef(DoCommand('get_property speed', 'ans_speed='), 1)
end;

procedure TCustomMPlayerControl.SetImagePath(AValue: string);
begin
  if DirectoryExistsUTF8(AValue) then
  begin
    FImagePath:=AValue;
    SetCurrentDirUTF8(AValue);
  end;
end;

procedure TCustomMPlayerControl.SetPosition(AValue: single);
begin
  if Running then
    SendMPlayerCommand(Format('pausing_keep seek %.3f 2', [AValue]));
end;

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
end.
