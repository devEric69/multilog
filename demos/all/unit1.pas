unit Unit1; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqldb, LResources, Forms, Controls, Graphics, Dialogs,
  ComCtrls, Buttons, ExtCtrls, StdCtrls, Spin, VirtualTrees, MultiLog,
  MultiLogLCLHelpers { add to goLogger, "hacked" proc. like SendBitmap },
  LogTreeView, IPCChannel, FileChannel, MemoChannel;

type

  { TForm1 }

  TForm1 = class(TForm)
    BitBtn1: TBitBtn;
    butClear: TBitBtn;
    btnDeleteLogFile1: TButton;
    butSubLog: TBitBtn;
    butException1: TButton;
    butString: TButton;
    butInteger: TButton;
    butFloat: TButton;
    butBoolean: TButton;
    butEnterMethod: TButton;
    butExitMethod: TButton;
    butCalledBy: TButton;
    butCallStack: TButton;
    butHeapInfo: TButton;
    butException: TButton;
    butStrings: TButton;
    butInfo: TButton;
    ButSendMemory: TButton;
    ButGenericCheckPoint: TButton;
    ButAddNamedCheckPoint: TButton;
    ButOpenImage: TButton;
    ButSendBitmap: TButton;
    butTestFilterOnWhichTrackingPurposesMsgAreLogged: TButton;
    butTestLog: TButton;
    butWatchString: TButton;
    butWatchInteger: TButton;
    butWarning: TButton;
    butError: TButton;
    chkLogAlmostWhith_lwdebug_reason: TCheckBox;
    chkShowUsedMethod: TCheckBox;
    chkShowUsedMethod1: TCheckBox;
    chkShowDynamicFilter_forWhatReasonsToLogActually: TCheckBox;
    gbxTrackingPurposes_and_messageUsed: TGroupBox;
    memLog: TMemo;
    memFileLog: TMemo;
    MemoTabSheet: TTabSheet;
    ShowTimeCheckBox: TCheckBox;
    FakeSQLQueryToRaiseAnException: TSQLQuery;
    FilenameSheet: TTabSheet;
    TimeFormatEdit: TEdit;
    ViewersPageControl: TPageControl;
    TreeTabSheet: TTabSheet;
    EditNamedCheckPoint: TEdit;
    EditWatchString: TEdit;
    EditInfo: TEdit;
    EditWarning: TEdit;
    EditError: TEdit;
    Image1: TImage;
    memoStrings: TMemo;
    butObject: TButton;
    comboBoolean: TComboBox;
    comboEnterMethod: TComboBox;
    editCalledBy: TEdit;
    editExitMethod: TEdit;
    editString: TEdit;
    OpenDialog1: TOpenDialog;
    PageBitmap: TTabSheet;
    pageGeneral: TTabSheet;
    spinWatchInteger: TSpinEdit;
    spinFloat: TFloatSpinEdit;
    Notebook1: TPageControl;
    pageWatches: TTabSheet;
    pageSpecialized: TTabSheet;
    pageMethods: TTabSheet;
    pageVariables: TTabSheet;
    spinInteger: TSpinEdit;
    Splitter1: TSplitter;
    procedure btnDeleteLogFile1Click(Sender: TObject);
    procedure btnLoadLogFileClick(Sender: TObject);
    procedure ButAddNamedCheckPointClick(Sender: TObject);
    procedure butBooleanClick(Sender: TObject);
    procedure butCalledByClick(Sender: TObject);
    procedure butCallStackClick(Sender: TObject);
    procedure butClearClick(Sender: TObject);
    procedure butEnterMethodClick(Sender: TObject);
    procedure butErrorClick(Sender: TObject);
    procedure butException1Click(Sender: TObject);
    procedure butExceptionClick(Sender: TObject);
    procedure butExitMethodClick(Sender: TObject);
    procedure butFloatClick(Sender: TObject);
    procedure ButGenericCheckPointClick(Sender: TObject);
    procedure butHeapInfoClick(Sender: TObject);
    procedure butInfoClick(Sender: TObject);
    procedure butIntegerClick(Sender: TObject);
    procedure ButOpenImageClick(Sender: TObject);
    procedure ButSendBitmapClick(Sender: TObject);
    procedure ButSendMemoryClick(Sender: TObject);
    procedure butStringClick(Sender: TObject);
    procedure butStringsClick(Sender: TObject);
    procedure butTestFilterOnWhichTrackingPurposesMsgAreLoggedClick(Sender: TObject);
    procedure butWarningClick(Sender: TObject);
    procedure butWatchIntegerClick(Sender: TObject);
    procedure butWatchStringClick(Sender: TObject);
    procedure chkShowDynamicFilter_forWhatReasonsToLogActuallyChange(
      Sender: TObject);
    procedure chkShowPrefixChange(Sender: TObject);
    procedure chkShowUsedMethodChange(Sender: TObject);
    procedure ObjectClick(Sender: TObject);
    procedure chkLogAlmostWhith_lwdebug_reasonClick(Sender: TObject);
    procedure ShowTimeCheckBoxChange(Sender: TObject);
    procedure ButSubLogClick(Sender: TObject);
    procedure TestLogClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure TimeFormatEditEditingDone(Sender: TObject);
  private
    { private declarations }
    FoMemoChannel: TMemoChannel;
    FoFileChannel: TFileChannel;
    FoLogTreeView: TLogTreeView;
    //new fake event's triggers (procedures of objects)
    FOnDoSmallCodePieceNum1: TNotifyEvent;
    FOnDoSmallCodePieceNum2: TNotifyEvent;
		FOnDoSmallCodePieceNum3: TNotifyEvent;
    FOnDoSmallCodePieceNum4: TNotifyEvent;
    FOnDoSmallCodePieceNum5: TNotifyEvent;
  public
    procedure DoSmallCodePieceNum1(Sender: TObject);
    procedure DoSmallCodePieceNum2(Sender: TObject);
    procedure DoSmallCodePieceNum3(Sender: TObject);
    procedure DoSmallCodePieceNum4(Sender: TObject);
    procedure DoSmallCodePieceNum5(Sender: TObject);
	  //new fake events managers's properties
    property OnDoSmallCodePieceNum1: TNotifyEvent read FOnDoSmallCodePieceNum1 write FOnDoSmallCodePieceNum1;
    property OnDoSmallCodePieceNum2: TNotifyEvent read FOnDoSmallCodePieceNum2 write FOnDoSmallCodePieceNum2;
    property OnDoSmallCodePieceNum3: TNotifyEvent read FOnDoSmallCodePieceNum3 write FOnDoSmallCodePieceNum3;
    property OnDoSmallCodePieceNum4: TNotifyEvent read FOnDoSmallCodePieceNum4 write FOnDoSmallCodePieceNum4;
    property OnDoSmallCodePieceNum5: TNotifyEvent read FOnDoSmallCodePieceNum5 write FOnDoSmallCodePieceNum5;
    { public declarations }
  end; 

var
  Form1: TForm1; 

implementation

uses
  LazFileUtils;

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
var
  sPathLogAppli: string;
  ieChannelOptions: TFileChannelOptions;
  bWantSpecificFileForSQLstatements: Boolean;
begin
  chkLogAlmostWhith_lwdebug_reasonClick(Sender);
  Notebook1.ActivePage:= pageGeneral;
  ViewersPageControl.ActivePage:= TreeTabSheet;

  //DoSmallCodePieceNum1, .. DoSmallCodePieceNum3 == fake events, to illustrate their indented registration inside the log file.
  Self.OnDoSmallCodePieceNum1:= @DoSmallCodePieceNum1;
  Self.OnDoSmallCodePieceNum2:= @DoSmallCodePieceNum2;
  Self.OnDoSmallCodePieceNum3:= @DoSmallCodePieceNum3;
  Self.OnDoSmallCodePieceNum4:= @DoSmallCodePieceNum4;
  Self.OnDoSmallCodePieceNum5:= @DoSmallCodePieceNum5;

  //dynamically created for those who haven't installed the package, and cannot drop this component from the palette.
  FoLogTreeView:= TLogTreeView.Create(Self);
  with FoLogTreeView do begin
    Parent:= TreeTabSheet;
    Left:= 2;
    Height:= 295;
    Top:= 2;
    Width:= 331;
    Align:= alClient;
    BorderSpacing.Around:= 2;
    DefaultItemHeight:= 18;
    ScrollBars:= ssAutoBoth;
    ShowTime:= False;
    TabOrder:= 0;
    TimeFormat:= 'hh:nn:ss:zzz';
  end;
  FoMemoChannel:= TMemoChannel.Create(memLog);
  FoMemoChannel.TimeFormat:= FoLogTreeView.TimeFormat;
  FoMemoChannel.ShowTime:= False;
  ieChannelOptions:= [FileChannel.fcoShowHeader, FileChannel.fcoShowTime]; bWantSpecificFileForSQLstatements:= True; sPathLogAppli:= Application.Location+'Log.txt';
  FoFileChannel:= TFileChannel.Create(sPathLogAppli, ieChannelOptions, bWantSpecificFileForSQLstatements);
  FoFileChannel.IndentCallstackTriggeredByEntermethod:= true;
  TimeFormatEdit.Text:= FoLogTreeView.TimeFormat;
  with goLogger do begin
    Channels.Add( FoLogTreeView.Channel );
    Channels.Add( FoMemoChannel );
    Channels.Add( FoFileChannel );
    Channels.Add( TIPCChannel.Create );
  end;
end;

procedure TForm1.chkShowPrefixChange(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to Pred(goLogger.Channels.Count) do begin
		if (goLogger.Channels.Items[i].ClassType = TFileChannel) then
			TFileChannel(goLogger.Channels[i]).ShowPrefixMethod:= true
    else if (goLogger.Channels.Items[i].ClassType = TMemoChannel) then
    	TMemoChannel(goLogger.Channels[i]).ShowPrefixMethod:= true;
  end;
end;


procedure TForm1.TestLogClick(Sender: TObject);
var
  oList: TStringList;
	setOldDynamic_forWhatReasonsToLog: TGroupOfForWhatReasonsToLogActually;
begin
  with goLogger do begin
  	setOldDynamic_forWhatReasonsToLog:= FilterDynamic_forWhatReasonsToLogActually;
    Send('A basic send');
   	SendColor('Red', clYellow);
    EnterMethod(Sender,'TestLogClick');
    SendInfo('A Text Message');
    SendInfo('Another Text Message');
    oList:= TStringList.Create;
    with oList do begin
      Add('aaaaaaa');
      Add('bbbbbbb');
      Add('ccccccc');
    end;
    SendInfo('A StringList', oList);
    oList.Destroy;
    SendError('A Error Message');
    ButSubLogClick(butSubLog);
    FilterDynamic_forWhatReasonsToLogActually:= [lwDebug,lwInfo];
    SendInfo('This Text Should NOT be logged');
    FilterDynamic_forWhatReasonsToLogActually:= [];
    SendInfo('This Text Should NOT be logged');
    FilterDynamic_forWhatReasonsToLogActually:= [];
    SendInfo([lwDebug, lwInfo],'And This Text Should be logged too!');
    FilterDynamic_forWhatReasonsToLogActually:= lwAllApplied;
    SendInfo('3 - the 3 Sets (targeted, filter applied on targeted, and resulting enabled, have been changed!)');
    //Exitmethod is called even if not active if there's a unpaired EnterMethod
    ExitMethod(Sender,'TestLogClick');
    FilterDynamic_forWhatReasonsToLogActually:= [lwInfo];
    SendInfo('4 - the 3 Sets (targeted, filter applied on targeted, and resulting enabled, have been changed!)');
    SendInfo('The last logged text (using existing IntersectingGroup2ForProcWithoutParamGroup2_OfWhatCanBeLogged==[lwInfo]).');
    FilterDynamic_forWhatReasonsToLogActually:= [];
    SendInfo([lwWarning],'The last logged text by comparing the ***temporary*** intersection between [lwWarning] and TargetedConstantGroup1_OfWhatCanBeLogged==[lwAll]). Bye!');
    SendInfo('This Text Should be logged: intersection betweeen IntersectingGroup2ForProcWithoutParamGroup2_OfWhatCanBeLogged==[] * TargetedConstantGroup1_OfWhatCanBeLogged==[lwAll] = [].');
    // Reset "targeted" set to full possibilities, and "filter to applied" to minimal, removing thoses available but not yet used (for the other examples, the others buttons)
    FilterDynamic_forWhatReasonsToLogActually:= lwAllApplied - [lwIPC, lwWorkers];
    Send('A basic send');
    FilterDynamic_forWhatReasonsToLogActually:= setOldDynamic_forWhatReasonsToLog;
  end;
end;



procedure TForm1.butTestFilterOnWhichTrackingPurposesMsgAreLoggedClick(Sender: TObject);
var
	oList: TStringList;
	setOldDynamic_forWhatReasonsToLog: TGroupOfForWhatReasonsToLogActually;
begin
    setOldDynamic_forWhatReasonsToLog:= goLogger.FilterDynamic_forWhatReasonsToLogActually;
    with goLogger do begin
      SendColor('Yellow', clYellow);
      FilterDynamic_forWhatReasonsToLogActually:= [];    //	 no reasons to log actually: [lwdebug] has been removed.
      Send('A- a Text Message ==> should not be viewed; there no "one shot set of reasons to log actually"');
      SendInfo([lwdebug], 'B- another informational Text Message ==> should be see with its added one shot [lwInfo]');
      FilterDynamic_forWhatReasonsToLogActually:= setOldDynamic_forWhatReasonsToLog;
      EnterMethod(Sender,'TestLogClick');
      oList:= TStringList.Create;
      with oList do begin
        Add('xxx');
        Add('yyy');
        Add('zzz');
      end;
      SendInfo('A StringList', oList);
      oList.Destroy;
      SendError('A Error Message');
      ButSubLogClick(butSubLog);
		  FilterDynamic_forWhatReasonsToLogActually:= [];
      Send('This Text Should NOT be logged');
      SendInfo('This Text Should NOT be logged too');
      SendInfo([lwInfo],'This text Should be logged with the one shot added group [lwInfo] ==> and it should be see with its added one shot [lwInfo]');
      SendInfo([lwdebug],'This text Should be logged with the one shot added group [lwInfo] ==> and it should be see with its added one shot [lwInfo]');
		  FilterDynamic_forWhatReasonsToLogActually:= [lwInfo];
      SendInfo('This Text Should be logged');
      SendInfo([lwDebug],'This Text Should be logged too ==> should be see with its added one shot [lwInfo]');
      ExitMethod(Sender,'TestLogClick');
      FilterDynamic_forWhatReasonsToLogActually:= setOldDynamic_forWhatReasonsToLog;
    end;
end;

procedure TForm1.butClearClick(Sender: TObject);
begin
  goLogger.Clear;
end;

procedure TForm1.butEnterMethodClick(Sender: TObject);
begin
  with comboEnterMethod do
  begin
    if (Text <> '') and (Items.IndexOf(Text) =-1) then begin
      Items.Add(Text);
      editExitMethod.Text:=Text;
      goLogger.EnterMethod(Text);
    end;
  end;
end;

procedure TForm1.butException1Click(Sender: TObject);
begin
  try
  	FakeSQLQueryToRaiseAnException.Open;
  except
    On E: Exception do
      goLogger.SendException('An S.Q.L. Exception example', E);
  end
end;

procedure TForm1.butExceptionClick(Sender: TObject);
begin
  try
    StrToInt('XXXXX');
  except
    On E: Exception do
      goLogger.SendException('A "basic" Exception example', E);
  end
end;

procedure TForm1.butExitMethodClick(Sender: TObject);
var
  i: Integer;
begin
  with editExitMethod do begin
    if Text <> '' then begin
      goLogger.ExitMethod(Text);
      i:=comboEnterMethod.Items.IndexOf(Text);
      if i <> -1 then
        comboEnterMethod.Items.Delete(i);
      Dec(i);
      if i <> -1 then
        Text:=comboEnterMethod.Items[i]
      else
        Text:='';
    end;
  end;
end;

procedure TForm1.butBooleanClick(Sender: TObject);
begin
  goLogger.SendInfo('A Boolean Variable', Boolean(comboBoolean.ItemIndex));
end;

procedure TForm1.ButAddNamedCheckPointClick(Sender: TObject);
begin
  if EditNamedCheckPoint.Text <> '' then begin
    goLogger.AddCheckPoint(EditNamedCheckPoint.Text);
    goLogger.AddCheckPoint(EditNamedCheckPoint.Text);
    goLogger.AddCheckPoint(EditNamedCheckPoint.Text);
  end;
end;

procedure TForm1.btnLoadLogFileClick(Sender: TObject);
var
  sFullPathFileName: string;
begin
  sFullPathFileName:= FoFileChannel.FullPathFileName;
  if FileExists(sFullPathFileName) then
  	memFileLog.Lines.LoadFromFile(sFullPathFileName);
end;

procedure TForm1.btnDeleteLogFile1Click(Sender: TObject);
var
  sFullPathFileName: string;
begin
  sFullPathFileName:= FoFileChannel.FullPathFileName;
  DeleteFileUTF8(sFullPathFileName);
  memFileLog.Lines.Clear;
end;

procedure TForm1.butCalledByClick(Sender: TObject);
begin
  if editCalledBy.Text <> '' then
    with goLogger do
      SendIf('Send only if Called By '+editCalledBy.Text,CalledBy(editCalledBy.Text));
end;

procedure TForm1.butCallStackClick(Sender: TObject);
begin
  goLogger.SendCallStack('A CallStack Example');
end;

procedure TForm1.butFloatClick(Sender: TObject);
begin
  goLogger.SendInfo('A Float Variable',spinFloat.Value);
end;

procedure TForm1.ButGenericCheckPointClick(Sender: TObject);
begin
  goLogger.AddCheckPoint;
  goLogger.AddCheckPoint;
  goLogger.AddCheckPoint;
end;

procedure TForm1.butInfoClick(Sender: TObject);
begin
  if EditInfo.Text <> '' then
    goLogger.SendInfo(EditInfo.Text);
end;

procedure TForm1.butWarningClick(Sender: TObject);
begin
  if EditWarning.Text <> '' then
    goLogger.SendWarning(EditWarning.Text);
end;

procedure TForm1.butErrorClick(Sender: TObject);
begin
  if EditWarning.Text <> '' then
    goLogger.SendError(EditError.Text);
end;

procedure TForm1.butIntegerClick(Sender: TObject);
begin
  goLogger.SendInfo('A Integer Variable', spinInteger.Value);
end;

procedure TForm1.ButOpenImageClick(Sender: TObject);
begin
  if OpenDialog1.Execute then
    Image1.Picture.LoadFromFile(OpenDialog1.FileName);
end;

procedure TForm1.ButSendBitmapClick(Sender: TObject);
begin
  goLogger.SendBitmap('ABitmap',Image1.Picture.Bitmap);
end;

procedure TForm1.ButSendMemoryClick(Sender: TObject);
var
 sStr: String;
begin
  sStr:= 'dfhejhrgtjehrgthjergthjergthjergterbdfngm';
  goLogger.SendMemory('The memory (through an @ddress), of a string found by the recipent(s) of the registred Channels', PChar(sStr), Length(sStr));	//sends a PChar towards the heap (whose used memory should decrease)
end;

procedure TForm1.butStringClick(Sender: TObject);
begin
  goLogger.SendInfo('A String Variable', editString.Text);
end;

procedure TForm1.butStringsClick(Sender: TObject);
begin
  if memoStrings.Lines.Count > 0 then begin
    goLogger.SendInfo('A TStrings', memoStrings.Lines);
  end;
end;

procedure TForm1.butHeapInfoClick(Sender: TObject);
begin
  goLogger.SendHeapInfo('A Heap Info Example');
end;

procedure TForm1.butWatchIntegerClick(Sender: TObject);
begin
  goLogger.Watch('X (varying in a loop, for example)', spinWatchInteger.Value);
end;

procedure TForm1.butWatchStringClick(Sender: TObject);
begin
  goLogger.Watch('Str (varying in a conditional concatenation''s, for example)', EditWatchString.Text);
end;

procedure TForm1.chkShowDynamicFilter_forWhatReasonsToLogActuallyChange(Sender: TObject);
begin
  FoLogTreeView.ShowDynamicFilter_forWhatReasonsToLogActually:= chkShowDynamicFilter_forWhatReasonsToLogActually.Checked;
	FoFileChannel.ShowDynamicFilter_forWhatReasonsToLogActually:= chkShowDynamicFilter_forWhatReasonsToLogActually.Checked;
  FoMemoChannel.ShowDynamicFilter_forWhatReasonsToLogActually:= chkShowDynamicFilter_forWhatReasonsToLogActually.Checked;
end;


procedure TForm1.chkShowUsedMethodChange(Sender: TObject);
begin
  FoLogTreeView.ShowPrefixMethod:= chkShowUsedMethod.Checked;
  FoFileChannel.ShowPrefixMethod:= chkShowUsedMethod.Checked;
  FoMemoChannel.ShowPrefixMethod:= chkShowUsedMethod.Checked;
end;

procedure TForm1.ObjectClick(Sender: TObject);
begin
  goLogger.SendInfo('An TObject Example', Sender);
end;

procedure TForm1.chkLogAlmostWhith_lwdebug_reasonClick(Sender: TObject);
begin
  if chkLogAlmostWhith_lwdebug_reason.Checked then
  	goLogger.FilterDynamic_forWhatReasonsToLogActually:= goLogger.FilterDynamic_forWhatReasonsToLogActually + [lwDebug]
	else
  	goLogger.FilterDynamic_forWhatReasonsToLogActually:= goLogger.FilterDynamic_forWhatReasonsToLogActually - [lwDebug];
end;


procedure TForm1.ShowTimeCheckBoxChange(Sender: TObject);
begin
  FoLogTreeView.ShowTime:= ShowTimeCheckBox.Checked;
  FoFileChannel.ShowTime:= ShowTimeCheckBox.Checked;
  FoMemoChannel.ShowTime:= ShowTimeCheckBox.Checked;
end;

procedure TForm1.DoSmallCodePieceNum1(Sender: TObject);
var
	iDummy, iFakeDummyCalc: integer;
begin
  goLogger.SubEventBetweenEnterAndExitMethods('❖ eventDoSmallCodePieceNum1 + Sender=' + Sender.ClassName + ' (I''m the base event studied.)');   // TNotifyEvent
  //do a stuff
  for iDummy:= 0 to 10 do
  	iFakeDummyCalc:= iFakeDummyCalc + 1;
  DoSmallCodePieceNum2(Sender);
  goLogger.SubEventBetweenEnterAndExitMethods('<event DoSmallCodePieceNum1 + Sender=' + Sender.ClassName);
  goLogger.SubEventBetweenEnterAndExitMethods('>event DoSmallCodePieceNum1 + Sender=' + Sender.ClassName);   // TNotifyEvent
  DoSmallCodePieceNum4(Sender);
  goLogger.SubEventBetweenEnterAndExitMethods('❖ event DoSmallCodePieceNum1 + Sender=' + Sender.ClassName + ' (Back to the base: we can now see the order of the chained events, their story.)');
end;

procedure TForm1.DoSmallCodePieceNum2(Sender: TObject);
var
	iFakeDummy, iFakeDummyCalc: integer;
begin
  goLogger.SubEventBetweenEnterAndExitMethods('>event DoSmallCodePieceNum2 + Sender=' + Sender.ClassName);
  //an event do a stuff normally:
  for iFakeDummy:= iFakeDummy to 20 do
  	iFakeDummyCalc:= iFakeDummyCalc+ 1;
  DoSmallCodePieceNum3(Sender);
  goLogger.SubEventBetweenEnterAndExitMethods('<event DoSmallCodePieceNum2 + Sender=' + Sender.ClassName);
end;

procedure TForm1.DoSmallCodePieceNum3(Sender: TObject);
begin
  goLogger.SubEventBetweenEnterAndExitMethods('>event DoSmallCodePieceNum3 + Sender=' + Sender.ClassName);
  goLogger.SubEventBetweenEnterAndExitMethods('<event DoSmallCodePieceNum3 + Sender=' + Sender.ClassName);
end;

procedure TForm1.DoSmallCodePieceNum4(Sender: TObject);
begin
  goLogger.SubEventBetweenEnterAndExitMethods('>event DoSmallCodePieceNum4 + Sender=' + Sender.ClassName);
  DoSmallCodePieceNum5(Sender);
  goLogger.SubEventBetweenEnterAndExitMethods('<event DoSmallCodePieceNum4 + Sender=' + Sender.ClassName);
end;

procedure TForm1.DoSmallCodePieceNum5(Sender: TObject);
var
  edtFakeSender: TEdit;
begin
  goLogger.SubEventBetweenEnterAndExitMethods('>event DoSmallCodePieceNum5 + Sender=' + Sender.ClassName);
  // Emulate that the original event is then chained with an subsequent OnEdit's event.
  // For example, the Dataset's and DataSource's events are very linked and interactive: which event call wich event, etc?
  edtFakeSender:= TEdit.Create(nil); edtFakeSender.Parent:= nil;
  DoSmallCodePieceNum3(edtFakeSender);
  DoSmallCodePieceNum2(edtFakeSender);
  edtFakeSender.Free;
  goLogger.SubEventBetweenEnterAndExitMethods('<event DoSmallCodePieceNum6 + Sender=' + Sender.ClassName);
end;



procedure TForm1.ButSubLogClick(Sender: TObject);
var
  setOfOldTargetedConstantGroup1_OfWhatCanBeLogged: TGroupOfForWhatReasonsToLogActually;
	bIsCalledBy: boolean;
begin
  with goLogger do begin
    setOfOldTargetedConstantGroup1_OfWhatCanBeLogged:= FilterDynamic_forWhatReasonsToLogActually;
    Send('A basic send');
    // false sequence of called functions, emulating a chained events sequence, such as coded in dbGrid.Onchange, dbNavigator.OnPost, dataSource.onChange, dataField.onValidate, dataSet.onPost, etc.
    EnterMethod(Sender,'ButSubLogClick');
    // Playing a little with the Calls Stack
    DoSmallCodePieceNum1(Sender);
    bIsCalledBy:= CalledBy('TestLogClick'); SendIf('Only show if called by TestLogClick', bIsCalledBy);
    bIsCalledBy:= CalledBy('Fake___LogClick'); SendIf('Only show if called by Fake___LogClick', bIsCalledBy);
    bIsCalledBy:= CalledBy('ButSubLogClick'); SendIf('Only show if called by ButSubLogClick', bIsCalledBy);
    SendInfo('AText inside ButSubLogClick');
    SendWarning('A Warning');
    SendCallStack('CallStack example');
    SendInfo('A String', 'sadjfgadsfbmsandfb');
    SendInfo('AInteger', 4957);
    SendInfo('A Boolean', True);
    ExitMethod(Sender, 'SubLogClick');
    FilterDynamic_forWhatReasonsToLogActually:= setOfOldTargetedConstantGroup1_OfWhatCanBeLogged;
  end;
end;


procedure TForm1.FormDestroy(Sender: TObject);
begin
  goLogger.Channels.Remove(FoLogTreeView.Channel);
  FreeAndNil(FoLogTreeView);
end;

procedure TForm1.TimeFormatEditEditingDone(Sender: TObject);
begin
  FoLogTreeView.TimeFormat := TimeFormatEdit.Text;
  FoMemoChannel.TimeFormat := TimeFormatEdit.Text;
end;


end.

