unit fMain;

{ Viewer for Multilog messages

  Copyright (C) 2006 Luiz Américo Pereira Câmara
  pascalive@bol.com.br

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, butShowPrefixMethod WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
  MA 02111-1307, USA.
}

{$mode objfpc}{$H+}

interface
//todo: - Use only one StringGrid for Watches (???)
//      - Optimize Watch update (Cache current values?)

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Grids, StdCtrls, MultiLog, VirtualTrees, ComCtrls, Buttons, simpleipc, WatchList,
  Menus, ATBinHex, ToggleLabel, SearchEdit;

type
  TMessageSet = 0..31; //	lwDebug..lwLast

  { TfrmMain }

  TfrmMain = class(TForm)
    BinHexViewer: TATBinHex;
    butRefresh: TButton;
    butSelectAll: TButton;
    butShowReasonsForLogging: TToolButton;
    butShowHelp: TToolButton;
    butUnSelectAll: TButton;
    checkSend: TCheckBox;
    checkWatch: TCheckBox;
    checkCounter: TCheckBox;
    checkColor: TCheckBox;
    checkSubEventBetweenEnterAndExitMethods: TCheckBox;
    checkHeapInfo: TCheckBox;
    checkInfo: TCheckBox;
    checkMemory: TCheckBox;
    checkCustomData: TCheckBox;
    checkBitmap: TCheckBox;
    checkWarning: TCheckBox;
    checkError: TCheckBox;
    checkValue: TCheckBox;
    checkConditional: TCheckBox;
    checkCheckPoint: TCheckBox;
    checkStrings: TCheckBox;
    checkObject: TCheckBox;
    checkException: TCheckBox;
    checkCallStack: TCheckBox;
    ComboWatchHistory: TComboBox;
    imgToolbar: TImageList;
    ImgViewer: TImage;
    imgMessages: TImageList;
    Label1: TLabel;
    lbMemorySize: TLabel;
    MainMenu1: TMainMenu;
    memoViewer: TMemo;
    MIAbout: TMenuItem;
    MISep1: TMenuItem;
    MIClearAll: TMenuItem;
    MIExit: TMenuItem;
    MIHelp: TMenuItem;
    MIFile: TMenuItem;
    nbWatches: TPageControl;
    nbViewer: TNotebook;
    PageHistory: TTabSheet;
    PageBitmap: TPage;
    PageHexViewer: TPage;
    pageNull: TPage;
    pageText: TPage;
    pageSelected: TTabSheet;
    pageLastest: TTabSheet;
    PanelImageViewer: TPanel;
    panelFilter: TPanel;
    panelMessages: TPanel;
    panelViewer: TPanel;
    panelLeft: TPanel;
    panelRight: TPanel;
    EditFilterMessages: TSearchEdit;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    gridCallStack: TStringGrid;
    gridLastestWatch: TStringGrid;
    gridSelectedWatch: TStringGrid;
    Splitter4: TSplitter;
    Splitter5: TSplitter;
    GridWatchHistory: TStringGrid;
    StringGridBitmap: TStringGrid;
    ToggleOptions: TToggleLabel;
    toolbarMain: TToolBar;
    butClear: TToolButton;
    butStop: TToolButton;
    butAlwaysOnTop: TToolButton;
    butShowPrefixMethod: TToolButton;
    vtreeMessages: TVirtualStringTree;
    procedure butAlwaysOnTopClick(Sender: TObject);
    procedure butRefreshClick(Sender: TObject);
    procedure butShowHelpClick(Sender: TObject);
    procedure butShowPrefixMethodClick(Sender: TObject);
    procedure butShowReasonsForLoggingClick(Sender: TObject);
    procedure butStopClick(Sender: TObject);
    procedure EditFilterMessagesExecute(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure butSelectAllClick(Sender: TObject);
    procedure butUnSelectAllClick(Sender: TObject);
    procedure ClearMessages(Sender: TObject);
    procedure ComboWatchHistorySelect(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ImgViewerDblClick(Sender: TObject);
    procedure MIAboutClick(Sender: TObject);
    procedure MIExitClick(Sender: TObject);
    procedure nbWatchesPageChanged(Sender: TObject);
    procedure PanelImageViewerDblClick(Sender: TObject);
    procedure QuitApplication(Sender: TObject);
    procedure ToggleOptionsChange(Sender: TObject);
    procedure vtreeMessagesFocusChanged(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex);
    procedure vtreeMessagesFocusChanging(Sender: TBaseVirtualTree;
      OldNode: PVirtualNode; NewNode: PVirtualNode; OldColumn: TColumnIndex;
      NewColumn: TColumnIndex; var Allowed: Boolean);
    procedure vtreeMessagesFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vtreeMessagesGetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: LongInt);
    procedure vtreeMessagesGetText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: UTF8String);
    procedure vtreeMessagesInitNode(Sender: TBaseVirtualTree;
      ParentNode: PVirtualNode; Node: PVirtualNode;
      var InitialStates: TVirtualNodeInitStates);
  private
    { private declarations }
    {$ifdef unix}
    FTimer: TTimer;
    {$endif}
    FTitleFilter: String;
    FiMethUsed: set of TMessageSet;
    FMessageCount: LongWord;
    FActiveWatch: TStringGrid;
    FCurrentMsg: TrecLogMessage;
    FLastParent: PVirtualNode;
    FLastNode: PVirtualNode;
    FIPCServer: TSimpleIPCServer;
    FWatches: TWatchList;
    FExpandParent: Boolean;
    FbShowPrefixMethod: Boolean;
    FbShowShowReasonsForLogging: Boolean;
    procedure FilterCallback(Sender: TBaseVirtualTree; Node: PVirtualNode; Data: Pointer; var Abort: Boolean);
    procedure SetupFilters;
    procedure ToggleFilterSelected(CheckState:Boolean);
    procedure WatchUpdateCallback(const AVariable,AValue: String);
    procedure NewWatchVariable(const AVariable: String; AIndex: PtrInt);
    procedure ReceiveMessage(Sender: TObject);
    procedure UpdateCallStack(var ANode: PVirtualNode);
    procedure UpdateWatches;
    procedure UpdateWatchHistory;
    procedure ShowBitmapInfo(ABitmap: TBitmap);
    procedure ApplicationIdle(Sender: TObject; var Done: Boolean);
    {$ifdef unix}
    procedure GetMessages(Sender: TObject);
    {$endif}
  public
    { public declarations }
  end; 

var
  frmMain: TfrmMain;

resourcestring
  csHint_On = ' (On)';
  csHint_Off = ' (Off)';
  csHint_AlwayOnTop = 'Always on top';
  csHint_DidacticMethod = 'Didactic display of the coded Lazarus\Pascal method';
	csHint_ReasonForLogging = 'Display the reasons for logging';

implementation

{$R *.lfm}

uses
  StrUtils, LCLIntf, LCLType, fAbout, fhelp{$If defined(DEBUG)},LazLoggerBase{$EndIf};

type
  TNodeData = record
    Title: String;
    AMethodUsed: Integer;
    MsgData: TStream;
    MsgTime: TDateTime;
    ASetFilterDynamic: TGroupOfForWhatReasonsToLogActually;
    Index: LongWord;
  end;
  PNodeData = ^TNodeData;

const
  PixelFormatNames: array [TPixelFormat] of String =
  (
    'pfDevice',
    'pf1bit',
    'pf4bit',
    'pf8bit',
    'pf15bit',
    'pf16bit',
    'pf24bit',
    'pf32bit',
    'pfCustom'
    );
  HandleTypeNames: array [TBitmapHandleType] of String =
  (	'bmDIB',
   	'bmDDB');
  
{ TfrmMain }

procedure TfrmMain.butUnSelectAllClick(Sender: TObject);
begin
  ToggleFilterSelected(False);
end;

procedure TfrmMain.ClearMessages(Sender: TObject);
begin
  vtreeMessages.Clear;
  gridCallStack.RowCount := 1;
  gridLastestWatch.RowCount := 1;
  gridSelectedWatch.RowCount := 1;
  GridWatchHistory.RowCount := 1;
  FWatches.Clear;
  ComboWatchHistory.Clear;
  //memoViewer.Lines.Clear;
  nbViewer.PageIndex:=0;//pageNull;
  FMessageCount:=0;
  FLastNode:=nil;
  FLastParent:=nil;
end;

procedure TfrmMain.ComboWatchHistorySelect(Sender: TObject);
begin
  UpdateWatchHistory;
end;

procedure TfrmMain.EditFilterMessagesExecute(Sender: TObject);
begin
  SetupFilters;
  //Scans all tree nodes
  vtreeMessages.IterateSubtree(nil,@FilterCallback,nil);
end;



procedure TfrmMain.butStopClick(Sender: TObject);
begin
  if butStop.Down then
    FIPCServer.OnMessage := nil
  else
    FIPCServer.OnMessage := @ReceiveMessage;
end;



procedure TfrmMain.butRefreshClick(Sender: TObject);
begin
  EditFilterMessagesExecute(Sender);
end;

procedure TfrmMain.butShowHelpClick(Sender: TObject);
var
  oFrmHelp: TfrmHelp;
begin
  oFrmHelp:= TfrmHelp.Create(Self);
  try
  	oFrmHelp.ShowModal;
  finally
    oFrmHelp.Free;
  end;
end;

procedure TfrmMain.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  if MessageDlg('Confirm Exit', 'Quit Multilog Viewer?', mtConfirmation, mbYesNo, 0) = mrNo then
    CanClose := False;
end;

procedure TfrmMain.butSelectAllClick(Sender: TObject);
begin
  ToggleFilterSelected(True);
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  {$ifdef unix}
  FTimer := TTimer.Create(Self);
  FTimer.Interval := 500;
  FTimer.OnTimer := @GetMessages;
  FTimer.Enabled := True;
  {$endif}
  vtreeMessages.NodeDataSize := SizeOf(TNodeData);
  FWatches := TWatchList.Create;
  FWatches.OnUpdate := @WatchUpdateCallback;
  FWatches.OnNewVariable := @NewWatchVariable;
  FIPCServer := TSimpleIPCServer.Create(nil);
  with FIPCServer do
  begin
    ServerID := 'ipc_log_server';
    Global := True;
    OnMessage := @ReceiveMessage;
    StartServer;
  end;
  SetupFilters;
	//IHM
  nbWatches.ActivePage:= pageLastest;
  butAlwaysOnTop.Down:= False; butAlwaysOnTop.Hint:= csHint_AlwayOnTop + csHint_Off;
  butShowPrefixMethod.Down:= False; butShowPrefixMethod.Hint:= csHint_DidacticMethod + csHint_Off;
  butShowReasonsForLogging.Down:= False; butShowReasonsForLogging.Hint:= csHint_ReasonForLogging + csHint_Off;
end;


procedure TfrmMain.butAlwaysOnTopClick(Sender: TObject);
begin
  if butAlwaysOnTop.Down then begin
    FormStyle := fsSystemStayOnTop;
    butAlwaysOnTop.Hint:= csHint_AlwayOnTop + csHint_On;
  end
  else begin
    FormStyle := fsNormal;
    butAlwaysOnTop.Hint:= csHint_AlwayOnTop + csHint_Off;
  end;
end;

procedure TfrmMain.butShowPrefixMethodClick(Sender: TObject);
begin
  if butShowPrefixMethod.Down then begin
  	butShowPrefixMethod.Hint:= csHint_DidacticMethod + csHint_On;
    FbShowPrefixMethod:= true;
  end
  else begin
  	butShowPrefixMethod.Hint:= csHint_DidacticMethod + csHint_Off;
    FbShowPrefixMethod:= false;
  end;
end;

procedure TfrmMain.butShowReasonsForLoggingClick(Sender: TObject);
begin
  if butShowReasonsForLogging.Down then begin
		butShowReasonsForLogging.Hint:= csHint_ReasonForLogging + csHint_On;
    FbShowShowReasonsForLogging:= true;
  end
  else begin
		butShowReasonsForLogging.Hint:= csHint_ReasonForLogging + csHint_Off;
	  FbShowShowReasonsForLogging:= false;
  end;
end;



procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  FIPCServer.Destroy;
  FWatches.Destroy;
end;

procedure TfrmMain.ImgViewerDblClick(Sender: TObject);
begin
  with ImgViewer.Picture.Bitmap do
    PanelImageViewer.Canvas.DrawFocusRect(Rect(0,0,Width + 1,Height + 1));
end;

procedure TfrmMain.MIAboutClick(Sender: TObject);
begin
  with TFormAbout.Create(Self) do
  try
    ShowModal;
  finally
    Destroy;
  end;
end;

procedure TfrmMain.MIExitClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmMain.nbWatchesPageChanged(Sender: TObject);
begin
  UpdateWatches;
end;

procedure TfrmMain.PanelImageViewerDblClick(Sender: TObject);
begin
  if PanelImageViewer.Color = clBtnFace then
    PanelImageViewer.Color := clWhite
  else
    PanelImageViewer.Color := clBtnFace;
end;

procedure TfrmMain.QuitApplication(Sender: TObject);
begin
  Close;
end;

procedure TfrmMain.ToggleOptionsChange(Sender: TObject);
begin
  if ToggleOptions.Expanded then
    panelFilter.Height := checkValue.Top + checkValue.Height + 4
  else
    panelFilter.Height := EditFilterMessages.Top + EditFilterMessages.Height + 4;
end;

procedure TfrmMain.vtreeMessagesFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
var
  oStream: TStringStream;
begin
  UpdateWatches;
  with PNodeData(Sender.GetNodeData(Node))^do
  begin
    if MsgData = nil then begin
      nbViewer.PageIndex:= 0;//pageNull;
      Exit;
    end;
    MsgData.Position:= 0;
    case AMethodUsed of
      methTStrings,methCallStack,methException,methHeapInfo,methCustomData:
      begin
        memoViewer.Lines.LoadFromStream(MsgData);
        nbViewer.PageIndex:= 1;//pageText;
      end;
      methObject:
      begin
        oStream:=TStringStream.Create('');
        ObjectBinaryToText(MsgData,oStream);
        memoViewer.Lines.Text:= oStream.DataString;
        nbViewer.PageIndex:= 1;//pageText;
        oStream.Destroy;
      end;
      methBitmap:
      begin
        ImgViewer.Picture.Bitmap.LoadFromStream(MsgData);
        nbViewer.PageIndex:= 2;//PageBitmap;
        ShowBitmapInfo(ImgViewer.Picture.Bitmap);
      end;
      methMemory:
      begin
        lbMemorySize.Caption := 'Size: ' + IntToStr(MsgData.Size);
        BinHexViewer.OpenStream(MsgData);
        nbViewer.PageIndex:= 3;//PageHexViewer;
      end;
    end;
  end;
end;

procedure TfrmMain.vtreeMessagesFocusChanging(Sender: TBaseVirtualTree; OldNode: PVirtualNode; NewNode: PVirtualNode; OldColumn: TColumnIndex; NewColumn: TColumnIndex; var Allowed: Boolean);
begin
  //Todo: merge with Changed?
  //The CallStack is only updated if the parent changes
  Allowed:=OldNode <> NewNode;
  if Allowed and ((OldNode = nil) or (NewNode = nil) or (OldNode^.Parent<>NewNode^.Parent)) then
    UpdateCallStack(NewNode);
  //warning NewNode value is not more valid after here
end;

procedure TfrmMain.vtreeMessagesFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
  with PNodeData(Sender.GetNodeData(Node))^ do
  begin
    Title:='';
    if MsgData <> nil then
      MsgData.Destroy;
  end;
end;

procedure TfrmMain.vtreeMessagesGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: LongInt);
begin
  ImageIndex:= PNodeData(Sender.GetNodeData(Node))^.AMethodUsed;
end;

procedure TfrmMain.vtreeMessagesGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: UTF8String);
var
  sWholeMsg: string = '';
begin
  if FbShowPrefixMethod then
  	sWholeMsg:= ctsLogPrefixesMethod[PNodeData(Sender.GetNodeData(Node))^.AMethodUsed] + '  ';
  if FbShowShowReasonsForLogging then
    sWholeMsg:= sWholeMsg + MultiLog.TLogChannelUtils.SetofToString(PNodeData(Sender.GetNodeData(Node))^.ASetFilterDynamic) + '  ';
  sWholeMsg:= sWholeMsg + PNodeData(Sender.GetNodeData(Node))^.Title;
  CellText:= sWholeMsg;
end;

procedure TfrmMain.vtreeMessagesInitNode(Sender: TBaseVirtualTree; ParentNode: PVirtualNode; Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
begin
  //WriteLn('InitNode Called');
  with PNodeData(Sender.GetNodeData(Node))^ do
  begin
    Title:= FCurrentMsg.sMsgText;
    MsgData:= FCurrentMsg.pData;
    MsgTime:= FCurrentMsg.dtMsgTime;
    AMethodUsed:= FCurrentMsg.iMethUsed;
    ASetFilterDynamic:= FCurrentMsg.setFilterDynamic;
    //In fast computers two or more messages can have the same TimeStamp
    //This leads to conflicts when determining the Watches values
    //Use an unique index instead
    Index:= FMessageCount;
    //Show only what matches filter criterias
    Sender.IsVisible[Node]:= (AMethodUsed in [methEnterMethod,methExitMethod]) or
     ((AMethodUsed in FiMethUsed) and IsWild(Title,FTitleFilter,True));
  end;
end;

procedure TfrmMain.FilterCallback(Sender: TBaseVirtualTree; Node: PVirtualNode; Data: Pointer; var Abort: Boolean);
begin
  with PNodeData(Sender.GetNodeData(Node))^ do
    Sender.IsVisible[Node]:= (AMethodUsed in [methEnterMethod,methExitMethod]) or
     ((AMethodUsed in FiMethUsed) and IsWild(Title,FTitleFilter,True));
end;

procedure TfrmMain.SetupFilters;
var
  AControl: TControl;
  i: Integer;
begin
  //Set Active Method's message set used to filter
  FiMethUsed:= [methEnterMethod,methExitMethod];	//always show Enter/ExitMethod
  with panelFilter do
    for i:= 0 to ControlCount - 1 do begin
      AControl:= Controls[i];
      if (AControl is TCheckBox) and (TCheckBox(AControl)).Checked then
        Include(FiMethUsed, AControl.Tag);
    end;
  //Set Title Filter
  FTitleFilter:= Trim(EditFilterMessages.Text)+'*';
  if Length(FTitleFilter) > 1 then //editFilter is not empty
    FTitleFilter:='*'+FTitleFilter;
  //writeln('FFilter:', FTitleFilter);
end;

procedure TfrmMain.ToggleFilterSelected(CheckState:Boolean);
var
  AControl: TControl;
  i: Integer;
begin
  with panelFilter do
   for i:= 0 to ControlCount - 1 do begin
     AControl:= Controls[i];
     if (AControl is TCheckBox) then
        TCheckBox(AControl).Checked:= CheckState;
   end;
end;

procedure TfrmMain.WatchUpdateCallback(const AVariable, AValue: String);
begin
  with FActiveWatch do
  begin
    RowCount:= RowCount+1;
    Cells[0,RowCount-1]:= AVariable;
    Cells[1,RowCount-1]:= AValue;
  end;
end;

procedure TfrmMain.NewWatchVariable(const AVariable: String; AIndex: PtrInt);
begin
  ComboWatchHistory.Items.AddObject(AVariable,TObject(AIndex));
  ComboWatchHistory.ItemIndex:= 0;	// select the first item in the combobox, in order to show where the watches are stored
end;



procedure TfrmMain.ReceiveMessage(Sender: TObject);
var
  iSizeOfText: Integer = 0; iSizeOfData: Integer = 0; iSizeOfSetFilterDynamic_speIPC: Integer = 0;
  sSetFilterDynamic_speIPC: string = '';
{$If defined(DEBUG)}
aSetOf: TGroupOfForWhatReasonsToLogActually;
{$EndIf}
begin
  Inc(FMessageCount);
  with TSimpleIPCServer(Sender).MsgData do begin
    Seek(0,soFromBeginning);
    ReadBuffer(FCurrentMsg.iMethUsed,SizeOf(Integer));
    ReadBuffer(FCurrentMsg.dtMsgTime,SizeOf(TDateTime));
    ReadBuffer(iSizeOfText,SizeOf(Integer)); // read FCurrentMsg.sMsgText[0]
    if iSizeOfText>0 then begin																						{ if there's a text to read? }
    	SetLength(FCurrentMsg.sMsgText,iSizeOfText);
    	ReadBuffer(FCurrentMsg.sMsgText[1],iSizeOfText);
    end
    else
    	FCurrentMsg.sMsgText:= '';
    ReadBuffer(iSizeOfSetFilterDynamic_speIPC,SizeOf(Integer));
    if iSizeOfSetFilterDynamic_speIPC>0 then	begin													{ if there's a text to read? }
      SetLength(sSetFilterDynamic_speIPC,iSizeOfSetFilterDynamic_speIPC);
      ReadBuffer(sSetFilterDynamic_speIPC[1],iSizeOfSetFilterDynamic_speIPC);
    end
    else
    	sSetFilterDynamic_speIPC:= '';
    FCurrentMsg.setFilterDynamic:= TGroupOfForWhatReasonsToLogActually(MultiLog.TLogChannelUtils.StringToSetof(sSetFilterDynamic_speIPC));
{$If defined(DEBUG)}
aSetOf:= [lwDebug, lwInfo];
LazLoggerBase.DebugLn('1-for remind:-@int([lwDebug, lwInfo])='+IntToStr(MultiLog.TLogChannelUtils.SetofToInt(aSetOf)));
aSetOf:= TGroupOfForWhatReasonsToLogActually(MultiLog.TLogChannelUtils.StringToSetof(sSetFilterDynamic_speIPC));
LazLoggerBase.DebugLn('2-comparaison:-@int(aSetOf)='+IntToStr(MultiLog.TLogChannelUtils.SetofToInt(aSetOf))+MultiLog.TLogChannelUtils.SetofToString(aSetOf));
{$EndIf;}
    ReadBuffer(iSizeOfData,SizeOf(Integer));
    if iSizeOfData > 0 then begin
      FCurrentMsg.pData:=TMemoryStream.Create;
      FCurrentMsg.pData.CopyFrom(TSimpleIPCServer(Sender).MsgData,iSizeOfData);
    end
    else
      FCurrentMsg.pData:=nil;
      
    case FCurrentMsg.iMethUsed of
      methEnterMethod: begin
        FLastNode:=vtreeMessages.AddChild(FLastParent,nil);
        if FExpandParent then
          vtreeMessages.Expanded[FLastParent]:=True
        else
          FExpandParent:=True;
        FLastParent:=FLastNode;
        vtreeMessages.ValidateNode(FLastNode,False);
      end;
      methExitMethod: begin
        if (FLastParent = nil) or (FLastParent^.Parent = vtreeMessages.RootNode) then
        begin
          FLastNode:=vtreeMessages.AddChild(nil,nil);
          FLastParent:=nil;
        end
        else
        begin
          FLastNode:=vtreeMessages.AddChild(FLastParent^.Parent,nil);
          FLastParent:=FLastNode^.Parent;
        end;
        vtreeMessages.ValidateNode(FLastNode,False);
      end;
      methWatch, methCounter: begin
        FWatches.Add(FCurrentMsg.sMsgText, FMessageCount, FCurrentMsg.iMethUsed = methCounter);
        UpdateWatches;
      end;
      methClear: begin
        ClearMessages(nil);
        FLastNode:=nil;
        FLastParent:=nil;
      end
      else begin   //++
        FLastNode:=vtreeMessages.AddChild(FLastParent,nil);
        vtreeMessages.ValidateNode(FLastNode,False);
        if FExpandParent then begin
          vtreeMessages.Expanded[FLastParent]:=True;
          FExpandParent:=False;
        end;
      end;				//++
    end;
  end;
end;

procedure TfrmMain.UpdateCallStack(var ANode: PVirtualNode);
var
  i:Integer;
begin
  //Writeln('UpdateCallstack');
  with vtreeMessages, gridCallStack do
  begin
    i:=GetNodeLevel(ANode);
    RowCount:=Succ(i);
    while i > 0 do
    begin
      Cells[0,i]:=PNodeData(GetNodeData(ANode^.Parent))^.Title;
      ANode:=ANode^.Parent;
      Dec(i);
    end;
  end;
end;

procedure TfrmMain.UpdateWatches;
var
  TempIndex: LongWord;
begin
  case nbWatches.PageIndex of
    0{Last},1{Selected}:
    begin
      if nbWatches.PageIndex = 0 then begin
        FActiveWatch:= gridLastestWatch;
        TempIndex:= FMessageCount;
      end
      else
      begin
        FActiveWatch:= gridSelectedWatch;
        if vtreeMessages.FocusedNode <> nil then
          TempIndex:= PNodeData(vtreeMessages.GetNodeData(vtreeMessages.FocusedNode))^.Index
        else
          TempIndex:= 0;
      end;
      FActiveWatch.RowCount:= 1;
      FWatches.Update(TempIndex);
    end;
    2{History}:
    begin
      UpdateWatchHistory;
    end;
  end;
end;

procedure TfrmMain.UpdateWatchHistory;
var
  i: Integer;
begin
  with ComboWatchHistory do begin
    if ItemIndex = -1 then
      Exit;
    with FWatches[PtrInt(Items.Objects[ItemIndex])] do begin
      GridWatchHistory.RowCount := Count + 1;
      for i := 1 to Count do
        GridWatchHistory.Cells[0,i] := Values[i-1];
    end;
  end;
end;

procedure TfrmMain.ShowBitmapInfo(ABitmap: TBitmap);
begin
  with StringGridBitmap, ABitmap do
  begin
    Cells[1,0] := IntToStr(Height);
    Cells[1,1] := IntToStr(Width);
    Cells[1,2] := PixelFormatNames[PixelFormat];
    Cells[1,3] := HandleTypeNames[HandleType];
    Cells[1,4] := '$'+IntToHex(TransparentColor,8);
  end;
end;

procedure TfrmMain.ApplicationIdle(Sender: TObject; var Done: Boolean);
begin
  Cursor:= crDefault; ReleaseCapture;	// =~ mouse's finally
end;

{$ifdef unix}
procedure TfrmMain.GetMessages(Sender: TObject);
begin
  while FIPCServer.PeekMessage(1,True) do sleep(0);
end;
{$endif}

initialization

end.

