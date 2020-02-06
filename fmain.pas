unit fmain;

{ Copyright (C) 2006 Luiz Américo Pereira Câmara

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
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

uses
  Classes, MultiLog, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,simpleipc,
  ComCtrls, Buttons, LCLIntf, LCLType, LCLProc, Menus, logtreeview;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    imgToolIcons: TImageList;
    popmnuClear: TMenuItem;
    PopupMenu1: TPopupMenu;
    btnShowReasonsForLogging: TToolButton;
    StatusBarMain: TStatusBar;
    ToolBar1: TToolBar;
    btnExpand: TToolButton;
    btn1: TToolButton;
    ToolButton2: TToolButton;
    btnCollapse: TToolButton;
    btnClear: TToolButton;
    btnStayOnTop: TToolButton;
    btnShowTimeButton: TToolButton;
    btnShowMethodButton: TToolButton;
    procedure ApplicationIdle(Sender: TObject; var Done: Boolean);
    procedure btnShowReasonsForLoggingClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure popmnuClearClick(Sender: TObject);
    procedure ReceiveMessage(Sender: TObject);
    procedure btnShowMethodButtonClick(Sender: TObject);
    procedure btnClearClick(Sender: TObject);
    procedure btnCollapseClick(Sender: TObject);
    procedure btnExpandClick(Sender: TObject);
    procedure btnStayOnTopClick(Sender: TObject);
    procedure btnShowTimeButtonClick(Sender: TObject);
  private
    FoLogTreeView1: TLogTreeView;
    FoIPCServer: TSimpleIPCServer;
    FiMessageCount: Integer;
    procedure UpdateStatusBar;
    { private declarations }
  public
    { public declarations }
  end; 

var
  frmMain: TfrmMain;

resourcestring
  csHint_On = ' (On)';
  csHint_Off = ' (Off)';
  csHint_AlwayOnTop = 'Always on top';
  csHint_ShowTime = 'Show Time';
  csHint_DidacticMethod = 'Didactic display of the coded Lazarus\Pascal method';
	csHint_ReasonForLogging = 'Display the reasons for logging';

implementation

{$R *.lfm}

{ TfrmMain }

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  {$ifdef unix}
  Application.OnIdle:= @ApplicationIdle;
  {$endif}
  FoLogTreeView1:= TLogTreeView.Create(Self);
  with FoLogTreeView1 do begin
    Parent:= Self;
    Left:= 0;
    Height:= 386;
    Top:= 36;
    Width:= 532;
    Align:= alClient;
    DefaultItemHeight:= 18;
    // -- ScrollBars:=
    ShowTime:= False;
    TabOrder:= 1;
    TimeFormat:= 'hh:nn:ss:zzz';
    PopupMenu:= PopupMenu1;
  end;
  FoIPCServer:= TSimpleIPCServer.Create(nil);
  with FoIPCServer do
  begin
    ServerID:='ipc_log_server';
    Global:=True;
    OnMessage:= @ReceiveMessage;
    StartServer;
  end;
  UpdateStatusBar;
  //set up bitBtns:
  btnStayOnTopClick(Sender); btnShowTimeButtonClick(Sender); btnShowMethodButtonClick(Sender); btnShowReasonsForLoggingClick(Sender);
end;

procedure TfrmMain.popmnuClearClick(Sender: TObject);
begin
  FoLogTreeView1.Clear;
  StatusBarMain.SimpleText:= '0 message';
end;

procedure TfrmMain.ApplicationIdle(Sender: TObject; var Done: Boolean);
begin
  FoIPCServer.PeekMessage(1,True);
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  FoIPCServer.Free;
  FoLogTreeView1.Free;
end;

procedure TfrmMain.ReceiveMessage(Sender: TObject);
var
  recAMsg: TrecLogMessage;
  iSizeOfText: Integer = 0;
  iSizeOfData: Integer = 0;
  iSizeOfSetFilterDynamic_speIPC: Integer = 0;
  sSetFilterDynamic_speIPC: string;
{$If defined(DEBUG)}
aSetOf: TGroupOfForWhatReasonsToLogActually;
{$EndIf}
begin
  with TSimpleIPCServer(Sender).MsgData do begin
    Seek(0,soFromBeginning);
    ReadBuffer(recAMsg.iMethUsed,SizeOf(Integer));
    ReadBuffer(recAMsg.dtMsgTime,SizeOf(TDateTime));
    ReadBuffer(iSizeOfText,SizeOf(Integer));
    if iSizeOfText>0 then begin																						{ if there's a text to read? }
    	SetLength(recAMsg.sMsgText,iSizeOfText);
    	ReadBuffer(recAMsg.sMsgText[1],iSizeOfText);
    end
    else
    	recAMsg.sMsgText:= '';
    ReadBuffer(iSizeOfSetFilterDynamic_speIPC,SizeOf(Integer));
    if iSizeOfSetFilterDynamic_speIPC>0 then	begin													{ if there's a text to read? }
    	SetLength(sSetFilterDynamic_speIPC,iSizeOfSetFilterDynamic_speIPC);
    	ReadBuffer(sSetFilterDynamic_speIPC[1],iSizeOfSetFilterDynamic_speIPC);
		end
    else
			sSetFilterDynamic_speIPC:= '';
    recAMsg.setFilterDynamic:= TGroupOfForWhatReasonsToLogActually(MultiLog.TLogChannelUtils.StringToSetof(sSetFilterDynamic_speIPC));
{$If defined(DEBUG)}
aSetOf:= [lwDebug, lwInfo];
DebugLn('1-for remind:-@int([lwDebug, lwInfo])='+IntToStr(MultiLog.TLogChannelUtils.SetofToInt(aSetOf)));
aSetOf:= TGroupOfForWhatReasonsToLogActually(MultiLog.TLogChannelUtils.StringToSetof(sSetFilterDynamic_speIPC));
DebugLn('2-comparaison:-@int(aSetOf)='+IntToStr(MultiLog.TLogChannelUtils.SetofToInt(aSetOf))+MultiLog.TLogChannelUtils.SetofToString(aSetOf));
{$EndIf}
    ReadBuffer(iSizeOfData,SizeOf(Integer));
    if iSizeOfData > 0 then begin
      recAMsg.pData:= TMemoryStream.Create;
      //WriteLn('[LogViewer] DataSize: ', iSizeOfData);
      //WriteLn('DataCopied: ', recAMsg.Data.CopyFrom(TSimpleIPCServer(Sender).MsgData,iSizeOfData));
      recAMsg.pData.CopyFrom(TSimpleIPCServer(Sender).MsgData,iSizeOfData);
    end
    else
      recAMsg.pData:=nil;
    FoLogTreeView1.AddMessage(recAMsg);
    if iSizeOfData > 0 then
      recAMsg.pData.Free;
  end;
  Inc(FiMessageCount);
  UpdateStatusBar;
end;


procedure TfrmMain.btnClearClick(Sender: TObject);
begin
  FoLogTreeView1.Clear;
  FiMessageCount:=0;
  UpdateStatusBar;
end;

procedure TfrmMain.btnCollapseClick(Sender: TObject);
begin
  FoLogTreeView1.FullCollapse;
end;

procedure TfrmMain.btnExpandClick(Sender: TObject);
begin
  FoLogTreeView1.FullExpand
end;

procedure TfrmMain.btnStayOnTopClick(Sender: TObject);
begin
  if btnStayOnTop.Down then begin
    FormStyle:= fsSystemStayOnTop;
    btnStayOnTop.Hint:= csHint_AlwayOnTop + csHint_On;
  end
  else begin
    FormStyle:= fsNormal;
    btnStayOnTop.Hint:= csHint_AlwayOnTop + csHint_Off;
  end;
end;

procedure TfrmMain.btnShowTimeButtonClick(Sender: TObject);
begin
  if btnShowTimeButton.Down then begin
  	FoLogTreeView1.ShowTime:= true;
    btnShowTimeButton.Hint:= csHint_ShowTime + csHint_On;
  end
  else begin
  	FoLogTreeView1.ShowTime:= false;
    btnShowTimeButton.Hint:= csHint_ShowTime + csHint_Off;
  end;
end;

procedure TfrmMain.btnShowMethodButtonClick(Sender: TObject);
begin
  if btnShowMethodButton.Down then begin
  	FoLogTreeView1.ShowPrefixMethod:= true;
    btnShowMethodButton.Hint:= csHint_DidacticMethod + csHint_On;
  end
  else begin
    FoLogTreeView1.ShowPrefixMethod:= false;
    btnShowMethodButton.Hint:= csHint_DidacticMethod + csHint_Off;
  end;
end;

procedure TfrmMain.btnShowReasonsForLoggingClick(Sender: TObject);
begin
  if btnShowReasonsForLogging.Down then begin
    FoLogTreeView1.ShowDynamicFilter_forWhatReasonsToLogActually:= true;
    btnShowReasonsForLogging.Hint:= csHint_ReasonForLogging + csHint_On;
  end
  else begin
    FoLogTreeView1.ShowDynamicFilter_forWhatReasonsToLogActually:= false;
    btnShowReasonsForLogging.Hint:= csHint_ReasonForLogging + csHint_Off;
  end;
end;

procedure TfrmMain.UpdateStatusBar;
begin
  StatusBarMain.SimpleText:= IntToStr(FiMessageCount)+' messages';
end;

end.

