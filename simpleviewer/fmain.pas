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
    BtnShowReasonsForLogging: TToolButton;
    StatusBarMain: TStatusBar;
    ToolBar1: TToolBar;
    tbutExpand: TToolButton;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    tbutCollapse: TToolButton;
    tbutClear: TToolButton;
    tbutStayOnTop: TToolButton;
    ShowTimeButton: TToolButton;
    ShowMethodButton: TToolButton;
    procedure ApplicationIdle(Sender: TObject; var Done: Boolean);
    procedure BtnShowReasonsForLoggingClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure popmnuClearClick(Sender: TObject);
    procedure ReceiveMessage(Sender: TObject);
    procedure ShowMethodButtonClick(Sender: TObject);
    procedure tbutClearClick(Sender: TObject);
    procedure tbutCollapseClick(Sender: TObject);
    procedure tbutExpandClick(Sender: TObject);
    procedure tbutStayOnTopClick(Sender: TObject);
    procedure ShowTimeButtonClick(Sender: TObject);
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
  AMsg: TrecLogMessage;
  SizeOfText, SizeOfData, SizeOfSetFilterDynamic_speIPC: Integer;
  sSetFilterDynamic_speIPC: string;
{$If defined(DEBUG)}
aSetOf: TGroupOfForWhatReasonsToLogActually;
{$EndIf}
begin
  with TSimpleIPCServer(Sender).MsgData do begin
    Seek(0,soFromBeginning);
    ReadBuffer(AMsg.iMethUsed,SizeOf(Integer));
    ReadBuffer(AMsg.dtMsgTime,SizeOf(TDateTime));
    ReadBuffer(SizeOfText,SizeOf(Integer));
    if SizeOfText>0 then begin																						{ if there's a text to read? }
    	SetLength(AMsg.sMsgText,SizeOfText);
    	ReadBuffer(AMsg.sMsgText[1],SizeOfText);
    end
    else
    	AMsg.sMsgText:= '';
    ReadBuffer(SizeOfSetFilterDynamic_speIPC,SizeOf(Integer));
    if SizeOfSetFilterDynamic_speIPC>0 then	begin													{ if there's a text to read? }
    	SetLength(sSetFilterDynamic_speIPC,SizeOfSetFilterDynamic_speIPC);
    	ReadBuffer(sSetFilterDynamic_speIPC[1],SizeOfSetFilterDynamic_speIPC);
		end
    else
			sSetFilterDynamic_speIPC:= '';
    AMsg.setFilterDynamic:= TGroupOfForWhatReasonsToLogActually(MultiLog.TLogChannelUtils.StringToSetof(sSetFilterDynamic_speIPC));
{$If defined(DEBUG)}
aSetOf:= [lwDebug, lwInfo];
DebugLn('1-for remind:-@int([lwDebug, lwInfo])='+IntToStr(MultiLog.TLogChannelUtils.SetofToInt(aSetOf)));
aSetOf:= TGroupOfForWhatReasonsToLogActually(MultiLog.TLogChannelUtils.StringToSetof(sSetFilterDynamic_speIPC));
DebugLn('2-comparaison:-@int(aSetOf)='+IntToStr(MultiLog.TLogChannelUtils.SetofToInt(aSetOf))+MultiLog.TLogChannelUtils.SetofToString(aSetOf));
{$EndIf}
    ReadBuffer(SizeOfData,SizeOf(Integer));
    if SizeOfData > 0 then begin
      AMsg.pData:= TMemoryStream.Create;
      //WriteLn('[LogViewer] DataSize: ', SizeOfData);
      //WriteLn('DataCopied: ', AMsg.Data.CopyFrom(TSimpleIPCServer(Sender).MsgData,SizeOfData));
      AMsg.pData.CopyFrom(TSimpleIPCServer(Sender).MsgData,SizeOfData);
    end
    else
      AMsg.pData:=nil;
    FoLogTreeView1.AddMessage(AMsg);
    if SizeOfData > 0 then
      AMsg.pData.Free;
  end;
  Inc(FiMessageCount);
  UpdateStatusBar;
end;


procedure TfrmMain.tbutClearClick(Sender: TObject);
begin
  FoLogTreeView1.Clear;
  FiMessageCount:=0;
  UpdateStatusBar;
end;

procedure TfrmMain.tbutCollapseClick(Sender: TObject);
begin
  FoLogTreeView1.FullCollapse;
end;

procedure TfrmMain.tbutExpandClick(Sender: TObject);
begin
  FoLogTreeView1.FullExpand
end;

procedure TfrmMain.tbutStayOnTopClick(Sender: TObject);
begin
  if tbutStayOnTop.Down then
    FormStyle:= fsSystemStayOnTop
  else
    FormStyle:= fsNormal;
end;

procedure TfrmMain.ShowTimeButtonClick(Sender: TObject);
begin
  FoLogTreeView1.ShowTime:= not FoLogTreeView1.ShowTime;
end;

procedure TfrmMain.ShowMethodButtonClick(Sender: TObject);
begin
	FoLogTreeView1.ShowPrefixMethod:= not FoLogTreeView1.ShowPrefixMethod;
end;

procedure TfrmMain.BtnShowReasonsForLoggingClick(Sender: TObject);
begin
	FoLogTreeView1.ShowDynamicFilter_forWhatReasonsToLogActually:= not FoLogTreeView1.ShowDynamicFilter_forWhatReasonsToLogActually;
end;

procedure TfrmMain.UpdateStatusBar;
begin
  StatusBarMain.SimpleText:= IntToStr(FiMessageCount)+' messages';
end;

end.

