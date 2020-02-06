unit IPCChannel;

{
Copyright (C) 2006 Luiz Américo Pereira Câmara
pascalive@bol.com.br

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
}

{$ifdef fpc}
{$mode objfpc}{$H+}
{$endif}

interface

uses
  Classes, SysUtils, {$ifdef fpc}simpleipc{$else}winipc{$endif}, multilog;

const
  IPC_DEFAULT_SERVER_NAME = 'ipc_log_server';

type

  { TIPCChannel }

  TIPCChannel = class (TLogChannel)
  private
    {$ifdef fpc}
    FoClient: TSimpleIPCClient;
    {$else}
    FClient: TWinIPCClient;
    {$endif}
    FoBuffer: TMemoryStream;
    FrecClearMessage: TrecLogMessage;
  public
    constructor Create(const ServerName: String = IPC_DEFAULT_SERVER_NAME);
    destructor Destroy; override;
    procedure Clear; override;
    procedure Deliver(const AMsg: TrecLogMessage);override;
  end;


implementation

const
  ZeroBuf: Integer = 0;

{ TIPCChannel }

constructor TIPCChannel.Create(const ServerName: string);
begin
  with FrecClearMessage do
  begin
    iMethUsed:=methClear;
    //sMsgText:='';
    //dtMsgTime:=Now;
    //Data:=nil;
    //Those are already nil
  end;
  FoBuffer:=TMemoryStream.Create;
  {$ifdef fpc}
  FoClient := TSimpleIPCClient.Create(nil);
  {$else}
  FClient := TWinIPCClient.Create(nil);
  {$endif}
  with FoClient do
  begin
    ServerID:=ServerName;
    //todo: Start server only when channel is active
    if ServerRunning then
    begin
      Self.Active := True;
      Active := True;
    end
    else
      Active := False;
  end;
end;

destructor TIPCChannel.Destroy;
begin
  FoClient.Destroy;
  FoBuffer.Destroy;
end;

procedure TIPCChannel.Clear;
begin
  Deliver(FrecClearMessage);
end;

{^^ Explanations:
IPCChannel delivers a specific information: @sSetFilterDynamic_speIPC[1]
}
procedure TIPCChannel.Deliver(const AMsg: TrecLogMessage);
var
  SizeOfText, SizeOfSetFilterDynamic_speIPC, SizeOfData: Integer;
  sSetFilterDynamic_speIPC: string;
begin
  with FoBuffer do
  begin
    sSetFilterDynamic_speIPC:= MultiLog.TLogChannelUtils.SetofToString(AMsg.setFilterDynamic);
    SizeOfSetFilterDynamic_speIPC:= Length(sSetFilterDynamic_speIPC);
    SizeOfText:=Length(AMsg.sMsgText);
    Seek(0,soFromBeginning);
    WriteBuffer(AMsg.iMethUsed,SizeOf(Integer));
    WriteBuffer(AMsg.dtMsgTime,SizeOf(TDateTime));
    WriteBuffer(SizeOfText,SizeOf(Integer));
    if SizeOfText>0 then																	{ if there's a text to write? }
    	WriteBuffer(AMsg.sMsgText[1],SizeOfText);
  	WriteBuffer(SizeOfSetFilterDynamic_speIPC,SizeOf(Integer));
		if SizeOfSetFilterDynamic_speIPC>0 then								{ if there's a text to write? }
	    WriteBuffer(sSetFilterDynamic_speIPC[1],SizeOfSetFilterDynamic_speIPC);
    if AMsg.pData <> nil then begin
      SizeOfData:= AMsg.pData.Size;
      //WriteLn('[IPCChannel] Size Of Stream: ',SizeOfData);
      WriteBuffer(SizeOfData,SizeOf(Integer));
      AMsg.pData.Position := 0;
      CopyFrom(AMsg.pData,SizeOfData);
      //WriteLn('DataCopied: ',CopyFrom(AMsg.o_Data,SizeOfData));
    end
    else
      WriteBuffer(ZeroBuf,SizeOf(Integer));//necessary?
  end;
  FoClient.SendMessage(mtUnknown,FoBuffer);
end;

end.

