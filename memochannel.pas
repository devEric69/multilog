unit MemoChannel;

{Unit containing TMemoChannel contributed by Avra (Жељко Аврамовић). TMemoChannel was based on TFileChannel.
TMemoChannel can be thread safe!)
}

{
  Copyright (C) 2006 Luiz Américo Pereira Câmara

  This library is free software; you can redistribute it and/or modify it
  under the terms of the FPC modified LGPL licence which can be found at:
  http://wiki.lazarus.freepascal.org/FPC_modified_LGPL.
}

{$ifdef fpc}
{$mode objfpc}{$H+}
{$endif}

interface

uses
  {$ifndef fpc}fpccompat,{$endif} Classes, SysUtils, StdCtrls, Forms, Math, MultiLog, strutils;

const
  MEMO_MINIMAL_NUMBER_OF_TOTAL_LINES = 200;
  MEMO_MINIMAL_NUMBER_OF_LINES_TO_DELETE_AT_ONCE = 100; // must be lower or equal to MEMO_MINIMAL_NUMBER_OF_TOTAL_LINES

type
  TLogMsgData = record
    Text: string;
  end;
  PLogMsgData = ^TLogMsgData;

  TMemoChannelOption = (fcoShowHeader, fcoShowPrefixMethod, fcoShowFilterDynamic_forWhichPurposesLogIsEffective, fcoShowTime, fcoUnlimitedBuffer, fcoRotaryBuffer);
  TMemoChannelOptions = set of TMemoChannelOption;


  { TMemoChannel }

  TMemoChannel = class(TLogChannel)
  private
    FrecMsg: TrecLogMessage;
    FoMemo: TMemo;
    FiRelativeIdent: Integer;
    FiBaseIdent: Integer;
    FbShowHeader: Boolean;
    FbUnlimitedBuffer: Boolean;
    FbRotaryBuffer: Boolean;
    FsTimeFormat: String;
    FiLogLinesLimit: Integer;
    FoWrapper: TLogChannelWrapper;
    procedure SetLogLinesLimit(AValue: Integer);
    procedure UpdateIdentation;
		{ Put log msg into queue that will be processed from the main thread - ie the application's main UI thread - after all other messages,
			ie the function returns immediately (asynchronous call), do not wait for the result, removing
			the possibility of a inter-messages deadlock in a window's queue. }
    procedure Write(const AMsg: string);
    procedure WriteStringsOfMsgDataStream();
    procedure WriteComponentOfMsgDataStream();
  public
    constructor Create(AMemo: TMemo; AChannelOptions: TMemoChannelOptions = [fcoShowHeader, fcoShowTime] );
    destructor Destroy; override;
    procedure SetShowTime(const AValue: Boolean); override;
		{ Called from main thread, after all other messages have been processed to allow thread safe TMemo access. }
    procedure WriteAsyncQueue(Data: PtrInt);
    procedure Deliver(const AMsg: TrecLogMessage); override;
    procedure Init; override;
    procedure Clear; override;
    property ShowHeader: boolean read FbShowHeader write FbShowHeader;
    property LogLinesLimit: integer read FiLogLinesLimit write SetLogLinesLimit;
    property TimeFormat: String read FsTimeFormat write FsTimeFormat;
  end;

implementation

uses

	LazLoggerBase;

{ TMemoChannel }

constructor TMemoChannel.Create(AMemo: TMemo; AChannelOptions: TMemoChannelOptions);
begin
  FoMemo:= AMemo;
  FoWrapper:= TLogChannelWrapper.Create(nil);
  FoWrapper.Channel:= Self;
  AMemo.FreeNotification(FoWrapper);
  FbShowPrefixMethod:= fcoShowPrefixMethod in AChannelOptions;	//can be disabled with ShowPrefixMethod property
  FbShowTime:= fcoShowTime in AChannelOptions;
  FbShow_DynamicFilter_forWhatReasonsToLogActually:= fcoShowFilterDynamic_forWhichPurposesLogIsEffective in AChannelOptions;
  FbShowHeader:= fcoShowHeader in AChannelOptions;
  FbRotaryBuffer:= fcoRotaryBuffer in AChannelOptions;
  FbUnlimitedBuffer:= fcoUnlimitedBuffer in AChannelOptions;
  Active:= True;
  FiLogLinesLimit:= MEMO_MINIMAL_NUMBER_OF_TOTAL_LINES;
  FsTimeFormat:= 'hh:nn:ss:zzz';
end;

destructor TMemoChannel.Destroy;
begin
  FoWrapper.Destroy;
  inherited Destroy;
end;

procedure TMemoChannel.Write(const AMsg: string);
var
  LogMsgToSend: PLogMsgData;
begin
  New(LogMsgToSend);
  LogMsgToSend^.Text := AMsg;
  Application.QueueAsyncCall(@WriteAsyncQueue, PtrInt(LogMsgToSend)); // put log msg into queue that will be processed from the main thread after all other messages
end;



procedure TMemoChannel.WriteAsyncQueue(Data: PtrInt);
var // called from main thread after all other messages have been processed to allow thread safe TMemo access
  ReceivedLogMsg: TLogMsgData;
  LineCount: Integer;
begin
  ReceivedLogMsg := PLogMsgData(Data)^;
  try
    if (FoMemo <> nil) and (not Application.Terminated) then begin
      if FoMemo.Lines.Count > LogLinesLimit then begin
        // FILO rotating bufferised memo
        if FbRotaryBuffer then begin
          FoMemo.Lines.BeginUpdate;
          try // less flickering compared to deleting first line for each newly added line
            for LineCount := 1 to Max(LoglinesLimit div 10, MEMO_MINIMAL_NUMBER_OF_LINES_TO_DELETE_AT_ONCE) do
              FoMemo.Lines.Delete(0);
          finally
            FoMemo.Lines.EndUpdate;
          end;
        end
        else
          if not FbUnlimitedBuffer then
            Clear; // clear whole bufferised memo when limit is reached
      end;
      FoMemo.Append(ReceivedLogMsg.Text);
    end;
  finally
    Dispose(PLogMsgData(Data));
  end;
end;

procedure TMemoChannel.UpdateIdentation;
var
  S: string;
begin
  S := '';
  if FbShowTime then
    S := FormatDateTime(FsTimeFormat, Time);
  FiBaseIdent := Length(S) + 3;
end;

procedure TMemoChannel.SetShowTime(const AValue: Boolean);
begin
  inherited SetShowTime(AValue);
  UpdateIdentation;
end;

procedure TMemoChannel.SetLogLinesLimit(AValue: Integer);
begin
  FiLogLinesLimit := Max(Abs(AValue), MEMO_MINIMAL_NUMBER_OF_TOTAL_LINES);
end;

procedure TMemoChannel.WriteStringsOfMsgDataStream();
var
  i: integer;
  {$If defined(DEBUG)}sDebug: string;{$EndIf}
begin
  if FrecMsg.pData.Size = 0 then Exit;	// pre-condition
  with TStringList.Create do begin
    try
      FrecMsg.pData.Position:=0;
      LoadFromStream(FrecMsg.pData);
      for i:= 0 to Count - 1 do begin
{$If defined(DEBUG)}
sDebug:= Strings[i];
{$EndIf}
       if (Trim(Strings[i])<>'') then
       		Write(Space(FiRelativeIdent+FiBaseIdent) + Strings[i]);
      end;
    finally
      Destroy;
    end;
  end;
end;

procedure TMemoChannel.WriteComponentOfMsgDataStream();
var
  TextStream: TStringStream;
begin
  TextStream := TStringStream.Create('');
  FrecMsg.pData.Seek(0, soFromBeginning);
  ObjectBinaryToText(FrecMsg.pData, TextStream);
  write(TextStream.DataString);	  //todo: better handling of format

  TextStream.Destroy;
end;

procedure TMemoChannel.Deliver(const AMsg: TrecLogMessage);
var
  sWholeMsg: string;
begin
  FrecMsg:= AMsg;
  sWholeMsg:= '';
  //Exit method identation must be set before
  if (FrecMsg.iMethUsed = methExitMethod) and (FiRelativeIdent >= 2) then
    Dec(FiRelativeIdent, 2);

  try
    if FbShowTime then
      sWholeMsg:= FormatDateTime(FsTimeFormat, FrecMsg.dtMsgTime) + '   ';
    sWholeMsg:= sWholeMsg + Space(FiRelativeIdent);
    //FbShowPrefixMethod can serve as qualifier for each current msg, allowing further thematic extractions
    if FbShowPrefixMethod then
      sWholeMsg:= sWholeMsg + (ctsLogPrefixesMethod[FrecMsg.iMethUsed] + ':   ');
    //write second qualifier explaining for which tracking purposes Msg are Logged
    if FbShow_DynamicFilter_forWhatReasonsToLogActually then
  		sWholeMsg:= sWholeMsg + TLogChannelUtils.SetofToString(AMsg.setFilterDynamic);
    sWholeMsg:= sWholeMsg + FrecMsg.sMsgText;
    write(sWholeMsg);
    //if there's a TStream to write...
    if FrecMsg.pData <> nil then begin
      case FrecMsg.iMethUsed of
        methTStrings, methCallStack, methHeapInfo, methException, methMemory: WriteStringsOfMsgDataStream();
        methObject: WriteComponentOfMsgDataStream();
      end;
    end;
  finally
    //Update enter method identation
    if (FrecMsg.iMethUsed = methEnterMethod) then
      Inc(FiRelativeIdent, 2);
  end;
end;

procedure TMemoChannel.Init;
var
  sBufferLimitedStr: string;
begin
  if FbRotaryBuffer or not FbUnlimitedBuffer then
    sBufferLimitedStr:= ' (buffer limited to ' + IntToStr(LogLinesLimit) + ' lines - simplier CPU use on the client side with IPC )'
  else
    sBufferLimitedStr:= '';

  if FbShowHeader then
    Write('=== Log Session Started at ' + DateTimeToStr(Now) + ' by ' + ApplicationName + sBufferLimitedStr + ' ===');
  UpdateIdentation;
end;

procedure TMemoChannel.Clear;
begin
  FoMemo.Lines.Clear;
  if FbRotaryBuffer or not FbUnlimitedBuffer then
  	Write(' (cleared buffer always limited to ' + IntToStr(LogLinesLimit) + ' lines - simplier CPU use on the client side with IPC )');
end;


end.
