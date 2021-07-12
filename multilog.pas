unit MultiLog;

{
Main unit of the Multilog logging system.
author of original MultiLog: Luiz Américo Pereira Câmara; pascalive@bol.com.br

nb: all units are encoded with UTF-8 without BOM, using EDI "file settings\encoding\UTF-8" contextual menu.

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
  {$ifndef fpc}Types, fpccompat,{$endif} Classes, SysUtils, syncobjs, math, TypInfo;

{$Include multilog_user1.inc}

type
  TIntegratedLogger = class;

  TMethodToLog = methInfo..methClear;

{$Include multilog_user2.inc}

  TGroupOfForWhatReasonsToLogActually = Set of TForWhichTrackingPurpose;

{$Include multilog_user3.inc}

  lwNone = [];

type
  TrecLogMessage = record
    iMethUsed: Integer;
    dtMsgTime: TDateTime;
    sMsgText: String;
    pData: TStream;
    setFilterDynamic: TGroupOfForWhatReasonsToLogActually;
  end;

  TCustomDataNotify = function (Sender: TIntegratedLogger; Data: Pointer; var DoSend: Boolean): String of Object;
  TCustomDataNotifyStatic = function (Sender: TIntegratedLogger; Data: Pointer; var DoSend: Boolean): String;


  { TLogChannelUtils }

  TLogChannelUtils = class
    class function SetofToInt(const aSetOf: TGroupOfForWhatReasonsToLogActually): integer;
    class function IntToSetof(const aSetof: Integer): TGroupOfForWhatReasonsToLogActually;
    class function SetofToString(const aSetOf: TGroupOfForWhatReasonsToLogActually): string;
    class function StringToSetof(const aSetOf: string): Integer;
    class function CountElements(const aSetOf: TGroupOfForWhatReasonsToLogActually): integer;
	end;


  { TLogChannel }

  TLogChannel = class
  private
    FbActive: Boolean;
	protected
    FbShowTime: Boolean;
    FbShowPrefixMethod: Boolean;
    FbShow_DynamicFilter_forWhatReasonsToLogActually: Boolean;
  public
    procedure SetShowTime(const AValue: Boolean); virtual;
    procedure Set_ShowFilterDynamic_forWhatReasonsToLogActually(AValue: Boolean); virtual;
    procedure Clear; virtual; abstract;
    procedure Deliver(const AMsg: TrecLogMessage); virtual; abstract;
    procedure Init; virtual;
    property Active: Boolean read FbActive write FbActive;
    property ShowTime: boolean read FbShowTime write SetShowTime;
    property ShowPrefixMethod: Boolean read FbShowPrefixMethod write FbShowPrefixMethod;
    property ShowDynamicFilter_forWhatReasonsToLogActually: Boolean read FbShow_DynamicFilter_forWhatReasonsToLogActually write Set_ShowFilterDynamic_forWhatReasonsToLogActually;
  end;
  
  { TChannelList }

  TChannelList = class
  private
    FoList: TFpList;
    function GetCount: Integer; {$ifdef fpc}inline;{$endif}
    function GetItems(AIndex: Integer): TLogChannel; {$ifdef fpc}inline;{$endif}
  public
    constructor Create;
    destructor Destroy; override;
    function Add(AChannel: TLogChannel):Integer;
    procedure Remove(AChannel:TLogChannel);
    property Count: Integer read GetCount;
    property Items[AIndex:Integer]: TLogChannel read GetItems; default;
  end;

  { TIntegratedLogger }



  TIntegratedLogger = class
  private
    FiMaxStackCount: Integer;
    FoChannels: TChannelList;
    FoLogStack: TStrings;
    FoCheckList: TStringList;
    FoCounterList: TStringList;
    FOnCustomData: TCustomDataNotify;
    FsetFilterDynamic_forWhatReasonsToLogActually: TGroupOfForWhatReasonsToLogActually;
    class var FoDefaultChannels: TChannelList;
    procedure GetCallStack(AStream:TStream);
    class function GetDefaultChannels: TChannelList; static;
    function GetEnabled: Boolean;
    procedure SetMaxStackCount(const AValue: Integer);
    procedure SetThreadSafe;
  protected
    procedure SendStream(AMethodUsed: Integer; const AText: String; o_AStream: TStream);
    procedure SendBuffer(AMethodUsed: Integer; const AText: String; var pBuffer; Count: LongWord);
		procedure SetFilterDynamic_forWhatReasonsToLogActually(AValue: TGroupOfForWhatReasonsToLogActually);
  public
    constructor Create;
    destructor Destroy; override;
    function CalledBy(const AMethodName: String): Boolean;
    procedure Clear;
    //Helper functions
    function RectToStr(const ARect: TRect): String; //inline
    function PointToStr(const APoint: TPoint): String; //inline
    //Send functions: for backward compatibility. SendInfo have been introduced in order to be harmonized with SendError and SendWarning
    procedure Send(const AText: String); overload;
    procedure Send(aSetOfFilterDynamicOverloadOneShot: TGroupOfForWhatReasonsToLogActually; const AText: String); overload;
    procedure Send(const AText: String; Args: array of const); overload;
    procedure Send(aSetOfFilterDynamicOverloadOneShot: TGroupOfForWhatReasonsToLogActually; const AText: String; Args: array of const); overload;
    procedure Send(const AText, AValue: String); overload;
    procedure Send(aSetOfFilterDynamicOverloadOneShot: TGroupOfForWhatReasonsToLogActually; const AText,AValue: String); overload;
    procedure Send(const AText: String; AValue: Integer); overload;
    procedure Send(aSetOfFilterDynamicOverloadOneShot: TGroupOfForWhatReasonsToLogActually; const AText: String; AValue: Integer); overload;
    {$ifdef fpc}
    procedure Send(const AText: String; AValue: Cardinal); overload;
    procedure Send(aSetOfFilterDynamicOverloadOneShot: TGroupOfForWhatReasonsToLogActually; const AText: String; AValue: Cardinal); overload;
    {$endif}
    procedure Send(const AText: String; AValue: Double); overload;
    procedure Send(aSetOfFilterDynamicOverloadOneShot: TGroupOfForWhatReasonsToLogActually; const AText: String; AValue: Double); overload;
    procedure Send(const AText: String; AValue: Int64); overload;
    procedure Send(aSetOfFilterDynamicOverloadOneShot: TGroupOfForWhatReasonsToLogActually; const AText: String; AValue: Int64); overload;
    procedure Send(const AText: String; AValue: QWord); overload;
    procedure Send(aSetOfFilterDynamicOverloadOneShot: TGroupOfForWhatReasonsToLogActually; const AText: String; AValue: QWord); overload;
    procedure Send(const AText: String; AValue: Boolean); overload;
    procedure Send(aSetOfFilterDynamicOverloadOneShot: TGroupOfForWhatReasonsToLogActually; const AText: String; AValue: Boolean); overload;
    procedure Send(const AText: String; const ARect: TRect); overload;
    procedure Send(aSetOfFilterDynamicOverloadOneShot: TGroupOfForWhatReasonsToLogActually; const AText: String; const ARect: TRect); overload;
    procedure Send(const AText: String; const APoint: TPoint); overload;
    procedure Send(aSetOfFilterDynamicOverloadOneShot: TGroupOfForWhatReasonsToLogActually; const AText: String; const APoint: TPoint); overload;
		{ Explanations: whe log with a methTStrings's method.}
    procedure Send(const AText: String; AStrList: TStrings); overload;
    procedure Send(aSetOfFilterDynamicOverloadOneShot: TGroupOfForWhatReasonsToLogActually; const AText: String; o_AStrList: TStrings); overload;
    { Explanations: whe log with a methObject's method.}
    procedure Send(const AText: String; AObject: TObject); overload;
    procedure Send(aSetOfFilterDynamicOverloadOneShot: TGroupOfForWhatReasonsToLogActually; const AText: String; AObject: TObject); overload;
    { Explanations: whe log with a methInfo's method.}
    procedure SendInfo(const AText: String); overload;
    procedure SendInfo(aSetOfFilterDynamicOverloadOneShot: TGroupOfForWhatReasonsToLogActually; const AText: String); overload;
    procedure SendInfo(const AText: String; Args: array of const); overload;
    procedure SendInfo(aSetOfFilterDynamicOverloadOneShot: TGroupOfForWhatReasonsToLogActually; const AText: String; Args: array of const); overload;
    procedure SendInfo(const AText, AValue: String); overload;
    procedure SendInfo(aSetOfFilterDynamicOverloadOneShot: TGroupOfForWhatReasonsToLogActually; const AText,AValue: String); overload;
    procedure SendInfo(const AText: String; AValue: Integer);
    procedure SendInfo(aSetOfFilterDynamicOverloadOneShot: TGroupOfForWhatReasonsToLogActually; const AText: String; AValue: Integer); overload;
    {$ifdef fpc}
    procedure SendInfo(const AText: String; AValue: Cardinal); overload;
    procedure SendInfo(aSetOfFilterDynamicOverloadOneShot: TGroupOfForWhatReasonsToLogActually; const AText: String; AValue: Cardinal); overload;
    {$endif}
    procedure SendInfo(const AText: String; AValue: Double); overload;
    procedure SendInfo(aSetOfFilterDynamicOverloadOneShot: TGroupOfForWhatReasonsToLogActually; const AText: String; AValue: Double); overload;
    procedure SendInfo(const AText: String; AValue: Int64); overload;
    procedure SendInfo(aSetOfFilterDynamicOverloadOneShot: TGroupOfForWhatReasonsToLogActually; const AText: String; AValue: Int64); overload;
    procedure SendInfo(const AText: String; AValue: QWord); overload;
    procedure SendInfo(aSetOfFilterDynamicOverloadOneShot: TGroupOfForWhatReasonsToLogActually; const AText: String; AValue: QWord); overload;
    procedure SendInfo(const AText: String; AValue: Boolean); overload;
    procedure SendInfo(aSetOfFilterDynamicOverloadOneShot: TGroupOfForWhatReasonsToLogActually; const AText: String; AValue: Boolean); overload;
    procedure SendInfo(const AText: String; const ARect: TRect); overload;
    procedure SendInfo(aSetOfFilterDynamicOverloadOneShot: TGroupOfForWhatReasonsToLogActually; const AText: String; const ARect: TRect); overload;
    procedure SendInfo(const AText: String; const APoint: TPoint); overload;
    procedure SendInfo(aSetOfFilterDynamicOverloadOneShot: TGroupOfForWhatReasonsToLogActually; const AText: String; const APoint: TPoint); overload;
		{ Explanations: whe log with a methTStrings's method.}
    procedure SendInfo(const AText: String; AStrList: TStrings); overload;
    procedure SendInfo(aSetOfFilterDynamicOverloadOneShot: TGroupOfForWhatReasonsToLogActually; const AText: String; o_AStrList: TStrings); overload;
    { Explanations: whe log with a methObject's method.}
    procedure SendInfo(const AText: String; AObject: TObject); overload;
    procedure SendInfo(aSetOfFilterDynamicOverloadOneShot: TGroupOfForWhatReasonsToLogActually; const AText: String; AObject: TObject); overload;
    { Explanations: whe log with a methValue's method.}
    procedure SendPointer(const AText: String; APointer: Pointer); overload;
    procedure SendPointer(aSetOfFilterDynamicOverloadOneShot: TGroupOfForWhatReasonsToLogActually; const AText: String; APointer: Pointer); overload;
    { Explanations: whe log with a methCallStack's method.}
    procedure SendCallStack(const AText: String); overload;
    procedure SendCallStack(aSetOfFilterDynamicOverloadOneShot: TGroupOfForWhatReasonsToLogActually; const AText: String); overload;
		{ Explanations: whe log with a methException's method.}
    procedure SendException(const AText: String; AException: Exception); overload;
    procedure SendException(aSetOfFilterDynamicOverloadOneShot: TGroupOfForWhatReasonsToLogActually; const AText: String; AException: Exception); overload;
		{ Explanations: check Exception hierarchy (ultimate ancestor=EDatabaseError || EStreamError || ...), to grab its specific fields into a dumped string.}
    function GetDescriptionOfSQLException (AException: Exception): string;
		{ Explanations: whe log with a methHeapInfo's method.}
    procedure SendHeapInfo(const AText: String); overload;
    procedure SendHeapInfo(aSetOfFilterDynamicOverloadOneShot: TGroupOfForWhatReasonsToLogActually; const AText: String);overload;
		{ Explanations: whe log with a methMemory's method.}
    procedure SendMemory(const AText: String; Address: Pointer; Size: LongWord; Offset: Integer = 0); overload;
    procedure SendMemory(aSetOfFilterDynamicOverloadOneShot: TGroupOfForWhatReasonsToLogActually; const AText: String; pAddress: Pointer; iSize: LongWord; iOffset: Integer = 0); overload;
    { Explanations: whe log with a methConditional's method.}
    procedure SendIf(const AText: String; Expression: Boolean); overload;
    procedure SendIf(Classes: TGroupOfForWhatReasonsToLogActually; const AText: String; Expression: Boolean); overload;
    procedure SendIf(const AText: String; Expression, IsTrue: Boolean); overload;
    procedure SendIf(aSetOfFilterDynamicOverloadOneShot: TGroupOfForWhatReasonsToLogActually; const AText: String; bExpression, bIsTrue: Boolean); overload;
		{ Explanations: whe log with a ltWarning's method.}
    procedure SendWarning(const AText: String); overload;
    procedure SendWarning(aSetOfFilterDynamicOverloadOneShot: TGroupOfForWhatReasonsToLogActually; const AText: String); overload;
    { Explanations: whe log with a ltError's method.}
    procedure SendError(const AText: String); overload;
    procedure SendError(aSetOfFilterDynamicOverloadOneShot: TGroupOfForWhatReasonsToLogActually; const AText: String); overload;
    { Explanations: whe log with a methCustomData's method.}
    procedure SendCustomData(const AText: String; Data: Pointer); overload;
    procedure SendCustomData(const AText: String; Data: Pointer; CustomDataFunction: TCustomDataNotify); overload;
    procedure SendCustomData(aSetOfFilterDynamicOverloadOneShot: TGroupOfForWhatReasonsToLogActually; const AText: String; Data: Pointer; CustomDataFunction: TCustomDataNotify); overload;
    procedure SendCustomData(aSetOfFilterDynamicOverloadOneShot: TGroupOfForWhatReasonsToLogActually; const AText: String; Data: Pointer); overload;
    procedure SendCustomData(aSetOfFilterDynamicOverloadOneShot: TGroupOfForWhatReasonsToLogActually; const AText: String; Data: Pointer; CustomDataFunction: TCustomDataNotifyStatic); overload;
    procedure SendCustomData(const AText: String; Data: Pointer; CustomDataFunction: TCustomDataNotifyStatic); overload;
		{ Explanations: whe log with a methCheckpoint's method.}
    procedure AddCheckPoint; overload;
    procedure AddCheckPoint(aSetOfFilterDynamicOverloadOneShot: TGroupOfForWhatReasonsToLogActually); overload;
    procedure AddCheckPoint(const sCheckName: String); overload;
    procedure AddCheckPoint(aSetOfFilterDynamicOverloadOneShot: TGroupOfForWhatReasonsToLogActually; const sCheckName: String); overload;
    { Explanations: whe log with a methCounter's method.}
    procedure IncCounter(const sCounterName: String); overload;
    procedure IncCounter(aSetOfFilterDynamicOverloadOneShot: TGroupOfForWhatReasonsToLogActually; const CounterName: String); overload;
		{ Explanations: whe log with a methCounter's method.}
    procedure DecCounter(const sCounterName: String); overload;
    procedure DecCounter(aSetOfFilterDynamicOverloadOneShot: TGroupOfForWhatReasonsToLogActually; const CounterName: String); overload;
    { Explanations: whe log with a methCounter's method.}
    procedure ResetCounter(const sCounterName: String); overload;
    procedure ResetCounter(aSetOfFilterDynamicOverloadOneShot: TGroupOfForWhatReasonsToLogActually; const CounterName: String); overload;
    { Explanations: get pointer on FoCounterList.Objects[CounterName].}
    function GetCounter(const CounterName: String): Integer; overload;
    { Explanations: whe log with a methCheckpoint's method.}
    procedure ResetCheckPoint; overload;
    procedure ResetCheckPoint(Classes: TGroupOfForWhatReasonsToLogActually); overload;
    procedure ResetCheckPoint(const sCheckName: String); overload;
    procedure ResetCheckPoint(aSetOfFilterDynamicOverloadOneShot: TGroupOfForWhatReasonsToLogActually;const CheckName: String); overload;
    { Explanations: whe log with a methEnterMethod's method.}
    procedure EnterMethod(const AMethodName: String; const AMessage: String = ''); overload;
    procedure EnterMethod(aSetOfFilterDynamicOverloadOneShot: TGroupOfForWhatReasonsToLogActually; const AMethodName: String; const AMessage: String = ''); overload;
    procedure EnterMethod(Sender: TObject; const AMethodName: String; const AMessage: String = ''); overload;
    procedure EnterMethod(aSetOfFilterDynamicOverloadOneShot: TGroupOfForWhatReasonsToLogActually; Sender: TObject; const AMethodName: String; const AMessage: String = ''); overload;
    { Explanations: whe log with a methExitMethod's method.}
    procedure ExitMethod(const AMethodName: String; const AMessage: String = ''); overload;
    procedure ExitMethod(Sender: TObject; const AMethodName: String; const AMessage: String = ''); overload;
    procedure ExitMethod(aSetOfFilterDynamicOverloadOneShot: TGroupOfForWhatReasonsToLogActually; const AMethodName: String; const AMessage: String = ''); overload;
    procedure ExitMethod({%H-}aSetOfFilterDynamicOverloadOneShot: TGroupOfForWhatReasonsToLogActually; Sender: TObject; const AMethodName: String; const AMessage: String = ''); overload;
    { Explanations: whe log with a methWatch's method.}
    procedure Watch(const AText, AValue: String); overload;
    procedure Watch(aSetOfFilterDynamicOverloadOneShot: TGroupOfForWhatReasonsToLogActually; const AText,AValue: String); overload;
    procedure Watch(const AText: String; AValue: Integer); overload;
    procedure Watch(aSetOfFilterDynamicOverloadOneShot: TGroupOfForWhatReasonsToLogActually; const AText: String; AValue: Integer); overload;
    {$ifdef fpc}
    procedure Watch(const AText: String; AValue: Cardinal); overload;
    procedure Watch(aSetOfFilterDynamicOverloadOneShot: TGroupOfForWhatReasonsToLogActually; const AText: String; AValue: Cardinal); overload;
    {$endif}
    procedure Watch(const AText: String; AValue: Double); overload;
    procedure Watch(aSetOfFilterDynamicOverloadOneShot: TGroupOfForWhatReasonsToLogActually; const AText: String; AValue: Double); overload;
    procedure Watch(const AText: String; AValue: Boolean); overload;
		procedure Watch(aSetOfFilterDynamicOverloadOneShot: TGroupOfForWhatReasonsToLogActually; const AText: String; AValue: Boolean); overload;
    { Explanations: whe log with a methSubEventBetweenEnterAndExitMethods's method.}
    procedure SubEventBetweenEnterAndExitMethods(const AText: String); overload;
    procedure SubEventBetweenEnterAndExitMethods(aSetOfFilterDynamicOverloadOneShot: TGroupOfForWhatReasonsToLogActually; const AText: String); overload;
    class property DefaultChannels: TChannelList read GetDefaultChannels;
    { public field that contains the filter set which is intersected with the *possible* tracking purposes =~ ..."pass-filter"'s set, blocking or not the logging. }
    property FilterDynamic_forWhatReasonsToLogActually: TGroupOfForWhatReasonsToLogActually read FsetFilterDynamic_forWhatReasonsToLogActually write SetFilterDynamic_forWhatReasonsToLogActually;
    property Channels: TChannelList read FoChannels;
    property LogStack: TStrings read FoLogStack;
    property MaxStackCount: Integer read FiMaxStackCount write SetMaxStackCount;
    property OnCustomData: TCustomDataNotify read FOnCustomData write FOnCustomData;
  end;

 { TLogChannelWrapper }

  TLogChannelWrapper = class(TComponent)
  private
    FoChannel: TLogChannel;
  protected
{ Explanations: if AComponent - a specialized channel, like TMemoChannel - is in a @code(TComponentState = [opRemove]),
and if there's a AComponent's FChannelWrapper that is memory managed by AComponent,
then this FChannelWrapper must stop immediately any activity.}
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    property Channel: TLogChannel read FoChannel write FoChannel;
  end;


  TFixedCriticalSection = class(TCriticalSection)
  private
    {$WARN 5029 off : Private field "$1.$2" is never used} // FDummy not used anywhere so switch off such warnings
    FDummy: array [0..95] of Byte; // fix multiprocessor cache safety http://blog.synopse.info/post/2016/01/09/Safe-locks-for-multi-thread-applications
  end;

  TGuardian = TFixedCriticalSection;


var
  goLogger: TIntegratedLogger;

implementation

uses
  db;

const
  DefaultCheckName = 'CheckPoint';

function FormatNumber(iValue: Integer):String;
var
  sTempStr: String;
  i, iDigits: Integer;
begin
  iDigits:= 0;
  Result:= '';
  sTempStr:= IntToStr(iValue);
  for i := length(sTempStr) downto 1 do begin
    //todo: implement using mod() -> get rids of iDigits
    if iDigits = 3 then begin
      iDigits:=0;
      Result:= DefaultFormatSettings.ThousandSeparator+Result;
    end;
    Result:= sTempStr[i]+Result;
    Inc(iDigits);
  end;
end;


{ Explanations: global procedure returns the literal description of the "sender" component that is at the origin of the event.}
function GetObjectDescription(Sender: TObject): String;
begin
  Result:= Sender.ClassName;
  if (Sender is TComponent) and (TComponent(Sender).Name <> '') then
    Result := Result + '(' + TComponent(Sender).Name + ')';
end;


var
  goGuardian: TGuardian;

{ TLogChannelUtils }

class function TLogChannelUtils.SetofToInt(const aSetOf: TGroupOfForWhatReasonsToLogActually): integer;
begin
  Result:= Integer(aSetOf);	// Beware: Cf. [*]
end;

class function TLogChannelUtils.IntToSetof(const aSetof: Integer): TGroupOfForWhatReasonsToLogActually;
begin
  result:= TGroupOfForWhatReasonsToLogActually(aSetof);	// Beware: Cf. [*]
end;

class function TLogChannelUtils.SetofToString(const aSetOf: TGroupOfForWhatReasonsToLogActually): string;
begin
  Result:= TypInfo.SetToString(PTypeInfo(TypeInfo(TGroupOfForWhatReasonsToLogActually)), Integer(aSetOf), true);
end;

class function TLogChannelUtils.StringToSetof(const aSetOf: string): Integer;
begin
  Result:= TypInfo.StringToSet(PTypeInfo(TypeInfo(TGroupOfForWhatReasonsToLogActually)),aSetOf);
end;

class function TLogChannelUtils.CountElements(const aSetOf: TGroupOfForWhatReasonsToLogActually): integer;
var
	k: TForWhichTrackingPurpose;
begin
  Result:= 0;
	for k:= lwDebug to lwLast do begin
    if k in aSetOf then
      Result:= Result+1;
  end;
end;

{ TIntegratedLogger }

procedure TIntegratedLogger.GetCallStack(AStream: TStream);
{$ifdef fpc}
var
  i: Longint;
  prevbp: Pointer;
  caller_frame,
  caller_addr,
  bp: Pointer;
  S: String;
{$endif}
begin
  {$ifdef fpc}
  //routine adapted from fpc source

  //This trick skip SendCallstack item
  //bp:=get_frame;                                             //get_frame=IP's frame
  bp:= get_caller_frame(get_frame);                            //BP = number of the current base frame
  try
    prevbp:=bp-1;                                              //prev_BP = number of the precedent base frame *)
    i:=0;
    //is_dev:=do_isdevice(textrec(f).Handle);
    while bp > prevbp Do                                       // while we can pop...
     begin
       caller_addr := get_caller_addr(bp);                     //we get the IP's caller
       caller_frame := get_caller_frame(bp);                   //and its BP
       if (caller_addr=nil) then
         break;                                                //We are back at the start point: all has been "poped"
       //todo: see what is faster concatenate string and use writebuffer or current
       S:=BackTraceStrFunc(caller_addr)+LineEnding;            //EI name
       AStream.WriteBuffer(S[1],Length(S));
       Inc(i);
       if (i>=FiMaxStackCount) or (caller_frame=nil) then
         break;
       prevbp:=bp;                                             //previous variable is set with the IP
       bp:=caller_frame;                                       //the IP becomes the courent caller of the courrent frame: we backward from one call frame
     end;
   except
     { prevent endless dump if an exception occured }
   end;
  {$endif} 
end;


{^^ Explanations:
When FsetFilterDynamic_forWhatReasonsToLogActually is changed by the calling programm, character ° is logged twice: here and internally in goLogger.SendStream.
^^}
procedure TIntegratedLogger.SetFilterDynamic_forWhatReasonsToLogActually(AValue: TGroupOfForWhatReasonsToLogActually);
begin
  if FsetFilterDynamic_forWhatReasonsToLogActually=AValue then Exit;

  goLogger.SendInfo('°');
  FsetFilterDynamic_forWhatReasonsToLogActually:= AValue;
end;

class function TIntegratedLogger.GetDefaultChannels: TChannelList;
begin
  if FoDefaultChannels = nil then
    FoDefaultChannels := TChannelList.Create;
  Result := FoDefaultChannels;
end;

function TIntegratedLogger.GetEnabled: Boolean;
begin
  Result:= FsetFilterDynamic_forWhatReasonsToLogActually <> [];
end;


procedure DispatchLogMessage(o_Channels: TChannelList; const Msg: TrecLogMessage);
var
  i: Integer;
  o_Channel: TLogChannel;
begin
  for i := 0 to o_Channels.Count - 1 do begin
    o_Channel := o_Channels[i];
    if o_Channel.Active then
      o_Channel.Deliver(Msg);
  end;
end;


procedure TIntegratedLogger.SendStream(AMethodUsed: Integer; const AText: String; o_AStream: TStream);
var
  recMsg: TrecLogMessage;



              {^^ Explanations:
							création d'un record dans le même ordre que sa déclaration en pense-bête...,
							...car surtout, s'il est envoyé en IPC et lu depuis un autre programme, la mémoire devra le relire dans cet ordre,
              en utilisant TLogChannelUtils pour encoder et décoder le champ SetOf du record-TrecLogMessage.
              ^^}
              procedure CreateMsg;
              begin
                with recMsg do begin
                  iMethUsed:= AMethodUsed;
                  dtMsgTime:= Now;
                  sMsgText:= AText;
                  pData:= o_AStream;
                end;
                if (AMethodUsed = methSubEventBetweenEnterAndExitMethods) then begin
                	recMsg.setFilterDynamic:= FsetFilterDynamic_forWhatReasonsToLogActually + [lwStudyChainedEvents];
                  recMsg.sMsgText:= recMsg.sMsgText;		// as a reminder of the unbreakable association betweeb some methode names and their homonymous group
								end
                else if (AMethodUsed = methEnterMethod) or (AMethodUsed = methExitMethod) then begin
              		recMsg.setFilterDynamic:= FsetFilterDynamic_forWhatReasonsToLogActually + [lwStudyChainedEvents, lwEvents];
                  recMsg.sMsgText:= recMsg.sMsgText;		// as a reminder of the unbreakable association betweeb some methode names and their homonymous group
                end
                else if (AMethodUsed = methError) or ((AMethodUsed = methException)) then begin
                  recMsg.setFilterDynamic:= FsetFilterDynamic_forWhatReasonsToLogActually + [lwError];
                  recMsg.sMsgText:= recMsg.sMsgText;		// as a reminder of the unbreakable association betweeb some methode names and their homonymous group
                end
                else if (AMethodUsed = methWarning) then begin
    	            recMsg.setFilterDynamic:= FsetFilterDynamic_forWhatReasonsToLogActually + [lwWarning];
                  recMsg.sMsgText:= recMsg.sMsgText;		// as a reminder of the unbreakable association betweeb some methode names and their homonymous group
                end
                else if (AMethodUsed = methInfo) then begin
    	            recMsg.setFilterDynamic:= FsetFilterDynamic_forWhatReasonsToLogActually + [lwInfo];
                  recMsg.sMsgText:= recMsg.sMsgText;		// as a reminder of the unbreakable association betweeb some methode names and their homonymous group
                end
                else
                  recMsg.setFilterDynamic:= FsetFilterDynamic_forWhatReasonsToLogActually;
              end;

begin
  CreateMsg;

  (*IsMultiThread == true, when unit cthreads is used by a project*)
  if IsMultiThread then
  	//Yes: it's a global variable created in this unit, and used in this unit only ;-)
    goGuardian.Enter;
  if FoDefaultChannels <> nil then
    DispatchLogMessage(FoDefaultChannels, recMsg);
  DispatchLogMessage(Channels, recMsg);
  if IsMultiThread then
    goGuardian.Leave;
end;

procedure TIntegratedLogger.SendBuffer(AMethodUsed: Integer; const AText: String; var pBuffer; Count: LongWord);
var
  oStream: TStream;
begin
  try
    if Count > 0 then begin
      oStream:= TMemoryStream.Create;
      oStream.Write(pBuffer,Count);
    end
    else
      oStream:= nil;
    SendStream(AMethodUsed,AText,oStream);	//nb: SendStream will free oStream
  finally
    oStream.Free;
  end;
end;

procedure TIntegratedLogger.SetMaxStackCount(const AValue: Integer);
begin
  if AValue < 256 then
    FiMaxStackCount := AValue
  else
    FiMaxStackCount := 256;
end;

procedure TIntegratedLogger.SetThreadSafe;
begin
  if IsMultiThread and not Assigned(goGuardian) then
    goGuardian:= TGuardian.Create
	else if (not IsMultiThread) and Assigned(goGuardian) then
  	FreeAndNil(goGuardian);
end;

constructor TIntegratedLogger.Create;
begin
  SetThreadSafe;
  FoChannels := TChannelList.Create;
  FiMaxStackCount := 20;
  FoLogStack := TStringList.Create;
  FoCheckList := TStringList.Create;
  with FoCheckList do begin
    CaseSensitive := False;
    Sorted := True; //Faster IndexOf?
  end;
  FoCounterList := TStringList.Create;
  with FoCounterList do begin
    CaseSensitive := False;
    Sorted := True; //Faster IndexOf?
  end;
  FsetFilterDynamic_forWhatReasonsToLogActually:= lwNone;		//categor{y|ies} of What is logged; by default, the logging and log nothing;
end;

destructor TIntegratedLogger.Destroy;
begin
  FoChannels.Destroy;
  FoLogStack.Destroy;
  FoCheckList.Destroy;
  FoCounterList.Destroy;
  if Assigned(goGuardian) then
    FreeAndNil(goGuardian);
end;

function TIntegratedLogger.CalledBy(const AMethodName: String): Boolean;
begin
  Result:= FoLogStack.IndexOf(UpperCase(AMethodName)) <> -1;
end;

procedure ClearChannels(o_Channels: TChannelList);
var
  i: Integer;
  o_Channel: TLogChannel;
begin
  for i := 0 to o_Channels.Count - 1 do begin
    o_Channel := o_Channels[i];
    if o_Channel.Active then
      o_Channel.Clear;
  end;
end;

procedure TIntegratedLogger.Clear;
begin
  if FoDefaultChannels <> nil then
    ClearChannels(FoDefaultChannels);
  ClearChannels(Channels);
end;

function TIntegratedLogger.RectToStr(const ARect: TRect): String;
begin
  with ARect do
    Result:= Format('(Left: %d; Top: %d; Right: %d; Bottom: %d)',[Left,Top,Right,Bottom]);
end;

function TIntegratedLogger.PointToStr(const APoint: TPoint): String;
begin
  with APoint do
    Result:= Format('(X: %d; Y: %d)',[X,Y]);
end;

procedure TIntegratedLogger.Send(const AText: String);
begin
  Send(FsetFilterDynamic_forWhatReasonsToLogActually,AText);
end;

procedure TIntegratedLogger.Send(aSetOfFilterDynamicOverloadOneShot: TGroupOfForWhatReasonsToLogActually; const AText: String);
var
  setOldFilterDynamic: TGroupOfForWhatReasonsToLogActually;
begin
  setOldFilterDynamic:= FsetFilterDynamic_forWhatReasonsToLogActually;
  FsetFilterDynamic_forWhatReasonsToLogActually:= FsetFilterDynamic_forWhatReasonsToLogActually + aSetOfFilterDynamicOverloadOneShot;
  if FsetFilterDynamic_forWhatReasonsToLogActually = [] then Exit;   //pre-conditions

  SendStream(methSend,AText,nil);
  FsetFilterDynamic_forWhatReasonsToLogActually:= setOldFilterDynamic;
end;

procedure TIntegratedLogger.Send(const AText: String; Args: array of const);
begin
	Send(FsetFilterDynamic_forWhatReasonsToLogActually,AText,Args);
end;

procedure TIntegratedLogger.Send(aSetOfFilterDynamicOverloadOneShot: TGroupOfForWhatReasonsToLogActually; const AText: String; Args: array of const);
  var
  setOldFilterDynamic: TGroupOfForWhatReasonsToLogActually;
begin
  setOldFilterDynamic:= FsetFilterDynamic_forWhatReasonsToLogActually;
  FsetFilterDynamic_forWhatReasonsToLogActually:= FsetFilterDynamic_forWhatReasonsToLogActually + aSetOfFilterDynamicOverloadOneShot;
  if FsetFilterDynamic_forWhatReasonsToLogActually = [] then Exit;   //pre-conditions

  SendStream(methSend, Format(AText,Args),nil);
  FsetFilterDynamic_forWhatReasonsToLogActually:= setOldFilterDynamic;
end;

procedure TIntegratedLogger.Send(const AText, AValue: String);
begin
	SendInfo(FsetFilterDynamic_forWhatReasonsToLogActually,AText,AValue);
end;

procedure TIntegratedLogger.Send(aSetOfFilterDynamicOverloadOneShot: TGroupOfForWhatReasonsToLogActually; const AText, AValue: String);
var
  setOldFilterDynamic: TGroupOfForWhatReasonsToLogActually;
begin
  setOldFilterDynamic:= FsetFilterDynamic_forWhatReasonsToLogActually;
  FsetFilterDynamic_forWhatReasonsToLogActually:= FsetFilterDynamic_forWhatReasonsToLogActually + aSetOfFilterDynamicOverloadOneShot;
  if FsetFilterDynamic_forWhatReasonsToLogActually = [] then Exit;   //pre-conditions

  SendStream(methValue,AText+' = '+AValue,nil);
  FsetFilterDynamic_forWhatReasonsToLogActually:= setOldFilterDynamic;
end;

procedure TIntegratedLogger.Send(const AText: String; AValue: Integer);
begin
  SendInfo(FsetFilterDynamic_forWhatReasonsToLogActually,AText,AValue);
end;

procedure TIntegratedLogger.Send(aSetOfFilterDynamicOverloadOneShot: TGroupOfForWhatReasonsToLogActually; const AText: String; AValue: Integer);
var
  setOldFilterDynamic: TGroupOfForWhatReasonsToLogActually;
begin
  setOldFilterDynamic:= FsetFilterDynamic_forWhatReasonsToLogActually;
  FsetFilterDynamic_forWhatReasonsToLogActually:= FsetFilterDynamic_forWhatReasonsToLogActually + aSetOfFilterDynamicOverloadOneShot;
  if FsetFilterDynamic_forWhatReasonsToLogActually = [] then Exit;   //pre-conditions

  SendStream(methValue,AText+' = '+IntToStr(AValue),nil);
  FsetFilterDynamic_forWhatReasonsToLogActually:= setOldFilterDynamic;
end;

procedure TIntegratedLogger.Send(const AText: String; AValue: Cardinal);
begin
  SendInfo(FsetFilterDynamic_forWhatReasonsToLogActually,AText,AValue);
end;

procedure TIntegratedLogger.Send(aSetOfFilterDynamicOverloadOneShot: TGroupOfForWhatReasonsToLogActually; const AText: String; AValue: Cardinal);
var
  setOldFilterDynamic: TGroupOfForWhatReasonsToLogActually;
begin
  setOldFilterDynamic:= FsetFilterDynamic_forWhatReasonsToLogActually;
  FsetFilterDynamic_forWhatReasonsToLogActually:= FsetFilterDynamic_forWhatReasonsToLogActually + aSetOfFilterDynamicOverloadOneShot;
  if FsetFilterDynamic_forWhatReasonsToLogActually = [] then Exit;   //pre-conditions

  SendStream(methValue,AText+' = '+IntToStr(AValue),nil);
  FsetFilterDynamic_forWhatReasonsToLogActually:= setOldFilterDynamic;
end;

procedure TIntegratedLogger.Send(const AText: String; AValue: Double);
begin
  SendInfo(FsetFilterDynamic_forWhatReasonsToLogActually,AText,AValue);
end;

procedure TIntegratedLogger.Send(aSetOfFilterDynamicOverloadOneShot: TGroupOfForWhatReasonsToLogActually; const AText: String; AValue: Double);
var
  setOldFilterDynamic: TGroupOfForWhatReasonsToLogActually;
begin
  setOldFilterDynamic:= FsetFilterDynamic_forWhatReasonsToLogActually;
  FsetFilterDynamic_forWhatReasonsToLogActually:= FsetFilterDynamic_forWhatReasonsToLogActually + aSetOfFilterDynamicOverloadOneShot;
  if FsetFilterDynamic_forWhatReasonsToLogActually = [] then Exit;   //pre-conditions

  SendStream(methValue,AText+' = '+FloatToStr(AValue),nil);
  FsetFilterDynamic_forWhatReasonsToLogActually:= setOldFilterDynamic;
end;

procedure TIntegratedLogger.Send(const AText: String; AValue: Int64);
begin
  SendInfo(FsetFilterDynamic_forWhatReasonsToLogActually,AText,AValue);
end;

procedure TIntegratedLogger.Send(aSetOfFilterDynamicOverloadOneShot: TGroupOfForWhatReasonsToLogActually; const AText: String; AValue: Int64);
var
  setOldFilterDynamic: TGroupOfForWhatReasonsToLogActually;
begin
  setOldFilterDynamic:= FsetFilterDynamic_forWhatReasonsToLogActually;
  FsetFilterDynamic_forWhatReasonsToLogActually:= FsetFilterDynamic_forWhatReasonsToLogActually + aSetOfFilterDynamicOverloadOneShot;
  if FsetFilterDynamic_forWhatReasonsToLogActually = [] then Exit;   //pre-conditions

  SendStream(methValue,AText+' = '+IntToStr(AValue),nil);
  FsetFilterDynamic_forWhatReasonsToLogActually:= setOldFilterDynamic;
end;

procedure TIntegratedLogger.Send(const AText: String; AValue: QWord);
begin
  SendInfo(FsetFilterDynamic_forWhatReasonsToLogActually,AText,AValue);
end;

procedure TIntegratedLogger.Send(aSetOfFilterDynamicOverloadOneShot: TGroupOfForWhatReasonsToLogActually; const AText: String; AValue: QWord);
var
  setOldFilterDynamic: TGroupOfForWhatReasonsToLogActually;
begin
  setOldFilterDynamic:= FsetFilterDynamic_forWhatReasonsToLogActually;
  FsetFilterDynamic_forWhatReasonsToLogActually:= FsetFilterDynamic_forWhatReasonsToLogActually + aSetOfFilterDynamicOverloadOneShot;
  if FsetFilterDynamic_forWhatReasonsToLogActually = [] then Exit;   //pre-conditions

  SendStream(methValue,AText+' = '+IntToStr(AValue),nil);
  FsetFilterDynamic_forWhatReasonsToLogActually:= setOldFilterDynamic;
end;

procedure TIntegratedLogger.Send(const AText: String; AValue: Boolean);
begin
  SendInfo(FsetFilterDynamic_forWhatReasonsToLogActually, AText, AValue);
end;

procedure TIntegratedLogger.Send(aSetOfFilterDynamicOverloadOneShot: TGroupOfForWhatReasonsToLogActually; const AText: String; AValue: Boolean);
var
  setOldFilterDynamic: TGroupOfForWhatReasonsToLogActually;
begin
  setOldFilterDynamic:= FsetFilterDynamic_forWhatReasonsToLogActually;
  FsetFilterDynamic_forWhatReasonsToLogActually:= FsetFilterDynamic_forWhatReasonsToLogActually + aSetOfFilterDynamicOverloadOneShot;
  if FsetFilterDynamic_forWhatReasonsToLogActually = [] then Exit;   //pre-conditions

  SendStream(methValue, AText + ' = ' + BoolToStr(AValue, True), nil);
  FsetFilterDynamic_forWhatReasonsToLogActually:= setOldFilterDynamic;
end;

procedure TIntegratedLogger.Send(const AText: String; const ARect: TRect);
begin
  SendInfo(FsetFilterDynamic_forWhatReasonsToLogActually,AText,ARect);
end;

procedure TIntegratedLogger.Send(aSetOfFilterDynamicOverloadOneShot: TGroupOfForWhatReasonsToLogActually; const AText: String; const ARect: TRect);
var
  setOldFilterDynamic: TGroupOfForWhatReasonsToLogActually;
begin
  setOldFilterDynamic:= FsetFilterDynamic_forWhatReasonsToLogActually;
  FsetFilterDynamic_forWhatReasonsToLogActually:= FsetFilterDynamic_forWhatReasonsToLogActually + aSetOfFilterDynamicOverloadOneShot;
  if FsetFilterDynamic_forWhatReasonsToLogActually = [] then Exit;   //pre-conditions

  SendStream(methValue,AText+ ' = '+RectToStr(ARect),nil);
  FsetFilterDynamic_forWhatReasonsToLogActually:= setOldFilterDynamic;
end;

procedure TIntegratedLogger.Send(const AText: String; const APoint: TPoint);
begin
  Send(FsetFilterDynamic_forWhatReasonsToLogActually,AText,APoint);
end;

procedure TIntegratedLogger.Send(aSetOfFilterDynamicOverloadOneShot: TGroupOfForWhatReasonsToLogActually; const AText: String; const APoint: TPoint);
var
  setOldFilterDynamic: TGroupOfForWhatReasonsToLogActually;
begin
  setOldFilterDynamic:= FsetFilterDynamic_forWhatReasonsToLogActually;
  FsetFilterDynamic_forWhatReasonsToLogActually:= FsetFilterDynamic_forWhatReasonsToLogActually + aSetOfFilterDynamicOverloadOneShot;
  if FsetFilterDynamic_forWhatReasonsToLogActually = [] then Exit;   //pre-conditions

  SendStream(methValue,AText+' = '+PointToStr(APoint),nil);
  FsetFilterDynamic_forWhatReasonsToLogActually:= setOldFilterDynamic;
end;

procedure TIntegratedLogger.Send(const AText: String; AStrList: TStrings);
begin
  SendInfo(FsetFilterDynamic_forWhatReasonsToLogActually,AText,AStrList);
end;

procedure TIntegratedLogger.Send(aSetOfFilterDynamicOverloadOneShot: TGroupOfForWhatReasonsToLogActually; const AText: String; o_AStrList: TStrings);
var
	sStr: string;
  setOldFilterDynamic: TGroupOfForWhatReasonsToLogActually;
begin
  setOldFilterDynamic:= FsetFilterDynamic_forWhatReasonsToLogActually;
  FsetFilterDynamic_forWhatReasonsToLogActually:= FsetFilterDynamic_forWhatReasonsToLogActually + aSetOfFilterDynamicOverloadOneShot;
  if FsetFilterDynamic_forWhatReasonsToLogActually = [] then Exit;   //pre-conditions

  if Assigned(o_AStrList) then
    if o_AStrList.Count>0 then
    	sStr:= o_AStrList.Text
    else
      sStr:= ' ' { fake o_AStrList.Text }
  else
    sStr:= ' '; { fake o_AStrList.Text }
  SendBuffer(methTStrings, AText, sStr[1], Length(sStr));
  FsetFilterDynamic_forWhatReasonsToLogActually:= setOldFilterDynamic;
end;

procedure TIntegratedLogger.Send(const AText: String; AObject: TObject);
begin
  SendInfo(FsetFilterDynamic_forWhatReasonsToLogActually,AText,AObject);
end;

procedure TIntegratedLogger.Send(aSetOfFilterDynamicOverloadOneShot: TGroupOfForWhatReasonsToLogActually; const AText: String; AObject: TObject);
var
  sTempStr: String;
  oStream: TStream;
  setOldFilterDynamic: TGroupOfForWhatReasonsToLogActually;
begin
  setOldFilterDynamic:= FsetFilterDynamic_forWhatReasonsToLogActually;
  FsetFilterDynamic_forWhatReasonsToLogActually:= FsetFilterDynamic_forWhatReasonsToLogActually + aSetOfFilterDynamicOverloadOneShot;
  if FsetFilterDynamic_forWhatReasonsToLogActually = [] then Exit;   //pre-conditions

  try
    oStream := nil;
    sTempStr := AText + ' [';
    if AObject <> nil then
    begin
      if AObject is TComponent then begin
        oStream := TMemoryStream.Create;
        oStream.WriteComponent(TComponent(AObject));
      end
      else
        sTempStr := sTempStr + GetObjectDescription(AObject) + ' / ';
    end;
    sTempStr := sTempStr + ('$' + HexStr(AObject) + ']');
    SendStream(methObject, sTempStr, oStream);
  finally
    oStream.Free;
    FsetFilterDynamic_forWhatReasonsToLogActually:= setOldFilterDynamic;
  end;
end;


procedure TIntegratedLogger.SendInfo(const AText: String);
begin
  SendInfo(FsetFilterDynamic_forWhatReasonsToLogActually + [lwInfo], AText);
end;


procedure TIntegratedLogger.SendInfo(const AText: String; Args: array of const);
begin
  SendInfo(FsetFilterDynamic_forWhatReasonsToLogActually + [lwInfo], AText, Args);
end;


procedure TIntegratedLogger.SendInfo(const AText, AValue: String);
begin
  SendInfo(FsetFilterDynamic_forWhatReasonsToLogActually + [lwInfo], AText, AValue);
end;


procedure TIntegratedLogger.SendInfo(const AText: String; AValue: Integer);
begin
  SendInfo(FsetFilterDynamic_forWhatReasonsToLogActually + [lwInfo], AText, AValue);
end;

{$ifdef fpc}
procedure TIntegratedLogger.SendInfo(const AText: String; AValue: Cardinal);
begin
  SendInfo(FsetFilterDynamic_forWhatReasonsToLogActually + [lwInfo], AText, AValue);
end;
{$endif}


procedure TIntegratedLogger.SendInfo(const AText: String; AValue: Double);
begin
  SendInfo(FsetFilterDynamic_forWhatReasonsToLogActually + [lwInfo], AText, AValue);
end;


procedure TIntegratedLogger.SendInfo(const AText: String; AValue: Int64);
begin
  SendInfo(FsetFilterDynamic_forWhatReasonsToLogActually + [lwInfo], AText, AValue);
end;


procedure TIntegratedLogger.SendInfo(const AText: String; AValue: QWord);
begin
  SendInfo(FsetFilterDynamic_forWhatReasonsToLogActually + [lwInfo], AText, AValue);
end;


procedure TIntegratedLogger.SendInfo(const AText: String; AValue: Boolean);
begin
  SendInfo(FsetFilterDynamic_forWhatReasonsToLogActually + [lwInfo], AText, AValue);
end;


procedure TIntegratedLogger.SendInfo(const AText: String; const ARect: TRect);
begin
  SendInfo(FsetFilterDynamic_forWhatReasonsToLogActually + [lwInfo], AText, ARect);
end;


procedure TIntegratedLogger.SendInfo(const AText: String; const APoint: TPoint);
begin
  SendInfo(FsetFilterDynamic_forWhatReasonsToLogActually + [lwInfo], AText, APoint);
end;


procedure TIntegratedLogger.SendInfo(const AText: String; AStrList: TStrings);
begin
  SendInfo(FsetFilterDynamic_forWhatReasonsToLogActually + [lwInfo], AText, AStrList);
end;


procedure TIntegratedLogger.SendInfo(const AText: String; AObject: TObject);
begin
  SendInfo(FsetFilterDynamic_forWhatReasonsToLogActually + [lwInfo], AText, AObject);
end;


procedure TIntegratedLogger.SendPointer(const AText: String; APointer: Pointer);
begin
  SendPointer(FsetFilterDynamic_forWhatReasonsToLogActually,AText,APointer);
end;


procedure TIntegratedLogger.SendCallStack(const AText: String);
begin
  SendCallStack(FsetFilterDynamic_forWhatReasonsToLogActually,AText);
end;


procedure TIntegratedLogger.SendException(const AText: String; AException: Exception);
begin
  SendException(FsetFilterDynamic_forWhatReasonsToLogActually + [lwError], AText, AException);
end;


procedure TIntegratedLogger.SendHeapInfo(const AText: String);
begin
  SendHeapInfo(FsetFilterDynamic_forWhatReasonsToLogActually,AText);
end;


procedure TIntegratedLogger.SendMemory(const AText: String; Address: Pointer; Size: LongWord; Offset: Integer);
begin
  SendMemory(FsetFilterDynamic_forWhatReasonsToLogActually,AText,Address,Size,Offset)
end;


procedure TIntegratedLogger.SendIf(const AText: String; Expression: Boolean);
begin
  SendIf(FsetFilterDynamic_forWhatReasonsToLogActually,AText,Expression,True);
end;


procedure TIntegratedLogger.SendIf(Classes: TGroupOfForWhatReasonsToLogActually; const AText: String; Expression: Boolean);
begin
  SendIf(Classes,AText,Expression,True);
end;


procedure TIntegratedLogger.SendIf(const AText: String; Expression, IsTrue: Boolean);
begin
  SendIf(FsetFilterDynamic_forWhatReasonsToLogActually,AText,Expression,IsTrue);
end;


procedure TIntegratedLogger.SendWarning(const AText: String);
begin
  SendWarning(FsetFilterDynamic_forWhatReasonsToLogActually + [lwWarning], AText);
end;


procedure TIntegratedLogger.SendError(const AText: String);
begin
  SendError(FsetFilterDynamic_forWhatReasonsToLogActually + [lwError], AText);
end;


procedure TIntegratedLogger.SendCustomData(const AText: String; Data: Pointer);
begin
  SendCustomData(FsetFilterDynamic_forWhatReasonsToLogActually,AText,Data,FOnCustomData);
end;


procedure TIntegratedLogger.SendCustomData(aSetOfFilterDynamicOverloadOneShot: TGroupOfForWhatReasonsToLogActually; const AText: String; Data: Pointer);
begin
  SendCustomData(aSetOfFilterDynamicOverloadOneShot,AText,Data,FOnCustomData);
end;


procedure TIntegratedLogger.SendCustomData(const AText: String; Data: Pointer; CustomDataFunction: TCustomDataNotify);
begin
  SendCustomData(FsetFilterDynamic_forWhatReasonsToLogActually,AText,Data,CustomDataFunction);
end;


procedure TIntegratedLogger.SendCustomData(const AText: String; Data: Pointer; CustomDataFunction: TCustomDataNotifyStatic);
begin
  SendCustomData(FsetFilterDynamic_forWhatReasonsToLogActually,AText,Data,CustomDataFunction);
end;


procedure TIntegratedLogger.AddCheckPoint;
begin
  AddCheckPoint(FsetFilterDynamic_forWhatReasonsToLogActually,DefaultCheckName);
end;


procedure TIntegratedLogger.AddCheckPoint(aSetOfFilterDynamicOverloadOneShot: TGroupOfForWhatReasonsToLogActually);
begin
  AddCheckPoint(aSetOfFilterDynamicOverloadOneShot,DefaultCheckName);
end;


procedure TIntegratedLogger.AddCheckPoint(const sCheckName: String);
begin
  AddCheckPoint(FsetFilterDynamic_forWhatReasonsToLogActually,sCheckName);
end;


procedure TIntegratedLogger.ResetCheckPoint;
begin
  ResetCheckPoint(FsetFilterDynamic_forWhatReasonsToLogActually,DefaultCheckName);
end;


procedure TIntegratedLogger.ResetCheckPoint(Classes: TGroupOfForWhatReasonsToLogActually);
begin
  ResetCheckPoint(Classes,DefaultCheckName);
end;


procedure TIntegratedLogger.ResetCheckPoint(const sCheckName: String);
begin
  ResetCheckPoint(FsetFilterDynamic_forWhatReasonsToLogActually,sCheckName);
end;


procedure TIntegratedLogger.IncCounter(const sCounterName: String);
begin
  IncCounter(FsetFilterDynamic_forWhatReasonsToLogActually,sCounterName);
end;


procedure TIntegratedLogger.DecCounter(const sCounterName: String);
begin
  DecCounter(FsetFilterDynamic_forWhatReasonsToLogActually,sCounterName);
end;


procedure TIntegratedLogger.ResetCounter(const sCounterName: String);
begin
  ResetCounter(FsetFilterDynamic_forWhatReasonsToLogActually,sCounterName);
end;


procedure TIntegratedLogger.EnterMethod(const AMethodName: String; const AMessage: String);
begin
  EnterMethod(FsetFilterDynamic_forWhatReasonsToLogActually + [lwStudyChainedEvents, lwEvents],nil,AMethodName,AMessage);
end;


procedure TIntegratedLogger.EnterMethod(aSetOfFilterDynamicOverloadOneShot: TGroupOfForWhatReasonsToLogActually; const AMethodName: String; const AMessage: String);
begin
  EnterMethod(aSetOfFilterDynamicOverloadOneShot + [lwStudyChainedEvents, lwEvents],nil,AMethodName,AMessage);
end;


procedure TIntegratedLogger.EnterMethod(Sender: TObject; const AMethodName: String; const AMessage: String);
begin
  EnterMethod(FsetFilterDynamic_forWhatReasonsToLogActually + [lwStudyChainedEvents, lwEvents],Sender,AMethodName,AMessage);
end;


procedure TIntegratedLogger.ExitMethod(const AMethodName: String; const AMessage: String);
begin
  ExitMethod(FsetFilterDynamic_forWhatReasonsToLogActually + [lwStudyChainedEvents, lwEvents],nil,AMethodName,AMessage);
end;


procedure TIntegratedLogger.ExitMethod(Sender: TObject; const AMethodName: String; const AMessage: String);
begin
  ExitMethod(FsetFilterDynamic_forWhatReasonsToLogActually + [lwStudyChainedEvents, lwEvents],Sender,AMethodName,AMessage);
end;


procedure TIntegratedLogger.ExitMethod(aSetOfFilterDynamicOverloadOneShot: TGroupOfForWhatReasonsToLogActually; const AMethodName: String; const AMessage: String);
begin
  ExitMethod(aSetOfFilterDynamicOverloadOneShot + [lwStudyChainedEvents, lwEvents],nil,AMethodName,AMessage);
end;


procedure TIntegratedLogger.Watch(const AText, AValue: String);
begin
  Watch(FsetFilterDynamic_forWhatReasonsToLogActually,AText,AValue);
end;


procedure TIntegratedLogger.Watch(const AText: String; AValue: Integer);
begin
  Watch(FsetFilterDynamic_forWhatReasonsToLogActually,AText,AValue);
end;


{$ifdef fpc}
procedure TIntegratedLogger.Watch(const AText: String; AValue: Cardinal);
begin
  Watch(FsetFilterDynamic_forWhatReasonsToLogActually,AText,AValue);
end;
{$endif}


procedure TIntegratedLogger.Watch(const AText: String; AValue: Double);
begin
  Watch(FsetFilterDynamic_forWhatReasonsToLogActually,AText,AValue);
end;


procedure TIntegratedLogger.Watch(const AText: String; AValue: Boolean);
begin
  Watch(FsetFilterDynamic_forWhatReasonsToLogActually,AText,AValue);
end;


procedure TIntegratedLogger.SubEventBetweenEnterAndExitMethods(const AText: String);
begin
  SubEventBetweenEnterAndExitMethods(FsetFilterDynamic_forWhatReasonsToLogActually + [lwStudyChainedEvents], AText);
end;




(* -- filtred method through intersection of Classes's set and ActivesClasses's set -- *)




procedure TIntegratedLogger.SendInfo(aSetOfFilterDynamicOverloadOneShot: TGroupOfForWhatReasonsToLogActually; const AText: String);
var
  setOldFilterDynamic: TGroupOfForWhatReasonsToLogActually;
begin
  setOldFilterDynamic:= FsetFilterDynamic_forWhatReasonsToLogActually;
  FsetFilterDynamic_forWhatReasonsToLogActually:= FsetFilterDynamic_forWhatReasonsToLogActually + [lwInfo] + aSetOfFilterDynamicOverloadOneShot;
  if FsetFilterDynamic_forWhatReasonsToLogActually = [] then Exit;   //pre-conditions

  SendStream(methInfo,AText,nil);
  FsetFilterDynamic_forWhatReasonsToLogActually:= setOldFilterDynamic;
end;


procedure TIntegratedLogger.SendInfo(aSetOfFilterDynamicOverloadOneShot: TGroupOfForWhatReasonsToLogActually; const AText: String; Args: array of const);
var
  setOldFilterDynamic: TGroupOfForWhatReasonsToLogActually;
begin
  setOldFilterDynamic:= FsetFilterDynamic_forWhatReasonsToLogActually;
  FsetFilterDynamic_forWhatReasonsToLogActually:= FsetFilterDynamic_forWhatReasonsToLogActually + [lwInfo] + aSetOfFilterDynamicOverloadOneShot;
  if FsetFilterDynamic_forWhatReasonsToLogActually = [] then Exit;   //pre-conditions

  SendStream(methInfo, Format(AText,Args),nil);
  FsetFilterDynamic_forWhatReasonsToLogActually:= setOldFilterDynamic;
end;


procedure TIntegratedLogger.SendInfo(aSetOfFilterDynamicOverloadOneShot: TGroupOfForWhatReasonsToLogActually; const AText, AValue: String);
var
  setOldFilterDynamic: TGroupOfForWhatReasonsToLogActually;
begin
  setOldFilterDynamic:= FsetFilterDynamic_forWhatReasonsToLogActually;
  FsetFilterDynamic_forWhatReasonsToLogActually:= FsetFilterDynamic_forWhatReasonsToLogActually + [lwInfo] + aSetOfFilterDynamicOverloadOneShot;
  if FsetFilterDynamic_forWhatReasonsToLogActually = [] then Exit;   //pre-conditions

  SendStream(methValue,AText+' = '+AValue,nil);
  FsetFilterDynamic_forWhatReasonsToLogActually:= setOldFilterDynamic;
end;


procedure TIntegratedLogger.SendInfo(aSetOfFilterDynamicOverloadOneShot: TGroupOfForWhatReasonsToLogActually; const AText: String; AValue: Integer);
var
  setOldFilterDynamic: TGroupOfForWhatReasonsToLogActually;
begin
  setOldFilterDynamic:= FsetFilterDynamic_forWhatReasonsToLogActually;
  FsetFilterDynamic_forWhatReasonsToLogActually:= FsetFilterDynamic_forWhatReasonsToLogActually + [lwInfo] + aSetOfFilterDynamicOverloadOneShot;
  if FsetFilterDynamic_forWhatReasonsToLogActually = [] then Exit;   //pre-conditions

  SendStream(methValue,AText+' = '+IntToStr(AValue),nil);
  FsetFilterDynamic_forWhatReasonsToLogActually:= setOldFilterDynamic;
end;

{$ifdef fpc}
procedure TIntegratedLogger.SendInfo(aSetOfFilterDynamicOverloadOneShot: TGroupOfForWhatReasonsToLogActually; const AText: String; AValue: Cardinal);
var
  setOldFilterDynamic: TGroupOfForWhatReasonsToLogActually;
begin
  setOldFilterDynamic:= FsetFilterDynamic_forWhatReasonsToLogActually;
  FsetFilterDynamic_forWhatReasonsToLogActually:= FsetFilterDynamic_forWhatReasonsToLogActually + [lwInfo] + aSetOfFilterDynamicOverloadOneShot;
  if FsetFilterDynamic_forWhatReasonsToLogActually = [] then Exit;   //pre-conditions

  SendStream(methValue,AText+' = '+IntToStr(AValue),nil);
  FsetFilterDynamic_forWhatReasonsToLogActually:= setOldFilterDynamic;
end;
{$endif}


procedure TIntegratedLogger.SendInfo(aSetOfFilterDynamicOverloadOneShot: TGroupOfForWhatReasonsToLogActually; const AText: String; AValue: Double);
var
  setOldFilterDynamic: TGroupOfForWhatReasonsToLogActually;
begin
  setOldFilterDynamic:= FsetFilterDynamic_forWhatReasonsToLogActually;
  FsetFilterDynamic_forWhatReasonsToLogActually:= FsetFilterDynamic_forWhatReasonsToLogActually + [lwInfo] + aSetOfFilterDynamicOverloadOneShot;
  if FsetFilterDynamic_forWhatReasonsToLogActually = [] then Exit;   //pre-conditions

  SendStream(methValue,AText+' = '+FloatToStr(AValue),nil);
  FsetFilterDynamic_forWhatReasonsToLogActually:= setOldFilterDynamic;
end;


procedure TIntegratedLogger.SendInfo(aSetOfFilterDynamicOverloadOneShot: TGroupOfForWhatReasonsToLogActually; const AText: String; AValue: Int64);
var
  setOldFilterDynamic: TGroupOfForWhatReasonsToLogActually;
begin
  setOldFilterDynamic:= FsetFilterDynamic_forWhatReasonsToLogActually;
  FsetFilterDynamic_forWhatReasonsToLogActually:= FsetFilterDynamic_forWhatReasonsToLogActually + [lwInfo] + aSetOfFilterDynamicOverloadOneShot;
  if FsetFilterDynamic_forWhatReasonsToLogActually = [] then Exit;   //pre-conditions

  SendStream(methValue,AText+' = '+IntToStr(AValue),nil);
  FsetFilterDynamic_forWhatReasonsToLogActually:= setOldFilterDynamic;
end;


procedure TIntegratedLogger.SendInfo(aSetOfFilterDynamicOverloadOneShot: TGroupOfForWhatReasonsToLogActually; const AText: String; AValue: QWord);
var
  setOldFilterDynamic: TGroupOfForWhatReasonsToLogActually;
begin
  setOldFilterDynamic:= FsetFilterDynamic_forWhatReasonsToLogActually;
  FsetFilterDynamic_forWhatReasonsToLogActually:= FsetFilterDynamic_forWhatReasonsToLogActually + [lwInfo] + aSetOfFilterDynamicOverloadOneShot;
  if FsetFilterDynamic_forWhatReasonsToLogActually = [] then Exit;   //pre-conditions

  SendStream(methValue,AText+' = '+IntToStr(AValue),nil);
  FsetFilterDynamic_forWhatReasonsToLogActually:= setOldFilterDynamic;
end;


procedure TIntegratedLogger.SendInfo(aSetOfFilterDynamicOverloadOneShot: TGroupOfForWhatReasonsToLogActually; const AText: String; AValue: Boolean);
var
  setOldFilterDynamic: TGroupOfForWhatReasonsToLogActually;
begin
  setOldFilterDynamic:= FsetFilterDynamic_forWhatReasonsToLogActually;
  FsetFilterDynamic_forWhatReasonsToLogActually:= FsetFilterDynamic_forWhatReasonsToLogActually + [lwInfo] + aSetOfFilterDynamicOverloadOneShot;
  if FsetFilterDynamic_forWhatReasonsToLogActually = [] then Exit;   //pre-conditions

  SendStream(methValue, AText + ' = ' + BoolToStr(AValue, True), nil);
  FsetFilterDynamic_forWhatReasonsToLogActually:= setOldFilterDynamic;
end;



procedure TIntegratedLogger.SendInfo(aSetOfFilterDynamicOverloadOneShot: TGroupOfForWhatReasonsToLogActually; const AText: String;const ARect: TRect);
var
  setOldFilterDynamic: TGroupOfForWhatReasonsToLogActually;
begin
  setOldFilterDynamic:= FsetFilterDynamic_forWhatReasonsToLogActually;
  FsetFilterDynamic_forWhatReasonsToLogActually:= FsetFilterDynamic_forWhatReasonsToLogActually + [lwInfo] + aSetOfFilterDynamicOverloadOneShot;
  if FsetFilterDynamic_forWhatReasonsToLogActually = [] then Exit;   //pre-conditions

  SendStream(methValue,AText+ ' = '+RectToStr(ARect),nil);
  FsetFilterDynamic_forWhatReasonsToLogActually:= setOldFilterDynamic;
end;


procedure TIntegratedLogger.SendInfo(aSetOfFilterDynamicOverloadOneShot: TGroupOfForWhatReasonsToLogActually; const AText: String; const APoint: TPoint);
var
  setOldFilterDynamic: TGroupOfForWhatReasonsToLogActually;
begin
  setOldFilterDynamic:= FsetFilterDynamic_forWhatReasonsToLogActually;
  FsetFilterDynamic_forWhatReasonsToLogActually:= FsetFilterDynamic_forWhatReasonsToLogActually + [lwInfo] + aSetOfFilterDynamicOverloadOneShot;
  if FsetFilterDynamic_forWhatReasonsToLogActually = [] then Exit;   //pre-conditions

  SendStream(methValue,AText+' = '+PointToStr(APoint),nil);
  FsetFilterDynamic_forWhatReasonsToLogActually:= setOldFilterDynamic;
end;


procedure TIntegratedLogger.SendInfo(aSetOfFilterDynamicOverloadOneShot: TGroupOfForWhatReasonsToLogActually; const AText: String; o_AStrList: TStrings);
var
	sStr: string;
  setOldFilterDynamic: TGroupOfForWhatReasonsToLogActually;
begin
  setOldFilterDynamic:= FsetFilterDynamic_forWhatReasonsToLogActually;
  FsetFilterDynamic_forWhatReasonsToLogActually:= FsetFilterDynamic_forWhatReasonsToLogActually + [lwInfo] + aSetOfFilterDynamicOverloadOneShot;
  if FsetFilterDynamic_forWhatReasonsToLogActually = [] then Exit;   //pre-conditions

  if Assigned(o_AStrList) then
    if o_AStrList.Count>0 then
    	sStr:= o_AStrList.Text
    else
      sStr:= ' ' { fake o_AStrList.Text }
  else
    sStr:= ' '; { fake o_AStrList.Text }
  SendBuffer(methTStrings, AText, sStr[1], Length(sStr));
  FsetFilterDynamic_forWhatReasonsToLogActually:= setOldFilterDynamic;
end;



procedure TIntegratedLogger.SendInfo(aSetOfFilterDynamicOverloadOneShot: TGroupOfForWhatReasonsToLogActually; const AText: String; AObject: TObject);
var
  sTempStr: String;
  oStream: TStream;
  setOldFilterDynamic: TGroupOfForWhatReasonsToLogActually;
begin
  setOldFilterDynamic:= FsetFilterDynamic_forWhatReasonsToLogActually;
  FsetFilterDynamic_forWhatReasonsToLogActually:= FsetFilterDynamic_forWhatReasonsToLogActually + [lwInfo] + aSetOfFilterDynamicOverloadOneShot;
  if FsetFilterDynamic_forWhatReasonsToLogActually = [] then Exit;   //pre-conditions

  try
    oStream := nil;
    sTempStr := AText + ' [';
    if AObject <> nil then
    begin
      if AObject is TComponent then begin
        oStream := TMemoryStream.Create;
        oStream.WriteComponent(TComponent(AObject));
      end
      else
        sTempStr := sTempStr + GetObjectDescription(AObject) + ' / ';
    end;
    sTempStr := sTempStr + ('$' + HexStr(AObject) + ']');
    SendStream(methObject, sTempStr, oStream);
  finally
    oStream.Free;
    FsetFilterDynamic_forWhatReasonsToLogActually:= setOldFilterDynamic;
  end;
end;


procedure TIntegratedLogger.SendPointer(aSetOfFilterDynamicOverloadOneShot: TGroupOfForWhatReasonsToLogActually; const AText: String; APointer: Pointer);
var
	setOldFilterDynamic: TGroupOfForWhatReasonsToLogActually;
begin
	setOldFilterDynamic:= FsetFilterDynamic_forWhatReasonsToLogActually;
  FsetFilterDynamic_forWhatReasonsToLogActually:= FsetFilterDynamic_forWhatReasonsToLogActually + aSetOfFilterDynamicOverloadOneShot;
  if FsetFilterDynamic_forWhatReasonsToLogActually = [] then Exit;   //pre-conditions

  SendStream(methValue, AText + ' = $' + HexStr(APointer), nil);
  FsetFilterDynamic_forWhatReasonsToLogActually:= setOldFilterDynamic;
end;


procedure TIntegratedLogger.SendCallStack(aSetOfFilterDynamicOverloadOneShot: TGroupOfForWhatReasonsToLogActually; const AText: String);
var
  oStream: TStream;
	setOldFilterDynamic: TGroupOfForWhatReasonsToLogActually;
begin
	setOldFilterDynamic:= FsetFilterDynamic_forWhatReasonsToLogActually;
  FsetFilterDynamic_forWhatReasonsToLogActually:= FsetFilterDynamic_forWhatReasonsToLogActually + aSetOfFilterDynamicOverloadOneShot;
  if FsetFilterDynamic_forWhatReasonsToLogActually = [] then Exit;   //pre-conditions

  try
    oStream:=TMemoryStream.Create;
    GetCallStack(oStream);
    SendStream(methCallStack,AText,oStream);	  //nb: SendStream will free oStream
  finally
    oStream.Free;
    FsetFilterDynamic_forWhatReasonsToLogActually:= setOldFilterDynamic;
  end;
end;



function TIntegratedLogger.GetDescriptionOfSQLException(AException: Exception): string;
	{$Include getdescriptionof_sql_exception_user.inc}
end;


procedure TIntegratedLogger.SendException(aSetOfFilterDynamicOverloadOneShot: TGroupOfForWhatReasonsToLogActually; const AText:  String; AException: Exception);
{$ifdef fpc}
var
  i: Integer;
  pFrames: PPointer;
  sStr: String;
	setOldFilterDynamic: TGroupOfForWhatReasonsToLogActually;
{$endif}
begin
  {$ifdef fpc}
 	setOldFilterDynamic:= FsetFilterDynamic_forWhatReasonsToLogActually;
  FsetFilterDynamic_forWhatReasonsToLogActually:= FsetFilterDynamic_forWhatReasonsToLogActually + [lwError] + aSetOfFilterDynamicOverloadOneShot;
  if FsetFilterDynamic_forWhatReasonsToLogActually = [] then Exit;   //pre-conditions

  if (AException <> nil) then
    sStr:= '▶ ' + AException.ClassName + ': ' + AException.Message + LineEnding + GetDescriptionOfSQLException(AException);
  // --NOK-- same info. as below; sStr:= sStr + BackTraceStrFunc(ExceptAddr);
  pFrames:= ExceptFrames;
  for i:= 0 to ExceptFrameCount - 1 do
    sStr:= sStr + LineEnding + BackTraceStrFunc(pFrames[i]);
  SendBuffer(methException,AText,sStr[1],Length(sStr));
  FsetFilterDynamic_forWhatReasonsToLogActually:= setOldFilterDynamic;
  {$endif}
end;



procedure TIntegratedLogger.SendHeapInfo(aSetOfFilterDynamicOverloadOneShot: TGroupOfForWhatReasonsToLogActually; const AText: String);
{$ifdef fpc}
var
  sStr: String;
  setOldFilterDynamic: TGroupOfForWhatReasonsToLogActually;
{$endif}
begin
  {$ifdef fpc}
 	setOldFilterDynamic:= FsetFilterDynamic_forWhatReasonsToLogActually;
  FsetFilterDynamic_forWhatReasonsToLogActually:= FsetFilterDynamic_forWhatReasonsToLogActually + aSetOfFilterDynamicOverloadOneShot;
  if FsetFilterDynamic_forWhatReasonsToLogActually = [] then Exit;   //pre-conditions

  with GetFPCHeapStatus do begin
    sStr:= 'All values are in [bytes]:'+LineEnding
    	+'MaxHeapSize: '+FormatNumber(MaxHeapSize)+LineEnding
      +'MaxHeapUsed: '+FormatNumber(MaxHeapUsed)+LineEnding
      +'CurrHeapSize: '+FormatNumber(CurrHeapSize)+LineEnding
      +'CurrHeapUsed: '+FormatNumber(CurrHeapUsed)+LineEnding
      +'CurrHeapFree: '+FormatNumber(CurrHeapFree);
  end;
  SendBuffer(methHeapInfo,AText,sStr[1],Length(sStr));
  FsetFilterDynamic_forWhatReasonsToLogActually:= setOldFilterDynamic;
  {$endif}
end;


procedure TIntegratedLogger.SendMemory(aSetOfFilterDynamicOverloadOneShot: TGroupOfForWhatReasonsToLogActually; const AText: String; pAddress: Pointer; iSize: LongWord; iOffset: Integer);
var
	setOldFilterDynamic: TGroupOfForWhatReasonsToLogActually;
begin
	setOldFilterDynamic:= FsetFilterDynamic_forWhatReasonsToLogActually;
  FsetFilterDynamic_forWhatReasonsToLogActually:= FsetFilterDynamic_forWhatReasonsToLogActually + aSetOfFilterDynamicOverloadOneShot;
  if FsetFilterDynamic_forWhatReasonsToLogActually = [] then Exit;   //pre-conditions

  if pAddress <> nil then begin
    if iOffset <> 0 then
      pAddress := pAddress + iOffset;
  end
  else begin
    //empty
    pAddress := Self;
    iSize := 0;
  end;
  SendBuffer(methMemory,AText,pAddress^,iSize);
  FsetFilterDynamic_forWhatReasonsToLogActually:= setOldFilterDynamic;
end;


procedure TIntegratedLogger.SendIf(aSetOfFilterDynamicOverloadOneShot: TGroupOfForWhatReasonsToLogActually; const AText: String; bExpression, bIsTrue: Boolean);
var
	setOldFilterDynamic: TGroupOfForWhatReasonsToLogActually;
begin
	setOldFilterDynamic:= FsetFilterDynamic_forWhatReasonsToLogActually;
  FsetFilterDynamic_forWhatReasonsToLogActually:= FsetFilterDynamic_forWhatReasonsToLogActually + aSetOfFilterDynamicOverloadOneShot;
  if FsetFilterDynamic_forWhatReasonsToLogActually = [] then Exit;   //pre-conditions

  SendStream(methConditional,AText,nil);
  FsetFilterDynamic_forWhatReasonsToLogActually:= setOldFilterDynamic;
end;


procedure TIntegratedLogger.SendWarning(aSetOfFilterDynamicOverloadOneShot: TGroupOfForWhatReasonsToLogActually; const AText: String);
var
	setOldFilterDynamic: TGroupOfForWhatReasonsToLogActually;
begin
	setOldFilterDynamic:= FsetFilterDynamic_forWhatReasonsToLogActually;
  FsetFilterDynamic_forWhatReasonsToLogActually:= FsetFilterDynamic_forWhatReasonsToLogActually + [lwWarning] + aSetOfFilterDynamicOverloadOneShot;
  if FsetFilterDynamic_forWhatReasonsToLogActually = [] then Exit;   //pre-conditions

  SendStream(methWarning,AText,nil);
  FsetFilterDynamic_forWhatReasonsToLogActually:= setOldFilterDynamic;
end;


procedure TIntegratedLogger.SendError(aSetOfFilterDynamicOverloadOneShot: TGroupOfForWhatReasonsToLogActually; const AText: String);
var
	setOldFilterDynamic: TGroupOfForWhatReasonsToLogActually;
begin
	setOldFilterDynamic:= FsetFilterDynamic_forWhatReasonsToLogActually;
  FsetFilterDynamic_forWhatReasonsToLogActually:= FsetFilterDynamic_forWhatReasonsToLogActually + [lwError] + aSetOfFilterDynamicOverloadOneShot;
  if FsetFilterDynamic_forWhatReasonsToLogActually = [] then Exit;   //pre-conditions

  SendStream(methError,AText,nil);
  FsetFilterDynamic_forWhatReasonsToLogActually:= setOldFilterDynamic;
end;


procedure TIntegratedLogger.SendCustomData(aSetOfFilterDynamicOverloadOneShot: TGroupOfForWhatReasonsToLogActually; const AText: String; Data: Pointer; CustomDataFunction: TCustomDataNotify);
var
  bDoSend: Boolean;
  sTempStr: String;
	setOldFilterDynamic: TGroupOfForWhatReasonsToLogActually;
begin
	setOldFilterDynamic:= FsetFilterDynamic_forWhatReasonsToLogActually;
  FsetFilterDynamic_forWhatReasonsToLogActually:= FsetFilterDynamic_forWhatReasonsToLogActually + aSetOfFilterDynamicOverloadOneShot;
  if FsetFilterDynamic_forWhatReasonsToLogActually = [] then Exit;   //pre-conditions

  bDoSend:=True;
  sTempStr:=CustomDataFunction(Self,Data,bDoSend);
  if bDoSend then
    SendBuffer(methCustomData,AText,sTempStr[1],Length(sTempStr));
  FsetFilterDynamic_forWhatReasonsToLogActually:= setOldFilterDynamic;
end;


procedure TIntegratedLogger.SendCustomData(aSetOfFilterDynamicOverloadOneShot: TGroupOfForWhatReasonsToLogActually; const AText: String; Data: Pointer; CustomDataFunction: TCustomDataNotifyStatic);
var
  bDoSend: Boolean;
  sTempStr: String;
	setOldFilterDynamic: TGroupOfForWhatReasonsToLogActually;
begin
	setOldFilterDynamic:= FsetFilterDynamic_forWhatReasonsToLogActually;
  FsetFilterDynamic_forWhatReasonsToLogActually:= FsetFilterDynamic_forWhatReasonsToLogActually + aSetOfFilterDynamicOverloadOneShot;
  if FsetFilterDynamic_forWhatReasonsToLogActually = [] then Exit;   //pre-conditions

  bDoSend:=True;
  sTempStr:=CustomDataFunction(Self,Data,bDoSend);
  if bDoSend then
    SendBuffer(methCustomData,AText,sTempStr[1],Length(sTempStr));
  FsetFilterDynamic_forWhatReasonsToLogActually:= setOldFilterDynamic;
end;


procedure TIntegratedLogger.AddCheckPoint(aSetOfFilterDynamicOverloadOneShot: TGroupOfForWhatReasonsToLogActually; const sCheckName: String);
var
  i: Integer;
  pOnObj: PtrInt;
	setOldFilterDynamic: TGroupOfForWhatReasonsToLogActually;
begin
	setOldFilterDynamic:= FsetFilterDynamic_forWhatReasonsToLogActually;
  FsetFilterDynamic_forWhatReasonsToLogActually:= FsetFilterDynamic_forWhatReasonsToLogActually + aSetOfFilterDynamicOverloadOneShot;
  if FsetFilterDynamic_forWhatReasonsToLogActually = [] then Exit;   //pre-conditions

  i:=FoCheckList.IndexOf(sCheckName);
  if i <> -1 then begin
    //Add a custom CheckList
    pOnObj:=PtrInt(FoCheckList.Objects[i])+1;
    FoCheckList.Objects[i]:=TObject(pOnObj);
  end
  else begin
    FoCheckList.AddObject(sCheckName,TObject(0));
    pOnObj:=0;
  end;
  SendStream(methCheckpoint,sCheckName+' #'+IntToStr(pOnObj),nil);
  FsetFilterDynamic_forWhatReasonsToLogActually:= setOldFilterDynamic;
end;


procedure TIntegratedLogger.IncCounter(aSetOfFilterDynamicOverloadOneShot: TGroupOfForWhatReasonsToLogActually; const CounterName: String );
var
  i: Integer;
  pOnObject: PtrInt;
	setOldFilterDynamic: TGroupOfForWhatReasonsToLogActually;
begin
	setOldFilterDynamic:= FsetFilterDynamic_forWhatReasonsToLogActually;
  FsetFilterDynamic_forWhatReasonsToLogActually:= FsetFilterDynamic_forWhatReasonsToLogActually + aSetOfFilterDynamicOverloadOneShot;
  if FsetFilterDynamic_forWhatReasonsToLogActually = [] then Exit;   //pre-conditions

  i := FoCounterList.IndexOf(CounterName);
  if i <> -1 then begin
    pOnObject := PtrInt(FoCounterList.Objects[i]) + 1;
    FoCounterList.Objects[i] := TObject(pOnObject);
  end
  else begin
    FoCounterList.AddObject(CounterName, TObject(1));
    pOnObject := 1;
  end;
  SendStream(methCounter,CounterName+'='+IntToStr(pOnObject),nil);
  FsetFilterDynamic_forWhatReasonsToLogActually:= setOldFilterDynamic;
end;


procedure TIntegratedLogger.DecCounter(aSetOfFilterDynamicOverloadOneShot: TGroupOfForWhatReasonsToLogActually; const CounterName: String);
var
  i: Integer;
  pOnObj: PtrInt;
	setOldFilterDynamic: TGroupOfForWhatReasonsToLogActually;
begin
	setOldFilterDynamic:= FsetFilterDynamic_forWhatReasonsToLogActually;
  FsetFilterDynamic_forWhatReasonsToLogActually:= FsetFilterDynamic_forWhatReasonsToLogActually + aSetOfFilterDynamicOverloadOneShot;
  if FsetFilterDynamic_forWhatReasonsToLogActually = [] then Exit;   //pre-conditions

  i := FoCounterList.IndexOf(CounterName);
  if i <> -1 then begin
    pOnObj := PtrInt(FoCounterList.Objects[i]) - 1;
    FoCounterList.Objects[i] := TObject(pOnObj);
  end
  else begin
    FoCounterList.AddObject(CounterName, TObject(-1));
    pOnObj := -1;
  end;
  SendStream(methCounter,CounterName+'='+IntToStr(pOnObj),nil);
  FsetFilterDynamic_forWhatReasonsToLogActually:= setOldFilterDynamic;
end;


procedure TIntegratedLogger.ResetCounter(aSetOfFilterDynamicOverloadOneShot: TGroupOfForWhatReasonsToLogActually; const CounterName: String);
var
  i: Integer;
	setOldFilterDynamic: TGroupOfForWhatReasonsToLogActually;
begin
	setOldFilterDynamic:= FsetFilterDynamic_forWhatReasonsToLogActually;
  FsetFilterDynamic_forWhatReasonsToLogActually:= FsetFilterDynamic_forWhatReasonsToLogActually + aSetOfFilterDynamicOverloadOneShot;
  if FsetFilterDynamic_forWhatReasonsToLogActually = [] then Exit;   //pre-conditions

  i := FoCounterList.IndexOf(CounterName);
  if i <> -1 then begin
    FoCounterList.Objects[i] := TObject(0);
    SendStream(methCounter, FoCounterList[i] + '=0', nil);
  end;
  FsetFilterDynamic_forWhatReasonsToLogActually:= setOldFilterDynamic;
end;

function TIntegratedLogger.GetCounter(const CounterName: String): Integer;
var
  i: Integer;
begin
  i := FoCounterList.IndexOf(CounterName);
  if i <> -1 then
    Result := PtrInt(FoCounterList.Objects[i])
  else
    Result := 0;
end;


procedure TIntegratedLogger.ResetCheckPoint(aSetOfFilterDynamicOverloadOneShot: TGroupOfForWhatReasonsToLogActually; const CheckName:String);
var
  i: Integer;
	setOldFilterDynamic: TGroupOfForWhatReasonsToLogActually;
begin
	setOldFilterDynamic:= FsetFilterDynamic_forWhatReasonsToLogActually;
  FsetFilterDynamic_forWhatReasonsToLogActually:= FsetFilterDynamic_forWhatReasonsToLogActually + aSetOfFilterDynamicOverloadOneShot;
  if FsetFilterDynamic_forWhatReasonsToLogActually = [] then Exit;   //pre-conditions

  i:= FoCheckList.IndexOf(CheckName);
  if i <> -1 then begin
    FoCheckList.Objects[i] := TObject(0);
    SendStream(methCheckpoint, CheckName+' #0',nil);
  end;
  FsetFilterDynamic_forWhatReasonsToLogActually:= setOldFilterDynamic;
end;


procedure TIntegratedLogger.EnterMethod(aSetOfFilterDynamicOverloadOneShot: TGroupOfForWhatReasonsToLogActually; Sender: TObject; const AMethodName: String; const AMessage: String);
var
  sAText: String;
	setOldFilterDynamic: TGroupOfForWhatReasonsToLogActually;
begin
	setOldFilterDynamic:= FsetFilterDynamic_forWhatReasonsToLogActually;
  FsetFilterDynamic_forWhatReasonsToLogActually:= FsetFilterDynamic_forWhatReasonsToLogActually + [lwStudyChainedEvents, lwEvents] + aSetOfFilterDynamicOverloadOneShot;
  if FsetFilterDynamic_forWhatReasonsToLogActually = [] then Exit;   //pre-conditions

  FoLogStack.Insert(0, UpperCase(AMethodName));
  if AMessage <> '' then
    sAText := AMessage
  else if Sender <> nil then
    sAText := GetObjectDescription(Sender) + '.' + AMethodName
  else
    sAText := AMethodName;
  SendStream(methEnterMethod, sAText, nil);
  FsetFilterDynamic_forWhatReasonsToLogActually:= setOldFilterDynamic;
end;


procedure TIntegratedLogger.ExitMethod(aSetOfFilterDynamicOverloadOneShot: TGroupOfForWhatReasonsToLogActually; Sender: TObject; const AMethodName: String; const AMessage: String);
var
  i: Integer;
  sAText: String;
	setOldFilterDynamic: TGroupOfForWhatReasonsToLogActually;
begin
	setOldFilterDynamic:= FsetFilterDynamic_forWhatReasonsToLogActually;
  //ensure that ExitMethod will be called always even if there's an unpaired Entermethod (!)
  if FoLogStack.Count = 0 then Exit;
  FsetFilterDynamic_forWhatReasonsToLogActually:= FsetFilterDynamic_forWhatReasonsToLogActually + [lwStudyChainedEvents, lwEvents] + aSetOfFilterDynamicOverloadOneShot;
  if FsetFilterDynamic_forWhatReasonsToLogActually = [] then Exit;   //pre-conditions

  //todo: see if is necessary to do Uppercase (set case sensitive to false?)
  i := FoLogStack.IndexOf(UpperCase(AMethodName));
  if i <> -1 then
    FoLogStack.Delete(i)
  else
    Exit;

  if AMessage <> '' then
    sAText := AMessage
  else if Sender <> nil then
    sAText := GetObjectDescription(Sender) + '.' + AMethodName
  else
    sAText := AMethodName;
  SendStream(methExitMethod, sAText, nil);
  FsetFilterDynamic_forWhatReasonsToLogActually:= setOldFilterDynamic;
end;


procedure TIntegratedLogger.Watch(aSetOfFilterDynamicOverloadOneShot: TGroupOfForWhatReasonsToLogActually; const AText: String; AValue: Integer);
var
	setOldFilterDynamic: TGroupOfForWhatReasonsToLogActually;
begin
	setOldFilterDynamic:= FsetFilterDynamic_forWhatReasonsToLogActually;
  FsetFilterDynamic_forWhatReasonsToLogActually:= FsetFilterDynamic_forWhatReasonsToLogActually + aSetOfFilterDynamicOverloadOneShot;
  if FsetFilterDynamic_forWhatReasonsToLogActually = [] then Exit;   //pre-conditions

  SendStream(methWatch,AText+'='+IntToStr(AValue),nil);
  FsetFilterDynamic_forWhatReasonsToLogActually:= setOldFilterDynamic;
end;


{$ifdef fpc}
procedure TIntegratedLogger.Watch(aSetOfFilterDynamicOverloadOneShot: TGroupOfForWhatReasonsToLogActually; const AText: String; AValue: Cardinal);
var
	setOldFilterDynamic: TGroupOfForWhatReasonsToLogActually;
begin
	setOldFilterDynamic:= FsetFilterDynamic_forWhatReasonsToLogActually;
  FsetFilterDynamic_forWhatReasonsToLogActually:= FsetFilterDynamic_forWhatReasonsToLogActually + aSetOfFilterDynamicOverloadOneShot;
  if FsetFilterDynamic_forWhatReasonsToLogActually = [] then Exit;   //pre-conditions

  SendStream(methWatch,AText+'='+IntToStr(AValue),nil);
  FsetFilterDynamic_forWhatReasonsToLogActually:= setOldFilterDynamic;
end;
{$endif}


procedure TIntegratedLogger.Watch(aSetOfFilterDynamicOverloadOneShot: TGroupOfForWhatReasonsToLogActually; const AText: String; AValue: Double);
var
	setOldFilterDynamic: TGroupOfForWhatReasonsToLogActually;
begin
	setOldFilterDynamic:= FsetFilterDynamic_forWhatReasonsToLogActually;
  FsetFilterDynamic_forWhatReasonsToLogActually:= FsetFilterDynamic_forWhatReasonsToLogActually + aSetOfFilterDynamicOverloadOneShot;
  if FsetFilterDynamic_forWhatReasonsToLogActually = [] then Exit;   //pre-conditions

  SendStream(methWatch,AText+'='+FloatToStr(AValue),nil);
  FsetFilterDynamic_forWhatReasonsToLogActually:= setOldFilterDynamic;
end;


procedure TIntegratedLogger.Watch(aSetOfFilterDynamicOverloadOneShot: TGroupOfForWhatReasonsToLogActually; const AText: String; AValue: Boolean);
var
	setOldFilterDynamic: TGroupOfForWhatReasonsToLogActually;
begin
	setOldFilterDynamic:= FsetFilterDynamic_forWhatReasonsToLogActually;
  FsetFilterDynamic_forWhatReasonsToLogActually:= FsetFilterDynamic_forWhatReasonsToLogActually + aSetOfFilterDynamicOverloadOneShot;
  if FsetFilterDynamic_forWhatReasonsToLogActually = [] then Exit;   //pre-conditions

  SendStream(methWatch,AText+'='+BoolToStr(AValue),nil);
  FsetFilterDynamic_forWhatReasonsToLogActually:= setOldFilterDynamic;
end;


procedure TIntegratedLogger.Watch(aSetOfFilterDynamicOverloadOneShot: TGroupOfForWhatReasonsToLogActually; const AText, AValue: String);
var
	setOldFilterDynamic: TGroupOfForWhatReasonsToLogActually;
begin
	setOldFilterDynamic:= FsetFilterDynamic_forWhatReasonsToLogActually;
  FsetFilterDynamic_forWhatReasonsToLogActually:= FsetFilterDynamic_forWhatReasonsToLogActually + aSetOfFilterDynamicOverloadOneShot;
  if FsetFilterDynamic_forWhatReasonsToLogActually = [] then Exit;   //pre-conditions

  SendStream(methWatch,AText+'='+AValue,nil);
  FsetFilterDynamic_forWhatReasonsToLogActually:= setOldFilterDynamic;
end;


procedure TIntegratedLogger.SubEventBetweenEnterAndExitMethods(aSetOfFilterDynamicOverloadOneShot: TGroupOfForWhatReasonsToLogActually; const AText: String);
var
	setOldFilterDynamic: TGroupOfForWhatReasonsToLogActually;
begin
	setOldFilterDynamic:= FsetFilterDynamic_forWhatReasonsToLogActually;
  FsetFilterDynamic_forWhatReasonsToLogActually:= FsetFilterDynamic_forWhatReasonsToLogActually + [lwStudyChainedEvents] + aSetOfFilterDynamicOverloadOneShot;
  if FsetFilterDynamic_forWhatReasonsToLogActually = [] then Exit;   //pre-conditions

  SendStream(methSubEventBetweenEnterAndExitMethods,AText,nil);
  FsetFilterDynamic_forWhatReasonsToLogActually:= setOldFilterDynamic;
end;


{ TChannelList }

function TChannelList.GetCount: Integer;
begin
  Result := FoList.Count;
end;

function TChannelList.GetItems(AIndex:Integer): TLogChannel;
begin
  Result := TLogChannel(FoList[AIndex]);
end;

constructor TChannelList.Create;
begin
  FoList := TFPList.Create;
end;

destructor TChannelList.Destroy;
var
  i: Integer;
begin
  //free the registered channels
  for i := FoList.Count - 1 downto 0 do
    Items[i].Free;
  FoList.Destroy;
end;

function TChannelList.Add(AChannel: TLogChannel):Integer;
begin
  Result := FoList.Add(AChannel);
  AChannel.Init;
end;

procedure TChannelList.Remove(AChannel: TLogChannel);
begin
  FoList.Remove(AChannel);
end;


{ TLogChannel }

procedure TLogChannel.Set_ShowFilterDynamic_forWhatReasonsToLogActually(AValue: Boolean);
begin
  if FbShow_DynamicFilter_forWhatReasonsToLogActually=AValue then Exit;
  FbShow_DynamicFilter_forWhatReasonsToLogActually:= AValue;
end;

procedure TLogChannel.SetShowTime(const AValue: Boolean);
begin
  FbShowTime:= AValue;
end;


procedure TLogChannel.Init;
begin
		//must be overriden in its descendants
end;



procedure TLogChannelWrapper.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and
  		(FoChannel <> nil) then
    FoChannel.Active := False;
end;

initialization
  goLogger:= TIntegratedLogger.Create;
finalization
  TIntegratedLogger.FoDefaultChannels.Free;
  goLogger.Free; goLogger:= nil;

end.

