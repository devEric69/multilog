unit MultiLog;
{$REGION 'Xls: Comments section'}
{§< @HTML(
<b>Main unit of the Multilog logging system.</b>  <br>
<span style="color: #4169E1;">author of MultiLog: Luiz Américo Pereira Câmara; pascalive@bol.com.br</span>  <br><br>

<div style="border: solid windowtext .5pt; padding: 1.0pt 4.0pt 1.0pt 4.0pt;">
nb1: all units are encoded with UTF-8 without BOM, using EDI "file settings\encoding\UTF-8" contextual menu.  <br>
nb2: this HTML documentation has been made with PasDoc - program named "pasdoc_gui". The comment marker is the character "§",
to include only comments that start with this merker, a documentation tool for the Object Pascal code:
<a href="https://github.com/pasdoc/pasdoc/wiki">https://github.com/pasdoc/pasdoc/wiki</a> . The pasdoc_gui's configuration file,
for those who want to update the documentation, is named "config.pds".
</div>)
}
{$ENDREGION}

{
  Main unit of the Multilog logging system

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
  {$ifndef fpc}Types, fpccompat,{$endif} Classes, SysUtils, syncobjs, math;

{$REGION 'Xls: Comments section'}
{§ MessageTypes  @br
  Below is the list of the Logger ***meth***od's list to log:
}
{$ENDREGION}
const
	methInfo				= 0;
  methError   		= 1;
  methWarning 		= 2;
  methValue 			= 3;
  methEnterMethod = 4;
  methExitMethod  = 5;
  methConditional = 6;
  methCheckpoint 	= 7;
  methTStrings 		= 8;
  methCallStack 	= 9;
  methObject 			= 10;
  methException 	= 11;
  methBitmap 			= 12;
  methHeapInfo 		= 13;
  methMemory 			= 14;
  methCustomData 	= 15;
  { hole }
  methWatch 			= 20;
  methCounter 		= 21;
  methColor				= 22;
  {§ We can use ltSubEventBetweenEnterAndExitMethods, to indent the Msg depending of it's level in the callstack, between EnterMethod..ExitMethod }
  methSubEventBetweenEnterAndExitMethods = 23;
  { hole }
  methClear 			= 100;


{$REGION 'Xls: Comments section'}
{§ LogClasses of stats, convention with lc prefix. @br
It's possible to define the constants to suit any need
distribution of statictics classes of WHAT msg are logged.
Here's the lwThings ie *l*ogger*w*hichTrackingPurposes logged list:
}
{$ENDREGION}

type
  TLogger = class;

  TMethodToLog = methInfo..methClear;

  TForWhichTrackingPurpose = (lwDebug=0, lwError, lwInfo, lwWarning, lwEvents,
  																																							lw_5, lw_6, lw_7,
  														lwStudyChainedEvents,
  																																							lw_10,lw_11,lw_12,lw_13,lw_14,lw_15,lw_16,lw_17,lw_18,lw_19,lw_20,lw_21,lw_22,lw_23,lw_24,lw_25,lw_26,lw_27,lw_28,lw_29,lw_30,
  														lwLast);
  TGroupOfForWhichTrackingPurposesMsgAreLogged = Set of TForWhichTrackingPurpose;


const
  lwAll = [lwDebug, lwError, lwInfo, lwWarning, lwEvents,
  				 lw_5, lw_6, lw_7,
  				 lwStudyChainedEvents,
  				 lw_10,lw_11,lw_12,lw_13,lw_14,lw_15,lw_16,lw_17,lw_18,lw_19,lw_20,lw_21,lw_22,lw_23,lw_24,lw_25,lw_26,lw_27,lw_28,lw_29,lw_30,
  				 lwLast];
  lwNone = [];

type
  TLogMessage = record
    iMethUsed: Integer;
    setGroupOfForWhichTrackingPurposesMsgAreLogged: TGroupOfForWhichTrackingPurposesMsgAreLogged;
    dtMsgTime: TDateTime;
    sMsgText: String;
    pData: TStream;
  end;

  TCustomDataNotify = function (Sender: TLogger; Data: Pointer; var DoSend: Boolean): String of Object;
  TCustomDataNotifyStatic = function (Sender: TLogger; Data: Pointer; var DoSend: Boolean): String;

  { TLogChannel }

  TLogChannel = class
  private
    FbActive: Boolean;
  public
    procedure Clear; virtual; abstract;
    procedure Deliver(const AMsg: TLogMessage); virtual; abstract;
    procedure Init; virtual;
    property Active: Boolean read FbActive write FbActive;
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

  { TLogger }


{$REGION 'Xls: Comments section'}
{§ Brief summmary of the processing of a TLogger's mathod call: @br
@html(<pre>
<u>step ❶:</u>
          |                                                              |
Calling program sends events;                               There's a distibution of groups of
each event can have its                                     methods too, each group specialized in
*Why*ThisLogging's justification.                           *How* to forward an event type towards its
So, there's a distribution of groups of                          channel's target
event's justifications
.../...
        (lwEvents)→                                                   (lwEvents)(methEnterMethod)→
          .../...                                                       .../...
        (lwEvents)→                                                   (lwEvents)(methExitMethod)→
   (lwNone)→                                                       (lwNone)(ltInfo)→
       (lwNone)→                                                     (lwNone)(ltInfo)→
  (lwStudyChainedEvents)(methSubEventBetweenEnterAndExitMethods)→                                            (lwStudyChainedEvents)(methSubEventBetweenEnterAndExitMethods)→
     .../...                                                        .../...
     (lwNone)→                                                      (lwNone)(methValue\@integer)→
  (lwStudyChainedEvents)(methSubEventBetweenEnterAndExitMethods)→                                           (lwStudyChainedEvents)(methSubEventBetweenEnterAndExitMethods)→
(lwNone)→                                                     (lwNone)(methValue\@boolean)→
.../...



<u>step ❷:</u>
         |                                                            |
There's a distibution of groups of                            ActiveClasses acts like a wall: if the *How* to forward isn't present
method, each group specialized in                             in the ActiveClasses's "set Of *How*" type, then, the event doesn't go further.
*How* to forward an event type towards its channel's target.  Let say that TargetedConstantGroup1_OfWhatCanBeLogged = [lwNone, methValue]:
.../...                                                               ↺|||
(lwEvents)(methEnterMethod)→                                              (lwEvents)(methEnterMethod)→
.../...                                                               ↺|||
(lwEvents)(methExitMethod)→                                               (lwEvents)(methExitMethod)→
.../...                                                               ↺|||
(lwStudyChainedEvents)(methSubEventBetweenEnterAndExitMethods)→         ↺|||
.../...                                                               ↺|||
  (lwNone)(methValue\@integer)→                                           (lwNone)(methValue\@integer)→
  (lwStudyChainedEvents)(methSubEventBetweenEnterAndExitMethods)→       ↺|||
(lwNone)(methValue\@boolean)→                                             (lwNone)(methValue\@boolean)→
(lwWarning)(methWarning)→                                                                     ↺|||
.../...                                                               ↺|||



<u>step ❸:</u>>
|                                                                                                               |
ActiveClasses acts like a wall: if the *How* to forward isn't present                               For each specialized Channel leading
in the ActiveClasses's "Set Of *How*" type, then, the event doesn't go further.                     to a display medium (TMemo □, TFileText ○,TLogTreeView ▶)
Let say that ActiveClasses = [methEnterMethod, methExitMethod, methValue]:                                will receive the msg and display it.
|
(lwEvents)(methEnterMethod)→                                                                                    ○ + □
|
(lwEvents)(methExitMethod)→                                                                                       □
|
|   .../...
|
(lwNone)(methValue\@integer)→                                                                                    ○
|
(lwNone)(methValue\@boolean)→                                                                                    ○ + ▶
.../...
</pre>)
}
{$ENDREGION}
  TLogger = class
  private
    FiMaxStackCount: Integer;
    FoChannels: TChannelList;
    FoLogStack: TStrings;
    FoCheckList: TStringList;
    FoCounterList: TStringList;
    FOnCustomData: TCustomDataNotify;
    FsetLast_TargetedConstantGroup1_OfWhatCanBeLogged: TGroupOfForWhichTrackingPurposesMsgAreLogged; // for remind
		FsetForWhichTrackingPurposesMsgAreLogged: TGroupOfForWhichTrackingPurposesMsgAreLogged;
    class var FoDefaultChannels: TChannelList;
    procedure GetCallStack(AStream:TStream);
    procedure SetEnabled(AValue: Boolean);
    class function GetDefaultChannels: TChannelList; static;
    function GetEnabled: Boolean;
    procedure Store_FsetForWhichTrackingPurposesMsgAreLogged(const setGroupIntersection: TGroupOfForWhichTrackingPurposesMsgAreLogged);
    procedure SetMaxStackCount(const AValue: Integer);
    procedure SetThreadSafe;
  protected
    procedure SendStream(AMethodUsed: Integer; const AText: String; o_AStream: TStream);
    procedure SendBuffer(AMethodUsed: Integer; const AText: String; var pBuffer; Count: LongWord);
  public
		{§ public field to allow use of include/exclude functions = [lwDebug, ...] =~ active which can be adjusted contextually in the calling program...}
    TargetedConstantGroup1_OfWhatCanBeLogged: TGroupOfForWhichTrackingPurposesMsgAreLogged;
    {§ public field to allow use of include/exclude functions = [lwDebug, ...] =~ ..."pass-filter"'s set, blocking or not the logging.}
    IntersectingGroup2ForProcWithoutParamGroup2_OfWhatCanBeLogged: TGroupOfForWhichTrackingPurposesMsgAreLogged;
    constructor Create;
    destructor Destroy; override;
    function CalledBy(const AMethodName: String): Boolean;
    procedure Clear;
    //Helper functions
    function RectToStr(const ARect: TRect): String; //inline
    function PointToStr(const APoint: TPoint): String; //inline
    //Send functions
    procedure Send(const AText: String); overload; {$ifdef fpc}inline;{$endif}
{$REGION 'Xls: Comments section'}
{§ Explanations: whe log for a purpose of lwInfo at last.
}
{$ENDREGION}
    procedure Send(IntersectingGroup2_OfWhatCanBeLogged: TGroupOfForWhichTrackingPurposesMsgAreLogged; const AText: String);overload;
{$REGION 'Xls: Comments section'}
{§ Explanations: logging; using public DefaultClasses containing Why this logging = [lwDebug, ...] which must intersect with ActiveClasses,
to pass this WHICH\HOW ltInfo's logging method.
}
{$ENDREGION}
    procedure Send(const AText: String; Args: array of const);overload; {$ifdef fpc}inline;{$endif}
{$REGION 'Xls: Comments section'}
    {§ Explanations: whe log with a ltInfo's method.}
{$ENDREGION}
    procedure Send(IntersectingGroup2_OfWhatCanBeLogged: TGroupOfForWhichTrackingPurposesMsgAreLogged; const AText: String; Args: array of const);overload;
{$REGION 'Xls: Comments section'}
{§ Explanations: logging; using public DefaultClasses containing Why this logging = [lwDebug, ...] which must intersect with ActiveClasses,
to pass this WHICH\HOW methValue's logging method.
}
{$ENDREGION}
    procedure Send(const AText, AValue: String);overload; {$ifdef fpc}inline;{$endif}
{$REGION 'Xls: Comments section'}
    {§ Explanations: whe log with a methValue's method.}
{$ENDREGION}
    procedure Send(IntersectingGroup2_OfWhatCanBeLogged: TGroupOfForWhichTrackingPurposesMsgAreLogged; const AText,AValue: String); overload;
{$REGION 'Xls: Comments section'}
    {§ Explanations: logging; using public DefaultClasses containing Why this logging = [lwDebug, ...] which must intersect with ActiveClasses,
    to pass this WHICH\HOW methValue's logging method.}
{$ENDREGION}
    procedure Send(const AText: String; AValue: Integer); overload; {$ifdef fpc}inline;{$endif}
{$REGION 'Xls: Comments section'}
	{§ Explanations: whe log with a methValue's method.}
{$ENDREGION}
    procedure Send(IntersectingGroup2_OfWhatCanBeLogged: TGroupOfForWhichTrackingPurposesMsgAreLogged; const AText: String; AValue: Integer);overload;
    {$ifdef fpc}
{$REGION 'Xls: Comments section'}
{§ Explanations: logging; using public DefaultClasses containing Why this logging = [lwDebug, ...] which must intersect with ActiveClasses,
to pass this WHICH\HOW methValue's logging method.}
{$ENDREGION}
    procedure Send(const AText: String; AValue: Cardinal); overload; {$ifdef fpc}inline;{$endif}
{$REGION 'Xls: Comments section'}
  {§ Explanations: whe log with a methValue's method.}
{$ENDREGION}
    procedure Send(IntersectingGroup2_OfWhatCanBeLogged: TGroupOfForWhichTrackingPurposesMsgAreLogged; const AText: String; AValue: Cardinal);overload;
    {$endif}
{$REGION 'Xls: Comments section'}
{§ Explanations: logging; using public DefaultClasses containing Why this logging = [lwDebug, ...] which must intersect with ActiveClasses,
to pass this WHICH\HOW methValue's logging method.}
{$ENDREGION}
    procedure Send(const AText: String; AValue: Double); overload; {$ifdef fpc}inline;{$endif}
{$REGION 'Xls: Comments section'}
  {§ Explanations: whe log with a methValue's method.}
{$ENDREGION}
    procedure Send(IntersectingGroup2_OfWhatCanBeLogged: TGroupOfForWhichTrackingPurposesMsgAreLogged; const AText: String; AValue: Double);overload;
{$REGION 'Xls: Comments section'}
{§ Explanations: logging; using public DefaultClasses containing Why this logging = [lwDebug, ...] which must intersect with ActiveClasses,
to pass this WHICH\HOW methValue's logging method.}
{$ENDREGION}
    procedure Send(const AText: String; AValue: Int64); overload; {$ifdef fpc}inline;{$endif}
{$REGION 'Xls: Comments section'}
  {§ Explanations: whe log with a methValue's method.}
{$ENDREGION}
    procedure Send(IntersectingGroup2_OfWhatCanBeLogged: TGroupOfForWhichTrackingPurposesMsgAreLogged; const AText: String; AValue: Int64);overload;
{$REGION 'Xls: Comments section'}
{§ Explanations: logging; using public DefaultClasses containing Why this logging = [lwDebug, ...] which must intersect with ActiveClasses,
to pass this WHICH\HOW methValue's logging method.}
{$ENDREGION}
    procedure Send(const AText: String; AValue: QWord); overload; {$ifdef fpc}inline;{$endif}
{$REGION 'Xls: Comments section'}
  {§ Explanations: whe log with a methValue's method.}
{$ENDREGION}
    procedure Send(IntersectingGroup2_OfWhatCanBeLogged: TGroupOfForWhichTrackingPurposesMsgAreLogged; const AText: String; AValue: QWord);overload;
{$REGION 'Xls: Comments section'}
{§ Explanations: logging; using public DefaultClasses containing Why this logging = [lwDebug, ...] which must intersect with ActiveClasses,
to pass this WHICH\HOW methValue's logging method.}
{$ENDREGION}
    procedure Send(const AText: String; AValue: Boolean); overload; {$ifdef fpc}inline;{$endif}
{$REGION 'Xls: Comments section'}
  {§ Explanations: whe log with a methValue's method.}
{$ENDREGION}
    procedure Send(IntersectingGroup2_OfWhatCanBeLogged: TGroupOfForWhichTrackingPurposesMsgAreLogged; const AText: String; AValue: Boolean);overload;
{$REGION 'Xls: Comments section'}
{§ Explanations: logging; using public DefaultClasses containing Why this logging = [lwDebug, ...] which must intersect with ActiveClasses,
to pass this WHICH\HOW methValue's logging method.}
{$ENDREGION}
    procedure Send(const AText: String; const ARect: TRect); overload; {$ifdef fpc}inline;{$endif}
{$REGION 'Xls: Comments section'}
  {§ Explanations: whe log with a methValue's method.}
{$ENDREGION}
    procedure Send(IntersectingGroup2_OfWhatCanBeLogged: TGroupOfForWhichTrackingPurposesMsgAreLogged; const AText: String; const ARect: TRect);overload;
{$REGION 'Xls: Comments section'}
{§ Explanations: logging; using public DefaultClasses containing Why this logging = [lwDebug, ...] which must intersect with ActiveClasses,
to pass this WHICH\HOW methValue's logging method.}
{$ENDREGION}
    procedure Send(const AText: String; const APoint: TPoint); overload; {$ifdef fpc}inline;{$endif}
{$REGION 'Xls: Comments section'}
  {§ Explanations: whe log with a methValue's method.}
{$ENDREGION}
    procedure Send(IntersectingGroup2_OfWhatCanBeLogged: TGroupOfForWhichTrackingPurposesMsgAreLogged; const AText: String; const APoint: TPoint);overload;
{$REGION 'Xls: Comments section'}
{§ Explanations: logging; using public DefaultClasses containing Why this logging = [lwDebug, ...] which must intersect with ActiveClasses,
to pass this WHICH\HOW methTStrings's logging method.}
{$ENDREGION}
    procedure Send(const AText: String; AStrList: TStrings); overload; {$ifdef fpc}inline;{$endif}
    procedure Send(IntersectingGroup2_OfWhatCanBeLogged: TGroupOfForWhichTrackingPurposesMsgAreLogged; const AText: String; o_AStrList: TStrings);overload;
{$REGION 'Xls: Comments section'}
	{§ Explanations: logging; using public DefaultClasses containing Why this logging = [lwDebug, ...] which must intersect with ActiveClasses,
	to pass this WHICH\HOW methObject's logging method.}
{$ENDREGION}
    procedure Send(const AText: String; AObject: TObject); overload; {$ifdef fpc}inline;{$endif}
{$REGION 'Xls: Comments section'}
  {§ Explanations: whe log with a methObject's method.}
{$ENDREGION}
    procedure Send(IntersectingGroup2_OfWhatCanBeLogged: TGroupOfForWhichTrackingPurposesMsgAreLogged; const AText: String; AObject: TObject);overload;
{$REGION 'Xls: Comments section'}
{§ Explanations: logging; using public DefaultClasses containing Why this logging = [lwDebug, ...] which must intersect with ActiveClasses,
to pass this WHICH\HOW methValue's logging method.}
{$ENDREGION}
    procedure SendPointer(const AText: String; APointer: Pointer); overload; {$ifdef fpc}inline;{$endif}
{$REGION 'Xls: Comments section'}
  {§ Explanations: whe log with a methValue's method.}
{$ENDREGION}
    procedure SendPointer(IntersectingGroup2_OfWhatCanBeLogged: TGroupOfForWhichTrackingPurposesMsgAreLogged; const AText: String; APointer: Pointer);overload;
{$REGION 'Xls: Comments section'}
{§ Explanations: logging; using public DefaultClasses containing Why this logging = [lwDebug, ...] which must intersect with ActiveClasses,
to pass this WHICH\HOW methCallStack's logging method.}
{$ENDREGION}
    procedure SendCallStack(const AText: String); overload; {$ifdef fpc}inline;{$endif}
{$REGION 'Xls: Comments section'}
{§ Explanations: whe log with a methCallStack's method.}
{$ENDREGION}
    procedure SendCallStack(IntersectingGroup2_OfWhatCanBeLogged: TGroupOfForWhichTrackingPurposesMsgAreLogged; const AText: String);overload;
{$REGION 'Xls: Comments section'}
{§ Explanations: logging; using public DefaultClasses containing Why this logging = [lwDebug, ...] which must intersect with ActiveClasses,
to pass this WHICH\HOW methException's logging method.}
{$ENDREGION}
    procedure SendException(const AText: String; AException: Exception);overload; {$ifdef fpc}inline;{$endif}
{$REGION 'Xls: Comments section'}
{§ Explanations: whe log with a methException's method.}
{$ENDREGION}
    procedure SendException(IntersectingGroup2_OfWhatCanBeLogged: TGroupOfForWhichTrackingPurposesMsgAreLogged; const AText: String; AException: Exception);overload;
{$REGION 'Xls: Comments section'}
{§ Explanations: check Exception hierarchy (ultimate ancestor=EDatabaseError || EStreamError || ...), to grab its specific fields into a dumped string.}
{$ENDREGION}
    function GetExceptionDescriptionFields(AException: Exception): string;
{$REGION 'Xls: Comments section'}
{§ Explanations: logging; using public DefaultClasses containing Why this logging = [lwDebug, ...] which must intersect with ActiveClasses,
to pass this WHICH\HOW methHeapInfo's logging method.}
{$ENDREGION}
    procedure SendHeapInfo(const AText: String); overload; {$ifdef fpc}inline;{$endif}
{$REGION 'Xls: Comments section'}
{§ Explanations: whe log with a methHeapInfo's method.}
{$ENDREGION}
    procedure SendHeapInfo(IntersectingGroup2_OfWhatCanBeLogged: TGroupOfForWhichTrackingPurposesMsgAreLogged; const AText: String);overload;
{$REGION 'Xls: Comments section'}
{§ Explanations: logging; using public DefaultClasses containing Why this logging = [lwDebug, ...] which must intersect with ActiveClasses,
to pass this WHICH\HOW methMemory's logging method.}
{$ENDREGION}
    procedure SendMemory(const AText: String; Address: Pointer; Size: LongWord; Offset: Integer = 0); overload; {$ifdef fpc}inline;{$endif}
{$REGION 'Xls: Comments section'}
{§ Explanations: whe log with a methMemory's method.}
{$ENDREGION}
    procedure SendMemory(IntersectingGroup2_OfWhatCanBeLogged: TGroupOfForWhichTrackingPurposesMsgAreLogged; const AText: String; pAddress: Pointer; iSize: LongWord; iOffset: Integer = 0);overload;
{$REGION 'Xls: Comments section'}
{§ Explanations: logging; using public DefaultClasses containing Why this logging = [lwDebug, ...] which must intersect with ActiveClasses,
to pass this WHICH\HOW methConditional's logging method.}
{$ENDREGION}
    procedure SendIf(const AText: String; Expression: Boolean); overload; {$ifdef fpc}inline;{$endif}
{$REGION 'Xls: Comments section'}
{§ Explanations: logging;
this methConditional method with overloaded specific parameter Classes, that allows us to pass or not [lwDebug, ...],
to verify an existing intersection with ActiveClasses.}
{$ENDREGION}
    procedure SendIf(Classes: TGroupOfForWhichTrackingPurposesMsgAreLogged; const AText: String; Expression: Boolean); overload;
{$REGION 'Xls: Comments section'}
{§ Explanations: logging; using public DefaultClasses containing Why this logging = [lwDebug, ...] which must intersect with ActiveClasses,
to pass this WHICH\HOW methConditional's logging method.}
{$ENDREGION}
    procedure SendIf(const AText: String; Expression, IsTrue: Boolean); overload; {$ifdef fpc}inline;{$endif}
{$REGION 'Xls: Comments section'}
{§ Explanations: whe log with a methConditional's method.}
{$ENDREGION}
    procedure SendIf(IntersectingGroup2_OfWhatCanBeLogged: TGroupOfForWhichTrackingPurposesMsgAreLogged; const AText: String; bExpression, bIsTrue: Boolean);overload;
{$REGION 'Xls: Comments section'}
{§ Explanations: logging; using public DefaultClasses containing Why this logging = [lwDebug, ...] which must intersect with ActiveClasses,
to pass this WHICH\HOW ltWarning's logging method.}
{$ENDREGION}
    procedure SendWarning(const AText: String); overload; {$ifdef fpc}inline;{$endif}
{$REGION 'Xls: Comments section'}
{§ Explanations: whe log with a ltWarning's method.}
{$ENDREGION}
    procedure SendWarning(IntersectingGroup2_OfWhatCanBeLogged: TGroupOfForWhichTrackingPurposesMsgAreLogged; const AText: String);overload;
{$REGION 'Xls: Comments section'}
{§ Explanations: logging; using public DefaultClasses containing Why this logging = [lwDebug, ...] which must intersect with ActiveClasses,
to pass this WHICH\HOW ltError's logging method.}
{$ENDREGION}
    procedure SendError(const AText: String); overload; {$ifdef fpc}inline;{$endif}
{$REGION 'Xls: Comments section'}
{§ Explanations: whe log with a ltError's method.}
{$ENDREGION}
    procedure SendError(IntersectingGroup2_OfWhatCanBeLogged: TGroupOfForWhichTrackingPurposesMsgAreLogged; const AText: String);overload;
{$REGION 'Xls: Comments section'}
{§ Explanations: logging; using public DefaultClasses containing Why this logging = [lwDebug, ...] which must intersect with ActiveClasses,
to pass this WHICH\HOW methCustomData's logging method.}
{$ENDREGION}
    procedure SendCustomData(const AText: String; Data: Pointer);overload; {$ifdef fpc}inline;{$endif}
{$REGION 'Xls: Comments section'}
{§ Explanations: logging; using public DefaultClasses containing Why this logging = [lwDebug, ...] which must intersect with ActiveClasses,
to pass this WHICH\HOW methCustomData's logging method.}
{$ENDREGION}
    procedure SendCustomData(const AText: String; Data: Pointer; CustomDataFunction: TCustomDataNotify);overload; {$ifdef fpc}inline;{$endif}
{$REGION 'Xls: Comments section'}
{§ Explanations: whe log with a methCustomData's method.}
{$ENDREGION}
    procedure SendCustomData(IntersectingGroup2_OfWhatCanBeLogged: TGroupOfForWhichTrackingPurposesMsgAreLogged; const AText: String; Data: Pointer; CustomDataFunction: TCustomDataNotify);overload;
{$REGION 'Xls: Comments section'}
{§ Explanations: logging;
this methCustomData method with overloaded specific parameter Classes, that allows us to pass or not [lwDebug, ...],
to verify an existing intersection with ActiveClasses.}
{$ENDREGION}
    procedure SendCustomData(IntersectingGroup2_OfWhatCanBeLogged: TGroupOfForWhichTrackingPurposesMsgAreLogged; const AText: String; Data: Pointer);overload;
{$REGION 'Xls: Comments section'}
{§ Explanations: whe log with a methCustomData's method.}
{$ENDREGION}
    procedure SendCustomData(IntersectingGroup2_OfWhatCanBeLogged: TGroupOfForWhichTrackingPurposesMsgAreLogged; const AText: String; Data: Pointer; CustomDataFunction: TCustomDataNotifyStatic);overload;
{$REGION 'Xls: Comments section'}
{§ Explanations: logging; using public DefaultClasses containing Why this logging = [lwDebug, ...] which must intersect with ActiveClasses,
to pass this WHICH\HOW methCustomData's logging method.}
{$ENDREGION}
    procedure SendCustomData(const AText: String; Data: Pointer; CustomDataFunction: TCustomDataNotifyStatic);overload; {$ifdef fpc}inline;{$endif}
{$REGION 'Xls: Comments section'}
{§ Explanations: logging; using public DefaultClasses containing Why this logging = [lwDebug, ...] which must intersect with ActiveClasses,
to pass this WHICH\HOW methCheckpoint's logging method.}
{$ENDREGION}
    procedure AddCheckPoint;overload; {$ifdef fpc}inline;{$endif}
{$REGION 'Xls: Comments section'}
{§ Explanations: logging;
This methCheckpoint method with overloaded specific parameter Classes, that allows us to pass or not [lwDebug, ...],
to verify an existing intersection with ActiveClasses.}
{$ENDREGION}
    procedure AddCheckPoint(IntersectingGroup2_OfWhatCanBeLogged: TGroupOfForWhichTrackingPurposesMsgAreLogged);overload; {$ifdef fpc}inline;{$endif}
{$REGION 'Xls: Comments section'}
{§ Explanations: logging; using public DefaultClasses containing Why this logging = [lwDebug, ...] which must intersect with ActiveClasses,
to pass this WHICH\HOW methCheckpoint's logging method.}
{$ENDREGION}
    procedure AddCheckPoint(const sCheckName: String);overload; {$ifdef fpc}inline;{$endif}
{$REGION 'Xls: Comments section'}
{§ Explanations: whe log with a methCheckpoint's method.}
{$ENDREGION}
    procedure AddCheckPoint(IntersectingGroup2_OfWhatCanBeLogged: TGroupOfForWhichTrackingPurposesMsgAreLogged; const sCheckName: String);overload;
{$REGION 'Xls: Comments section'}
{§ Explanations: logging; using public DefaultClasses containing Why this logging = [lwDebug, ...] which must intersect with ActiveClasses,
to pass this WHICH\HOW methCounter's logging method.}
{$ENDREGION}
    procedure IncCounter(const sCounterName: String);overload; {$ifdef fpc}inline;{$endif}
{$REGION 'Xls: Comments section'}
{§ Explanations: whe log with a methCounter's method.}
{$ENDREGION}
    procedure IncCounter(IntersectingGroup2_OfWhatCanBeLogged: TGroupOfForWhichTrackingPurposesMsgAreLogged; const CounterName: String);overload;
{$REGION 'Xls: Comments section'}
{§ Explanations: logging; using public DefaultClasses containing Why this logging = [lwDebug, ...] which must intersect with ActiveClasses,
to pass this WHICH\HOW methCounter's logging method.}
{$ENDREGION}
    procedure DecCounter(const sCounterName: String);overload; {$ifdef fpc}inline;{$endif}
{$REGION 'Xls: Comments section'}
{§ Explanations: whe log with a methCounter's method.}
{$ENDREGION}
    procedure DecCounter(IntersectingGroup2_OfWhatCanBeLogged: TGroupOfForWhichTrackingPurposesMsgAreLogged; const CounterName: String);overload;
{$REGION 'Xls: Comments section'}
{§ Explanations: logging; using public DefaultClasses containing Why this logging = [lwDebug, ...] which must intersect with ActiveClasses,
to pass this WHICH\HOW methCounter's logging method.}
{$ENDREGION}
    procedure ResetCounter(const sCounterName: String);overload; {$ifdef fpc}inline;{$endif}
{$REGION 'Xls: Comments section'}
{§ Explanations: whe log with a methCounter's method.}
{$ENDREGION}
    procedure ResetCounter(IntersectingGroup2_OfWhatCanBeLogged: TGroupOfForWhichTrackingPurposesMsgAreLogged; const CounterName: String);overload;
    function GetCounter(const CounterName: String): Integer;
{$REGION 'Xls: Comments section'}
{§ Explanations: logging; using public DefaultClasses containing Why this logging = [lwDebug, ...] which must intersect with ActiveClasses,
to pass this WHICH\HOW methCheckpoint's logging method.}
{$ENDREGION}
    procedure ResetCheckPoint;overload; {$ifdef fpc}inline;{$endif}
{$REGION 'Xls: Comments section'}
{§ Explanations: logging;
This methCheckpoint method with overloaded specific parameter Classes, that allows us to pass or not [lwDebug, ...],
to verify an existing intersection with ActiveClasses.}
{$ENDREGION}
    procedure ResetCheckPoint(Classes: TGroupOfForWhichTrackingPurposesMsgAreLogged);overload;
{$REGION 'Xls: Comments section'}
{§ Explanations: logging; using public DefaultClasses containing Why this logging = [lwDebug, ...] which must intersect with ActiveClasses,
to pass this WHICH\HOW methCheckpoint's logging method.}
{$ENDREGION}
    procedure ResetCheckPoint(const sCheckName: String);overload; {$ifdef fpc}inline;{$endif}
{$REGION 'Xls: Comments section'}
{§ Explanations: whe log with a methCheckpoint's method.}
{$ENDREGION}
    procedure ResetCheckPoint(IntersectingGroup2_OfWhatCanBeLogged: TGroupOfForWhichTrackingPurposesMsgAreLogged;const CheckName: String);overload;
{$REGION 'Xls: Comments section'}
{§ Explanations: logging; using public DefaultClasses containing Why this logging = [lwDebug, ...] which must intersect with ActiveClasses,
to pass this WHICH\HOW ltEntertMethod's logging method.}
{$ENDREGION}
    procedure EnterMethod(const AMethodName: String; const AMessage: String = ''); overload; {$ifdef fpc}inline;{$endif}
{$REGION 'Xls: Comments section'}
{§ Explanations: logging;
This ltEntertMethod method with overloaded specific parameter Classes, that allows us to pass or not [lwDebug, ...],
to verify an existing intersection with ActiveClasses.}
{$ENDREGION}
    procedure EnterMethod(IntersectingGroup2_OfWhatCanBeLogged: TGroupOfForWhichTrackingPurposesMsgAreLogged; const AMethodName: String; const AMessage: String = ''); overload;
{$REGION 'Xls: Comments section'}
{§ Explanations: logging; using public DefaultClasses containing Why this logging = [lwDebug, ...] which must intersect with ActiveClasses,
to pass this WHICH\HOW ltEntertMethod's logging method.}
{$ENDREGION}
    procedure EnterMethod(Sender: TObject; const AMethodName: String; const AMessage: String = ''); overload; {$ifdef fpc}inline;{$endif}
{$REGION 'Xls: Comments section'}
{§ Explanations: whe log with a methEnterMethod's method.}
{$ENDREGION}
    procedure EnterMethod(IntersectingGroup2_OfWhatCanBeLogged: TGroupOfForWhichTrackingPurposesMsgAreLogged; Sender: TObject; const AMethodName: String; const AMessage: String = '');overload;
{$REGION 'Xls: Comments section'}
{§ Explanations: logging; using public DefaultClasses containing Why this logging = [lwDebug, ...] which must intersect with ActiveClasses,
to pass this WHICH\HOW methExitMethod's logging method.}
{$ENDREGION}
    procedure ExitMethod(const AMethodName: String; const AMessage: String = ''); overload; {$ifdef fpc}inline;{$endif}
{$REGION 'Xls: Comments section'}
{§ Explanations: logging; using public DefaultClasses containing Why this logging = [lwDebug, ...] which must intersect with ActiveClasses,
to pass this WHICH\HOW methExitMethod's logging method.}
{$ENDREGION}
    procedure ExitMethod(Sender: TObject; const AMethodName: String; const AMessage: String = ''); overload; {$ifdef fpc}inline;{$endif}
{$REGION 'Xls: Comments section'}
{§ Explanations: logging;
This methExitMethod method with overloaded specific parameter Classes, that allows us to pass or not [lwDebug, ...],
to verify an existing intersection with ActiveClasses.}
{$ENDREGION}
    procedure ExitMethod(IntersectingGroup2_OfWhatCanBeLogged: TGroupOfForWhichTrackingPurposesMsgAreLogged; const AMethodName: String; const AMessage: String = ''); overload; {$ifdef fpc}inline;{$endif}
{$REGION 'Xls: Comments section'}
{§ Explanations: whe log with a methExitMethod's method.}
{$ENDREGION}
    procedure ExitMethod({%H-}IntersectingGroup2_OfWhatCanBeLogged: TGroupOfForWhichTrackingPurposesMsgAreLogged; Sender: TObject; const AMethodName: String; const AMessage: String = '');overload;
{$REGION 'Xls: Comments section'}
{§ Explanations: logging; using public DefaultClasses containing Why this logging = [lwDebug, ...] which must intersect with ActiveClasses,
to pass this WHICH\HOW methWatch's logging method.}
{$ENDREGION}
    procedure Watch(const AText, AValue: String); overload; {$ifdef fpc}inline;{$endif}
{$REGION 'Xls: Comments section'}
{§ Explanations: whe log with a methWatch's method.}
{$ENDREGION}
    procedure Watch(IntersectingGroup2_OfWhatCanBeLogged: TGroupOfForWhichTrackingPurposesMsgAreLogged; const AText,AValue: String);overload;
{$REGION 'Xls: Comments section'}
{§ Explanations: logging; using public DefaultClasses containing Why this logging = [lwDebug, ...] which must intersect with ActiveClasses,
to pass this WHICH\HOW methWatch's logging method.}
{$ENDREGION}
    procedure Watch(const AText: String; AValue: Integer); overload; {$ifdef fpc}inline;{$endif}
{$REGION 'Xls: Comments section'}
{§ Explanations: whe log with a methWatch's method.}
{$ENDREGION}
    procedure Watch(IntersectingGroup2_OfWhatCanBeLogged: TGroupOfForWhichTrackingPurposesMsgAreLogged; const AText: String; AValue: Integer);overload;
    {$ifdef fpc}
{$REGION 'Xls: Comments section'}
{§ Explanations: logging; using public DefaultClasses containing Why this logging = [lwDebug, ...] which must intersect with ActiveClasses,
to pass this WHICH\HOW methCheckpoint's logging method.}
{$ENDREGION}
    procedure Watch(const AText: String; AValue: Cardinal); overload; {$ifdef fpc}inline;{$endif}
{$REGION 'Xls: Comments section'}
{§ Explanations: whe log with a methWatch's method.}
{$ENDREGION}
    procedure Watch(IntersectingGroup2_OfWhatCanBeLogged: TGroupOfForWhichTrackingPurposesMsgAreLogged; const AText: String; AValue: Cardinal);overload;
    {$endif}
{$REGION 'Xls: Comments section'}
{§ Explanations: logging; using public DefaultClasses containing Why this logging = [lwDebug, ...] which must intersect with ActiveClasses,
to pass this WHICH\HOW methCheckpoint's logging method.}
{$ENDREGION}
    procedure Watch(const AText: String; AValue: Double); overload; {$ifdef fpc}inline;{$endif}
{$REGION 'Xls: Comments section'}
{§ Explanations: whe log with a methWatch's method.}
{$ENDREGION}
    procedure Watch(IntersectingGroup2_OfWhatCanBeLogged: TGroupOfForWhichTrackingPurposesMsgAreLogged; const AText: String; AValue: Double);overload;
{$REGION 'Xls: Comments section'}
{§ Explanations: logging; using public DefaultClasses containing Why this logging = [lwDebug, ...] which must intersect with ActiveClasses,
to pass this WHICH\HOW methCheckpoint's logging method.}
{$ENDREGION}
    procedure Watch(const AText: String; AValue: Boolean); overload; {$ifdef fpc}inline;{$endif}
{$REGION 'Xls: Comments section'}
{§ Explanations: whe log with a methWatch's method.}
{$ENDREGION}
		procedure Watch(IntersectingGroup2_OfWhatCanBeLogged: TGroupOfForWhichTrackingPurposesMsgAreLogged; const AText: String; AValue: Boolean);overload;
{$REGION 'Xls: Comments section'}
{§ Explanations: logging; using public DefaultClasses containing Why this logging = [lwDebug, ...] which must intersect with ActiveClasses,
to pass this WHICH\HOW ltInfo's logging method.}
{$ENDREGION}
    procedure SubEventBetweenEnterAndExitMethods(const AText: String); overload; {$ifdef fpc}inline;{$endif}
{$REGION 'Xls: Comments section'}
{§ Explanations: whe log with a methSubEventBetweenEnterAndExitMethods's method.}
{$ENDREGION}
    procedure SubEventMethodBetweenEnterAndExitMethods(IntersectingGroup2_OfWhatCanBeLogged: TGroupOfForWhichTrackingPurposesMsgAreLogged; const AText: String);overload;
    class property DefaultChannels: TChannelList read GetDefaultChannels;
    property Enabled: Boolean read GetEnabled write SetEnabled;
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

{$REGION 'Xls: Comments section'}
{§ Explanations: if AComponent - a specialized channel, like TMemoChannel - is in a @code(TComponentState = [opRemove]),
and if there's a AComponent's FChannelWrapper that is memory managed by AComponent,
then this FChannelWrapper must stop immediately any activity.}
{$ENDREGION}
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
  goLogger: TLogger;

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


{$REGION 'Xls: Comments section'}
  {§ Explanations: global procedure returns the literal description of the "sender" component that is at the origin of the event.}
{$ENDREGION}
function GetObjectDescription(Sender: TObject): String;
begin
  Result:= Sender.ClassName;
  if (Sender is TComponent) and (TComponent(Sender).Name <> '') then
    Result := Result + '(' + TComponent(Sender).Name + ')';
end;


var
  goGuardian: TGuardian;

{ TLogger }

procedure TLogger.GetCallStack(AStream: TStream);
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

procedure TLogger.SetEnabled(AValue: Boolean);
begin
  if AValue then begin
    if TargetedConstantGroup1_OfWhatCanBeLogged = [] then
      TargetedConstantGroup1_OfWhatCanBeLogged := FsetLast_TargetedConstantGroup1_OfWhatCanBeLogged;
  end
  else begin
    FsetLast_TargetedConstantGroup1_OfWhatCanBeLogged := TargetedConstantGroup1_OfWhatCanBeLogged;
    TargetedConstantGroup1_OfWhatCanBeLogged := [];
  end;
end;

class function TLogger.GetDefaultChannels: TChannelList;
begin
  if FoDefaultChannels = nil then
    FoDefaultChannels := TChannelList.Create;
  Result := FoDefaultChannels;
end;

function TLogger.GetEnabled: Boolean;
begin
  Result:= TargetedConstantGroup1_OfWhatCanBeLogged <> [];
end;

procedure TLogger.Store_FsetForWhichTrackingPurposesMsgAreLogged(const setGroupIntersection: TGroupOfForWhichTrackingPurposesMsgAreLogged);
begin
	FsetForWhichTrackingPurposesMsgAreLogged:= setGroupIntersection;
end;

procedure DispatchLogMessage(o_Channels: TChannelList; const Msg: TLogMessage);
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


procedure TLogger.SendStream(AMethodUsed: Integer; const AText: String; o_AStream: TStream);
var
  recMsg: TLogMessage;
  setOldForWhichTrackingPurposesMsgAreLogged: TGroupOfForWhichTrackingPurposesMsgAreLogged;
begin
  with recMsg do begin
    iMethUsed := AMethodUsed;
    dtMsgTime := Now;
    sMsgText := AText;
    pData := o_AStream;
  end;
  setOldForWhichTrackingPurposesMsgAreLogged:= FsetForWhichTrackingPurposesMsgAreLogged;
  if (AMethodUsed = methSubEventBetweenEnterAndExitMethods) or (AMethodUsed = methEnterMethod) or (AMethodUsed = methExitMethod) then
  	recMsg.setGroupOfForWhichTrackingPurposesMsgAreLogged:= FsetForWhichTrackingPurposesMsgAreLogged + [lwStudyChainedEvents]
	else if (AMethodUsed = methError) or ((AMethodUsed = methException)) then
  	recMsg.setGroupOfForWhichTrackingPurposesMsgAreLogged:= FsetForWhichTrackingPurposesMsgAreLogged + [lwError]
	else if (AMethodUsed = methWarning) then
		recMsg.setGroupOfForWhichTrackingPurposesMsgAreLogged:= FsetForWhichTrackingPurposesMsgAreLogged + [lwWarning]
  else if (AMethodUsed = methInfo) then
		recMsg.setGroupOfForWhichTrackingPurposesMsgAreLogged:= FsetForWhichTrackingPurposesMsgAreLogged + [lwInfo]
	else
  	recMsg.setGroupOfForWhichTrackingPurposesMsgAreLogged:= FsetForWhichTrackingPurposesMsgAreLogged;

  (*IsMultiThread == true, when unit cthreads is used by a project*)
  if IsMultiThread then
  	//Yes: it's a global variable created in this unit, and used in this unit only ;-)
    goGuardian.Enter;
  if FoDefaultChannels <> nil then
    DispatchLogMessage(FoDefaultChannels, recMsg);
  DispatchLogMessage(Channels, recMsg);
  FsetForWhichTrackingPurposesMsgAreLogged:= setOldForWhichTrackingPurposesMsgAreLogged;
  if IsMultiThread then
    goGuardian.Leave;
end;

procedure TLogger.SendBuffer(AMethodUsed: Integer; const AText: String; var pBuffer; Count: LongWord);
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

procedure TLogger.SetMaxStackCount(const AValue: Integer);
begin
  if AValue < 256 then
    FiMaxStackCount := AValue
  else
    FiMaxStackCount := 256;
end;

procedure TLogger.SetThreadSafe;
begin
  if IsMultiThread and not Assigned(goGuardian) then
    goGuardian:= TGuardian.Create
	else if (not IsMultiThread) and Assigned(goGuardian) then
  	FreeAndNil(goGuardian);
end;

constructor TLogger.Create;
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
  TargetedConstantGroup1_OfWhatCanBeLogged := [lwDebug]; 		//categor{y|ies} of What is logged; lwDebug = 0
  IntersectingGroup2ForProcWithoutParamGroup2_OfWhatCanBeLogged := [lwDebug];		//categor{y|ies} of What is logged; lwDebug = 0
end;

destructor TLogger.Destroy;
begin
  FoChannels.Destroy;
  FoLogStack.Destroy;
  FoCheckList.Destroy;
  FoCounterList.Destroy;
  if Assigned(goGuardian) then
    FreeAndNil(goGuardian);
end;

function TLogger.CalledBy(const AMethodName: String): Boolean;
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

procedure TLogger.Clear;
begin
  if FoDefaultChannels <> nil then
    ClearChannels(FoDefaultChannels);
  ClearChannels(Channels);
end;

function TLogger.RectToStr(const ARect: TRect): String;
begin
  with ARect do
    Result:= Format('(Left: %d; Top: %d; Right: %d; Bottom: %d)',[Left,Top,Right,Bottom]);
end;

function TLogger.PointToStr(const APoint: TPoint): String;
begin
  with APoint do
    Result:= Format('(X: %d; Y: %d)',[X,Y]);
end;



procedure TLogger.Send(const AText: String);
begin
  Send(IntersectingGroup2ForProcWithoutParamGroup2_OfWhatCanBeLogged,AText);
end;


procedure TLogger.Send(const AText: String; Args: array of const);
begin
  Send(IntersectingGroup2ForProcWithoutParamGroup2_OfWhatCanBeLogged,AText,Args);
end;


procedure TLogger.Send(const AText, AValue: String);
begin
  Send(IntersectingGroup2ForProcWithoutParamGroup2_OfWhatCanBeLogged,AText,AValue);
end;


procedure TLogger.Send(const AText: String; AValue: Integer);
begin
  Send(IntersectingGroup2ForProcWithoutParamGroup2_OfWhatCanBeLogged,AText,AValue);
end;

{$ifdef fpc}
procedure TLogger.Send(const AText: String; AValue: Cardinal);
begin
  Send(IntersectingGroup2ForProcWithoutParamGroup2_OfWhatCanBeLogged,AText,AValue);
end;
{$endif}


procedure TLogger.Send(const AText: String; AValue: Double);
begin
  Send(IntersectingGroup2ForProcWithoutParamGroup2_OfWhatCanBeLogged,AText,AValue);
end;


procedure TLogger.Send(const AText: String; AValue: Int64);
begin
  Send(IntersectingGroup2ForProcWithoutParamGroup2_OfWhatCanBeLogged,AText,AValue);
end;


procedure TLogger.Send(const AText: String; AValue: QWord);
begin
  Send(IntersectingGroup2ForProcWithoutParamGroup2_OfWhatCanBeLogged,AText,AValue);
end;


procedure TLogger.Send(const AText: String; AValue: Boolean);
begin
  Send(IntersectingGroup2ForProcWithoutParamGroup2_OfWhatCanBeLogged, AText, AValue);
end;


procedure TLogger.Send(const AText: String; const ARect: TRect);
begin
  Send(IntersectingGroup2ForProcWithoutParamGroup2_OfWhatCanBeLogged,AText,ARect);
end;


procedure TLogger.Send(const AText: String; const APoint: TPoint);
begin
  Send(IntersectingGroup2ForProcWithoutParamGroup2_OfWhatCanBeLogged,AText,APoint);
end;


procedure TLogger.Send(const AText: String; AStrList: TStrings);
begin
  Send(IntersectingGroup2ForProcWithoutParamGroup2_OfWhatCanBeLogged,AText,AStrList);
end;


procedure TLogger.Send(const AText: String; AObject: TObject);
begin
  Send(IntersectingGroup2ForProcWithoutParamGroup2_OfWhatCanBeLogged,AText,AObject);
end;


procedure TLogger.SendPointer(const AText: String; APointer: Pointer);
begin
  SendPointer(IntersectingGroup2ForProcWithoutParamGroup2_OfWhatCanBeLogged,AText,APointer);
end;


procedure TLogger.SendCallStack(const AText: String);
begin
  SendCallStack(IntersectingGroup2ForProcWithoutParamGroup2_OfWhatCanBeLogged,AText);
end;


procedure TLogger.SendException(const AText: String; AException: Exception);
begin
  SendException(IntersectingGroup2ForProcWithoutParamGroup2_OfWhatCanBeLogged,AText,AException);
end;


procedure TLogger.SendHeapInfo(const AText: String);
begin
  SendHeapInfo(IntersectingGroup2ForProcWithoutParamGroup2_OfWhatCanBeLogged,AText);
end;


procedure TLogger.SendMemory(const AText: String; Address: Pointer; Size: LongWord; Offset: Integer);
begin
  SendMemory(IntersectingGroup2ForProcWithoutParamGroup2_OfWhatCanBeLogged,AText,Address,Size,Offset)
end;


procedure TLogger.SendIf(const AText: String; Expression: Boolean);
begin
  SendIf(IntersectingGroup2ForProcWithoutParamGroup2_OfWhatCanBeLogged,AText,Expression,True);
end;


procedure TLogger.SendIf(Classes: TGroupOfForWhichTrackingPurposesMsgAreLogged; const AText: String; Expression: Boolean);
begin
  SendIf(Classes,AText,Expression,True);
end;


procedure TLogger.SendIf(const AText: String; Expression, IsTrue: Boolean);
begin
  SendIf(IntersectingGroup2ForProcWithoutParamGroup2_OfWhatCanBeLogged,AText,Expression,IsTrue);
end;


procedure TLogger.SendWarning(const AText: String);
begin
  SendWarning(IntersectingGroup2ForProcWithoutParamGroup2_OfWhatCanBeLogged,AText);
end;


procedure TLogger.SendError(const AText: String);
begin
  SendError(IntersectingGroup2ForProcWithoutParamGroup2_OfWhatCanBeLogged,AText);
end;


procedure TLogger.SendCustomData(const AText: String; Data: Pointer);
begin
  SendCustomData(IntersectingGroup2ForProcWithoutParamGroup2_OfWhatCanBeLogged,AText,Data,FOnCustomData);
end;


procedure TLogger.SendCustomData(IntersectingGroup2_OfWhatCanBeLogged: TGroupOfForWhichTrackingPurposesMsgAreLogged; const AText: String; Data: Pointer);
begin
  SendCustomData(IntersectingGroup2_OfWhatCanBeLogged,AText,Data,FOnCustomData);
end;


procedure TLogger.SendCustomData(const AText: String; Data: Pointer; CustomDataFunction: TCustomDataNotify);
begin
  SendCustomData(IntersectingGroup2ForProcWithoutParamGroup2_OfWhatCanBeLogged,AText,Data,CustomDataFunction);
end;


procedure TLogger.SendCustomData(const AText: String; Data: Pointer; CustomDataFunction: TCustomDataNotifyStatic);
begin
  SendCustomData(IntersectingGroup2ForProcWithoutParamGroup2_OfWhatCanBeLogged,AText,Data,CustomDataFunction);
end;


procedure TLogger.AddCheckPoint;
begin
  AddCheckPoint(IntersectingGroup2ForProcWithoutParamGroup2_OfWhatCanBeLogged,DefaultCheckName);
end;


procedure TLogger.AddCheckPoint(IntersectingGroup2_OfWhatCanBeLogged: TGroupOfForWhichTrackingPurposesMsgAreLogged);
begin
  AddCheckPoint(IntersectingGroup2_OfWhatCanBeLogged,DefaultCheckName);
end;


procedure TLogger.AddCheckPoint(const sCheckName: String);
begin
  AddCheckPoint(IntersectingGroup2ForProcWithoutParamGroup2_OfWhatCanBeLogged,sCheckName);
end;


procedure TLogger.ResetCheckPoint;
begin
  ResetCheckPoint(IntersectingGroup2ForProcWithoutParamGroup2_OfWhatCanBeLogged,DefaultCheckName);
end;


procedure TLogger.ResetCheckPoint(Classes: TGroupOfForWhichTrackingPurposesMsgAreLogged);
begin
  ResetCheckPoint(Classes,DefaultCheckName);
end;


procedure TLogger.ResetCheckPoint(const sCheckName: String);
begin
  ResetCheckPoint(IntersectingGroup2ForProcWithoutParamGroup2_OfWhatCanBeLogged,sCheckName);
end;


procedure TLogger.IncCounter(const sCounterName: String);
begin
  IncCounter(IntersectingGroup2ForProcWithoutParamGroup2_OfWhatCanBeLogged,sCounterName);
end;


procedure TLogger.DecCounter(const sCounterName: String);
begin
  DecCounter(IntersectingGroup2ForProcWithoutParamGroup2_OfWhatCanBeLogged,sCounterName);
end;


procedure TLogger.ResetCounter(const sCounterName: String);
begin
  ResetCounter(IntersectingGroup2ForProcWithoutParamGroup2_OfWhatCanBeLogged,sCounterName);
end;


procedure TLogger.EnterMethod(const AMethodName: String; const AMessage: String);
begin
  EnterMethod(IntersectingGroup2ForProcWithoutParamGroup2_OfWhatCanBeLogged,nil,AMethodName,AMessage);
end;


procedure TLogger.EnterMethod(IntersectingGroup2_OfWhatCanBeLogged: TGroupOfForWhichTrackingPurposesMsgAreLogged; const AMethodName: String; const AMessage: String);
begin
  EnterMethod(IntersectingGroup2_OfWhatCanBeLogged,nil,AMethodName,AMessage);
end;


procedure TLogger.EnterMethod(Sender: TObject; const AMethodName: String; const AMessage: String);
begin
  EnterMethod(IntersectingGroup2ForProcWithoutParamGroup2_OfWhatCanBeLogged,Sender,AMethodName,AMessage);
end;


procedure TLogger.ExitMethod(const AMethodName: String; const AMessage: String);
begin
  ExitMethod(IntersectingGroup2ForProcWithoutParamGroup2_OfWhatCanBeLogged,nil,AMethodName,AMessage);
end;


procedure TLogger.ExitMethod(Sender: TObject; const AMethodName: String; const AMessage: String);
begin
  ExitMethod(IntersectingGroup2ForProcWithoutParamGroup2_OfWhatCanBeLogged,Sender,AMethodName,AMessage);
end;


procedure TLogger.ExitMethod(IntersectingGroup2_OfWhatCanBeLogged: TGroupOfForWhichTrackingPurposesMsgAreLogged; const AMethodName: String; const AMessage: String);
begin
  ExitMethod(IntersectingGroup2_OfWhatCanBeLogged,nil,AMethodName,AMessage);
end;


procedure TLogger.Watch(const AText, AValue: String);
begin
  Watch(IntersectingGroup2ForProcWithoutParamGroup2_OfWhatCanBeLogged,AText,AValue);
end;


procedure TLogger.Watch(const AText: String; AValue: Integer);
begin
  Watch(IntersectingGroup2ForProcWithoutParamGroup2_OfWhatCanBeLogged,AText,AValue);
end;


{$ifdef fpc}
procedure TLogger.Watch(const AText: String; AValue: Cardinal);
begin
  Watch(IntersectingGroup2ForProcWithoutParamGroup2_OfWhatCanBeLogged,AText,AValue);
end;
{$endif}


procedure TLogger.Watch(const AText: String; AValue: Double);
begin
  Watch(IntersectingGroup2ForProcWithoutParamGroup2_OfWhatCanBeLogged,AText,AValue);
end;


procedure TLogger.Watch(const AText: String; AValue: Boolean);
begin
  Watch(IntersectingGroup2ForProcWithoutParamGroup2_OfWhatCanBeLogged,AText,AValue);
end;


procedure TLogger.SubEventBetweenEnterAndExitMethods(const AText: String);
begin
  SubEventMethodBetweenEnterAndExitMethods(IntersectingGroup2ForProcWithoutParamGroup2_OfWhatCanBeLogged,AText);
end;




(* -- filtred method through intersection of Classes's set and ActivesClasses's set -- *)




procedure TLogger.Send(IntersectingGroup2_OfWhatCanBeLogged: TGroupOfForWhichTrackingPurposesMsgAreLogged; const AText: String);
var
  setGroupIntersection: TGroupOfForWhichTrackingPurposesMsgAreLogged;
begin
  setGroupIntersection:= IntersectingGroup2_OfWhatCanBeLogged * TargetedConstantGroup1_OfWhatCanBeLogged;
  if setGroupIntersection = [] then Exit;    //pre-conditions: the intersection of IntersectingGroup2_OfWhatCanBeLogged and ActivesClasses must not be empty

  Store_FsetForWhichTrackingPurposesMsgAreLogged(setGroupIntersection + [lwInfo]);
  SendStream(methInfo,AText,nil);
end;


procedure TLogger.Send(IntersectingGroup2_OfWhatCanBeLogged: TGroupOfForWhichTrackingPurposesMsgAreLogged; const AText: String; Args: array of const);
var
  setGroupIntersection: TGroupOfForWhichTrackingPurposesMsgAreLogged;
begin
  setGroupIntersection:= IntersectingGroup2_OfWhatCanBeLogged * TargetedConstantGroup1_OfWhatCanBeLogged;
  if setGroupIntersection = [] then Exit;   //pre-conditions: the intersection of IntersectingGroup2_OfWhatCanBeLogged and ActivesClasses must not be empty

  Store_FsetForWhichTrackingPurposesMsgAreLogged(setGroupIntersection + [lwInfo]);
  SendStream(methInfo, Format(AText,Args),nil);
end;


procedure TLogger.Send(IntersectingGroup2_OfWhatCanBeLogged: TGroupOfForWhichTrackingPurposesMsgAreLogged; const AText, AValue: String);
var
  setGroupIntersection: TGroupOfForWhichTrackingPurposesMsgAreLogged;
begin
  setGroupIntersection:= IntersectingGroup2_OfWhatCanBeLogged * TargetedConstantGroup1_OfWhatCanBeLogged;
  if setGroupIntersection = [] then Exit;  //pre-conditions: the intersection of IntersectingGroup2_OfWhatCanBeLogged and ActivesClasses must not be empty

  Store_FsetForWhichTrackingPurposesMsgAreLogged(setGroupIntersection);
  SendStream(methValue,AText+' = '+AValue,nil);
end;


procedure TLogger.Send(IntersectingGroup2_OfWhatCanBeLogged: TGroupOfForWhichTrackingPurposesMsgAreLogged; const AText: String; AValue: Integer);
var
  setGroupIntersection: TGroupOfForWhichTrackingPurposesMsgAreLogged;
begin
  setGroupIntersection:= IntersectingGroup2_OfWhatCanBeLogged * TargetedConstantGroup1_OfWhatCanBeLogged;
  if setGroupIntersection = [] then Exit;  //pre-conditions: the intersection of IntersectingGroup2_OfWhatCanBeLogged and ActivesClasses must not be empty

  Store_FsetForWhichTrackingPurposesMsgAreLogged(setGroupIntersection);
  SendStream(methValue,AText+' = '+IntToStr(AValue),nil);
end;

{$ifdef fpc}
procedure TLogger.Send(IntersectingGroup2_OfWhatCanBeLogged: TGroupOfForWhichTrackingPurposesMsgAreLogged; const AText: String; AValue: Cardinal);
var
  setGroupIntersection: TGroupOfForWhichTrackingPurposesMsgAreLogged;
begin
  setGroupIntersection:= IntersectingGroup2_OfWhatCanBeLogged * TargetedConstantGroup1_OfWhatCanBeLogged;
  if setGroupIntersection = [] then Exit;  //pre-conditions: the intersection of IntersectingGroup2_OfWhatCanBeLogged and ActivesClasses must not be empty

  Store_FsetForWhichTrackingPurposesMsgAreLogged(setGroupIntersection);
  SendStream(methValue,AText+' = '+IntToStr(AValue),nil);
end;
{$endif}


procedure TLogger.Send(IntersectingGroup2_OfWhatCanBeLogged: TGroupOfForWhichTrackingPurposesMsgAreLogged; const AText: String; AValue: Double);
var
  setGroupIntersection: TGroupOfForWhichTrackingPurposesMsgAreLogged;
begin
  setGroupIntersection:= IntersectingGroup2_OfWhatCanBeLogged * TargetedConstantGroup1_OfWhatCanBeLogged;
  if setGroupIntersection = [] then Exit;	//pre-conditions: the intersection of IntersectingGroup2_OfWhatCanBeLogged and ActivesClasses must not be empty

  Store_FsetForWhichTrackingPurposesMsgAreLogged(setGroupIntersection);
  SendStream(methValue,AText+' = '+FloatToStr(AValue),nil);
end;


procedure TLogger.Send(IntersectingGroup2_OfWhatCanBeLogged: TGroupOfForWhichTrackingPurposesMsgAreLogged; const AText: String; AValue: Int64);
var
  setGroupIntersection: TGroupOfForWhichTrackingPurposesMsgAreLogged;
begin
  setGroupIntersection:= IntersectingGroup2_OfWhatCanBeLogged * TargetedConstantGroup1_OfWhatCanBeLogged;
  if setGroupIntersection = [] then Exit;  //pre-conditions: the intersection of IntersectingGroup2_OfWhatCanBeLogged and ActivesClasses must not be empty

  Store_FsetForWhichTrackingPurposesMsgAreLogged(setGroupIntersection);
  SendStream(methValue,AText+' = '+IntToStr(AValue),nil);
end;


procedure TLogger.Send(IntersectingGroup2_OfWhatCanBeLogged: TGroupOfForWhichTrackingPurposesMsgAreLogged; const AText: String; AValue: QWord);
var
  setGroupIntersection: TGroupOfForWhichTrackingPurposesMsgAreLogged;
begin
  setGroupIntersection:= IntersectingGroup2_OfWhatCanBeLogged * TargetedConstantGroup1_OfWhatCanBeLogged;
  if setGroupIntersection = [] then Exit;  //pre-conditions: the intersection of IntersectingGroup2_OfWhatCanBeLogged and ActivesClasses must not be empty

  Store_FsetForWhichTrackingPurposesMsgAreLogged(setGroupIntersection);
  SendStream(methValue,AText+' = '+IntToStr(AValue),nil);
end;


procedure TLogger.Send(IntersectingGroup2_OfWhatCanBeLogged: TGroupOfForWhichTrackingPurposesMsgAreLogged; const AText: String; AValue: Boolean);
var
  setGroupIntersection: TGroupOfForWhichTrackingPurposesMsgAreLogged;
begin
  setGroupIntersection:= IntersectingGroup2_OfWhatCanBeLogged * TargetedConstantGroup1_OfWhatCanBeLogged;
  if setGroupIntersection = [] then Exit;  //pre-conditions: the intersection of IntersectingGroup2_OfWhatCanBeLogged and ActivesClasses must not be empty

  Store_FsetForWhichTrackingPurposesMsgAreLogged(setGroupIntersection);
  SendStream(methValue, AText + ' = ' + BoolToStr(AValue, True), nil);
end;



procedure TLogger.Send(IntersectingGroup2_OfWhatCanBeLogged: TGroupOfForWhichTrackingPurposesMsgAreLogged; const AText: String;const ARect: TRect);
var
  setGroupIntersection: TGroupOfForWhichTrackingPurposesMsgAreLogged;
begin
  setGroupIntersection:= IntersectingGroup2_OfWhatCanBeLogged * TargetedConstantGroup1_OfWhatCanBeLogged;
  if setGroupIntersection = [] then Exit;  //pre-conditions: the intersection of IntersectingGroup2_OfWhatCanBeLogged and ActivesClasses must not be empty

  Store_FsetForWhichTrackingPurposesMsgAreLogged(setGroupIntersection);
  SendStream(methValue,AText+ ' = '+RectToStr(ARect),nil);
end;


procedure TLogger.Send(IntersectingGroup2_OfWhatCanBeLogged: TGroupOfForWhichTrackingPurposesMsgAreLogged; const AText: String; const APoint: TPoint);
var
  setGroupIntersection: TGroupOfForWhichTrackingPurposesMsgAreLogged;
begin
  setGroupIntersection:= IntersectingGroup2_OfWhatCanBeLogged * TargetedConstantGroup1_OfWhatCanBeLogged;
  if setGroupIntersection = [] then Exit;  //pre-conditions: the intersection of IntersectingGroup2_OfWhatCanBeLogged and ActivesClasses must not be empty

  Store_FsetForWhichTrackingPurposesMsgAreLogged(setGroupIntersection);
  SendStream(methValue,AText+' = '+PointToStr(APoint),nil);
end;


procedure TLogger.Send(IntersectingGroup2_OfWhatCanBeLogged: TGroupOfForWhichTrackingPurposesMsgAreLogged; const AText: String; o_AStrList: TStrings);
var
  sStr: String;
  setGroupIntersection: TGroupOfForWhichTrackingPurposesMsgAreLogged;
begin
  setGroupIntersection:= IntersectingGroup2_OfWhatCanBeLogged * TargetedConstantGroup1_OfWhatCanBeLogged;
  if setGroupIntersection = [] then Exit;  //pre-conditions: the intersection of IntersectingGroup2_OfWhatCanBeLogged and ActivesClasses must not be empty

  Store_FsetForWhichTrackingPurposesMsgAreLogged(setGroupIntersection);
  if Assigned(o_AStrList) then
    if o_AStrList.Count>0 then
    	sStr:= o_AStrList.Text
    else
      sStr:= ' ' { fake o_AStrList.Text }
  else
    sStr:= ' '; { fake o_AStrList.Text }
  SendBuffer(methTStrings, AText, sStr[1], Length(sStr));
end;



procedure TLogger.Send(IntersectingGroup2_OfWhatCanBeLogged: TGroupOfForWhichTrackingPurposesMsgAreLogged; const AText: String; AObject: TObject);
var
  sTempStr: String;
  oStream: TStream;
  setGroupIntersection: TGroupOfForWhichTrackingPurposesMsgAreLogged;
begin
  setGroupIntersection:= IntersectingGroup2_OfWhatCanBeLogged * TargetedConstantGroup1_OfWhatCanBeLogged;
  if setGroupIntersection = [] then Exit;	  //pre-conditions: the intersection of IntersectingGroup2_OfWhatCanBeLogged and ActivesClasses must not be empty

  Store_FsetForWhichTrackingPurposesMsgAreLogged(setGroupIntersection);
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
  end;
end;


procedure TLogger.SendPointer(IntersectingGroup2_OfWhatCanBeLogged: TGroupOfForWhichTrackingPurposesMsgAreLogged; const AText: String; APointer: Pointer);
var
	setGroupIntersection: TGroupOfForWhichTrackingPurposesMsgAreLogged;
begin
  setGroupIntersection:= IntersectingGroup2_OfWhatCanBeLogged * TargetedConstantGroup1_OfWhatCanBeLogged;
  if setGroupIntersection = [] then Exit;	  //pre-conditions: the intersection of IntersectingGroup2_OfWhatCanBeLogged and ActivesClasses must not be empty

  Store_FsetForWhichTrackingPurposesMsgAreLogged(setGroupIntersection);
  SendStream(methValue, AText + ' = $' + HexStr(APointer), nil);
end;


procedure TLogger.SendCallStack(IntersectingGroup2_OfWhatCanBeLogged: TGroupOfForWhichTrackingPurposesMsgAreLogged; const AText: String);
var
  oStream: TStream;
	setGroupIntersection: TGroupOfForWhichTrackingPurposesMsgAreLogged;
begin
  setGroupIntersection:= IntersectingGroup2_OfWhatCanBeLogged * TargetedConstantGroup1_OfWhatCanBeLogged;
  if setGroupIntersection = [] then Exit;	  //pre-conditions: the intersection of IntersectingGroup2_OfWhatCanBeLogged and ActivesClasses must not be empty

  Store_FsetForWhichTrackingPurposesMsgAreLogged(setGroupIntersection);
  try
    oStream:=TMemoryStream.Create;
    GetCallStack(oStream);
    SendStream(methCallStack,AText,oStream);	  //nb: SendStream will free oStream
  finally
    oStream.Free;
  end;
end;



function TLogger.GetExceptionDescriptionFields(AException: Exception): string;
var
  sDescrFields: string;
begin
  sDescrFields:= '';
  if (AException is EDatabaseError) then
    if (AException is EUpdateError) then begin
	    with (AException as EUpdateError) do begin
        sDescrFields:= sDescrFields + '- Context:' + Context + LineEnding;
        sDescrFields:= sDescrFields + '- ErrorCode:' + IntToStr(ErrorCode) + LineEnding;
        sDescrFields:= sDescrFields + '- PreviousError:' + IntToStr(PreviousError) + LineEnding;
        sDescrFields:= sDescrFields + '- OriginalException:' + OriginalException.ClassName + ' - ' + AException.Message + LineEnding;
        result:= sDescrFields;
        Exit;
      end
		end
{  else if (AException is EDomError) then begin
  	.../...
   end
   else if (AException is EStreamError) .../... then begin
    .../...
   end  }
end;


procedure TLogger.SendException(IntersectingGroup2_OfWhatCanBeLogged: TGroupOfForWhichTrackingPurposesMsgAreLogged; const AText:  String; AException: Exception);
{$ifdef fpc}
var
  i: Integer;
  pFrames: PPointer;
  sStr: String;
  setGroupIntersection: TGroupOfForWhichTrackingPurposesMsgAreLogged;
{$endif}
begin
  {$ifdef fpc}
  setGroupIntersection:= IntersectingGroup2_OfWhatCanBeLogged * TargetedConstantGroup1_OfWhatCanBeLogged;
  if setGroupIntersection = [] then Exit;	  //pre-conditions: the intersection of IntersectingGroup2_OfWhatCanBeLogged and ActivesClasses must not be empty

  Store_FsetForWhichTrackingPurposesMsgAreLogged(setGroupIntersection + [lwError]);
  if (AException <> nil) then
    sStr:= AException.ClassName + ' - ' + AException.Message + LineEnding + GetExceptionDescriptionFields(AException);
  sStr:= sStr + BackTraceStrFunc(ExceptAddr);
  pFrames:= ExceptFrames;
  for i:= 0 to ExceptFrameCount - 1 do
    sStr:= sStr + (LineEnding + BackTraceStrFunc(pFrames[i]));
  SendBuffer(methException,AText,sStr[1],Length(sStr));
  {$endif}
end;



procedure TLogger.SendHeapInfo(IntersectingGroup2_OfWhatCanBeLogged: TGroupOfForWhichTrackingPurposesMsgAreLogged; const AText: String);
{$ifdef fpc}
var
  sStr: String;
  setGroupIntersection: TGroupOfForWhichTrackingPurposesMsgAreLogged;
{$endif}
begin
  {$ifdef fpc}
  setGroupIntersection:= IntersectingGroup2_OfWhatCanBeLogged * TargetedConstantGroup1_OfWhatCanBeLogged;
  if setGroupIntersection = [] then Exit;		  //pre-conditions: the intersection of IntersectingGroup2_OfWhatCanBeLogged and ActivesClasses must not be empty

  Store_FsetForWhichTrackingPurposesMsgAreLogged(setGroupIntersection);
  with GetFPCHeapStatus do begin
    sStr:= 'All values are in [bytes]:'+LineEnding
    	+'MaxHeapSize: '+FormatNumber(MaxHeapSize)+LineEnding
      +'MaxHeapUsed: '+FormatNumber(MaxHeapUsed)+LineEnding
      +'CurrHeapSize: '+FormatNumber(CurrHeapSize)+LineEnding
      +'CurrHeapUsed: '+FormatNumber(CurrHeapUsed)+LineEnding
      +'CurrHeapFree: '+FormatNumber(CurrHeapFree);
  end;
  SendBuffer(methHeapInfo,AText,sStr[1],Length(sStr));
  {$endif}
end;


procedure TLogger.SendMemory(IntersectingGroup2_OfWhatCanBeLogged: TGroupOfForWhichTrackingPurposesMsgAreLogged; const AText: String; pAddress: Pointer; iSize: LongWord; iOffset: Integer);
var
	setGroupIntersection: TGroupOfForWhichTrackingPurposesMsgAreLogged;
begin
  setGroupIntersection:= IntersectingGroup2_OfWhatCanBeLogged * TargetedConstantGroup1_OfWhatCanBeLogged;
  if setGroupIntersection = [] then Exit; 			  //pre-conditions: the intersection of IntersectingGroup2_OfWhatCanBeLogged and ActivesClasses must not be empty

  Store_FsetForWhichTrackingPurposesMsgAreLogged(setGroupIntersection);
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
end;


procedure TLogger.SendIf(IntersectingGroup2_OfWhatCanBeLogged: TGroupOfForWhichTrackingPurposesMsgAreLogged; const AText: String; bExpression, bIsTrue: Boolean);
var
	setGroupIntersection: TGroupOfForWhichTrackingPurposesMsgAreLogged;
begin
  setGroupIntersection:= IntersectingGroup2_OfWhatCanBeLogged * TargetedConstantGroup1_OfWhatCanBeLogged;
  if (setGroupIntersection = []) or (bExpression <> bIsTrue) then Exit; 			  //pre-conditions: the intersection of IntersectingGroup2_OfWhatCanBeLogged and ActivesClasses must not be empty

  Store_FsetForWhichTrackingPurposesMsgAreLogged(setGroupIntersection);
  SendStream(methConditional,AText,nil);
end;


procedure TLogger.SendWarning(IntersectingGroup2_OfWhatCanBeLogged: TGroupOfForWhichTrackingPurposesMsgAreLogged; const AText: String);
var
	setGroupIntersection: TGroupOfForWhichTrackingPurposesMsgAreLogged;
begin
  setGroupIntersection:= IntersectingGroup2_OfWhatCanBeLogged * TargetedConstantGroup1_OfWhatCanBeLogged;
  if setGroupIntersection = [] then Exit;		 			  //pre-conditions: the intersection of IntersectingGroup2_OfWhatCanBeLogged and ActivesClasses must not be empty

  Store_FsetForWhichTrackingPurposesMsgAreLogged(setGroupIntersection + [lwWarning]);
  SendStream(methWarning,AText,nil);
end;


procedure TLogger.SendError(IntersectingGroup2_OfWhatCanBeLogged: TGroupOfForWhichTrackingPurposesMsgAreLogged; const AText: String);
var
	setGroupIntersection: TGroupOfForWhichTrackingPurposesMsgAreLogged;
begin
  setGroupIntersection:= IntersectingGroup2_OfWhatCanBeLogged * TargetedConstantGroup1_OfWhatCanBeLogged;
  if setGroupIntersection = [] then Exit;		 			  //pre-conditions: the intersection of IntersectingGroup2_OfWhatCanBeLogged and ActivesClasses must not be empty

  Store_FsetForWhichTrackingPurposesMsgAreLogged(setGroupIntersection + [lwError]);
  SendStream(methError,AText,nil);
end;


procedure TLogger.SendCustomData(IntersectingGroup2_OfWhatCanBeLogged: TGroupOfForWhichTrackingPurposesMsgAreLogged; const AText: String; Data: Pointer; CustomDataFunction: TCustomDataNotify);
var
  bDoSend: Boolean;
  sTempStr: String;
	setGroupIntersection: TGroupOfForWhichTrackingPurposesMsgAreLogged;
begin
  setGroupIntersection:= IntersectingGroup2_OfWhatCanBeLogged * TargetedConstantGroup1_OfWhatCanBeLogged;
  if (setGroupIntersection = []) or (not Assigned(CustomDataFunction)) then Exit;	//pre-conditions: the intersection of IntersectingGroup2_OfWhatCanBeLogged and ActivesClasses must not be empty

  Store_FsetForWhichTrackingPurposesMsgAreLogged(setGroupIntersection);
  bDoSend:=True;
  sTempStr:=CustomDataFunction(Self,Data,bDoSend);
  if bDoSend then
    SendBuffer(methCustomData,AText,sTempStr[1],Length(sTempStr));
end;


procedure TLogger.SendCustomData(IntersectingGroup2_OfWhatCanBeLogged: TGroupOfForWhichTrackingPurposesMsgAreLogged; const AText: String; Data: Pointer; CustomDataFunction: TCustomDataNotifyStatic);
var
  bDoSend: Boolean;
  sTempStr: String;
	setGroupIntersection: TGroupOfForWhichTrackingPurposesMsgAreLogged;
begin
  setGroupIntersection:= IntersectingGroup2_OfWhatCanBeLogged * TargetedConstantGroup1_OfWhatCanBeLogged;
  if (setGroupIntersection = []) or (not Assigned(CustomDataFunction)) then Exit;	//pre-conditions: the intersection of IntersectingGroup2_OfWhatCanBeLogged and ActivesClasses must not be empty

  Store_FsetForWhichTrackingPurposesMsgAreLogged(setGroupIntersection);
  bDoSend:=True;
  sTempStr:=CustomDataFunction(Self,Data,bDoSend);
  if bDoSend then
    SendBuffer(methCustomData,AText,sTempStr[1],Length(sTempStr));
end;


procedure TLogger.AddCheckPoint(IntersectingGroup2_OfWhatCanBeLogged: TGroupOfForWhichTrackingPurposesMsgAreLogged; const sCheckName: String);
var
  i: Integer;
  pOnObj: PtrInt;
	setGroupIntersection: TGroupOfForWhichTrackingPurposesMsgAreLogged;
begin
  setGroupIntersection:= IntersectingGroup2_OfWhatCanBeLogged * TargetedConstantGroup1_OfWhatCanBeLogged;
  if setGroupIntersection = [] then Exit;	//pre-conditions: the intersection of IntersectingGroup2_OfWhatCanBeLogged and ActivesClasses must not be empty

  Store_FsetForWhichTrackingPurposesMsgAreLogged(setGroupIntersection);
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
end;


procedure TLogger.IncCounter(IntersectingGroup2_OfWhatCanBeLogged: TGroupOfForWhichTrackingPurposesMsgAreLogged; const CounterName: String );
var
  i: Integer;
  pOnObject: PtrInt;
	setGroupIntersection: TGroupOfForWhichTrackingPurposesMsgAreLogged;
begin
  setGroupIntersection:= IntersectingGroup2_OfWhatCanBeLogged * TargetedConstantGroup1_OfWhatCanBeLogged;
  if setGroupIntersection = [] then Exit;	//pre-conditions: the intersection of IntersectingGroup2_OfWhatCanBeLogged and ActivesClasses must not be empty

  Store_FsetForWhichTrackingPurposesMsgAreLogged(setGroupIntersection);
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
end;


procedure TLogger.DecCounter(IntersectingGroup2_OfWhatCanBeLogged: TGroupOfForWhichTrackingPurposesMsgAreLogged; const CounterName: String);
var
  i: Integer;
  pOnObj: PtrInt;
	setGroupIntersection: TGroupOfForWhichTrackingPurposesMsgAreLogged;
begin
  setGroupIntersection:= IntersectingGroup2_OfWhatCanBeLogged * TargetedConstantGroup1_OfWhatCanBeLogged;
  if setGroupIntersection = [] then Exit;	//pre-conditions: the intersection of IntersectingGroup2_OfWhatCanBeLogged and ActivesClasses must not be empty

  Store_FsetForWhichTrackingPurposesMsgAreLogged(setGroupIntersection);
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
end;


procedure TLogger.ResetCounter(IntersectingGroup2_OfWhatCanBeLogged: TGroupOfForWhichTrackingPurposesMsgAreLogged; const CounterName: String);
var
  i: Integer;
	setGroupIntersection: TGroupOfForWhichTrackingPurposesMsgAreLogged;
begin
  setGroupIntersection:= IntersectingGroup2_OfWhatCanBeLogged * TargetedConstantGroup1_OfWhatCanBeLogged;
  if setGroupIntersection = [] then Exit;	//pre-conditions: the intersection of IntersectingGroup2_OfWhatCanBeLogged and ActivesClasses must not be empty

  Store_FsetForWhichTrackingPurposesMsgAreLogged(setGroupIntersection);
  i := FoCounterList.IndexOf(CounterName);
  if i <> -1 then
  begin
    FoCounterList.Objects[i] := TObject(0);
    SendStream(methCounter, FoCounterList[i] + '=0', nil);
  end;
end;

function TLogger.GetCounter(const CounterName: String): Integer;
var
  i: Integer;
begin
  i := FoCounterList.IndexOf(CounterName);
  if i <> -1 then
    Result := PtrInt(FoCounterList.Objects[i])
  else
    Result := 0;
end;


procedure TLogger.ResetCheckPoint(IntersectingGroup2_OfWhatCanBeLogged: TGroupOfForWhichTrackingPurposesMsgAreLogged; const CheckName:String);
var
  i: Integer;
	setGroupIntersection: TGroupOfForWhichTrackingPurposesMsgAreLogged;
begin
  setGroupIntersection:= IntersectingGroup2_OfWhatCanBeLogged * TargetedConstantGroup1_OfWhatCanBeLogged;
  if setGroupIntersection = [] then Exit;    //pre-conditions: the intersection of IntersectingGroup2_OfWhatCanBeLogged and ActivesClasses must not be empty

  Store_FsetForWhichTrackingPurposesMsgAreLogged(setGroupIntersection);
  i:= FoCheckList.IndexOf(CheckName);
  if i <> -1 then begin
    FoCheckList.Objects[i] := TObject(0);
    SendStream(methCheckpoint, CheckName+' #0',nil);
  end;
end;


procedure TLogger.EnterMethod(IntersectingGroup2_OfWhatCanBeLogged: TGroupOfForWhichTrackingPurposesMsgAreLogged; Sender: TObject; const AMethodName: String; const AMessage: String);
var
  sAText: String;
	setGroupIntersection: TGroupOfForWhichTrackingPurposesMsgAreLogged;
begin
  setGroupIntersection:= IntersectingGroup2_OfWhatCanBeLogged * TargetedConstantGroup1_OfWhatCanBeLogged;
  if setGroupIntersection = [] then Exit;	//pre-conditions: the intersection of IntersectingGroup2_OfWhatCanBeLogged and ActivesClasses must not be empty

  Store_FsetForWhichTrackingPurposesMsgAreLogged(setGroupIntersection + [lwEvents]);
  FoLogStack.Insert(0, UpperCase(AMethodName));
  if AMessage <> '' then
    sAText := AMessage
  else if Sender <> nil then
    sAText := GetObjectDescription(Sender) + '.' + AMethodName
  else
    sAText := AMethodName;
  SendStream(methEnterMethod, sAText, nil);
end;


procedure TLogger.ExitMethod(IntersectingGroup2_OfWhatCanBeLogged: TGroupOfForWhichTrackingPurposesMsgAreLogged; Sender: TObject; const AMethodName: String; const AMessage: String);
var
  i: Integer;
  sAText: String;
	setGroupIntersection: TGroupOfForWhichTrackingPurposesMsgAreLogged;
begin
  //ensure that ExitMethod will be called always even if there's an unpaired Entermethod (!)
  if FoLogStack.Count = 0 then Exit;
  setGroupIntersection:= IntersectingGroup2_OfWhatCanBeLogged * TargetedConstantGroup1_OfWhatCanBeLogged;
  if setGroupIntersection = [] then Exit;	//pre-conditions: the intersection of IntersectingGroup2_OfWhatCanBeLogged and ActivesClasses must not be empty

  Store_FsetForWhichTrackingPurposesMsgAreLogged(setGroupIntersection + [lwEvents]);
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
end;


procedure TLogger.Watch(IntersectingGroup2_OfWhatCanBeLogged: TGroupOfForWhichTrackingPurposesMsgAreLogged; const AText: String; AValue: Integer);
var
	setGroupIntersection: TGroupOfForWhichTrackingPurposesMsgAreLogged;
begin
  setGroupIntersection:= IntersectingGroup2_OfWhatCanBeLogged * TargetedConstantGroup1_OfWhatCanBeLogged;
  if setGroupIntersection = [] then Exit;	//pre-conditions: the intersection of IntersectingGroup2_OfWhatCanBeLogged and ActivesClasses must not be empty

  Store_FsetForWhichTrackingPurposesMsgAreLogged(setGroupIntersection);
  SendStream(methWatch,AText+'='+IntToStr(AValue),nil);
end;


{$ifdef fpc}
procedure TLogger.Watch(IntersectingGroup2_OfWhatCanBeLogged: TGroupOfForWhichTrackingPurposesMsgAreLogged; const AText: String; AValue: Cardinal);
var
	setGroupIntersection: TGroupOfForWhichTrackingPurposesMsgAreLogged;
begin
  setGroupIntersection:= IntersectingGroup2_OfWhatCanBeLogged * TargetedConstantGroup1_OfWhatCanBeLogged;
  if setGroupIntersection = [] then Exit;	//pre-conditions: the intersection of IntersectingGroup2_OfWhatCanBeLogged and ActivesClasses must not be empty

  Store_FsetForWhichTrackingPurposesMsgAreLogged(setGroupIntersection);
  SendStream(methWatch,AText+'='+IntToStr(AValue),nil);
end;
{$endif}


procedure TLogger.Watch(IntersectingGroup2_OfWhatCanBeLogged: TGroupOfForWhichTrackingPurposesMsgAreLogged; const AText: String; AValue: Double);
var
  setGroupIntersection: TGroupOfForWhichTrackingPurposesMsgAreLogged;
begin
  setGroupIntersection:= IntersectingGroup2_OfWhatCanBeLogged * TargetedConstantGroup1_OfWhatCanBeLogged;
  if setGroupIntersection = [] then Exit;	//pre-conditions: the intersection of IntersectingGroup2_OfWhatCanBeLogged and ActivesClasses must not be empty

  Store_FsetForWhichTrackingPurposesMsgAreLogged(setGroupIntersection);
  SendStream(methWatch,AText+'='+FloatToStr(AValue),nil);
end;


procedure TLogger.Watch(IntersectingGroup2_OfWhatCanBeLogged: TGroupOfForWhichTrackingPurposesMsgAreLogged; const AText: String; AValue: Boolean);
var
	setGroupIntersection: TGroupOfForWhichTrackingPurposesMsgAreLogged;
begin
  setGroupIntersection:= IntersectingGroup2_OfWhatCanBeLogged * TargetedConstantGroup1_OfWhatCanBeLogged;
  if setGroupIntersection = [] then Exit;	//pre-conditions: the intersection of IntersectingGroup2_OfWhatCanBeLogged and ActivesClasses must not be empty

  Store_FsetForWhichTrackingPurposesMsgAreLogged(setGroupIntersection);
  SendStream(methWatch,AText+'='+BoolToStr(AValue),nil);
end;


procedure TLogger.Watch(IntersectingGroup2_OfWhatCanBeLogged: TGroupOfForWhichTrackingPurposesMsgAreLogged; const AText, AValue: String);
var
	setGroupIntersection: TGroupOfForWhichTrackingPurposesMsgAreLogged;
begin
  setGroupIntersection:= IntersectingGroup2_OfWhatCanBeLogged * TargetedConstantGroup1_OfWhatCanBeLogged;
  if setGroupIntersection = [] then Exit;    //pre-conditions: the intersection of IntersectingGroup2_OfWhatCanBeLogged and ActivesClasses must not be empty

  Store_FsetForWhichTrackingPurposesMsgAreLogged(setGroupIntersection);
  SendStream(methWatch,AText+'='+AValue,nil);
end;


procedure TLogger.SubEventMethodBetweenEnterAndExitMethods(IntersectingGroup2_OfWhatCanBeLogged: TGroupOfForWhichTrackingPurposesMsgAreLogged; const AText: String);
var
	setGroupIntersection: TGroupOfForWhichTrackingPurposesMsgAreLogged;
begin
  setGroupIntersection:= IntersectingGroup2_OfWhatCanBeLogged * TargetedConstantGroup1_OfWhatCanBeLogged;
  if setGroupIntersection = [] then Exit;    //pre-conditions: the intersection of IntersectingGroup2_OfWhatCanBeLogged and ActivesClasses must not be empty

  Store_FsetForWhichTrackingPurposesMsgAreLogged(setGroupIntersection);
  SendStream(methSubEventBetweenEnterAndExitMethods,AText,nil);
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

procedure TLogChannel.Init;
begin
		//must be overriden in its descendants
end;



procedure TLogChannelWrapper.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if	(Operation = opRemove) and
  		(FoChannel <> nil) then
    FoChannel.Active := False;
end;

initialization
  goLogger:=TLogger.Create;
finalization
  TLogger.FoDefaultChannels.Free;
  goLogger.Free;

end.

