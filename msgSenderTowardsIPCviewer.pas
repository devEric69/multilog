program msgSenderTowardsIPCviewer;

{$ifdef fpc}
	{$mode objfpc}{$H+}
{$endif}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, Interfaces,
  Graphics, MultiLog, filechannel, ipcchannel;

type

  { TMyObject }

  TMyObject = class(TComponent)
  private
    FiId: Integer;
  published
    property Id: Integer read FiId write FiId;
  end;

{ program's global variables }

var
  goList: TStrings;
  goObj: TMyObject;		{ create and destroyed in its Initialization and Finalization }
  {$If defined(DEBUG)}giCounter: integer;{$EndIf}


begin
  goList:= TStringList.Create;
  with goLogger {destroyed in its unit's Finalization section } do begin
    Channels.Add(TFileChannel.Create('test.log'));	{ create a log file }
    Channels.Add(TIPCChannel.Create);
    FilterDynamic_forWhatReasonsToLogActually:= [lwDebug..lwError];
    Send('Hello 1');
    Send('Hello 2');
    IncCounter('Counter1');
	  Watch('AStrWatch','XXXX');
		SendIf('Only show if not called by TestLogClick', not CalledBy('TestLogClick'));
   	Try
     	Raise Exception.Create('Manual exception num#1 raised!');
   	Except
     	On E: Exception do
       	SendException(E.Message,E);
   	End;
    IncCounter('Counter1');
    Watch('AStrWatch','XXXX');
    Send('An empty StringList', goList);
    IncCounter('CounteR1');
    Watch('AIntWatch', 123);
    Send('A Text Message');
    IncCounter('CounTER1');
    Send('Another Text Message');
    with goList do begin
      Add('aaaaaa');
      Add('bbbbbb');
      Add('cccccc');
    end;
    DecCounter('Counter1');
    Watch('AIntWatch', 321);
    SendError('A Error Message');
    ResetCounter('Counter1');
    Watch('ASTRWatch','YYYY');
    goObj:= TMyObject.Create(nil);
    goObj.Name:= 'The_Obj_Name';
    goObj.Id:= 2;
    Send('An Object\Component', goObj);
    goObj.Free;
{$If defined(DEBUG)}
FilterDynamic_forWhatReasonsToLogActually:= [lwDebug];
SendInfo([lwDebug], 'MultiLog API `Info` for *debug* purpose');
IncCounter([lwDebug], 'Counter1');
Watch('AStrWatch', 'XXXX');
SendError([lwDebug], 'MultiLog API `Error` for *debug* purpose');
for giCounter:= 1 to 5 do begin
  IncCounter([lwDebug], 'Counter1');
end;
{$EndIf}
    EnterMethod('DoIt');
    Send('AText inside DoIt');
    SendWarning('A Warning');
    Send('A String','sadjfgadsfbmsandfb');
    Send('AInteger', 4957);
    Send('A Boolean',True);
    SendCallStack('A CallStack example');
    ExitMethod('DoIt');
    Send('A StringList', goList);
    FilterDynamic_forWhatReasonsToLogActually:= [lwInfo];
    Send('This Text Should be logged with [lwInfo]');
    Send([lwIPC],'This Text Should be logged with [lwInfo,lwIPC]');
    SendInfo('Info');
    SendError('Error');
    SendWarning('Warning');
{$If defined(DEBUG)}
Try
  Raise Exception.Create('Manual exception raised!');
  AddCheckPoint([lwDebug], 'Check if goes here?');
Except
  On E:Exception do begin
    SendHeapInfo([lwDebug], 'HeapInfo in except..end;');
    SendException(E.Message, E);
  end;
End;
{$EndIf}
  end;
  goList.Free;
end.

