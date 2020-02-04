program msgSenderTowardsIPCviewer;

{$ifdef fpc}
{$mode objfpc}{$H+}
{$endif}

uses
  Classes, SysUtils,
  Graphics, filechannel, ipcchannel, MultiLog, MultiLogLCLHelpers;

type

  { TMyObject }

  TMyObject = class(TComponent)
  private
    FiId: Integer;
  published
    property Id: Integer read FiId write FiId;
  end;

var
  goList: TStrings;
  goObj: TMyObject;
  giCounter: integer;
begin
    goList:= TStringList.Create;
    with goLogger do begin
      Channels.Add(TFileChannel.Create('test.log'));	{ create a log file }
      Channels.Add(TIPCChannel.Create);
      FilterDynamic_forWhatReasonsToLogActually:= [lwDebug..lwError];
      Send('Hello 1');
      Send('Hello 2');
      IncCounter('Counter1');
	    Watch('AStrWatch','XXXX');
      (* So, meth. will all be logged as TargetedConstantGroup1_OfWhatCanBeLogged*IntersectingGroup2ForProcWithoutParamGroup2_OfWhatCanBeLogged=[lwError] tracking purpose;
    	  Nevertheless, it's not libelled in the specialised targeted TreeView of simpleIPCviewer.lpi or multilogviewer.lpi,
        whose binary must be lauched before executing this program.*)
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
      //Vérifier que [lwDebug] n'est pas dans les motifs d'envois de msg
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

