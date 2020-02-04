unit FileChannel;

{
Copyright (C) 2006 Luiz Américo Pereira Câmara

TFileChannel modified by devEric69 (Éric Moutié):
- it became thread safe, eg TMemoChannel's modifications.
- added the possibility to have an overview, a "film" of the events for which we want to anderstand the order of their inter-calls.

This library is free software; you can redistribute it and/or modify it
under the terms of the FPC modified LGPL licence which can be found at:
http://wiki.lazarus.freepascal.org/FPC_modified_LGPL.

Unit containing TFileChannel. Updated by devEric69 (Ėric Moutié).
There is the possibility to see the indentation of events in the calls stack.

}


{$ifdef fpc}
{$mode objfpc}{$H+}
{$endif}

interface

uses
  {$ifndef fpc}fpccompat,{$endif} Classes, SysUtils, db, multilog;

type
  TFilelogThematics = (fltLogNormal, fltLogSQL);
  TFileChannelOption = (fcoShowHeader, fcoShowPrefixMethod, fcoShowTime, fcoShowDynamicFilter_forWhatReasonsToLogActually);
  TFileChannelOptions = set of TFileChannelOption;
  TStatesOfLogstack = record
  	bEnterMethodCalled: Boolean;
    bShouldIndentASubEvent: Boolean;
    bExitMethodCalled: Boolean;
  end;
	PStatesOfLogstack = ^TStatesOfLogstack;


  { TFileChannelDataHelperFunctions }

type
  TFileChannelDataHelperFunctions = class
    private
      FsFilePath: string;
        FiHandleDataFile: THandle;
        FiLockSpecifier: Byte;
    protected
      function GetSize: Int64;
    public
      constructor Create(const sFilePath: string); reintroduce; overload;
      destructor Destroy; override;
      property Size: Int64 read GetSize;
  end;


  { TFileChannel }

  TFileChannel = class (TLogChannel)
  private
    FrecMsg: TrecLogMessage;
    { (!!!) It must be *full* path like '/home/user_1/.TheAppli/Log.txt' or 'C:\Program Files\TheAppli\Log.txt'.}
    FsFullPathFileName: string;
    	FFileHandle: TextFile;
     	FieFilelogThematics: TFilelogThematics;
    FiBaseIdent: Integer;
    	FiRelativeIdent: Integer;
 		FbIndentCallstackTriggeredByEntermethod: boolean;
    	FiCallstackIndent: Integer;
      FiAddressEBPcurrentFrame: Pointer;
	    FiLevelStack: integer;
    FShowHeader: Boolean;	//'=== Log Session Started at '.../...
      { is set to True when '=== Log Session Started at ' has been written one time in the log normal, at the launch of the application.}
    	FbShowHeaderNormalWritten: boolean;
      { is set to True when '=== Log Session Started at ' has been written one time in the log SQL, at the launch of the application.}
      FbShowHeaderSQL_Written: boolean;
    FsTimeFormat: string;
    FpRecStatesOfLogstack: PStatesOfLogstack;
    FiMaxLogSize: int64;
    FbWantSpecificFileForSQLstatements: Boolean;
    (* identation management *)
    procedure UpdateBaseTimeIdentation;
    procedure UpdateRelativeIndentation;
    procedure UpdateEBPIndentation;
{ Explanations:
Warning: it is ***not*** used to send a Call Stack back from a crash, which you would like to indent nicely: the crashed Call stack is already recovered in multiLog.

This is used to see the indentation of events, that have an event like @code(send|watch|)... coded between:
- the beginning of an event @code(procedure TForm1.btnPostClick(Sender: TObject); enter_METHOD_[>>];)...
- ...and the end of the same event @code(exit_METHOD)_[<<]).
Indeed, by considering only potentially codable events, such as those used for a database management, they can be encapsulated in a complex way,
between the basic click of a button @code(btnPostData();) that can initiate the request to repaint controls, ..., format the input, ...,
data transfer via a datalink, ..., data verification, ..., related calculations of calculated fields, ..., datSet Post, ...
In addition, many events have an onBeforEvent, onEvent and onAfterEvent.

==> the view of the graph of the indentation of the events \ methods \... called between @code(enter_METHOD_[>>>];) and @code(exit_METHOD)_[<<<];),
as they are "pushed" \ "poped" into the stack of calls makes it possible to understand through an indented holistic view,
an event management whose inter-calls of sub-events are not understood, if they have been coded in an event called too early or too late,
if an under-event has been called several times by other under-events without regard to context, etc.

It is therefore a means of more in-depth understanding an event programming, and therefore of helping to develop events management:
procedure TForm1.SubLogClick(Sender: TObject);
begin
|--> EnterMethod [>>]: goLogger.EnterMethod(Sender,'SubLogClick');
   |--> entrée event 1: goLogger.SubEventBetweenEnterAndExitMethods('>event DoSmallCodePieceNum2 + Sender=' + Sender.ClassName);
      |-->
         |-->
      |-->
         |-->
            |--> goLogger.SubEventBetweenEnterAndExitMethods('><event DoSmallCodePieceNum6 + Sender=' + Sender.ClassName + ' (I''m the last push inside the callstack. Now, we pop from the callstack.)');
         |-->
      |-->
   |--> sortie event 1: goLogger.SubEventBetweenEnterAndExitMethods('<event DoSmallCodePieceNum1 + Sender=' + Sender.ClassName);
|--> ExitMethod [<<: ExitMethod(Sender,'SubLogClick');
end;
}
    function GetEBPIndentation: integer;
    function CallstackSize: int64;
		{ Explanations: returns de file's size; -1 if it doesn't exist. }
    function GetFileLogSize: int64;
    (* methods of writing stream-text in the log *)
    procedure WriteStringsOfMsgDataStream();
    procedure WriteComponentOfMsgDataStream();
    function StreamIntoRawString(): string;
    (* log management *)
		{ Explanations: check if the size is larger than the maximum allowed; in this case, the log file est saved and a new one is created and used. }
    procedure CheckIfNecessaryToSaveAndCreateNewFileLog;
		{ Explanations: create a NEW FsPathFileLog file! }
    procedure FileLogCreate;
    function GetTimestampFromToday: string;
		{ Explanations: close, remove all lock(s), threads's connexion(s), with FsPathFileLog. }
    procedure CloseFileLog;
		{ Explanations: ckeck if FMsg est SQL related. returns True, if it's a SQL-oriented message.) }
    function IsSQLwordPresentInMsg: Boolean;
		{ Explanations: set the FieCurrentFileLogTheme property. Does not have to be public or published. Usefulness due to its side effects, nothing more.}
    procedure SetFilelogThematic(AFileLogThematic: TFilelogThematics);
    procedure OpenRightFilelogThematic;
  protected
  public
		{ Explanations: constructor.
			AFileName is the *full* path and the name of the logging file.)
			AChannelOptions: it's a set of options (timestamp, Why this msg, ...) that qualifies each message written in the log file.)
			- param: AbWantSpecificFileForSQLstatements:
			=False if you want to use a single log file for your whole application, where you will write everything.
			=True if you want to create another SQL-oriented log file, in which only the SQL Exceptions, the msq which may have their dumped
			content as text with a substring 'SQL' or 'sql' will be logged. It's an SQL-oriented log file named "AFileName+'_SQL'+.your_possible_file_ext".)
			- returns: object instance. }
    constructor Create(const AFileName: String; AChannelOptions: TFileChannelOptions = [fcoShowHeader, fcoShowTime]; AbWantSpecificFileForSQLstatements: boolean = false);
    destructor Destroy; override;
    procedure Clear; override;
    procedure Deliver(const AMsg: TrecLogMessage);override;
		{ Explanations: init, post-constructor;
		goal: create the log file(s) that will be used by the application,
		according to the parameters passed to the constructor.}
    procedure Init; override;
    property ShowHeader: Boolean read FShowHeader write FShowHeader;
    property TimeFormat: String read FsTimeFormat write FsTimeFormat;
    property MaxLogSize: int64 read FiMaxLogSize default 1000000; (*1 Mo = 4.10^9 [b]*)
    property IndentCallstackTriggeredByEntermethod: boolean read FbIndentCallstackTriggeredByEntermethod write FbIndentCallstackTriggeredByEntermethod default true;
		property FullPathFileName: string read FsFullPathFileName;
		property CurrentFileLogTheme: TFilelogThematics read FieFilelogThematics write SetFilelogThematic default fltLogNormal;
  end;




implementation

uses
	FileUtil, fpccompat, strutils, multilogAsm, math, RtlConsts,
  LazLoggerBase, Dialogs;


var { Prevents 2 TFileChannel's objects, to write in race concurrency inside de same log file. }
  goGuardianFile: TGuardian;

const
  csSuffixForSQLlogFile: string = '_SQL';
  csSQL_FileName  = 'Log_SQL.txt';
  csNormal_FileName  = 'Log.txt';

{ TFileChannel }


function TFileChannel.CallstackSize: int64;
begin
  result:= int64(StackTop)-int64(StackBottom);	// =~ same information as StackLength, ie it's an offset.
end;


procedure TFileChannel.WriteStringsOfMsgDataStream();
var
  i: Integer;
{$IFDEF DEBUG}
sTemp: string;
{$ENDIF}
  oTStringList: TStringList;
begin
  if FrecMsg.pData.Size = 0 then Exit;	// pre-condition
  oTStringList:= TStringList.Create;
  with oTStringList do begin
    try
      FrecMsg.pData.Position:=0;
      LoadFromStream(FrecMsg.pData);
      for i:= 0 to Count - 1 do begin
{$IFDEF DEBUG}
sTemp:= Strings[i];
{$ENDIF}
        WriteLn(FFileHandle,Space(FiBaseIdent+FiRelativeIdent) + Strings[i]);
      end;
    finally
      FreeAndNil(oTStringList);
    end;
  end;
end;


function TFileChannel.StreamIntoRawString(): string;
begin
  if not Assigned(FrecMsg.pData) then begin	// pre-condition
		result:= '';
		Exit;
  end;

  with TStringStream.Create('') do begin
    try
      FrecMsg.pData.Position:= 0;
      CopyFrom(FrecMsg.pData, FrecMsg.pData.Size - FrecMsg.pData.Position);
      Result:= DataString;
    finally
      Free;
    end;
  end;
end;


procedure TFileChannel.WriteComponentOfMsgDataStream();
var
  TextStream: TStringStream;
begin
  TextStream:= TStringStream.Create('');
  FrecMsg.pData.Seek(0, soFromBeginning);
  ObjectBinaryToText(FrecMsg.pData,TextStream);
  Write(FFileHandle, TextStream.DataString);  	//todo: better handling of format

  TextStream.Destroy;
end;


procedure TFileChannel.CheckIfNecessaryToSaveAndCreateNewFileLog;

      procedure BackupEtDeleteFileLog;
      {Explanations: Copy the log file with "xxx_timestamp.log.txt.old" extension.
      Then, delete the original saved file.}
      var
        sFullPathFileDest: TFileName;
        sFilePath, sFileOldExt, sFileNewExt, sFileName: string;
      begin
        (*close the log-file, before making its copy*)
        CloseFileLog;

        sFilePath:= ExtractFilePath(FsFullPathFileName);
        sFileOldExt:= ExtractFileExt(FsFullPathFileName);
        sFileNewExt:= '.old'+ExtractFileExt(FsFullPathFileName);
        sFileName:= ExtractFileName(FsFullPathFileName);
        sFileName:= LeftStr(sFileName, Length(sFileName)-Length(sFileOldExt));
        (*finally, the backup-file will be named... _AAAAMMJJ_HHMM.old.xxx*)
        sFullPathFileDest:= sFilePath+sFileName+'_'+GetTimestampFromToday+'_'+LeftStr(ReplaceStr(TimeToStr(Now), ':', ''), 4)+sFileNewExt;
        if FileExists(sFullPathFileDest) then
        	SysUtils.DeleteFile(sFullPathFileDest);
         CopyFile(FsFullPathFileName, sFullPathFileDest);
        SysUtils.DeleteFile(FsFullPathFileName);
      end;

begin
  if (GetFileLogSize > FiMaxLogSize) then begin
    BackupEtDeleteFileLog;
    FileLogCreate;
  end;
end;


procedure TFileChannel.CloseFileLog;
begin
  {$I-}
  try
    AssignFile(FFileHandle, FsFullPathFileName);
  finally
    CloseFile(FFileHandle);
    if IOResult = 103 then begin
      (*CloseFile sends IOResult-error 103, if the file wasn't open; reset IOResult with 0.*)
      InOutRes:= 0;
    end;
  end;
  {$I+}
end;


procedure TFileChannel.FileLogCreate;
begin
  Assert(not FileExists(FsFullPathFileName), 'TFileChannel.FileLogCreate: File '+FsFullPathFileName+' must already be deleted!');	// How to call this method

  AssignFile(FFileHandle, FsFullPathFileName);
	ReWrite(FFileHandle);	// Create new log file
  Writeln(FFileHandle, '=== File created at ', DateTimeToStr(Now),' by ', ApplicationName,' ===');
  WriteLn(FFileHandle,'=== Log Session Started at ', DateTimeToStr(Now),' by ', ApplicationName,' ===');
  Close(FFileHandle);
end;



function TFileChannel.GetFileLogSize: int64;
var
  oFileData: TFileChannelDataHelperFunctions;
begin
  oFileData:= TFileChannelDataHelperFunctions.Create(Self.FsFullPathFileName);
  Result:= oFileData.Size;
  FreeAndNil(oFileData);
end;


function TFileChannel.GetTimestampFromToday: string;
var
  sTodayDateTime: string;
begin
  sTodayDateTime:= FormatDateTime('yyyy/mm/dd', Now);
  {$if defined(Win64)}
    result:= StringReplace(sTodayDateTime, '/', '-', [rfReplaceAll]); // the slash is not allowed in a Windows file name
  {$ElseIf defined(UNIX)}
    result:= sTodayDateTime;
  {$endif}
end;



constructor TFileChannel.Create(const AFileName: String; AChannelOptions: TFileChannelOptions; AbWantSpecificFileForSQLstatements: boolean = false);
begin
  if IsMultiThread then
    goGuardianFile:= TGuardian.Create;
  FiMaxLogSize:= 1000000;                 		//default property
  FbShowTime:= fcoShowTime in AChannelOptions;
  FShowHeader:= fcoShowHeader in AChannelOptions;
  FbShowPrefixMethod:= fcoShowPrefixMethod in AChannelOptions; //can be disabled with ShowPrefixMethod property
  FbShow_DynamicFilter_forWhatReasonsToLogActually:= fcoShowPrefixMethod in AChannelOptions;
  	FbShowHeaderNormalWritten:= False;
    FbShowHeaderSQL_Written:= False;
  Active:= True;
  FsTimeFormat:= 'hh:nn:ss:zzz';
  FsFullPathFileName:= AFileName;
  	FieFilelogThematics:= fltLogNormal;		//default property
  FbWantSpecificFileForSQLstatements:= AbWantSpecificFileForSQLstatements;
  FbIndentCallstackTriggeredByEntermethod:= false;
	FiAddressEBPcurrentFrame:= StackTop; FiLevelStack:= 0;
  FpRecStatesOfLogstack:= new(PStatesOfLogstack);
  with FpRecStatesOfLogstack^ do begin
    bEnterMethodCalled:= false;
    bExitMethodCalled:= false;
  end;
end;


destructor TFileChannel.Destroy;
begin
  if Assigned(FpRecStatesOfLogstack) then
    Dispose(FpRecStatesOfLogstack);
  if Assigned(goGuardianFile) then
    FreeAndNil(goGuardianFile);
  inherited Destroy;
end;



procedure TFileChannel.Init;
begin
  //Create || Open the log file(s)
  CurrentFileLogTheme:= fltLogNormal;
  UpdateBaseTimeIdentation;
  if (FbWantSpecificFileForSQLstatements) then begin
    CurrentFileLogTheme:= fltLogSQL;
    UpdateBaseTimeIdentation;
  end;
  //we "re-point" to the default fltLogNormal log file
  CurrentFileLogTheme:= fltLogNormal;
end;


procedure TFileChannel.Clear;
begin
  if FsFullPathFileName <> '' then
    Rewrite(FFileHandle);
end;



function TFileChannel.IsSQLwordPresentInMsg: Boolean;
const
  csSQLmarker = '[SQL error] '; //[*] exists the same homonym and bijective const in unit MultiLog, function TIntegratedLogger.GetDescriptionOfSQLException
begin
  Result:= AnsiContainsStr(FrecMsg.sMsgText, csSQLmarker);
end;



function TFileChannel.GetEBPIndentation: integer;
var
  i: integer;
  iPt1: pointer; //first pusher entry by current_method, in the stack
begin
  iPt1:= GetEBP;
	if iPt1 < FiAddressEBPcurrentFrame then begin
    (*récup nouvelle base du dernier cadre pushé*)
    FiLevelStack:= FiLevelStack+1;
  end
  else if iPt1 > FiAddressEBPcurrentFrame then begin
    (*récup nouvelle base du dernier cadre en haut de la pile suite au pop du précédent*)
    FiLevelStack:= math.Max(0, FiLevelStack-1);
  end;
  (*le prochain coup que l'on revient dans cette fonction, on connaîtra FiAdresseEBPCadreCourant*)
  FiAddressEBPcurrentFrame:= iPt1;
  result:= 0;
  if FiLevelStack > 0 then begin
    for i:= 0 to FiLevelStack-1 do begin
      result:= result + 4; // How many space(s) should we add after goLogger.EnterMethod()?
    end;
  end;
end;


procedure TFileChannel.UpdateBaseTimeIdentation;
var
  S: String;
begin
  S:= '';
  if FbShowTime then
    S:= FormatDateTime(FsTimeFormat,Time);
  FiBaseIdent:= Length(S)+1;
end;


procedure TFileChannel.UpdateRelativeIndentation;
begin
  if (FrecMsg.iMethUsed = methEnterMethod) then begin
    //reference's "screenshot" of the top of callstack
  	FbIndentCallstackTriggeredByEntermethod:= True;
    FiAddressEBPcurrentFrame:= StackTop;
    FiCallstackIndent:= 0; FiLevelStack:= 0;
    //Update EnterMethod identation
    Inc(FiRelativeIdent, 3);
    with FpRecStatesOfLogstack^ do begin
    	bEnterMethodCalled:= True;
    	bShouldIndentASubEvent:= True;
    	bExitMethodCalled:= False;
    end;
  end
  else if (FrecMsg.iMethUsed = methExitMethod) then begin
    //whe stop to track the callstack: it's the end of the indentation of sub-events
    FbIndentCallstackTriggeredByEntermethod:= False;		// End of callstack [methEnterMethod..methExitMethod] indentation
    FiCallstackIndent:= 0; FiLevelStack:= 0;
    //Update EnterMethod identation
    Dec(FiRelativeIdent, 3);
    with FpRecStatesOfLogstack^ do begin
    	bEnterMethodCalled:= True;
    	bShouldIndentASubEvent:= False;
    	bExitMethodCalled:= True;
    end;
  end;
end;


procedure TFileChannel.UpdateEBPIndentation;
begin
  if FbIndentCallstackTriggeredByEntermethod and (FrecMsg.iMethUsed = methSubEventBetweenEnterAndExitMethods) then begin
    //we are always between [methEnterMethod..methExitMethod], and that's a sub-event: so, it must be indented via it's level in the callstack
    with FpRecStatesOfLogstack^ do begin
    	bEnterMethodCalled:= True;
    	bShouldIndentASubEvent:= True;
    	bExitMethodCalled:= False;
    end;
    //Update EBPIndentation
    FiCallstackIndent:= GetEBPIndentation;
	end
	else if FbIndentCallstackTriggeredByEntermethod and (FrecMsg.iMethUsed <> methSubEventBetweenEnterAndExitMethods) then begin
    //we are always beteween [methEnterMethod..methExitMethod] and that's not a sub-event: so, it must not be indented via it's level in the callstack
    with FpRecStatesOfLogstack^ do begin
			bEnterMethodCalled:= True;
    	bShouldIndentASubEvent:= False;
    	bExitMethodCalled:= False;
    end;
  end
end;


procedure TFileChannel.OpenRightFilelogThematic;
var
  iPosSQL: integer;
begin
  try
    iPosSQL:= Pos(csSuffixForSQLlogFile, FsFullPathFileName);
    //Check for correct use: FsFullPathFileName must be correctly set according with FieFilelogThematics
    Assert(  	((FieFilelogThematics=fltLogSQL) and (iPosSQL>0)) or
  					  ((FieFilelogThematics=fltLogNormal) and (iPosSQL=0)), 'incoherent FieCurrentFileLogTheme & FFullPathFileName. See SetForCurrentFileLogThemeFullPathFileName...');

    if (FsFullPathFileName <> '') then begin
      Assign(FFileHandle, FsFullPathFileName);	//(re)-create the file's OS-inode-Handle, in bijection with the assigned file
      if FileExists(FsFullPathFileName) then
	        Append(FFileHandle)
      else
        Rewrite(FFileHandle);
    end
    else
      FFileHandle:= Output;
    //If Asked, Write the start of application's session, but only one time per application's life
    if FShowHeader then begin
      if (FieFilelogThematics = fltLogNormal) and not FbShowHeaderNormalWritten then begin
			  WriteLn(FFileHandle, '=== Log Session Started at ', DateTimeToStr(Now), ' by ', ApplicationName, ' ===');
        FbShowHeaderNormalWritten:= True;
      end
      else if (FieFilelogThematics = fltLogSQL) and not FbShowHeaderSQL_Written then begin
        WriteLn(FFileHandle, '=== Log Session Started at ', DateTimeToStr(Now), ' by ', ApplicationName, ' ===');
        FbShowHeaderSQL_Written:= True;
      end;
    end;
    if FsFullPathFileName <> '' then
      Close(FFileHandle);
  except
    on E: Exception do
    	ShowMessage('TFileChannel.OpenRightFilelogThematic: ' + E.Message + LineEnding + 'check the file''s rights (' + FsFullPathFileName + ') or delete it.');
  end;
end;



procedure TFileChannel.SetFilelogThematic(AFileLogThematic: TFilelogThematics);

  							procedure SetTheRightThemeChoiceFileLogToBeWritten;
                begin
                  CloseFileLog;
                  //FsFullPathFileName points to the "verbose" log file of the software
                  if (FieFilelogThematics = fltLogNormal) then
                    FsFullPathFileName:= ExtractFilePath(FsFullPathFileName) + csNormal_FileName // -- StringReplace(FsFullPathFileName, csSuffixForSQLlogFile, '', [rfReplaceAll])
                  //FsFullPathFileName points to the file containing SQL \ Exception \ ... queries: database management software crashes 95%, due to contextually false SQL queries
                  else if (FieFilelogThematics = fltLogSQL) then begin
                    FsFullPathFileName:= ExtractFilePath(FsFullPathFileName) + csSQL_FileName;
                  end;
                  OpenRightFilelogThematic;
                end;

begin
  //in which logging file should we write?
  if (FieFilelogThematics <> AFileLogThematic) then
    FieFilelogThematics:= AFileLogThematic;
 	SetTheRightThemeChoiceFileLogToBeWritten;
end;


procedure TFileChannel.Deliver(const AMsg: TrecLogMessage);
var
  sWholeMsg: string;
  bIsSQLwordPresentInMsg: boolean;
begin
  try

    //only one thread at a time, can now execute the code below; the others must be patient
    if IsMultiThread then
      goGuardianFile.Acquire;

    FrecMsg:= AMsg;
    //has the maximum allowable limit size fot the log file been reached?
		CheckIfNecessaryToSaveAndCreateNewFileLog;
		//we point to the right logging file according to what will be logged, and where it should be logged
    if (FbWantSpecificFileForSQLstatements) then begin
			bIsSQLwordPresentInMsg:= IsSQLwordPresentInMsg;
      if bIsSQLwordPresentInMsg then
        CurrentFileLogTheme:= fltLogSQL
			else
        CurrentFileLogTheme:= fltLogNormal;
		end
    else
	    CurrentFileLogTheme:= fltLogNormal;

    sWholeMsg := '';
    if FbShowTime then begin
      sWholeMsg := FormatDateTime(FsTimeFormat, FrecMsg.dtMsgTime) + ' ';
    	UpdateBaseTimeIdentation;
    end;
    //Update ExitMethod identation
    UpdateRelativeIndentation;
    sWholeMsg:= sWholeMsg + fpccompat.Space(FiRelativeIdent);
    //FbShowPrefixMethod can serve as prime qualifier for each current msg, allowing further thematic extractions
    if FbShowPrefixMethod then
      sWholeMsg := sWholeMsg + (ctsLogPrefixesMethod[FrecMsg.iMethUsed] + ':   ');
   	//write second qualifier explaining for which tracking purposes Msg are Logged
    if FbShow_DynamicFilter_forWhatReasonsToLogActually then
    	//write second qualifier explaining for which tracking purposes Msg are logged
 	 		sWholeMsg := sWholeMsg + TLogChannelUtils.SetofToString(FrecMsg.setFilterDynamic);
		//Update if there are a sequence of "lwStudyChainedEvents Why messages" (due to a sequence of [methEnterMethod... ltSubEventBetweenEnterAndExitMethods_, ltSubEventBetweenEnterAndExitMethods_2, etc, ...methExitMethod]) to study.
		UpdateEBPIndentation;
    sWholeMsg:= sWholeMsg + fpccompat.Space( ifthen(FpRecStatesOfLogstack^.bShouldIndentASubEvent, FiCallstackIndent, 0) );
    sWholeMsg:= sWholeMsg + FrecMsg.sMsgText;

    if (FsFullPathFileName <> '') then
      Append(FFileHandle);
    WriteLn(FFileHandle, sWholeMsg);
    //Update ExitMethod identation
    UpdateRelativeIndentation;
    //if there's a TStream to write
    if FrecMsg.pData <> nil then
    begin
      case FrecMsg.iMethUsed of
        methTStrings, methCallStack, methHeapInfo, methException, methMemory: WriteStringsOfMsgDataStream();
        methObject: WriteComponentOfMsgDataStream();
      end;
    end;
  finally
    if (FsFullPathFileName <> '') then
      Close(FFileHandle);
    if IsMultiThread then
      goGuardianFile.Release;
  end;
end;


{ TFileChannelDataHelperFunctions }

constructor TFileChannelDataHelperFunctions.Create(const sFilePath: string);
begin
  { It does not yet exist any lock on the file's byte(s) }
  FiLockSpecifier:= 0;
  { Make file if it ain't there }
  if not FileExists(sFilePath) then
    FiHandleDataFile := FileCreate(sFilePath);
  if FiHandleDataFile < 0 then
    raise EFCreateError.CreateFmt(SFCreateError, [sFilePath]);
  { Close handle returned by FileCreate so we can open it in shared mode }
  FileClose(FiHandleDataFile);
  FiHandleDataFile := FileOpen(sFilePath, fmOpenReadWrite or fmShareDenyNone);
  if FiHandleDataFile < 0 then
    raise EFOpenError.CreateFmt(SFOpenError, [sFilePath]);
  FsFilePath:= sFilePath;
end;


destructor TFileChannelDataHelperFunctions.Destroy;
begin
  FileClose(FiHandleDataFile);
  inherited Destroy;
end;


function TFileChannelDataHelperFunctions.GetSize: Int64;
begin
  Result:= FileSize(FsFilePath);
end;


end.

