{%MainUnit multilog.pas}

{ Below is the list of the Logger ***meth***od's list to log: }
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
  methSend        = 16;
  { hole }
  methWatch 			= 20;
  methCounter 		= 21;
  methColor				= 22;
  { We can use ltSubEventBetweenEnterAndExitMethods, to indent the Msg depending of it's level in the callstack, between EnterMethod..ExitMethod }
  methSubEventBetweenEnterAndExitMethods = 23;
  { hole [ToDo: add IMAGE_method] }
  methClear 			= 100;


  ctsLogPrefixesMethod: array [methInfo..methSubEventBetweenEnterAndExitMethods] of string = (
    'INFO_method',
    'ERROR_method',
    'WARNING_method',
    'VALUE_method',
    '>>ENTER_method',
    '<<EXIT__method',
    'CONDITIONAL_method',
    'CHECKPOINT_method',
    'TSTRINGS_method',
    'CALL_STACK_method',
    'OBJECT_method',
    'EXCEPTION_method',
    'BITMAP_method',
    'HEAP_INFO_method',
    'MEMORY_method',
    'CUSTOM_DATA_method',
    'SEND_method',
    '', '', '',  { hole }
    'WATCH_method',
    'COUNTER_method',
    'COLOR_method',
    'SUBEVENT_method');


