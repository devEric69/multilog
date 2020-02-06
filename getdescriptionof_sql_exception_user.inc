{%MainUnit multilog.pas}


{--function TIntegratedLogger.GetDescriptionFieldsOfException (AException: Exception): string;--}
var
  sDescr: string = '';
const
  csSQLmarker = '[SQL error] '; // do not change: [*] the same constant exists in the unit FileChannel
begin
  (* 	Here, you can customize your SQL Exception retrievals, depending on your own hierarchy,
  		which itself often depends on your database driver (SQLdb, IBX, Zeos, ...)	*)
  //Exceptions SQL parsées de la plus ancêtre EDatabaseError .. vers la plus descendante
  if (AException is EDatabaseError) then
  	with (AException as EDatabaseError) do begin
      sDescr:= sDescr + csSQLmarker + LineEnding;	// insert the same marker\qualifier which identify all the SQL exceptions hierachy
		end;
          if (AException is EUpdateError) then begin
	          with (AException as EUpdateError) do begin
              sDescr:= sDescr + '- Context:' + Context + LineEnding;
              sDescr:= sDescr + '- ErrorCode:' + IntToStr(ErrorCode) + LineEnding;
              sDescr:= sDescr + '- PreviousError:' + IntToStr(PreviousError) + LineEnding;
              sDescr:= sDescr + '- OriginalException:' + OriginalException.ClassName + ' - ' + Message + LineEnding;
            end;
          end;
  //Exceptions non SQL ie les autres
{  else if (AException is EDomError) then begin
  	.../...
   end
   else if (AException is EStreamError) .../... then begin
    .../...
   end  }
   result:= sDescr;
{--end;--}
