{%MainUnit multilog.pas}

const
  (*[*] Beware: if the number of elements is > 32, then TLogChannelUtils must deal with {$PACKSET 4} and use a typecast on the "Set of" with Int64.
   	if the number of elements is > 32, `Set of TForWhichTrackingPurpose` cannot be TypeCasted in Integer. See Lazarus\forum\topic,48204.msg347046.*)
  lwAllApplied = [lwDebug, lwError, lwInfo, lwWarning, lwEvents,
  								lwIPC, lwWorkers,
  								lwStudyChainedEvents];
  lwAll = [lwDebug, lwError, lwInfo, lwWarning, lwEvents,
  				 lwIPC, lwWorkers, lw_7, lw_8,
  				 lwStudyChainedEvents,
  				 lw_10,lw_11,lw_12,lw_13,lw_14,lw_15,lw_16,lw_17,lw_18,lw_19,lw_20,lw_21,lw_22,lw_23,lw_24,lw_25,lw_26,lw_27,lw_28,lw_29,lw_30,
  				 lwLast];





