{%MainUnit fhelp.pas}
const

csMemHelp = 'Reminder:' + LineEnding +
'~~~~~~~~~~' + LineEnding +
'The "Watch" and "Counter" messages do not appear in the TreeView (they are the 2 exceptions to the display''s rule).'  + LineEnding +
'They are displayed in the bottom left panel of the main window. This bottom left panel has 3 tabs:' + LineEnding +
'* "Last" tab: this tab refreshes itself in real time to display each new incoming Watch\Counter message'  + LineEnding +
'with its associated value.'  + LineEnding +
'So, it''s here and not in the TreeView, that you can see arriving Watch\Counter in real time.' + LineEnding +
'* "Selected" tab: this tab displays the existing Watch\Counter with their associated value at the time of the TreeView''s selected-clicked node.' + LineEnding +
'You can go back in the history of the Watch\Counter, step by step, ie clic by clic on a TreeView''s node.' + LineEnding +
'* "History" tab: this tab owns a combobox, where you can choose a Watch\Counter among all the received ones.'  + LineEnding +
'Then, it displays its linear\flat history below, regardless if you''re clicking or not in the TreeView aside.';





