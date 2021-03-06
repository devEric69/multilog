# Multilog
This is a simplified integration of the Multilog's logging system. 

[You can read here, an overview of the features \ modifications in this fork compared to the original one](https://github.com/devEric69/multilog/blob/master/multilog_doc/overwiew.htm): it's generally speaking the same things as below, repeated differently.
</br><br>
<i> <u>Beware:</u> this fork compiles and works. But I'm still in the process of integrating it into a project: so, it's still in alpha version (it will be modified to make it more flexible and allow everyone to write\code its own logging rules in $Log_SQL.txt, etc).</i></br>

***

</br>
<h1 align="center"> Why this "MultiLog"'s fork? </h1>
Basically, it's the same as "MultiLog" at 97%.</br>
<br>
The only difference is that this is a simplified integration of the logging.</br>
This small transformation is a simplified integration of the logging mechanism, of the logged messages: to summarize, originelle Multilog's TLogger class allows the variation of both, a group of target reasons, and a group of source reasons associated with each message. In the end, the original Multilog's TLogger class logs a message with the intersection of both.</br>
The Class TIntegratedLogger here, uses only one group: the group associated with the message. This group must be set \ modified before the logging call(s).</br>
Each modification of this group, is logged with the character °: so, I can follow where the messages association group is, has been, modified.</br>
Another small change: the Class TFileChannel has been modified to be "thread safe", too. And it can indent the logged events by watching the ESP machine register's variation.
  
</br></br></br>
<h3> Brief summmary of the processing of "MultiLog": </h3>

![multilog_doc/overwiewFiles/ExampleOfMinimumFilter.png](https://github.com/devEric69/multilog/blob/master/multilog_doc/overwiewFiles/ExampleOfMinimumFilter.png)

</br>
<u>prerequisites ❶:</u></br>
The Class TIntegratedLogger has a group of methods to trace a program: methEnterMethod, methExitMethod, methValue, methInfo, methWaring, an so one.</br>
Many methods then... <b>without one: methDebug (there's not a group of methods that carry the methDebug signature) !</b><br>
</br>
<u>prerequisites ❷:</u></br>
<b>Repeated again, MultiLog is composed of sets of methods (each set has its own signature meth*) to trace a program, WITHOUT the methDebug.</b></br>
</br>
<u>prerequisites ❸:</u></br>
==> In TIntegratedLogger, there remains only 1 classification group - a dynamic group \ "set of: it is simply the filter that lists the ***effectively purposes***, the reasons (among all the preceding possible) associated with each TIntegratedLogger's logging method considered, explaining why one writes (actually and sub-sequentially) in the log (see TIntegratedLogger.FsetFilterDynamic_ofWhatCanBeEffectivelyLogged field).</br>

</br></br></br>
<h3>Global explanation:</h3>
Now, a method - which carries an intrinsic signature named "meth*" - is chosen to log. Let's say a simple method: methInfo, methWarning, methError, ...</br>
For which purpose(s), classification group(s)?</br> 
At least, the simplest: lwInfo, or lwWarning, or lwError, or lwDebug.
However, there can be several classification groups for the chosen method: (lwInfo, lwDebug), or (lwWarning, lwDebug), or (lwError, lwDebug). Or (lwInfo, lwDebug, lwIPC), or (lwWarning, lwDebug, lwIPC), or (lwError, lwDebug, lwIPC), and so on depending on how you analize things. 
You can define yours, in multi_user*.inc.
Beware: a group like (lwInfo, lwWarning, lwError, lwDebug, lwIPC) seems unclear, for me anymay. It's up to you ^^.
That's all there is to anderstand...
<pre>
<u><i>Drawn summary...:</i></u></br>
</br>
                     |
  several TIntegratedLogger's API calls 
          are written in the source:
                     |
                  .../...
FsetFilterDynamic_ofWhatCanBeEffectivelyLogged:= [lwEvents];
        (methEnterMethod)→
        (methValue\@integer)→  .  .  (TChannels; it goes below).  .  .>
        (methExitMethod)→
FsetFilterDynamic_ofWhatCanBeEffectivelyLogged:= [lwEvents,lwDebug];
                  .../...
  (methSubEventBetweenEnterAndExitMethods\@boolean)→
                  .../...
                     |
</br></br>
<u><i>...the continuation:</i></u></br>
<br>
each message is multiplexed: it is sent by each existing display channel over time,
on a display medium (TMemo □, TFileText ○,TLogTreeView ▶) where it is displayed:

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;------→            ○ + □
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;|
.  .(TChannels).  .  .>(methValue\@integer)--→&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; □
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;|
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;|
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;|
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;------→            ○
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;|
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;------→            ○ + ▶
</pre>


</br></br></br>
<h3>Here are some screenshots that illustrate the major features (unchanged):</h3>
</br>

➔<b> IPC </b>(<b>I</b>nter <b>P</b>rocesses <b>C</b>ommunication) between between client programs (with ou without GUI, on the left of the image below), and a server program (on the right) which displays the received messages, sent by the above mentioned client applications:

![multilog_doc/overwiewFiles/simpleviewer.png](https://github.com/devEric69/multilog/blob/master/multilog_doc/overwiewFiles/simpleviewer.png)

![multilog_doc/overwiewFiles/clientsandServerIPC.png](https://github.com/devEric69/multilog/blob/master/multilog_doc/overwiewFiles/clientsandServerIPC.png)

 
</br></br></br>
<h3> Other little modifications: </h3>
• SQL Exceptions that inherit from EDatabaseError are all logged in their <b>own Log_SQL.txt</b>. Indeed, in an SQL application, the majority of errors are due to bad SQL statements, leading to subsequent normal Exceptions and errors.</br>
You can customize your own SQL Exception retrievals, depending on your own (SQL Classes) hierarchy, which itself often depends on your database driver (SQLdb, IBX, Zeos, ...), in the file getdescriptionof_sql_exception_user.inc.</br>
• The TFileText medium display is 'thread safe' (in the same way that TMemo was made 'thread safe') with a specialized Semaphore named a Critical section of code. It separates the descrition of SQL Exceptions from the others, each group in its own text file.</br>
• TGroupOfForWhatReasonsToLogActually can be written to, read from, a variable in the ini configuration file. 
This allows to imagine methods to activate - deactivate - a DEBUG mode, at run-time, at the client, for example.</br> 
• added possibility, to see the reasons for each logging: such a log line was added for [lwDebug, lwInfo] reasons; such another line was written for [lwError, lwIPC] reasons; the last one is a simple [lwWarning]; etc.</br> 
• As previously stated, the text file medium can indent the logged entangled events by watching the ESP machine register's variation. 
What's for? With linked components in a chain of responsabilities (like TDbGrid<->TDataSource<->TDataSet), wich themselves have chained events inside them, it is sometimes easier to code a single line in all existing events (mentionning the parameters that can be used or even changed there) in order to anderstand the movies of contextual events, and to study where whe have to code what:

![multilog_doc/overwiewFiles/autoMultiLog_IndentedEBP.png](https://github.com/devEric69/multilog/blob/master/multilog_doc/overwiewFiles/autoMultiLog_IndentedEBP.png) </br>
 
