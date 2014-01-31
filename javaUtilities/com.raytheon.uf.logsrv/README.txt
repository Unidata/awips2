##
# This software was developed and / or modified by Raytheon Company,
# pursuant to Contract DG133W-05-CQ-1067 with the US Government.
# 
# U.S. EXPORT CONTROLLED TECHNICAL DATA
# This software product contains export-restricted data whose
# export/transfer/disclosure is restricted by U.S. law. Dissemination
# to non-U.S. persons whether in the United States or abroad requires
# an export license or other authorization.
# 
# Contractor Name:        Raytheon Company
# Contractor Address:     6825 Pine Street, Suite 340
#                         Mail Stop B8
#                         Omaha, NE 68106
#                         402.291.0100
# 
# See the AWIPS II Master Rights File ("Master Rights File.pdf") for
# further licensing information.
##



Build instructions
-----------------------
In Eclipse run the build.xml by right clicking on the build.xml file and
running it.  That will produce a tar.gz with everything you need inside of it.


Install instructions
----------------------- 
Place the tar.gz file where you would like to install it, and run tar -xvf on
the tar.gz file.  Next go into the conf directory and modify config.xml to
 the best settings for your cluster.  Then open receiver.xml and go to the
 bottom of the file.  Edit the address to the name of the machine where
 you are installing the log service, and pick a port if you are not happy with
 the default.  Once your config settings are right, run the bin/logsrv.sh
 script to start the log service.
 
 
 Setup of Client Processes
 ------------------
 At this point the log service is running but nothing is reporting to it.  You
 can configure any Java process using logback to report to it, but with the
 current version that is just EDEX and CAVE.  (Note technically you could
 configure AlertViz to report to the service too, but that is fairly pointless).
 
 To configure EDEX or CAVE to report to the log service, find the logback
 config files for those applications.  In EDEX they can typically be found at
 /awips2/edex/conf.  If you are not sure which logback file corresponds to
 which JVM, look at the shell scripts in /awips2/edex/etc.  If not explicitly
 stated in their corresponding shell script, then the JVMs use the logback file
 specified in default.sh.
 
 In CAVE the logback config files can typically be found at
 /awips2/cave/plugins/com.raytheon.uf.viz.core_${VERSION}.
 
 Once found, use a text editor to open the logback config file corresponding
 to the process you wish to report to the log service.  You need to add two
 things to the file, an appender and an appender-ref.
 
 Add an appender like the example below in the appenders section (generally
 towards the top of the file):
 
<appender class="ch.qos.logback.classic.net.SocketAppender" name="remoteLogSrv">
   <includeCallerData>false</includeCallerData>
   <port>5477</port>
   <remoteHost>dev33.oma.us.ray.com</remoteHost>
   <filter class="ch.qos.logback.classic.filter.ThresholdFilter">
   <level>WARN</level>
   </filter>
</appender>
 
 Replace the remoteHost address with the machine where you installed the
 log service.  Note that you must have a network route between the two machines
 as it uses a socket to report the errors.  Replace the port with the port you chose
 in receiver.xml.  You can alter the threshold if need be.
 
 Next you must add your new remoteLogSrv appender to a logger.  Note that a
 logger can have multiple appenders, but any logger with additivity="false" will
 not go through the root log.  For EDEX, the recommendation is to add
 it to the root logger.  For example:
 
<root>
   <level value="INFO"/>
   <appender-ref ref="asyncConsole"/>
   <appender-ref ref="remoteLogSrv"/>
</root>
 
 For CAVE, the recommendation is to add it to the CaveLogger.  For example:
 
<logger name="CaveLogger" additivity="false">
   <level value="ALL"/>
   <appender-ref ref="AsyncCaveLogAppender"/>
   <appender-ref ref="remoteLogSrv"/>
</logger>

Once you save the modified logback config file, you're done.  Logback will
automatically pick up a changed configuration within a minute or two, and
the Java processes you configured will start appending to the socket.


How it works
--------------
The log service is listening on the socket and will store the messages in a
derby database on the filesystem, and then at the scheduled time it will
attempt to analyze and consolidate the errors based on the information it has
available.  Then it will send an email using the configuration, reporting in an order
of what errors it thinks are most significant.

Note that it does not matter if the Java processes have the socket appender
configured but the log service is not running.  They will try to connect but then
go back to normal logging.  If the log service is running, they will resume sending
log messages through the socket.  Therefore, it does not hurt to have
everything configured even if the log service or the reporting processes are not
running.


Impacts
-------------
The log service has very little overhead.  The appenders, as configured, will be just
another appender so the "normal" logs will continue to function as usual.  With the
threshold configuration, only WARN and ERROR messages are sent to the log
service.  All of this is done asynchronously so the processing threads that logged
a message will not wait for the message to get sent.  Messages sent to the
service are sent over TCP and are buffered.  As long as the network path to
the log service is not slower than the rate at which the WARN or ERROR messages
are produced, you should not notice any slowdowns or impacts due to processes
reporting to the log service.

Of course, the less an application produces WARN or ERROR messages, the less
overhead.  And the reporting can always be disabled by undoing the modifications
to logback configuration files specified in the setup instructions.  Again, if you
disable reporting to the log service, you do not need to restart the Java process.  


More information
--------------
For more information about the socket appender, please see
http://logback.qos.ch/manual/appenders.html#SocketAppender

For more information about logback configuration files, please see
http://logback.qos.ch/manual/configuration.html#syntax


Bugs and/or Improvements
--------------
If you encounter a bug with the log service or have an idea of how it can be
improved, send an email to nathan.jensen@raytheon.com.

