/**
 * This software was developed and / or modified by Raytheon Company,
 * pursuant to Contract DG133W-05-CQ-1067 with the US Government.
 * 
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * This software product contains export-restricted data whose
 * export/transfer/disclosure is restricted by U.S. law. Dissemination
 * to non-U.S. persons whether in the United States or abroad requires
 * an export license or other authorization.
 * 
 * Contractor Name:        Raytheon Company
 * Contractor Address:     6825 Pine Street, Suite 340
 *                         Mail Stop B8
 *                         Omaha, NE 68106
 *                         402.291.0100
 * 
 * See the AWIPS II Master Rights File ("Master Rights File.pdf") for
 * further licensing information.
 **/
package com.raytheon.openfire.plugin.detailedfeedlog;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;
import java.util.Queue;
import java.util.TimerTask;
import java.util.concurrent.ConcurrentLinkedQueue;

import org.jivesoftware.openfire.MessageRouter;
import org.jivesoftware.openfire.XMPPServer;
import org.jivesoftware.openfire.container.Plugin;
import org.jivesoftware.openfire.container.PluginManager;
import org.jivesoftware.openfire.muc.MUCEventDispatcher;
import org.jivesoftware.util.JiveGlobals;
import org.jivesoftware.util.Log;
import org.jivesoftware.util.PropertyEventDispatcher;
import org.jivesoftware.util.PropertyEventListener;
import org.jivesoftware.util.TaskEngine;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.raytheon.openfire.plugin.detailedfeedlog.listener.DetailedFeedLogEventListener;

/**
 * Plugin that logs and purges qualifying packets in Openfire.
 * 
 * This plugin handles logging history so to prevent conflicting messages, the
 * group chat history settings need to be turned off on the Openfire server.
 * Group Chat > Group Chat Setting > History Settings > Don't Show History
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 23, 2012            mnash       Initial creation
 * Mar 31, 2014 2937       bgonzale    Separate room logging from other logging
 *                                     in a 'Rooms' subdirectory so that readLogInfo()
 *                                     only parses room logs. Add finally block to close
 *                                     the buffered readers.
 * Apr 07, 2014 2937       bgonzale    Use new LogEntry toString and fromString methods
 *                                     and changed error handling to log each bad line.
 *                                     Create new DateFormatter for the purge thread.
 *                                     Moved presence/site determination out of log method.
 * 
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */

public class DetailedFeedLogPlugin implements Plugin, PropertyEventListener {

    // TODO, need to implement a read from the log file for if the server goes
    // down

    private static Logger logger = LoggerFactory
            .getLogger(DetailedFeedLogPlugin.class);

    private static Map<String, Queue<LogEntry>> entries;

    private static final int defaultPurgeLogTime = 21600;

    private static final int defaultRunInterval = 60;

    // how long ago to purge, anything older than this many seconds will be
    // purged
    private int purgeLogTime = JiveGlobals.getIntProperty(LOG_TIME_TO_KEEP,
            defaultPurgeLogTime);

    // how often to run the purge
    private int runInterval = JiveGlobals.getIntProperty(LOG_PURGE_INTERVAL,
            defaultRunInterval);

    private static final int SECONDS_TO_MILLIS = 1000;

    // the constant for the global value for how long to keep in the logs
    private static final String LOG_TIME_TO_KEEP = "detailedlogttl";

    // the constant for the global value for how often to purge
    private static final String LOG_PURGE_INTERVAL = "detailedlogfreq";

    // the task that will run the purge
    private TimerTask task;

    private DetailedFeedLogEventListener feedListener;

    private static final String ROOM_DIR = "Rooms";

    private static final String DATE_FORMAT_STR = "MM/dd/yyyy h:mm a";

    private DateFormat dateFormat = new SimpleDateFormat(DATE_FORMAT_STR);

    private File ROOM_LOG_DIR;

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.jivesoftware.openfire.container.Plugin#initializePlugin(org.jivesoftware
     * .openfire.container.PluginManager, java.io.File)
     */
    @Override
    public void initializePlugin(PluginManager arg0, File arg1) {
        // register a listener for updates to the openfire configurations page
        PropertyEventDispatcher.addListener(this);

        entries = new HashMap<String, Queue<LogEntry>>();

        // log dir for rooms
        String logDirName = Log.getLogDirectory() + ROOM_DIR;
        File logDir = new File(logDirName);
        try {
            logDir.mkdir();
        } catch (SecurityException e) {
            logger.error("Failed to create Room log directory.", e);
        }
        ROOM_LOG_DIR = logDir;

        // read the log in from the file
        readLogInfo();

        // start the timer that inits the purging capability
        initPurge();

        feedListener = new DetailedFeedLogEventListener();
        MessageRouter router = XMPPServer.getInstance().getMessageRouter();
        feedListener.setRouter(router);

        // Make it possible for the listener to receive events.
        MUCEventDispatcher.addListener(feedListener);
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.jivesoftware.openfire.container.Plugin#destroyPlugin()
     */
    @Override
    public void destroyPlugin() {
        // want to clean up nicely, so remove the chat listener
        MUCEventDispatcher.removeListener(feedListener);

        // flush all the current information out of log and write it to the feed
        // log file
        writeToFile(dateFormat);
        entries.clear();

        // cancel the task so we aren't trying to purge when there is no plugin
        // running
        TaskEngine.getInstance().cancelScheduledTask(task);
        PropertyEventDispatcher.removeListener(this);
    }

    private void readLogInfo() {
        File[] files = ROOM_LOG_DIR.listFiles();

        for (File file : files) {
            BufferedReader bReader = null;
            try {
                FileReader reader = new FileReader(file);
                bReader = new BufferedReader(reader);
                while (bReader.ready()) {
                    String line = bReader.readLine();
                    try {
                        LogEntry entry = LogEntry.fromString(line, dateFormat);
                        addToMemoryLog(entry,
                                file.getName()
                                .replaceAll(".log", ""));
                    } catch (ParseException e) {
                        logger.error("Failed in log " + file.getName()
                                + " to parse date from line: " + line, e);
                    } catch (ArrayIndexOutOfBoundsException e) {
                        logger.info("Failed in log " + file.getName()
                                + " to read line:" + line, e);
                    }
                }
            } catch (FileNotFoundException e) {
                logger.error("Unable to find " + file.getName(), e);
            } catch (IOException e) {
                logger.error("Unable to read from " + file.getName(), e);
            } finally {
                if (bReader != null) {
                    try {
                        bReader.close();
                    } catch (IOException e) {
                        StringBuilder sb = new StringBuilder(
                                "Failed to close log file: ").append(file
                                .getAbsolutePath());
                        logger.error(sb.toString(), e);
                    }
                }
            }
        }
    }

    /**
     * To log in openfire
     * 
     * @param user
     *            - fqname
     * @param message
     */
    public static void log(String user, String site, String message,
            String room) {
        logger.info("Logging : " + user + " \"" + message + "\"");
        Date date = new Date();
        LogEntry entry = new LogEntry(date, site, user, message);
        addToMemoryLog(entry, room);
    }

    /**
     * Add the log message to memory
     * 
     * @param entry
     * @param room
     */
    private static void addToMemoryLog(LogEntry entry,
            String room) {
        Queue<LogEntry> queue = null;
        if (entries.containsKey(room)) {
            queue = entries.get(room);
        } else {
            queue = addRoomToLog(room);
        }
        queue.add(entry);
    }

    public static Queue<LogEntry> addRoomToLog(String room) {
        Queue<LogEntry> queue = new ConcurrentLinkedQueue<LogEntry>();
        entries.put(room, queue);
        return queue;
    }

    public static void removeRoomFromLog(String room) {
        entries.remove(room);
    }

    /**
     * Write out the current in memory log to a file
     */
    private void writeToFile(DateFormat dateFormat) {
        try {
            // writes out each log file for each room, all active and permanent,
            // and for rooms that the purge has not removed all logs yet
            for (String room : entries.keySet()) {
                File file = new File(ROOM_LOG_DIR, room + ".log");
                FileWriter writer = new FileWriter(file);

                for (LogEntry entry : entries.get(room)) {
                    writer.write(entry.toString(dateFormat));
                    writer.write("\n");
                }
                writer.close();

                // clean up empty logs
                if (file.length() == 0) {
                    removeRoomFromLog(room);
                    file.delete();
                }
            }
        } catch (IOException e) {
            logger.error("Unable to write to file", e);
        }
    }

    /**
     * Method for starting the TimerTask that runs the purge on the file
     */
    private void initPurge() {
        task = new TimerTask() {
            @Override
            public void run() {
                Thread thread = new Thread(new Runnable() {
                    @Override
                    public void run() {
                        // the current time minus the purge log time in
                        // millis creates the latest time to keep
                        Date latestTimeToKeep = new Date(
                                System.currentTimeMillis()
                                        - (purgeLogTime * SECONDS_TO_MILLIS));
                        // DateFormat for this thread
                        DateFormat dateFormat = new SimpleDateFormat(
                                "MM/dd/yyyy h:mm a");
                        String dateText = dateFormat.format(latestTimeToKeep);
                        logger.info("Purging the log of anything before "
                                + dateText);

                        for (String room : entries.keySet()) {
                            Queue<LogEntry> queue = entries.get(room);
                            LogEntry entry = queue.peek();
                            while (entry != null) {
                                if (entry.getDate().before(latestTimeToKeep)) {
                                    queue.remove();
                                    entry = queue.peek();
                                } else {
                                    break;
                                }
                            }
                        }

                        // write to file now since we have possibly purged items
                        writeToFile(dateFormat);
                    }
                });
                thread.run();
            }
        };
        // schedule this to start in runInterval and to run every runInterval
        // seconds
        TaskEngine.getInstance().schedule(task,
                runInterval * SECONDS_TO_MILLIS,
                runInterval * SECONDS_TO_MILLIS);
    }

    /**
     * @return the log
     */
    public static Queue<LogEntry> getLog(String room) {
        return entries.get(room);
    }

    public void propertyDeleted(String arg0, Map<String, Object> arg1) {
        // do nothing, as we want to keep the current properties if some are
        // deleted
    }

    @Override
    public void propertySet(String arg0, Map<String, Object> arg1) {
        if (arg0.equals(LOG_PURGE_INTERVAL)) {
            // 60 is the default value (1 minute)
            int tempPurgeInterval = JiveGlobals.getIntProperty(arg0,
                    defaultRunInterval);
            if (tempPurgeInterval != runInterval) {
                runInterval = tempPurgeInterval;
                logger.info("Log Purge Time has been changed to " + runInterval
                        + " seconds");
                TaskEngine.getInstance().cancelScheduledTask(task);
                TaskEngine.getInstance().schedule(task,
                        runInterval * SECONDS_TO_MILLIS,
                        runInterval * SECONDS_TO_MILLIS);
            }
        } else if (arg0.equals(LOG_TIME_TO_KEEP)) {
            // 21600 is the default value (6 hours)
            int tempPurgeTime = JiveGlobals.getIntProperty(arg0,
                    defaultPurgeLogTime);
            if (tempPurgeTime != purgeLogTime) {
                purgeLogTime = tempPurgeTime;
                logger.info("Log Time to Keep has been changed to "
                        + purgeLogTime + " seconds");
            }
        }
    }

    @Override
    public void xmlPropertyDeleted(String arg0, Map<String, Object> arg1) {
        // do nothing, as we don't care about xml properties deleted
    }

    @Override
    public void xmlPropertySet(String arg0, Map<String, Object> arg1) {
        // do nothing, as we don't care about xml properties set
    }
}
