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
package com.raytheon.uf.edex.log;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Timer;
import java.util.TimerTask;
import java.util.Map.Entry;
import java.util.regex.Pattern;

import org.apache.log4j.Appender;
import org.apache.log4j.AppenderSkeleton;
import org.apache.log4j.spi.AppenderAttachable;
import org.apache.log4j.spi.LoggingEvent;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 25, 2010            rjpeter     Initial creation
 * 
 * </pre>
 * 
 * @author rjpeter
 * @version 1.0
 */

public class ThreadBasedAppender extends AppenderSkeleton implements Appender,
        AppenderAttachable {
    private Map<String, Appender> appenderMap = new HashMap<String, Appender>();

    private Map<String, List<Pattern>> threadPatterns = new HashMap<String, List<Pattern>>();

    private Map<String, Appender> threadAppenderCache = new HashMap<String, Appender>();

    private Map<String, Long> threadAppenderTimeCache = new HashMap<String, Long>();

    private String defaultAppenderName;

    private Appender defaultAppender;

    // keep thread names and their associated appender mapped for 10 minutes
    private static final long RETENTION_TIME = 1000 * 60 * 1;

    private Timer cacheTimer;

    @Override
    public void activateOptions() {
        if (defaultAppenderName != null) {
            defaultAppender = appenderMap.get(defaultAppenderName);
        }

        // only keep around thread patterns that match the list of appenders
        if (threadPatterns.size() > 0) {
            Iterator<String> threadPatKeyIter = threadPatterns.keySet()
                    .iterator();
            while (threadPatKeyIter.hasNext()) {
                String appender = threadPatKeyIter.next();
                if (!appenderMap.containsKey(appender)) {
                    threadPatKeyIter.remove();
                }
            }
        }

        // setup a timed purge of the cached threads
        cacheTimer = new Timer();

        TimerTask purgeCache = new TimerTask() {
            @Override
            public void run() {
                removeOldEntries();
            }
        };

        cacheTimer.schedule(purgeCache, 1000, 60000);
    }

    @Override
    public void addAppender(Appender newAppender) {
        if (newAppender != null && newAppender.getName() != null) {
            appenderMap.put(newAppender.getName(), newAppender);
        }
    }

    @Override
    public Enumeration<Appender> getAllAppenders() {
        return Collections.enumeration(appenderMap.values());
    }

    @Override
    public Appender getAppender(String name) {
        if (name != null) {
            return appenderMap.get(name);
        }

        return null;
    }

    @Override
    public boolean isAttached(Appender appender) {
        if (appender != null) {
            return appenderMap.containsKey(appender.getName());
        }

        return false;
    }

    @Override
    public void removeAllAppenders() {
        appenderMap.clear();
    }

    @Override
    public void removeAppender(Appender appender) {
        if (appender != null) {
            appenderMap.remove(appender.getName());
        }
    }

    @Override
    public void removeAppender(String name) {
        if (name != null) {
            appenderMap.remove(name);
        }
    }

    @Override
    protected void append(LoggingEvent event) {
        String threadName = event.getThreadName();
        Appender app = null;

        app = threadAppenderCache.get(threadName);

        // TODO: Is the null appender possible?
        if (app == null) {
            // determine which appender to use
            APPENDER_SEARCH: for (Entry<String, List<Pattern>> entry : threadPatterns
                    .entrySet()) {
                for (Pattern pat : entry.getValue()) {
                    if (pat.matcher(threadName).matches()) {
                        String appenderName = entry.getKey();
                        app = appenderMap.get(appenderName);
                        break APPENDER_SEARCH;
                    }
                }
            }

            if (app == null && defaultAppender != null) {
                app = defaultAppender;
            }

            if (app != null) {
                synchronized (this) {
                    // not modifiying the map directly to avoid concurrent
                    // exceptions without forcing synchronization
                    Map<String, Appender> tmp = new HashMap<String, Appender>(
                            threadAppenderCache);
                    tmp.put(threadName, app);
                    threadAppenderCache = tmp;
                    Map<String, Long> tmpTime = new HashMap<String, Long>(
                            threadAppenderTimeCache);
                    tmpTime.put(threadName, System.currentTimeMillis());
                    threadAppenderTimeCache = tmpTime;
                }
            }
        }

        if (app != null) {
            // value already exists, no sync block necessary
            threadAppenderTimeCache.put(threadName, System.currentTimeMillis());
            app.doAppend(event);
        }
    }

    @Override
    public void close() {
    }

    @Override
    public boolean requiresLayout() {
        return true;
    }

    public void setThreadPatterns(String value) {
        String[] appenderPatterns = value.split("[;\n]");
        for (String appenderPattern : appenderPatterns) {
            String[] tokens = appenderPattern.split("[:,]");
            if (tokens.length > 1) {
                String appender = tokens[0];
                List<Pattern> patterns = new ArrayList<Pattern>(
                        tokens.length - 1);
                for (int i = 1; i < tokens.length; i++) {
                    patterns.add(Pattern.compile(tokens[i]));
                }
                threadPatterns.put(appender, patterns);
            }
        }
    }

    public void setDefaultAppender(String defaultAppender) {
        defaultAppenderName = defaultAppender;
    }

    private void removeOldEntries() {
        long curTime = System.currentTimeMillis();
        List<String> keysToRemove = new ArrayList<String>(threadAppenderCache
                .size());

        for (Entry<String, Long> entry : threadAppenderTimeCache.entrySet()) {
            if (curTime - entry.getValue() > RETENTION_TIME) {
                keysToRemove.add(entry.getKey());
            }
        }

        for (String key : keysToRemove) {
            // synchornized inside the loop so that it is locked for short
            // periods so new log threads can continue to run
            synchronized (this) {
                // not modifiying the map directly to avoid concurrent
                // exceptions without forcing synchronization
                Map<String, Appender> tmp = new HashMap<String, Appender>(
                        threadAppenderCache);
                tmp.remove(key);
                threadAppenderCache = tmp;
                Map<String, Long> tmpTime = new HashMap<String, Long>(
                        threadAppenderTimeCache);
                tmpTime.remove(key);
                threadAppenderTimeCache = tmpTime;
            }
        }
    }
}
