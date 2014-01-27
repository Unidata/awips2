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
package com.raytheon.uf.common.status.logback;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Timer;
import java.util.TimerTask;
import java.util.regex.Pattern;

import ch.qos.logback.classic.spi.ILoggingEvent;
import ch.qos.logback.core.Appender;
import ch.qos.logback.core.AppenderBase;
import ch.qos.logback.core.spi.AppenderAttachable;

/**
 * Appender for logging based on the thread name of the logging event.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 25, 2010            rjpeter     Initial creation
 * Jun 24, 2013 2142       njensen     Changes for logback compatibility
 * 
 * </pre>
 * 
 * @author rjpeter
 * @version 1.0
 */

public class ThreadBasedAppender<E extends ILoggingEvent> extends
        AppenderBase<E> implements AppenderAttachable<E> {
    private Map<String, Appender<E>> appenderMap = new HashMap<String, Appender<E>>();

    private Map<String, List<Pattern>> threadPatterns = new HashMap<String, List<Pattern>>();

    private Map<String, Appender<E>> threadAppenderCache = new HashMap<String, Appender<E>>();

    private Map<String, Long> threadAppenderTimeCache = new HashMap<String, Long>();

    private String defaultAppenderName;

    private Appender<E> defaultAppender;

    // keep thread names and their associated appender mapped for 10 minutes
    private static final long RETENTION_TIME = 1000 * 60 * 1;

    private Timer cacheTimer;

    @Override
    public void start() {
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
        super.start();
    }

    @Override
    public void addAppender(Appender<E> newAppender) {
        if (newAppender != null && newAppender.getName() != null) {
            appenderMap.put(newAppender.getName(), newAppender);
        }
    }

    @Override
    public Appender<E> getAppender(String name) {
        if (name != null) {
            return appenderMap.get(name);
        }

        return null;
    }

    @Override
    public boolean isAttached(Appender<E> appender) {
        if (appender != null) {
            return appenderMap.containsKey(appender.getName());
        }

        return false;
    }

    @Override
    public void detachAndStopAllAppenders() {
        appenderMap.clear();
    }

    @Override
    public boolean detachAppender(Appender<E> appender) {
        boolean retVal = false;
        if (appender != null) {
            retVal = detachAppender(appender.getName());
        }
        return retVal;
    }

    @Override
    public boolean detachAppender(String name) {
        boolean retVal = false;
        if (name != null) {
            Appender<E> app = appenderMap.remove(name);
            if (app != null) {
                retVal = true;
            }
        }

        return retVal;
    }

    @Override
    protected void append(E event) {
        String threadName = event.getThreadName();
        Appender<E> app = null;

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
                    Map<String, Appender<E>> tmp = new HashMap<String, Appender<E>>(
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
        List<String> keysToRemove = new ArrayList<String>(
                threadAppenderCache.size());

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
                Map<String, Appender<E>> tmp = new HashMap<String, Appender<E>>(
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

    @Override
    public Iterator<Appender<E>> iteratorForAppenders() {
        return appenderMap.values().iterator();
    }
}
