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
package com.raytheon.uf.viz.stats.collector;

import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;

import com.raytheon.uf.common.event.EventBus;
import com.raytheon.uf.common.stats.StatisticsEvent;

/**
 * Statistics Collector used to collect stats from different locations.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 8, 2013             mpduff      Initial creation
 * Feb 19, 2013  1635      dhladky     Safety from null
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class StatsCollector {
    private static final StatsCollector instance = new StatsCollector();

    /**
     * Concurrent map of events -> start time in ms.
     */
    private final ConcurrentMap<String, StatisticsEvent> statsDataMap = new ConcurrentHashMap<String, StatisticsEvent>();

    private StatsCollector() {

    }

    /**
     * Get the instance.
     * 
     * @return The instance
     */
    public static final StatsCollector getInstance() {
        return instance;
    }

    /**
     * Mark a start point for this event.
     * 
     * @param key
     *            The Statistics key
     * 
     * @param event
     *            The event to track
     */
    public static void start(String key, StatisticsEvent event) {
        getInstance().setStart(key, event);
    }

    /**
     * Mark a stop point for this event.
     * 
     * @param key
     *            StatisticsEvent
     */
    public static void stop(String key) {
        getInstance().setStop(key);
    }

    /**
     * Mark a start point for this event.
     * 
     * @param key
     *            The Statistics key
     * @param event
     *            The event to track
     */
    public void setStart(String key, StatisticsEvent event) {
        statsDataMap.put(key, event);
    }

    /**
     * Mark a stop point for this id.
     * 
     * @param key
     *            The Statistics key
     */
    public void setStop(String key) {
        StatisticsEvent event = statsDataMap.remove(key);
        if (event != null) {
            event.finalizeEvent();
            EventBus.publish(event);
        }
    }
}
