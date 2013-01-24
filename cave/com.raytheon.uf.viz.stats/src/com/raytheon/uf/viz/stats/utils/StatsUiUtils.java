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
package com.raytheon.uf.viz.stats.utils;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import com.google.common.annotations.VisibleForTesting;
import com.raytheon.uf.common.stats.data.StatsEventData;
import com.raytheon.uf.common.stats.xml.StatisticsAggregate;
import com.raytheon.uf.common.stats.xml.StatisticsConfig;
import com.raytheon.uf.common.stats.xml.StatisticsEvent;
import com.raytheon.uf.common.stats.xml.StatisticsGroup;

/**
 * Utility class for the Statistics UI.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 8, 2012     728     mpduff      Initial creation
 *
 * </pre>
 *
 * @author mpduff
 * @version 1.0
 */

public class StatsUiUtils {
    /** List of statistics configuration objects */
    private List<StatisticsConfig> configList;

    /**
     * Map of category -> Map of event type -> event type object
     */
    private final Map<String, Map<String, StatsEventData>> eventMap = new HashMap<String, Map<String, StatsEventData>>();

    /**
     * Constructor
     */
    public StatsUiUtils() {
    }

    /**
     * Generate the configuration objects.
     *
     * @param configList
     */
    public void generateData(List<StatisticsConfig> configList) {
        this.configList = configList;

        for (StatisticsConfig config: configList) {
            processConfig(config);
        }
    }

    /**
     * Process a configuration object.
     *
     * @param config configuration object
     */
    @VisibleForTesting
    void processConfig(StatisticsConfig config) {
        for (StatisticsEvent event: config.getEvents()) {
            processEvent(event);
        }
    }

    /**
     * Process a configuration event object
     *
     * @param event event config object
     */
    @VisibleForTesting
    void processEvent(StatisticsEvent event) {
        if (!eventMap.containsKey(event.getCategory())) {
            eventMap.put(event.getCategory(), new HashMap<String, StatsEventData>());
        }

        StatsEventData eventData = new StatsEventData();

        eventData.setDisplayName(event.getDisplayName());
        eventData.setType(event.getType());
        List<StatisticsAggregate> attributeList = event.getAggregateList();
        for (StatisticsAggregate attribute: attributeList) {
            eventData.addAttribute(attribute.getDisplayName());
        }

        for (StatisticsGroup group: event.getGroupList()) {
            eventData.addGroup(group.getDisplayName(), group.getName());
        }

        eventMap.get(event.getCategory()).put(eventData.getType(), eventData);
    }

    /**
     * Get the event types for this category.
     *
     * @param category The category
     * @return map of event display names -> event id
     */
    @VisibleForTesting
    public Map<String, String> getEventTypes(String category) {
        Map<String, StatsEventData> eventsMap = eventMap.get(category);
        Map<String, String> typeMap = new TreeMap<String, String>();
        for (String key: eventsMap.keySet()) {
            StatsEventData data = eventsMap.get(key);
            typeMap.put(data.getDisplayName(), data.getType());
        }
        return typeMap;
    }

    /**
     * Get the attributes/data types for the given category and event.
     *
     * @param category The category
     * @param type The event type
     * @return Map of attribute display name -> field
     */
    @VisibleForTesting
    public Map<String, String> getEventAttributes(String category, String type) {
        Map<String, String> attMap = new TreeMap<String, String>();
        for (StatisticsConfig config: configList) {
            for (StatisticsEvent event: config.getEvents()) {
                if (event.getCategory().equals(category) && event.getDisplayName().equals(type)) {
                    for (StatisticsAggregate agg: event.getAggregateList()) {
                        attMap.put(agg.getDisplayName(), agg.getField());
                    }
                }
            }
        }

        return attMap;
    }

    /**
     * @return the eventMap
     */
    @VisibleForTesting
    public Map<String, Map<String, StatsEventData>> getEventMap() {
        return eventMap;
    }

    /**
     * Get the StatsEventData object for the given category and event type.
     *
     * @param category category
     * @param type event type
     * @return StatsEventData object
     */
    @VisibleForTesting
    public StatsEventData getEventData(String category, String type) {
        return eventMap.get(category).get(type);
    }

    /**
     * Get the aggregate config file for the given category, event type, and aggregate display name
     *
     * @param category category
     * @param typeID event type id
     * @param attributeDisplayName attribut display name
     * @return StatisticsAggregate object
     */
    public StatisticsAggregate getAggregateConfig(String category,
            String typeID, String attributeDisplayName) {
        for (StatisticsConfig config : configList) {
            for (StatisticsEvent event: config.getEvents()) {
                if (event.getCategory().equals(category) && event.getType().equals(typeID)) {
                    for (StatisticsAggregate agg: event.getAggregateList()) {
                        if (agg.getDisplayName().equals(attributeDisplayName)) {
                            return agg;
                        }
                    }
                }
            }
        }

        return null;
    }
}
