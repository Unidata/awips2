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
package com.raytheon.uf.edex.stats.util;

import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.google.common.annotations.VisibleForTesting;
import com.raytheon.uf.common.event.Event;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.serialization.JAXBManager;
import com.raytheon.uf.common.stats.xml.StatisticsAggregate;
import com.raytheon.uf.common.stats.xml.StatisticsConfig;
import com.raytheon.uf.common.stats.xml.StatisticsEvent;
import com.raytheon.uf.common.stats.xml.StatisticsGroup;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.util.ReflectionException;
import com.raytheon.uf.common.util.ReflectionUtil;

/**
 * Loads StatisticsConfig files from localization.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 21, 2012            jsanchez    Updated error handling and validated config files.
 * Nov 07, 2012   1317     mpduff      Update config files.
 * Nov 29, 2012   1350     rjpeter     Updated to static, fixed localization, increased validation.
 * Jan 15, 2013   1487     djohnson    Make validate() static and public, so it can be run independently.
 * </pre>
 * 
 * @author jsanchez
 * 
 */
public class ConfigLoader {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(ConfigLoader.class);

    private final JAXBManager jaxbManager;

    private static final ConfigLoader instance = new ConfigLoader();

    private final IPathManager pm = PathManagerFactory.getPathManager();

    private List<StatisticsConfig> configurations = Collections.emptyList();

    private Map<String, StatisticsEvent> classToEventMap = Collections
            .emptyMap();

    private static final String STATS_DIR = "stats";

    public static ConfigLoader getInstance() {
        return instance;
    }

    private ConfigLoader() {
        try {
            jaxbManager = new JAXBManager(StatisticsConfig.class);
            load();
        } catch (Exception e) {
            throw new ExceptionInInitializerError(e);
        }
    }

    /**
     * Returns a list of all StatisticsConfig files.
     * 
     * @return
     */
    public List<StatisticsConfig> getConfigurations() {
        return configurations;
    }

    /**
     * Returns a map of event type to statistics event.
     * 
     * @return
     */
    public Map<String, StatisticsEvent> getTypeView() {
        return classToEventMap;
    }

    /**
     * Loads the StatisticsConfig files in the STATS_DIR directory.
     */
    public void load() throws Exception {
        LocalizationContext[] searchContext = pm
                .getLocalSearchHierarchy(LocalizationType.EDEX_STATIC);
        Map<String, LocalizationFile> statConfs = new HashMap<String, LocalizationFile>();

        // grab all stats from contexts, allowing overwrite by name
        for (LocalizationContext ctx : searchContext) {
            LocalizationFile[] localizationFiles = pm.listFiles(ctx, STATS_DIR,
                    null, false, true);
            for (LocalizationFile lf : localizationFiles) {
                String name = lf.getName();
                if (!statConfs.containsKey(name)) {
                    statConfs.put(name, lf);
                }
            }
        }

        if (!statConfs.isEmpty()) {
            List<StatisticsConfig> myConfigurations = new ArrayList<StatisticsConfig>(
                    statConfs.size());
            Map<String, StatisticsEvent> myEvents = new HashMap<String, StatisticsEvent>();

            for (LocalizationFile lf : statConfs.values()) {
                StatisticsConfig config = lf.jaxbUnmarshal(
                        StatisticsConfig.class, jaxbManager);
                if (config != null) {
                    validate(myEvents, config);
                    if (!config.getEvents().isEmpty()) {
                        myConfigurations.add(config);
                    }
                }
            }

            configurations = myConfigurations;
            classToEventMap = myEvents;
        }
    }

    /**
     * Validates the StatisticsConfig remove invalid
     * events/aggregates/duplicates.
     * 
     * @param config
     */
    @VisibleForTesting
    public static void validate(Map<String, StatisticsEvent> eventMap,
            StatisticsConfig config) {
        for (Iterator<StatisticsEvent> iter = config.getEvents().iterator(); iter
                .hasNext();) {
            StatisticsEvent event = iter.next();
            String eventType = event.getType();
            if (!eventMap.containsKey(eventType)) {
                try {
                    Class<?> clazz = Class.forName(eventType);
                    // verify the type is an Event
                    clazz.asSubclass(Event.class);

                    // validate groupBy fields can be found
                    List<StatisticsGroup> groups = event.getGroupList();
                    if (groups != null) {
                        List<Method> groupByMethods = new ArrayList<Method>(
                                groups.size());
                        Set<String> currentFields = new HashSet<String>();

                        for (Iterator<StatisticsGroup> groupIter = groups
                                .iterator(); groupIter.hasNext();) {
                            StatisticsGroup group = groupIter.next();
                            String fieldName = group.getName();
                            if (!currentFields.contains(fieldName)) {
                                try {
                                    Method m = ReflectionUtil.getGetterMethod(
                                            clazz, fieldName);
                                    groupByMethods.add(m);
                                    currentFields.add(fieldName);
                                } catch (ReflectionException e) {
                                    groupIter.remove();
                                    statusHandler
                                            .warn("'"
                                                    + fieldName
                                                    + "' does not have getter method. Group being removed");
                                }
                            } else {
                                statusHandler
                                        .warn("'"
                                                + fieldName
                                                + "' already defined. Duplicate group being removed.");
                            }
                        }

                        event.setGroupByMethods(groupByMethods);
                    }

                    // validate aggregate methods can be found and are primitive
                    List<StatisticsAggregate> curAggregates = event
                            .getAggregateList();
                    List<Method> aggregateMethods = new ArrayList<Method>(
                            curAggregates.size());
                    Set<String> currentFields = new HashSet<String>();

                    for (Iterator<StatisticsAggregate> aggrIter = curAggregates
                            .iterator(); aggrIter.hasNext();) {
                        StatisticsAggregate aggregate = aggrIter.next();
                        String aggregateField = aggregate.getField();

                        try {
                            Field field = clazz
                                    .getDeclaredField(aggregateField);
                            if (field.getType().isPrimitive()) {
                                if (!currentFields.contains(aggregateField)) {
                                    try {
                                        Method m = ReflectionUtil
                                                .getGetterMethod(clazz,
                                                        aggregateField);
                                        aggregateMethods.add(m);
                                        currentFields.add(aggregateField);
                                    } catch (ReflectionException e) {
                                        aggrIter.remove();
                                        statusHandler
                                                .warn("'"
                                                        + aggregateField
                                                        + "' does not have getter method. Aggregate being removed");
                                    }
                                } else {
                                    aggrIter.remove();
                                    statusHandler
                                            .warn("'"
                                                    + aggregateField
                                                    + "' already defined. Duplicate aggregate being removed. ");
                                }
                            } else {
                                aggrIter.remove();
                                statusHandler
                                        .warn("'"
                                                + aggregateField
                                                + "' not a primitive type. Aggregate being removed. ");
                            }
                        } catch (NoSuchFieldException e) {
                            aggrIter.remove();
                            statusHandler
                                    .warn("'"
                                            + aggregateField
                                            + "' not a valid field. Aggregate being removed. ");
                        }
                    }

                    if (!curAggregates.isEmpty()) {
                        event.setAggregateMethods(aggregateMethods);
                        eventMap.put(eventType, event);
                    } else {
                        iter.remove();
                    }
                } catch (ClassNotFoundException e) {
                    iter.remove();
                    statusHandler.warn("'" + eventType
                            + "' not a valid type. Type being removed. ");
                } catch (ClassCastException e) {
                    iter.remove();
                    statusHandler.warn("'" + eventType
                            + "' not an Event type. Type being removed. ");
                }
            } else {
                iter.remove();
                statusHandler
                        .warn("'"
                                + eventType
                                + "' is already defined.  StatisticsEvent being skipped.");
            }
        }
    }
}
