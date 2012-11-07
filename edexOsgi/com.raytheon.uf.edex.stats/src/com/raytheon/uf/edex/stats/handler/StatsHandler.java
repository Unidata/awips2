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
package com.raytheon.uf.edex.stats.handler;

import java.util.HashSet;
import java.util.List;
import java.util.Set;

import com.google.common.eventbus.AllowConcurrentEvents;
import com.google.common.eventbus.Subscribe;
import com.raytheon.uf.common.event.Event;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.common.stats.StatsRecord;
import com.raytheon.uf.common.stats.xml.StatisticsConfig;
import com.raytheon.uf.common.stats.xml.StatisticsEvent;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.edex.database.dao.CoreDao;
import com.raytheon.uf.edex.database.dao.DaoConfig;
import com.raytheon.uf.edex.event.EventBus;

/**
 * Subscribes to the event bus and stores them in the appropriate stats table
 *
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 21, 2012            jsanchez    Removed instance variable of event bus.
 * Nov 07, 2012   1317     mpduff      Updated config files.
 *
 * </pre>
 *
 * @author jsanchez
 *
 */
public class StatsHandler {
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(StatsHandler.class);

    private final CoreDao dao = new CoreDao(DaoConfig.forClass("metadata",
            StatsRecord.class));

    private static Set<String> validEventTypes = new HashSet<String>();

    /**
     * Registers StatsHandler with the event bus
     */
    public StatsHandler() {
        EventBus.getInstance().register(this);
    }

    @Subscribe
    @AllowConcurrentEvents
    public void eventListener(Event event) {
        String clazz = String.valueOf(event.getClass().getName());

        if (validEventTypes.contains(clazz)) {
            try {
                byte[] bytes = SerializationUtil.transformToThrift(event);

                StatsRecord record = new StatsRecord();
                record.setDate(event.getDate());
                record.setEventType(clazz);
                record.setEvent(bytes);
                dao.persist(record);
            } catch (SerializationException e) {
                statusHandler.error("Error transforming to Thrift.", e);
            }
        }
    }

    /**
     * Set the valid event types.
     *
     * @param configurations
     *            List of StatisticsConfig objects
     */
    public static void setValidEventTypes(List<StatisticsConfig> configurations) {
        validEventTypes = new HashSet<String>();
        for (StatisticsConfig config : configurations) {
            for (StatisticsEvent event : config.getEvents()) {
                validEventTypes.add(event.getType());
            }
        }
    }
}
