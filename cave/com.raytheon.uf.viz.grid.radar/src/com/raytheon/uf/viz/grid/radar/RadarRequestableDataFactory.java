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
package com.raytheon.uf.viz.grid.radar;

import java.util.HashMap;
import java.util.Map;

import org.apache.commons.collections4.map.ReferenceMap;

import com.raytheon.uf.common.dataplugin.annotations.DataURI;
import com.raytheon.uf.common.dataplugin.message.DataURINotificationMessage;
import com.raytheon.uf.common.dataplugin.radar.RadarRecord;
import com.raytheon.uf.common.dataplugin.radar.util.RadarConstants;
import com.raytheon.uf.common.dataplugin.radar.util.RadarRecordUtil;
import com.raytheon.uf.common.jms.notification.INotificationObserver;
import com.raytheon.uf.common.jms.notification.NotificationException;
import com.raytheon.uf.common.jms.notification.NotificationMessage;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.notification.jobs.NotificationManagerJob;
import com.raytheon.viz.awipstools.IToolChangedListener;
import com.raytheon.viz.awipstools.ToolsDataManager;
import com.raytheon.viz.radar.IRadarConfigListener;
import com.raytheon.viz.radar.ui.RadarDisplayManager;

/**
 * Provides a cache of RadarRequestableDataObjects, since these objects have a
 * SoftReference to the raw data, loading a cached object might be able to
 * provide faster data loading.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- -----------------
 * Jul 28, 2021  8611     randerso  Initial creation
 * Feb 22, 2023  9021     mapeters  Add SRM support
 * Mar 21, 2023  2033496  mapeters  Fix cache key to differentiate SRM/RRV
 * Dec 20, 2023  2036519  mapeters  Wrap data in soft references, and move SRM config
 *                                  listening from the individual SRM data records to here
 * Jun 17, 2024  2037092  mapeters  Move instance into LazyHolder
 *
 * </pre>
 *
 * @author randerso
 */
public class RadarRequestableDataFactory implements INotificationObserver,
        IRadarConfigListener, IToolChangedListener {
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(RadarRequestableDataFactory.class);

    private static final String DATAURI_PREFIX = DataURI.SEPARATOR
            + RadarConstants.PLUGIN_NAME + DataURI.SEPARATOR;

    /**
     * Initialization-on-demand holder to prevent instantiation until
     * getInstance() is actually called. Specifically needed for unit tests to
     * be able to mock getInstance().
     */
    private static class LazyHolder {
        private static final RadarRequestableDataFactory instance = new RadarRequestableDataFactory();
    }

    public static RadarRequestableDataFactory getInstance() {
        return LazyHolder.instance;
    }

    /**
     * Cache of parameter abbreviation -> data URI -> requestable data. The
     * parameter abbreviation is included to differentiate RRV and SRM, since
     * they use the same product codes.
     */
    private final Map<String, ReferenceMap<String, RadarRequestableData>> requestableDataMap = new HashMap<>();

    private RadarRequestableDataFactory() {
        NotificationManagerJob.addObserver("edex.alerts", this);
        RadarDisplayManager.getInstance().addListener(this);
        ToolsDataManager.getInstance().addStormTrackChangedListener(this);
    }

    public RadarRequestableData getRadarRequestableData(RadarRecord record,
            String parameterAbbrev) throws VizException {
        String uri = record.getDataURI();
        synchronized (requestableDataMap) {
            Map<String, RadarRequestableData> uriToDataMap = requestableDataMap
                    .computeIfAbsent(parameterAbbrev,
                            p -> new ReferenceMap<>());
            RadarRequestableData rval = uriToDataMap.get(uri);
            if (rval == null) {
                if (RadarRecordUtil.SRM.equals(parameterAbbrev)) {
                    /*
                     * Special handling for the SRM entry in
                     * RadarProductCodes.xml
                     */
                    rval = new RadarSRMRequestableData(record, parameterAbbrev);
                } else {
                    rval = new RadarRequestableData(record, parameterAbbrev);
                }
                uriToDataMap.put(uri, rval);
            }

            return rval;
        }
    }

    @Override
    public void notificationArrived(NotificationMessage[] messages) {
        for (NotificationMessage message : messages) {
            try {
                handleNotificationMessage(message);
            } catch (NotificationException e) {
                statusHandler.warn(
                        "RadarRequestableDataFactory failed to process a notification",
                        e);
            }
        }

    }

    private void handleNotificationMessage(NotificationMessage message)
            throws NotificationException {
        Object payload = message.getMessagePayload();
        if (payload instanceof DataURINotificationMessage) {
            handleDataURINotificationMessage(
                    (DataURINotificationMessage) payload);
        }
    }

    private void handleDataURINotificationMessage(
            DataURINotificationMessage message) {
        /*
         * Remove any cached data for the updated URIs, so that the updates are
         * seen the next time those URIs are requested.
         */
        for (String dataUri : message.getDataURIs()) {
            if (dataUri.startsWith(DATAURI_PREFIX)) {
                synchronized (requestableDataMap) {
                    for (Map<String, RadarRequestableData> uriToDataMap : requestableDataMap
                            .values()) {
                        uriToDataMap.remove(dataUri);
                    }
                }
            }
        }
    }

    @Override
    public void toolChanged() {
        handleSRMConfigUpdate();
    }

    @Override
    public void updateConfig() {
        handleSRMConfigUpdate();
    }

    private void handleSRMConfigUpdate() {
        synchronized (requestableDataMap) {
            Map<String, RadarRequestableData> srmDataMap = requestableDataMap
                    .get(RadarRecordUtil.SRM);
            if (srmDataMap != null) {
                for (RadarRequestableData srmData : srmDataMap.values()) {
                    ((RadarSRMRequestableData) srmData).clearCache();
                }
            }
        }
    }
}
