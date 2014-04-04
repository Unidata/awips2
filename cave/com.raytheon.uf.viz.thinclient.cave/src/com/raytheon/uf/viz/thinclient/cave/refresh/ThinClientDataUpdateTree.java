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
package com.raytheon.uf.viz.thinclient.cave.refresh;

import java.text.SimpleDateFormat;
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TimeZone;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataquery.requests.DbQueryRequest;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint.ConstraintType;
import com.raytheon.uf.common.dataquery.responses.DbQueryResponse;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.msgs.GetServerTimeRequest;
import com.raytheon.uf.common.time.msgs.GetServerTimeResponse;
import com.raytheon.uf.viz.core.RecordFactory;
import com.raytheon.uf.viz.core.alerts.AlertMessage;
import com.raytheon.uf.viz.core.catalog.LayerProperty;
import com.raytheon.uf.viz.core.datastructure.DataCubeContainer;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.requests.ThriftClient;
import com.raytheon.uf.viz.core.rsc.AbstractRequestableResourceData;
import com.raytheon.uf.viz.core.rsc.AbstractResourceData;
import com.raytheon.uf.viz.core.rsc.updater.DataUpdateTree;
import com.raytheon.viz.grid.inv.RadarUpdater;
import com.raytheon.viz.grid.util.RadarAdapter;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 13, 2011            bsteffen     Initial creation
 * Feb 21, 2014 DR 16744   D. Friedman  Add radar/grid updates
 * Apr  1, 2014 DR 17220   D. Friedman Handle uninitialized grid inventory
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

public class ThinClientDataUpdateTree extends DataUpdateTree {
    private IUFStatusHandler statusHandler = UFStatus
            .getHandler(ThinClientDataUpdateTree.class);

    private static final SimpleDateFormat DATE_FORMAT = new SimpleDateFormat(
            "yyyy-MM-dd HH:mm:ss.SSS");

    private long lastQuery = 0l;

    private long serverTimeLag = Long.MIN_VALUE;

    public static synchronized ThinClientDataUpdateTree getInstance() {
        DataUpdateTree instance = DataUpdateTree.getInstance();
        if (!(instance instanceof ThinClientDataUpdateTree)) {
            instance = new ThinClientDataUpdateTree();
            setCustomInstance(instance);
        }
        return (ThinClientDataUpdateTree) instance;
    }

    private ThinClientDataUpdateTree() {
        DATE_FORMAT.setTimeZone(TimeZone.getTimeZone("GMT"));
        lastQuery = System.currentTimeMillis();
    }

    public Collection<AlertMessage> updateAllData() {
        String time = DATE_FORMAT.format(new Date(lastQuery
                - getServerTimeOffset()));
        lastQuery = System.currentTimeMillis();
        Set<AlertMessage> messages = new HashSet<AlertMessage>();
        for (DataPair pair : getDataPairs()) {
            AbstractResourceData resourceData = pair.data.getResourceData();
            if (!(resourceData instanceof AbstractRequestableResourceData)
                    || resourceData.isFrozen())
                continue;
            Map<String, RequestConstraint> metadata = pair.metadata;
            metadata = new HashMap<String, RequestConstraint>(metadata);
            metadata.put("insertTime", new RequestConstraint(time,
                    ConstraintType.GREATER_THAN));
            LayerProperty property = new LayerProperty();
            try {
                property.setEntryQueryParameters(metadata, false);
                List<Object> records = DataCubeContainer.getData(property,
                        60000);
                if (records != null && !records.isEmpty()) {
                    for (Object record : records) {
                        if (record instanceof PluginDataObject) {
                            PluginDataObject pdo = (PluginDataObject) record;
                            AlertMessage am = new AlertMessage();
                            am.dataURI = pdo.getDataURI();
                            am.decodedAlert = RecordFactory.getInstance()
                                    .loadMapFromUri(am.dataURI);
                            messages.add(am);
                        }
                    }
                }
            } catch (VizException e) {
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                        e);
            }
        }
        getRadarUpdates(time, messages);
        getGridUpdates(time, messages);
        return messages;
    }

    /**
     * Get radar update messages. This is needed to update the
     * radar-as-gridded-data inventory.
     */
    private void getRadarUpdates(String time, Set<AlertMessage> messages) {
        Set<AlertMessage> radarMessages = new HashSet<AlertMessage>();
        Map<String, RequestConstraint> metadata = RadarAdapter.getInstance().getUpdateConstraints();
        if (metadata == null) {
            // Can happen if grid inventory has not been initialized
            return;
        }
        metadata = new HashMap<String, RequestConstraint>(metadata);
        metadata.put("insertTime", new RequestConstraint(time,
                ConstraintType.GREATER_THAN));
        LayerProperty property = new LayerProperty();
        try {
            property.setEntryQueryParameters(metadata, false);
            List<Object> records = DataCubeContainer.getData(property,
                    60000); // 60-second timeout
            if (records != null && !records.isEmpty()) {
                for (Object record : records) {
                    if (record instanceof PluginDataObject) {
                        PluginDataObject pdo = (PluginDataObject) record;
                        AlertMessage am = new AlertMessage();
                        am.dataURI = pdo.getDataURI();
                        am.decodedAlert = RecordFactory.getInstance()
                                .loadMapFromUri(am.dataURI);
                        radarMessages.add(am);
                    }
                }
            }
            messages.addAll(radarMessages);
            for (String dataURI: RadarUpdater.getInstance().convertRadarAlertsToGridDatauris(radarMessages)) {
                AlertMessage am = new AlertMessage();
                am.dataURI = dataURI;
                am.decodedAlert = RecordFactory.getInstance()
                        .loadMapFromUri(am.dataURI);
                messages.add(am);
            }
        } catch (VizException e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                    e);
        }
    }

    /** Get gridded data update messages. */
    private void getGridUpdates(String time, Set<AlertMessage> messages) {
        Map<String, RequestConstraint> newQuery = new HashMap<String, RequestConstraint>();
        DbQueryRequest dbRequest = new DbQueryRequest();
        newQuery.put("pluginName", new RequestConstraint("grid"));
        newQuery.put("insertTime", new RequestConstraint(time,
                ConstraintType.GREATER_THAN));
        dbRequest.setConstraints(newQuery);
        dbRequest.addRequestField("dataURI");
        DbQueryResponse response = null;
        try {
            response = (DbQueryResponse) ThriftClient.sendRequest(dbRequest);
            for (String dataURI: response.getFieldObjects("dataURI", String.class)) {
                AlertMessage am = new AlertMessage();
                am.dataURI = dataURI;
                am.decodedAlert = RecordFactory.getInstance()
                        .loadMapFromUri(am.dataURI);
                messages.add(am);
            }
        } catch (VizException e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                    e);
        }
    }

    /**
     * Get the estimated difference between the clock on the server and the
     * local clock. The offset returned from this method will always be slightly
     * earlier than the actual server time because of network latency. The
     * earlier time guarantees that all updates are retrieved but may result in
     * updates being retrieved twice if the data is inserted during this one
     * second window
     * 
     * @return
     */
    private long getServerTimeOffset() {
        if (serverTimeLag == Long.MIN_VALUE) {
            try {
                GetServerTimeResponse response = (GetServerTimeResponse) ThriftClient
                        .sendRequest(new GetServerTimeRequest());
                serverTimeLag = System.currentTimeMillis() - response.getTime();
            } catch (VizException e) {
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                        e);
                return 1000l;
            }
        }
        // put in a 1 second overlap in case insert time is a bit off.
        return serverTimeLag + 1000l;
    }

}
