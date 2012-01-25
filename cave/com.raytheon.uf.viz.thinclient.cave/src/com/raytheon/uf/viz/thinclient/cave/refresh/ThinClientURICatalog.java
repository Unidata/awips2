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

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.util.IPropertyChangeListener;
import org.eclipse.jface.util.PropertyChangeEvent;

import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint.ConstraintType;
import com.raytheon.uf.common.dataquery.requests.TimeQueryRequest;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.SimulatedTime;
import com.raytheon.uf.viz.core.datastructure.DataCubeContainer;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.URICatalog;
import com.raytheon.uf.viz.thinclient.Activator;
import com.raytheon.uf.viz.thinclient.preferences.ThinClientPreferenceConstants;

/**
 * 
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 15, 2011            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class ThinClientURICatalog extends URICatalog implements
        IPropertyChangeListener {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(ThinClientURICatalog.class);

    private boolean enableMenuTimes = false;

    public static synchronized ThinClientURICatalog getInstance() {
        URICatalog instance = URICatalog.getInstance();
        if (!(instance instanceof ThinClientURICatalog)) {
            instance = new ThinClientURICatalog();
            setCustomInstance(instance);
        }
        return (ThinClientURICatalog) instance;
    }

    private ThinClientURICatalog() {
        super();
        IPreferenceStore store = Activator.getDefault().getPreferenceStore();
        enableMenuTimes = !store
                .getBoolean(ThinClientPreferenceConstants.P_DISABLE_MENU_TIMES);
        store.addPropertyChangeListener(this);
    }

    @Override
    public void propertyChange(PropertyChangeEvent event) {
        if (ThinClientPreferenceConstants.P_DISABLE_MENU_TIMES.equals(event
                .getProperty())) {
            enableMenuTimes = !Boolean.valueOf(String.valueOf(event
                    .getNewValue()));
            if (enableMenuTimes) {
                scheduleRebuildSchedulerJob();
            } else {
                cancelRebuildSchedulerJob();
            }
        }
    }

    @Override
    protected void catalogAndQueryDataURIsInternal(
            Map<Map<String, RequestConstraint>, List<IURIRefreshCallback>> rcMap,
            IProgressMonitor monitor) {
        queryMenuTimes(rcMap, monitor, true);
    }

    private void queryMenuTimes(
            Map<Map<String, RequestConstraint>, List<IURIRefreshCallback>> rcMap,
            IProgressMonitor monitor, boolean insert) {
        Map<String, List<TimeQueryRequest>> requests = new HashMap<String, List<TimeQueryRequest>>();
        Map<TimeQueryRequest, List<IURIRefreshCallback>> callbacks = new HashMap<TimeQueryRequest, List<IURIRefreshCallback>>();
        for (Map.Entry<Map<String, RequestConstraint>, List<IURIRefreshCallback>> entry : rcMap
                .entrySet()) {
            Map<String, RequestConstraint> map = entry.getKey();
            RequestConstraint pluginRC = map.get("pluginName");
            if (pluginRC == null
                    || pluginRC.getConstraintType() != ConstraintType.EQUALS) {
                statusHandler
                        .error("Unable to Determine plugin type of a menu item, times will not appear.");
                continue;
            }
            String pluginName = pluginRC.getConstraintValue();
            TimeQueryRequest request = new TimeQueryRequest();
            request.setPluginName(pluginName);
            request.setMaxQuery(true);
            request.setSimDate(SimulatedTime.getSystemTime().getTime());
            request.setQueryTerms(map);
            List<TimeQueryRequest> requestList = requests.get(pluginName);
            if (requestList == null) {
                requestList = new ArrayList<TimeQueryRequest>();
                requests.put(pluginName, requestList);
            }
            requestList.add(request);
            callbacks.put(request, entry.getValue());
        }
        for (Map.Entry<String, List<TimeQueryRequest>> entry : requests
                .entrySet()) {
            List<TimeQueryRequest> requestList = entry.getValue();
            if (monitor != null && monitor.isCanceled()) {
                // If we are cancelled add our runnables back into the queue
                // instead of requesting times
                synchronized (this) {
                    for (int i = 0; i < requestList.size(); i++) {
                        Map<String, RequestConstraint> terms = requestList.get(
                                i).getQueryTerms();
                        List<IURIRefreshCallback> runnables = queuedRCMap
                                .get(terms);
                        List<IURIRefreshCallback> callbackList = callbacks
                                .get(requestList.get(i));
                        if (runnables == null) {
                            queuedRCMap.put(terms, callbackList);
                        } else {
                            runnables.addAll(callbackList);
                        }
                    }
                }
                continue;
            }
            List<List<DataTime>> times = null;
            try {
                times = DataCubeContainer.performTimeQueries(entry.getKey(),
                        requestList);
            } catch (VizException e) {
                statusHandler.error("Error getting menu green times", e);
                continue;
            }
            for (int i = 0; i < times.size(); i++) {
                DataTime time = null;
                List<DataTime> timeList = times.get(i);
                if (timeList != null && !timeList.isEmpty()) {
                    time = timeList.get(0);
                }
                List<IURIRefreshCallback> callBackList = callbacks
                        .get(requestList.get(i));
                for (IURIRefreshCallback callBack : callBackList) {
                    callBack.updateTime(time);
                }
                if (insert) {
                    for (Map<String, RequestConstraint> updateMap : DataCubeContainer
                            .getBaseUpdateConstraints(requestList.get(i)
                                    .getQueryTerms())) {
                        insertCriteria(updateMap, callBackList, false);
                    }
                }
            }
        }
        rebuildTree();
    }

    @Override
    protected void scheduleRebuildSchedulerJob() {
        if (enableMenuTimes) {
            super.scheduleRebuildSchedulerJob();
        }
    }

    public void requeryAllMenuTimes() {
        if (enableMenuTimes) {
            Map<Map<String, RequestConstraint>, List<IURIRefreshCallback>> map = new HashMap<Map<String, RequestConstraint>, List<IURIRefreshCallback>>();
            for (DataPair pair : getDataPairs()) {
                map.put(pair.metadata, pair.data);
            }
            queryMenuTimes(map, null, false);
        }
    }

}
