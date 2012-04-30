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
package com.raytheon.viz.pointdata.util;

import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.derivparam.tree.DataTree;
import com.raytheon.uf.common.pointdata.GetPointDataTreeRequest;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.SimulatedTime;
import com.raytheon.uf.viz.core.alerts.AlertMessage;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.requests.ThriftClient;
import com.raytheon.viz.alerts.IAlertObserver;
import com.raytheon.viz.alerts.observers.ProductAlertObserver;
import com.raytheon.viz.pointdata.PointDataRequest;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 13, 2010            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

public class PointDataInventory extends AbstractPointDataInventory implements
        IAlertObserver {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(PointDataInventory.class);

    public PointDataInventory(List<String> plugins) {
        super(plugins);
        for (String plugin : plugins) {
            ProductAlertObserver.addObserver(plugin, this);
        }
    }

    protected List<String> getBaseParams(String pluginName, String type)
            throws VizException {
        String typeKey = getTypeKey(pluginName);
        Map<String, RequestConstraint> constraints = new HashMap<String, RequestConstraint>();
        constraints.put(typeKey, new RequestConstraint(type));
        String[] params = PointDataRequest.getParameterNames(pluginName,
                constraints);
        return Arrays.asList(params);
    }

    protected DataTree getInitialTree() {
        Map<String, String> requestMap = new HashMap<String, String>();
        for (String plugin : plugins) {
            requestMap.put(plugin, getTypeKey(plugin));
        }
        DataTree tree = null;
        try {
            tree = (DataTree) ThriftClient
                    .sendRequest(new GetPointDataTreeRequest(requestMap));
        } catch (VizException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error creating point data inventory", e);
            return null;
        }
        return tree;
    }

    public String getTypeKey(String pluginName) {
        if (pluginName.equals("qc")) {
            return "qcType";
        }
        return super.getTypeKey(pluginName);
    }

    @Override
	public void alertArrived(Collection<AlertMessage> alertMessages) {
		for (AlertMessage message : alertMessages) {
			DataTime dataTime = (DataTime) message.decodedAlert.get("dataTime");
			if (dataTime.getRefTime().before(
					SimulatedTime.getSystemTime().getTime())) {
				String pluginName = message.decodedAlert.get(PLUGIN_NAME)
						.toString();
				String source = pluginName;
				String typeKey = getTypeKey(pluginName);
				if (!PLUGIN_NAME.equals(typeKey)) {
					source += message.decodedAlert.get(typeKey).toString();
				}

				if (getAllSources() != null
						&& !getAllSources().contains(source)) {
					try {
						initTree(derParLibrary);
					} catch (VizException e) {
						statusHandler.handle(Priority.PROBLEM,
								e.getLocalizedMessage(), e);
					}
				}
			} 
		}
	}

}
