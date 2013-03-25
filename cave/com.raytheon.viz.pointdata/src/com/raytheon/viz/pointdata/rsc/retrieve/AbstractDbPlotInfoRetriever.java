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
package com.raytheon.viz.pointdata.rsc.retrieve;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.catalog.DbQuery;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.IResourceDataChanged;
import com.raytheon.uf.viz.core.rsc.IResourceDataChanged.ChangeType;
import com.raytheon.viz.pointdata.PlotInfo;

/**
 * Abstract class for retrieving available products and returning them as
 * PlotInfo objects.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 9, 2009            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public abstract class AbstractDbPlotInfoRetriever extends
        AbstractPlotInfoRetriever {
    protected abstract void addColumns(DbQuery dq);

    protected abstract PlotInfo getPlotInfo(Object[] data);

    public List<PlotInfo> getStations(
            HashMap<String, RequestConstraint> metadataMap) throws VizException {
        DbQuery dq = getQueryObject(metadataMap);
        return runStationQuery(dq);
    }

    protected DbQuery getQueryObject(
            HashMap<String, RequestConstraint> metadataMap) {
        RequestConstraint rc = metadataMap.get("pluginName");
        DbQuery dq = new DbQuery(rc.getConstraintValue());
        addColumns(dq);

        for (String key : metadataMap.keySet()) {
            if (!key.equals("pluginName")) {
                RequestConstraint rc1 = metadataMap.get(key);
                dq.addConstraint(key, rc1.getConstraintType(),
                        rc1.getConstraintValue());

            }
        }

        return dq;
    }

    public void getStations(IResourceDataChanged listener, DataTime time,
            HashMap<String, RequestConstraint> metadataMap) throws VizException {
        DbQuery dq = getQueryObject(metadataMap);
        List<PlotInfo> info = runStationQuery(dq);
        listener.resourceChanged(ChangeType.DATA_UPDATE,
                info.toArray(new PlotInfo[0]));
    }

    protected List<PlotInfo> runStationQuery(DbQuery dq) throws VizException {
        long t0 = System.currentTimeMillis();
        List<Object[]> availableStations = dq.performQuery();
        System.out.println("Time spent on db query: "
                + (System.currentTimeMillis() - t0));

        List<PlotInfo> info = new ArrayList<PlotInfo>();
        for (int i = 0; i < availableStations.size(); i++) {
            Object[] data = availableStations.get(i);
            PlotInfo stationInfo = getPlotInfo(data);
            info.add(stationInfo);
        }
        return info;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#equals(java.lang.Object)
     */
    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        return true;
    }
}
