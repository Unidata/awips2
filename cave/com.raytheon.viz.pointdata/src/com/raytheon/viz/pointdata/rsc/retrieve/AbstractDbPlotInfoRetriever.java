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
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

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
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Oct 09, 2009           bsteffen  Initial creation
 * Dec 07, 2021  8341     randerso  Move plot performance logging into perf log.
 *                                  Add additional info to performance logging.
 *
 * </pre>
 *
 * @author bsteffen
 */
public abstract class AbstractDbPlotInfoRetriever
        extends AbstractPlotInfoRetriever {
    protected abstract void addColumns(DbQuery dq);

    protected abstract PlotInfo getPlotInfo(Object[] data);

    public List<PlotInfo> getStations(
            Map<String, RequestConstraint> metadataMap) throws VizException {
        DbQuery dq = getQueryObject(metadataMap);
        return runStationQuery(dq);
    }

    protected DbQuery getQueryObject(
            Map<String, RequestConstraint> metadataMap) {
        RequestConstraint rc = metadataMap.get("pluginName");
        DbQuery dq = new DbQuery(rc.getConstraintValue());
        addColumns(dq);

        for (Entry<String, RequestConstraint> entry : metadataMap.entrySet()) {
            if (!"pluginName".equals(entry.getKey())) {
                RequestConstraint rc1 = entry.getValue();
                dq.addConstraint(entry.getKey(), rc1.getConstraintType(),
                        rc1.getConstraintValue());

            }
        }

        return dq;
    }

    @Override
    public void getStations(IResourceDataChanged listener, DataTime time,
            Map<String, RequestConstraint> metadataMap) throws VizException {
        DbQuery dq = getQueryObject(metadataMap);
        List<PlotInfo> info = runStationQuery(dq);
        listener.resourceChanged(ChangeType.DATA_UPDATE,
                info.toArray(new PlotInfo[0]));
    }

    protected List<PlotInfo> runStationQuery(DbQuery dq) throws VizException {
        long t0 = System.currentTimeMillis();
        List<Object[]> availableStations = dq.performQuery();

        perfLog.logDuration(
                String.format("DB query constraints: [%s]",
                        dq.getConstraints()),
                (System.currentTimeMillis() - t0));

        List<PlotInfo> info = new ArrayList<>();
        for (int i = 0; i < availableStations.size(); i++) {
            Object[] data = availableStations.get(i);
            PlotInfo stationInfo = getPlotInfo(data);
            info.add(stationInfo);
        }
        return info;
    }

    @Override
    public int hashCode() {
        return getClass().hashCode();
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (obj == null) {
            return false;
        }
        if (getClass() != obj.getClass()) {
            return false;
        }
        return true;
    }
}
