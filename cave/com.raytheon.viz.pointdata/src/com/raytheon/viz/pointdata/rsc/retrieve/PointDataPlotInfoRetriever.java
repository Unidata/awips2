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

import java.sql.Timestamp;
import java.util.HashMap;
import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;

import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.DataTime.FLAG;
import com.raytheon.uf.viz.core.catalog.DbQuery;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.IResourceDataChanged;
import com.raytheon.uf.viz.core.rsc.IResourceDataChanged.ChangeType;
import com.raytheon.viz.pointdata.PlotInfo;

/**
 * 
 * A plotInfoRetriever for all pointData types
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
@XmlAccessorType(XmlAccessType.NONE)
public class PointDataPlotInfoRetriever extends AbstractDbPlotInfoRetriever {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(PointDataPlotInfoRetriever.class);

    protected Object onlyRefTimeFlagLock = new Object();

    protected boolean onlyRefTime = false;

    @Override
    protected void addColumns(DbQuery dq) {
        dq.addColumn("dataURI");
        dq.addColumn("location.latitude");
        dq.addColumn("location.longitude");
        dq.addColumn("location.stationId");
        if (onlyRefTime) {
            // refTime retrieval is much faster
            dq.addColumn("dataTime.refTime");
        } else {
            dq.addColumn("dataTime");
        }
    }

    @Override
    protected PlotInfo getPlotInfo(Object[] data) {
        PlotInfo stationInfo = new PlotInfo();
        stationInfo.dataURI = (String) data[0];
        stationInfo.latitude = (Double) data[1];
        stationInfo.longitude = (Double) data[2];
        stationInfo.stationId = (String) data[3];
        if (stationInfo.stationId == null) {
            stationInfo.stationId = "" + data[1] + "#" + data[2];
        }

        if (data[4] instanceof DataTime) {
            stationInfo.dataTime = (DataTime) data[4];
        } else if (data[4] instanceof Timestamp) {
            stationInfo.dataTime = new DataTime((Timestamp) data[4]);
        } else {
            String message = "Incorrect dataTime class type from database, expected "
                    + DataTime.class.getName()
                    + " or "
                    + Timestamp.class.getName()
                    + " but recieved a "
                    + data[4].getClass().getName();
            statusHandler.handle(Priority.CRITICAL, message, new Exception(
                    message));
        }

        return stationInfo;
    }

    @Override
    public void getStations(IResourceDataChanged listener, DataTime time,
            HashMap<String, RequestConstraint> metadataMap) throws VizException {
        DbQuery dq = null;
        synchronized (onlyRefTimeFlagLock) {
            onlyRefTime = !time.getUtilityFlags().contains(FLAG.FCST_USED);
            dq = getQueryObject(metadataMap);
        }
        List<PlotInfo> info = runStationQuery(dq);
        listener.resourceChanged(ChangeType.DATA_UPDATE,
                info.toArray(new PlotInfo[0]));
    }
}
