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

package com.raytheon.uf.common.dataplugin.gfe.server.notify;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.raytheon.uf.common.dataplugin.gfe.GridDataHistory;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.ParmID;
import com.raytheon.uf.common.message.WsId;
import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.TimeRange;

/**
 * Notification that a grid has been changed.<br>
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 04/08/08     #875       bphillip    Initial Creation
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */

@DynamicSerialize
public class GridUpdateNotification extends GfeNotification implements
        ISerializableObject {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(GridUpdateNotification.class);

    /** The parmId of the updated grid */

    @DynamicSerializeElement
    private ParmID parmId;

    /** The time range of the update */

    @DynamicSerializeElement
    private TimeRange replacementTimeRange;

    /** The grid times that have been changed */

    @DynamicSerializeElement
    private Map<TimeRange, List<GridDataHistory>> histories;

    @DynamicSerializeElement
    /* The workstation ID of who changed the grid */
    private WsId workstationID;

    /**
     * Creates a new GridUpdateNotification
     */
    public GridUpdateNotification() {

    }

    /**
     * Creates a new GridUpdateNotification
     * 
     * @param parmId
     *            The parmID of the updated grid
     * @param replacementTimeRange
     *            The time range of the update
     * @param timeRanges
     *            The grid times that have been changed
     * @param workstationID
     *            The workstation ID of who changed the grid
     */
    public GridUpdateNotification(ParmID parmId,
            TimeRange replacementTimeRange,
            Map<TimeRange, List<GridDataHistory>> histories,
            WsId workstationID, String siteID) {
        this.parmId = parmId;
        this.replacementTimeRange = replacementTimeRange;
        this.workstationID = workstationID;
        this.siteID = siteID;
        if (histories != null) {
            this.histories = new HashMap<TimeRange, List<GridDataHistory>>();
            for (TimeRange tr : histories.keySet()) {
                List<GridDataHistory> histList = histories.get(tr);
                List<GridDataHistory> newHist = new ArrayList<GridDataHistory>(
                        histList.size());
                // we are cloning history because clone CURRENTLY does not
                // clone the history.parent field, and the history.parent field
                // could
                // have the raw grid data which is unnecessary for a
                // GridUpdateNotification
                for (GridDataHistory hist : histList) {
                    try {
                        newHist.add(hist.clone());
                    } catch (CloneNotSupportedException e) {
                        statusHandler.handle(Priority.PROBLEM,
                                e.getLocalizedMessage(), e);
                        // not the end of the world to pass along the original,
                        // just
                        // it potentially has the raw data of the grid through
                        // the
                        // history.parent
                        newHist.add(hist);
                    }
                }
                this.histories.put(tr, newHist);
            }
        }
    }

    public ParmID getParmId() {
        return parmId;
    }

    public void setParmId(ParmID parmId) {
        this.parmId = parmId;
    }

    public TimeRange getReplacementTimeRange() {
        return replacementTimeRange;
    }

    public void setReplacementTimeRange(TimeRange replacementTimeRange) {
        this.replacementTimeRange = replacementTimeRange;
    }

    public Map<TimeRange, List<GridDataHistory>> getHistories() {
        return histories;
    }

    public void setHistories(Map<TimeRange, List<GridDataHistory>> histories) {
        this.histories = histories;
    }

    public WsId getWorkstationID() {
        return workstationID;
    }

    public void setWorkstationID(WsId workstationID) {
        this.workstationID = workstationID;
    }

    @Override
    public String toString() {
        StringBuilder str = new StringBuilder();
        str.append("ParmID: ").append(this.parmId.toString()).append("\n");
        str.append("Replacement TimeRange: ")
                .append(this.replacementTimeRange.toString()).append("\n");
        if ((this.histories != null) && (this.histories.keySet() != null)) {
            str.append("Replacement Times: ")
                    .append(this.histories.keySet().toString()).append("\n");
        }
        return str.toString();
    }

}
