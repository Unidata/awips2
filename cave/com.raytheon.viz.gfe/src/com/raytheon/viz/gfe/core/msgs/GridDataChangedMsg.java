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
package com.raytheon.viz.gfe.core.msgs;

import com.raytheon.uf.common.dataplugin.gfe.db.objects.ParmID;
import com.raytheon.uf.common.time.TimeRange;

/**
 * A Message sent when the grid data or grid inventory has changed.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 31, 2010            dgilling     Initial creation
 * 
 * </pre>
 * 
 * @author dgilling
 * @version 1.0
 */

public class GridDataChangedMsg extends Message {

    private ParmID parmID;

    private TimeRange timeRange;

    /**
     * Creates a GridDataChangedMsg instance with the supplied ParmID and
     * TimeRange.
     * 
     * @param parmID
     *            The ParmID that corresponds to the changed grid data.
     * @param timeRange
     *            The time range over which the grid data applies.
     */
    public GridDataChangedMsg(ParmID parmID, TimeRange timeRange) {
        super();
        this.parmID = parmID;
        this.timeRange = timeRange;
    }

    public ParmID getParmID() {
        return parmID;
    }

    public TimeRange getTimeRange() {
        return timeRange;
    }
}
