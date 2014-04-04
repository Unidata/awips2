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

package com.raytheon.uf.common.dataplugin.gfe.server.request;

import java.util.ArrayList;
import java.util.List;

import com.raytheon.uf.common.dataplugin.gfe.db.objects.GFERecord;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.ParmID;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.time.TimeRange;

/**
 * Encapsulates a request to verify if a grid is able to be saved
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 04/08/08     #875       bphillip    Initial Creation
 * 06/17/08     #940       bphillip    Implemented GFE Locking
 * 04/03/2014  #2737       randerso    Moved clientSendStatus from SaveGridRequest
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */

@DynamicSerialize
public class SaveGridRequest {

    /** The grid slices */

    @DynamicSerializeElement
    private List<GFERecord> gridSlices;

    /** The parmID of the request */

    @DynamicSerializeElement
    private ParmID parmId;

    /** The time range to replace */

    @DynamicSerializeElement
    private TimeRange replacementTimeRange;

    /**
     * Creates a new VerifyGridRequest
     */
    public SaveGridRequest() {
        gridSlices = new ArrayList<GFERecord>();
    }

    public SaveGridRequest(ParmID parmId, TimeRange replacementTimeRange,
            List<GFERecord> gridSlices) {
        this.parmId = parmId;
        this.replacementTimeRange = replacementTimeRange;
        this.gridSlices = gridSlices;
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

    public List<GFERecord> getGridSlices() {
        return gridSlices;
    }

    public void setGridSlices(List<GFERecord> gridSlices) {
        this.gridSlices = gridSlices;
    }

    public List<TimeRange> getGridSliceTimes() {
        List<TimeRange> times = new ArrayList<TimeRange>();
        for (GFERecord rec : gridSlices) {
            times.add(rec.getTimeRange());
        }
        return times;
    }

    public void addRecord(GFERecord rec) {
        this.gridSlices.add(rec);
    }
}
