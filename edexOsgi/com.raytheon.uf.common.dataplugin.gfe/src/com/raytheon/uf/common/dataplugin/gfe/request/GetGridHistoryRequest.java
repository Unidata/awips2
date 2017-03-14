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
package com.raytheon.uf.common.dataplugin.gfe.request;

import java.util.List;

import com.raytheon.uf.common.dataplugin.gfe.db.objects.ParmID;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.time.TimeRange;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 24, 2011            randerso     Initial creation
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

@DynamicSerialize
public class GetGridHistoryRequest extends AbstractGfeRequest {
    @DynamicSerializeElement
    private ParmID parmID;

    @DynamicSerializeElement
    private List<TimeRange> timeRanges;

    /**
     * @return the parmID
     */
    public ParmID getParmID() {
        return parmID;
    }

    /**
     * @param parmID
     *            the parmID to set
     */
    public void setParmID(ParmID parmID) {
        this.parmID = parmID;
    }

    /**
     * @return the timeRanges
     */
    public List<TimeRange> getTimeRanges() {
        return timeRanges;
    }

    /**
     * @param timeRanges
     *            the timeRanges to set
     */
    public void setTimeRanges(List<TimeRange> timeRanges) {
        this.timeRanges = timeRanges;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#toString()
     */
    @Override
    public String toString() {
        StringBuilder s = new StringBuilder();
        s.append(super.toString()).append(',');
        s.append(parmID);
        return s.toString();
    }

}
