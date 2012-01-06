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

import com.raytheon.uf.common.dataplugin.gfe.db.objects.ParmID;
import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.time.TimeRange;

/**
 * ISC Request class
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 08/19/09     1995       bphillip    Initial creation
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
@DynamicSerialize
public class SendISCRequest implements ISerializableObject {

    /** The parmID being requested */
    @DynamicSerializeElement
    private ParmID parmId;

    /** The time range requested */
    @DynamicSerializeElement
    private TimeRange timeRange;

    /**
     * Creates a new SendISCRequest
     */
    public SendISCRequest() {

    }

    /**
     * Creates a new SendISCRequest
     * 
     * @param parmID
     *            The parmID requested
     * @param timeRange
     *            The time range requested
     */
    public SendISCRequest(ParmID parmID, TimeRange timeRange) {
        this.parmId = parmID;
        this.timeRange = timeRange;
    }

    public boolean equals(Object obj) {

        if (!(obj instanceof SendISCRequest)) {
            return false;
        }

        SendISCRequest rhs = (SendISCRequest) obj;
        if (this.parmId.equals(rhs.getParmId())
                && this.timeRange.equals(rhs.getTimeRange())) {
            return true;
        }

        return false;
    }

    public String toString() {
        StringBuffer buffer = new StringBuffer();
        buffer.append("(");
        if (parmId == null) {
            buffer.append("null");
        } else {
            buffer.append(parmId.toString());
        }
        buffer.append(",");
        if (timeRange == null) {
            buffer.append("null");
        } else {
            buffer.append(timeRange.toString());
        }
        buffer.append(")");
        return buffer.toString();
    }

    /**
     * Gets the parmID
     * 
     * @return The parmID
     */
    public ParmID getParmId() {
        return parmId;
    }

    /**
     * Sets the parmID
     * 
     * @param parmId
     *            The parmID
     */
    public void setParmId(ParmID parmId) {
        this.parmId = parmId;
    }

    /**
     * Gets the time range
     * 
     * @return The time range
     */
    public TimeRange getTimeRange() {
        return timeRange;
    }

    /**
     * Sets the time range
     * 
     * @param timeRange
     *            The time range
     */
    public void setTimeRange(TimeRange timeRange) {
        this.timeRange = timeRange;
    }

}
