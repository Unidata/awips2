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

import java.util.List;

import com.raytheon.uf.common.dataplugin.gfe.db.objects.ParmID;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.time.TimeRange;

/**
 * Encapsulates a request for grid data
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 04/08/08     #875       bphillip    Initial Creation
 * 07/01/2014   #3149      randerso    Simplified to contain only a ParmID and 
 *                                     list of TimeRanges.
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */

@DynamicSerialize
public class GetGridRequest {

    /** The records to be saved */

    @DynamicSerializeElement
    private List<TimeRange> times;

    /** The parmID of the request */

    @DynamicSerializeElement
    private ParmID parmId;

    /**
     * Creates a new GetGridRequest
     */
    public GetGridRequest() {

    }

    /**
     * Creates a new GetGridRequest
     * 
     * @param parmId
     *            The parm ID of the requested grids
     * @param times
     *            The times of the requested grids
     */
    public GetGridRequest(ParmID parmId, List<TimeRange> times) {
        this.parmId = parmId;
        this.times = times;
    }

    public ParmID getParmId() {
        return parmId;
    }

    public void setParmId(ParmID parmId) {
        this.parmId = parmId;
    }

    public List<TimeRange> getTimes() {
        return this.times;
    }

    public void setTimes(List<TimeRange> times) {
        this.times = times;
    }

}
