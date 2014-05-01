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
import com.raytheon.uf.common.serialization.ISerializableObject;
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
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */

@DynamicSerialize
public class GetGridRequest implements ISerializableObject {

    /** The records to be saved */

    @DynamicSerializeElement
    private List<GFERecord> records;

    /** The parmID of the request */

    @DynamicSerializeElement
    private ParmID parmId;

    /**
     * Denotes whether the data retrieved from this request will be converted to
     * match the corresponding grid parm info
     */

    @DynamicSerializeElement
    private boolean convertUnit = false;

    /**
     * @return the convertUnit
     */
    public boolean isConvertUnit() {
        return convertUnit;
    }

    /**
     * @param convertUnit
     *            the convertUnit to set
     */
    public void setConvertUnit(boolean convertUnit) {
        this.convertUnit = convertUnit;
    }

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
    public GetGridRequest(List<GFERecord> records) {
        if (records.size() > 0) {
            parmId = records.get(0).getParmId();
        }
        this.records = records;
    }

    public GetGridRequest(ParmID parmId, List<TimeRange> trs) {
        this.parmId = parmId;
        records = new ArrayList<GFERecord>();
        for (TimeRange tr : trs) {
            records.add(new GFERecord(parmId, tr));
        }
    }

    public ParmID getParmId() {
        return parmId;
    }

    public void setParmId(ParmID parmId) {
        this.parmId = parmId;
    }

    public List<TimeRange> getTimes() {
        List<TimeRange> times = new ArrayList<TimeRange>();
        for (GFERecord rec : records) {
            times.add(rec.getTimeRange());
        }
        return times;
    }

    public List<GFERecord> getRecords() {
        return records;
    }

    public void setRecords(List<GFERecord> records) {
        this.records = records;
    }

}
