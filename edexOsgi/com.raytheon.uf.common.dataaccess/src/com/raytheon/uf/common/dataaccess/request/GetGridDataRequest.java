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
package com.raytheon.uf.common.dataaccess.request;

import java.util.Collections;
import java.util.List;

import com.raytheon.uf.common.dataaccess.IDataRequest;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.TimeRange;

/**
 * Thrift request to retrieve gridded data from the Data Access Framework.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 24, 2013            dgilling     Initial creation
 * 
 * </pre>
 * 
 * @author dgilling
 * @version 1.0
 */

@DynamicSerialize
public final class GetGridDataRequest extends AbstractDataAccessRequest {

    private List<DataTime> requestedTimes;

    private TimeRange requestedPeriod;

    public GetGridDataRequest() {
        // no-op, for serialization
    }

    public GetGridDataRequest(final IDataRequest request,
            final List<DataTime> requestedTimes) {
        super(request);
        this.requestedTimes = requestedTimes;
        this.requestedPeriod = null;
    }

    public GetGridDataRequest(final IDataRequest request,
            final TimeRange requestedPeriod) {
        super(request);
        this.requestedPeriod = requestedPeriod;
        this.requestedTimes = Collections.emptyList();
    }

    public List<DataTime> getRequestedTimes() {
        return requestedTimes;
    }

    public void setRequestedTimes(List<DataTime> requestedTimes) {
        this.requestedTimes = requestedTimes;
    }

    public TimeRange getRequestedPeriod() {
        return requestedPeriod;
    }

    public void setRequestedPeriod(TimeRange requestedPeriod) {
        this.requestedPeriod = requestedPeriod;
    }
}
