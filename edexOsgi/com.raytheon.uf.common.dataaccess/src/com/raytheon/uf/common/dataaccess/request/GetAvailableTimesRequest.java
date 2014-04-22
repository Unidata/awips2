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

import com.raytheon.uf.common.dataaccess.IDataRequest;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;

/**
 * Thrift request to retrieve list of available times for data from the Data
 * Access Framework.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * May 23, 2013           dgilling    Initial creation
 * Mar 03, 2014  2673     bsteffen    Add ability to query only ref times.
 * 
 * </pre>
 * 
 * @author dgilling
 * @version 1.0
 */

@DynamicSerialize
public final class GetAvailableTimesRequest extends AbstractDataAccessRequest {

    private boolean refTimeOnly = false;

    public GetAvailableTimesRequest() {
        super();
    }

    public GetAvailableTimesRequest(final IDataRequest request) {
        super(request);
    }

    public boolean isRefTimeOnly() {
        return refTimeOnly;
    }

    public void setRefTimeOnly(boolean refTimeOnly) {
        this.refTimeOnly = refTimeOnly;
    }

}
