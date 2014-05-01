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
package com.raytheon.uf.common.dataquery.requests;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.serialization.comm.IServerRequest;

/**
 * Send multiple {@link TimeQueryRequest}s at once. This can be more efficient
 * than sending multiple requests individually because it reduces the network
 * overhead. The response will be a List<List<DataTime>> where the list contains
 * one entry for each request, in the same order as the requests.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Jul0 1, 2011           rjpeter     Initial creation
 * Dec 18, 2013  2579     bsteffen    Class javadoc
 * 
 * </pre>
 * 
 * @author rjpeter
 * @version 1.0
 * @see TimeQueryRequest
 */
@DynamicSerialize
public class TimeQueryRequestSet implements IServerRequest {
    @DynamicSerializeElement
    private TimeQueryRequest[] requests;

    public TimeQueryRequest[] getRequests() {
        return requests;
    }

    public void setRequests(TimeQueryRequest[] requests) {
        this.requests = requests;
    }
}
