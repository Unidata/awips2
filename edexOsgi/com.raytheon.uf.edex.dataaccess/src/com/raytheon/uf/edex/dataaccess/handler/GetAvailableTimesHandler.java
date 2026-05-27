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
package com.raytheon.uf.edex.dataaccess.handler;

import com.raytheon.uf.common.dataaccess.DataAccessLayer;
import com.raytheon.uf.common.dataaccess.request.GetAvailableTimesRequest;
import com.raytheon.uf.common.serialization.comm.IRequestHandler;
import com.raytheon.uf.common.time.DataTime;

/**
 * Request handler for <code>GetAvailableTimesRequest</code>. Returns answer as
 * array of <code>DataTime</code> objects.
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

public final class GetAvailableTimesHandler implements
        IRequestHandler<GetAvailableTimesRequest> {

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.serialization.comm.IRequestHandler#handleRequest
     * (com.raytheon.uf.common.serialization.comm.IServerRequest)
     */
    @Override
    public DataTime[] handleRequest(final GetAvailableTimesRequest request)
            throws Exception {
        return DataAccessLayer
.getAvailableTimes(
                request.getRequestParameters(), request.isRefTimeOnly());
    }

}
