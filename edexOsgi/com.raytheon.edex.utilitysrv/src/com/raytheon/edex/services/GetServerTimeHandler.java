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
package com.raytheon.edex.services;

import com.raytheon.uf.common.serialization.comm.IRequestHandler;
import com.raytheon.uf.common.time.msgs.GetServerTimeRequest;
import com.raytheon.uf.common.time.msgs.GetServerTimeResponse;

/**
 * Return the current time on the server, beware network latency.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 29, 2012            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class GetServerTimeHandler implements
        IRequestHandler<GetServerTimeRequest> {

    @Override
    public Object handleRequest(GetServerTimeRequest request) throws Exception {
        GetServerTimeResponse response = new GetServerTimeResponse();
        response.setTime(System.currentTimeMillis());
        return response;
    }

}
