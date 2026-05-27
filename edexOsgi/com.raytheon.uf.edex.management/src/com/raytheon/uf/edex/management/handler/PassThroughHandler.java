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
package com.raytheon.uf.edex.management.handler;

import com.raytheon.uf.common.management.request.PassThroughRequest;
import com.raytheon.uf.common.serialization.comm.IRequestHandler;
import com.raytheon.uf.edex.management.InterEdexCommunicator;
import com.raytheon.uf.edex.management.MgmtUtil;

/**
 * Passes a request from this JVM to another
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 9, 2010            njensen     Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class PassThroughHandler implements IRequestHandler<PassThroughRequest> {

    @Override
    public Object handleRequest(PassThroughRequest request) throws Exception {
        String port = MgmtUtil.getPortNumber(request.getHostname(),
                request.getJvmName());
        String address = MgmtUtil.buildAddress(request.getHostname(), port);
        return InterEdexCommunicator.sendRequest(request.getRequest(), address);
    }

}
