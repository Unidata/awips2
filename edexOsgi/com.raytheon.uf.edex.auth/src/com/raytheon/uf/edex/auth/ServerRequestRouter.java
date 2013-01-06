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
package com.raytheon.uf.edex.auth;

import com.raytheon.uf.common.auth.req.AbstractPrivilegedRequest;
import com.raytheon.uf.common.serialization.comm.IRequestRouter;
import com.raytheon.uf.common.serialization.comm.IServerRequest;
import com.raytheon.uf.edex.auth.req.ServerPrivilegedRequestHandler;

/**
 * Routes the request directly to the RemoteRequestServer.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 9, 2010             rjpeter     Initial creation
 * Nov 15, 2012 1322       djohnson    Allow servers the ability to bypass authorization.
 * 
 * </pre>
 * 
 * @author rjpeter
 * @version 1.0
 */
public class ServerRequestRouter implements IRequestRouter {
    @Override
    public Object route(IServerRequest request) throws Exception {
        // Wrap privileged requests so they are not checked for privileges
        // internally to the server
        if (request instanceof AbstractPrivilegedRequest) {
            request = new ServerPrivilegedRequestHandler.ServerPrivilegedRequest(request);
        }

        return RemoteRequestServer.getInstance().handleThriftRequest(request);
    }

}
