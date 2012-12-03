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
package com.raytheon.uf.viz.core.requests;

import com.raytheon.uf.common.auth.req.AbstractPrivilegedRequest;
import com.raytheon.uf.common.serialization.comm.IRequestRouter;
import com.raytheon.uf.common.serialization.comm.IServerRequest;
import com.raytheon.uf.viz.core.VizServers;
import com.raytheon.uf.viz.core.auth.UserController;

/**
 * Serializes the request using thrift and sends it to the server.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 09, 2010            rjpeter     Initial creation
 * Dec 03, 2012 1377       djohnson    Use serviceKey to find destination url.
 * 
 * </pre>
 * 
 * @author rjpeter
 * @version 1.0
 */
public class ServerRequestRouter implements IRequestRouter {
    private final String serviceKey;

    private String httpAddress;

    public ServerRequestRouter(String serviceKey) {
        this.serviceKey = serviceKey;
    }

    @Override
    public Object route(IServerRequest request) throws Exception {
        if (request instanceof AbstractPrivilegedRequest) {
            ((AbstractPrivilegedRequest) request).setUser(UserController
                    .getUserObject());
        }
        // Must be lazily-created because localization is not initialized when
        // Spring creates the bean. Does not require synchronization because the
        // calculated value will always be the same
        if (httpAddress == null) {
            httpAddress = VizServers.getInstance()
                    .getServerLocation(serviceKey);
        }

        return ThriftClient.sendRequest(request, httpAddress);
    }
}
