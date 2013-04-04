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

import static org.mockito.Matchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import org.junit.Before;
import org.junit.Test;

import com.raytheon.uf.common.datadelivery.bandwidth.IBandwidthRequest;
import com.raytheon.uf.common.localization.msgs.GetServersRequest;
import com.raytheon.uf.common.serialization.comm.IRequestHandler;
import com.raytheon.uf.edex.auth.req.ServerPrivilegedRequestHandler;
import com.raytheon.uf.edex.auth.req.ServerPrivilegedRequestHandler.ServerPrivilegedRequest;

/**
 * Test {@link ServerPrivilegedRequestHandler}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 15, 2012 1286       djohnson     Initial creation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
@SuppressWarnings({ "unchecked", "rawtypes" })
public class ServerRequestRouterTest {

    private static final ServerRequestRouter ROUTER = new ServerRequestRouter();

    private static final IBandwidthRequest PRIVILEGED_REQUEST = new IBandwidthRequest();

    private static final GetServersRequest UNPRIVILEGED_REQUEST = new GetServersRequest();

    private final HandlerRegistry registry = mock(HandlerRegistry.class);

    private final IRequestHandler serverPrivilegedHandler = mock(IRequestHandler.class);

    private final IRequestHandler unprivilegedHandler = mock(IRequestHandler.class);

    @Before
    public void setUp() {
        RemoteRequestServer.getInstance().setRegistry(registry);

        when(
                registry.getRequestHandler(ServerPrivilegedRequest.class
                        .getCanonicalName())).thenReturn(
                serverPrivilegedHandler);
        when(
                registry.getRequestHandler(GetServersRequest.class
                        .getCanonicalName())).thenReturn(unprivilegedHandler);
    }

    @Test
    public void testWrapsPrivilegedRequestInServerPrivilegedRequest()
            throws Exception {
        ROUTER.route(PRIVILEGED_REQUEST);

        verify(serverPrivilegedHandler).handleRequest(
                any(ServerPrivilegedRequest.class));
    }

    @Test
    public void testDoesNotWrapUnprivilegedRequest() throws Exception {
        ROUTER.route(UNPRIVILEGED_REQUEST);

        verify(unprivilegedHandler).handleRequest(UNPRIVILEGED_REQUEST);
    }
}
