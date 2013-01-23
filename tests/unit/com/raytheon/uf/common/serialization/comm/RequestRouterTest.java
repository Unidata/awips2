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
package com.raytheon.uf.common.serialization.comm;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;

import org.junit.Before;
import org.junit.Test;

import com.raytheon.uf.common.util.registry.RegistryException;

/**
 * Test {@link RequestRouter}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 14, 2012 1286       djohnson     Initial creation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public class RequestRouterTest {

    private static final String SERVICE1_KEY = "server1Key";

    private static final String SERVICE2_KEY = "server2Key";

    private final IRequestRouter requestServerRouter = mock(IRequestRouter.class);

    private final IRequestRouter server1Router = mock(IRequestRouter.class);

    private final IRequestRouter server2Router = mock(IRequestRouter.class);

    private final IServerRequest serverRequest = mock(IServerRequest.class);

    /**
     * Registers a router for the specified server key.
     * 
     * @param key
     *            the key
     * @param router
     *            the router
     * @throws RegistryException
     *             on error
     */
    public static void register(String key, IRequestRouter router)
            throws RegistryException {
        RequestRouter.getRouterRegistry().register(key, router);
    }

    /**
     * Clears the router registry.
     */
    public static void clearRegistry() {
        RequestRouter.getRouterRegistry().clear();
    }

    @Before
    public void setUp() throws RegistryException {
        clearRegistry();
        register(RequestRouter.REQUEST_SERVICE, requestServerRouter);
        register(SERVICE1_KEY, server1Router);
        register(SERVICE2_KEY, server2Router);
    }

    @Test
    public void testWithoutServiceSendsToDefaultRouter() throws Exception {
        RequestRouter.route(serverRequest);

        verify(requestServerRouter).route(serverRequest);
    }
    
    @Test
    public void testWithServiceSendsToRegisteredRouter() throws Exception {
        
        RequestRouter.route(serverRequest, SERVICE1_KEY);

        verify(server1Router).route(serverRequest);
        verify(server2Router, never()).route(serverRequest);
        verify(requestServerRouter, never()).route(serverRequest);
    }

    @Test
    public void testWithService2SendsToRegisteredRouter() throws Exception {

        RequestRouter.route(serverRequest, SERVICE2_KEY);

        verify(server2Router).route(serverRequest);
        verify(server1Router, never()).route(serverRequest);
        verify(requestServerRouter, never()).route(serverRequest);
    }

    @Test
    public void testUnregisteredRouterWillRouteToRequestRouter()
            throws Exception {
        RequestRouter.route(serverRequest, "noRegisteredServer");

        verify(requestServerRouter).route(serverRequest);
        verify(server2Router, never()).route(serverRequest);
        verify(server1Router, never()).route(serverRequest);
    }

    @Test(expected = RegistryException.class)
    public void testRegisteringTwoRequestRoutersWithSameKeyThrowsException()
            throws Exception {
        register(SERVICE1_KEY, server2Router);
    }
}
