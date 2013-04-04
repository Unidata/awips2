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
package com.raytheon.uf.edex.ebxml.registry;

import static org.junit.Assert.assertEquals;

import java.util.Arrays;

import org.junit.After;
import org.junit.BeforeClass;
import org.junit.Test;

import com.raytheon.uf.common.auth.RequestConstants;
import com.raytheon.uf.common.datadelivery.registry.Provider;
import com.raytheon.uf.common.datadelivery.registry.ProviderFixture;
import com.raytheon.uf.common.datadelivery.request.DataDeliveryConstants;
import com.raytheon.uf.common.registry.OperationStatus;
import com.raytheon.uf.common.registry.RegistryConstants;
import com.raytheon.uf.common.registry.RegistryManager;
import com.raytheon.uf.common.registry.RegistryManagerTest;
import com.raytheon.uf.common.registry.RegistryResponse;
import com.raytheon.uf.common.registry.ebxml.ThriftRegistryHandler;
import com.raytheon.uf.common.serialization.comm.RequestRouterTest;
import com.raytheon.uf.common.util.DeployTestProperties;
import com.raytheon.uf.common.util.registry.RegistryException;
import com.raytheon.uf.edex.auth.RemoteServerRequestRouter;

/**
 * Deploy test for {@link RegistryManager}. Deploy tests must execute against a
 * running EDEX instance.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 13, 2012 1169       djohnson     Initial creation
 * Oct 17, 2012 0726       djohnson     Add {@link #setDeployInstance()}.
 * Nov 15, 2012 1286       djohnson     Use RequestRouter.
 * Dec 06, 2012 1397       djohnson     Also set the request router.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public class RegistryManagerDeployTest {

    private static final Provider provider = ProviderFixture.INSTANCE.get();

    /**
     * Sets the deploy test thrift registry handler instance.
     */
    public static void setDeployInstance() {
        // this allows the configuration wiring to be checked for validity
        // first, otherwise any exceptions would be hidden in registry response
        // objects
        DeployTestProperties.getInstance();
        final ThriftRegistryHandler handler = new ThriftRegistryHandler();
        RegistryManagerTest.setInstance(handler);
        try {
            RequestRouterTest.clearRegistry();
            final RemoteServerRequestRouter requestRouter = new RemoteServerRequestRouter(
                    DeployTestProperties.getInstance().getRequestServer());
            final RemoteServerRequestRouter dataDeliveryRouter = new RemoteServerRequestRouter(DeployTestProperties
                    .getInstance().getDataDeliveryServer());
            RequestRouterTest.register(
                    DataDeliveryConstants.DATA_DELIVERY_SERVER, dataDeliveryRouter);
            RequestRouterTest.register(
                    RegistryConstants.EBXML_REGISTRY_SERVICE, dataDeliveryRouter);
            RequestRouterTest.register(RequestConstants.REQUEST_SERVER,
                    requestRouter);
        } catch (RegistryException e) {
            throw new RuntimeException(e);
        }
    }

    @BeforeClass
    public static void staticSetUp() {
        setDeployInstance();
    }

    @After
    public void cleanUp() {
        RegistryManager.removeRegistyObjects(Arrays.asList(provider));
    }

    @Test
    public void testUnableToStoreAlreadyStoredRegistryObject() {
        RegistryResponse<Provider> response = RegistryManager
                .storeRegistryObject(provider);
        assertEquals("First store should have succeeded!",
                OperationStatus.SUCCESS, response.getStatus());
        response = RegistryManager.storeRegistryObject(provider);
        assertEquals("Second store should have failed!",
                OperationStatus.FAILED, response.getStatus());
    }

    @Test
    public void testAbleToStoreAlreadyStoredRegistryObjectWithStoreOrReplace() {
        RegistryResponse<Provider> response = RegistryManager
                .storeRegistryObject(provider);
        assertEquals("First store should have succeeded!",
                OperationStatus.SUCCESS, response.getStatus());
        response = RegistryManager.storeOrReplaceRegistryObject(provider);
        assertEquals("Second store should have succeeded!",
                OperationStatus.SUCCESS, response.getStatus());
    }
}
