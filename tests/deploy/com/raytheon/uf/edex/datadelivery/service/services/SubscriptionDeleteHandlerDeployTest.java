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
package com.raytheon.uf.edex.datadelivery.service.services;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;

import java.util.Arrays;

import org.junit.BeforeClass;
import org.junit.Test;

import com.raytheon.uf.common.datadelivery.registry.PendingSubscription;
import com.raytheon.uf.common.datadelivery.registry.PendingSubscriptionFixture;
import com.raytheon.uf.common.datadelivery.registry.Subscription;
import com.raytheon.uf.common.datadelivery.registry.SubscriptionDeleteRequest;
import com.raytheon.uf.common.datadelivery.registry.SubscriptionFixture;
import com.raytheon.uf.common.datadelivery.registry.handlers.DataDeliveryHandlers;
import com.raytheon.uf.common.datadelivery.registry.handlers.IPendingSubscriptionHandler;
import com.raytheon.uf.common.datadelivery.registry.handlers.ISubscriptionHandler;
import com.raytheon.uf.common.datadelivery.request.DataDeliveryConstants;
import com.raytheon.uf.common.registry.ebxml.RegistryUtil;
import com.raytheon.uf.common.registry.handler.RegistryObjectHandlersUtil;
import com.raytheon.uf.common.serialization.comm.RequestRouter;
import com.raytheon.uf.common.util.DeployTestProperties;
import com.raytheon.uf.edex.ebxml.registry.RegistryManagerDeployTest;

/**
 * Test SubscriptionDeleteHandler.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 27, 2012 1187       djohnson     Initial creation
 * Oct 17, 2012 0726       djohnson     Use {@link RegistryManagerDeployTest#setDeployInstance()}.
 * Nov 15, 2012 1286       djohnson     Use RequestRouter.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public class SubscriptionDeleteHandlerDeployTest {

    @BeforeClass
    public static void staticSetUp() {
        RegistryObjectHandlersUtil.init();
        RegistryManagerDeployTest.setDeployInstance();
    }

    @Test
    public void testDeletingSubscriptionDeletesPendingAlso() throws Exception {

        Subscription subscription = SubscriptionFixture.INSTANCE.get();
        PendingSubscription pending = PendingSubscriptionFixture.INSTANCE.get();

        ISubscriptionHandler subHandler = DataDeliveryHandlers
                .getSubscriptionHandler();
        IPendingSubscriptionHandler pendingHandler = DataDeliveryHandlers
                .getPendingSubscriptionHandler();
        try {
            subHandler.store(subscription);
            pendingHandler.store(pending);

            assertNotNull(
                    "The pending subscription should have been retrievable!",
                    pendingHandler.getBySubscription(subscription));

            SubscriptionDeleteRequest request = new SubscriptionDeleteRequest(
                    Arrays.asList(RegistryUtil
                            .getRegistryObjectKey(subscription)),
                    ISubscriptionHandler.class, DeployTestProperties
                            .getInstance()
                            .getUserId());

            RequestRouter.route(request,
                    DataDeliveryConstants.DATA_DELIVERY_SERVER);

            assertNull(
                    "The pending subscription should have been deleted when deleting the subscription!",
                    pendingHandler.getBySubscription(subscription));
        } finally {
            try {
                subHandler.delete(subscription);
            } finally {
                pendingHandler.delete(pending);
            }
        }
    }

}
