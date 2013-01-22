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
package com.raytheon.uf.common.datadelivery.registry.handlers;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.Matchers.emptyCollectionOf;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertThat;
import static org.mockito.Matchers.any;
import static org.mockito.Matchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import org.junit.Before;
import org.junit.Test;

import com.raytheon.uf.common.datadelivery.registry.GroupDefinition;
import com.raytheon.uf.common.datadelivery.registry.GroupDefinitionServiceRequest;
import com.raytheon.uf.common.datadelivery.registry.Subscription;
import com.raytheon.uf.common.datadelivery.registry.SubscriptionBuilder;
import com.raytheon.uf.common.datadelivery.service.GroupDefinitionService;
import com.raytheon.uf.common.datadelivery.service.ISubscriptionNotificationService;
import com.raytheon.uf.common.registry.RegistryManagerTest;
import com.raytheon.uf.common.registry.handler.RegistryHandlerException;
import com.raytheon.uf.common.registry.handler.RegistryObjectHandlersUtil;
import com.raytheon.uf.edex.datadelivery.service.services.GroupDefinitionServiceHandler;

/**
 * Test {@link GroupDefinitionHandler}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 18, 2013 1441       djohnson     Initial creation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public class GroupDefinitionServiceTest {

    private static final String GROUP_NAME = "someGroup";

    private ISubscriptionHandler subscriptionHandler;

    private IGroupDefinitionHandler groupHandler;

    private final ISubscriptionNotificationService subscriptionNotificationService = mock(ISubscriptionNotificationService.class);

    private final GroupDefinitionService service = new GroupDefinitionService() {
        @Override
        protected Object sendRequest(GroupDefinitionServiceRequest request)
                throws RegistryHandlerException {
            try {
                return new GroupDefinitionServiceHandler(
                        subscriptionNotificationService).handleRequest(request);
            } catch (Exception e) {
                throw new RegistryHandlerException(e);
            }
        }
    };

    private final GroupDefinition group = new GroupDefinition();

    @Before
    public void setUp() throws RegistryHandlerException {
        RegistryObjectHandlersUtil.initMemory();
        RegistryManagerTest.setMockInstance();

        subscriptionHandler = DataDeliveryHandlers.getSubscriptionHandler();
        groupHandler = DataDeliveryHandlers.getGroupDefinitionHandler();

        group.setGroupName(GROUP_NAME);
        groupHandler.store(group);

        Subscription subscription = new SubscriptionBuilder().withGroupName(
                GROUP_NAME).build();
        Subscription subscription2 = new Subscription(subscription, "sub2");

        subscriptionHandler.store(subscription);
        subscriptionHandler.store(subscription2);
    }

    @Test
    public void deletingAGroupUpdatesSubscriptionsToNotHaveAGroupName()
            throws RegistryHandlerException {

        service.deleteGroupDefinition(group);

        assertThat(subscriptionHandler.getByGroupName(GROUP_NAME),
                is(emptyCollectionOf(Subscription.class)));
    }

    @Test
    public void deletingAGroupNotifiesOfSubscriptionUpdates()
            throws RegistryHandlerException {
        service.deleteGroupDefinition(group);

        verify(subscriptionNotificationService, times(2))
                .sendUpdatedSubscriptionNotification(any(Subscription.class),
                        anyString());
    }

    @Test
    public void deletingAGroupDeletesTheGroup() throws RegistryHandlerException {

        service.deleteGroupDefinition(group);

        assertNull(groupHandler.getByName(GROUP_NAME));
    }

}
