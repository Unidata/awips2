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
package com.raytheon.uf.edex.datadelivery.bandwidth.retrieval;

import static org.hamcrest.Matchers.emptyCollectionOf;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.not;
import static org.junit.Assert.assertThat;
import static org.mockito.Matchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.util.List;
import java.util.concurrent.ConcurrentLinkedQueue;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

import com.raytheon.uf.common.datadelivery.registry.Network;
import com.raytheon.uf.common.datadelivery.registry.OpenDapGriddedDataSetMetaDataFixture;
import com.raytheon.uf.common.datadelivery.registry.ProviderFixture;
import com.raytheon.uf.common.datadelivery.registry.Subscription;
import com.raytheon.uf.common.datadelivery.registry.SubscriptionBuilder;
import com.raytheon.uf.common.datadelivery.registry.handlers.DataDeliveryHandlers;
import com.raytheon.uf.common.localization.PathManagerFactoryTest;
import com.raytheon.uf.common.registry.handler.RegistryHandlerException;
import com.raytheon.uf.common.registry.handler.RegistryObjectHandlersUtil;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.util.SpringFiles;
import com.raytheon.uf.edex.core.EdexException;
import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.datadelivery.bandwidth.dao.IBandwidthDao;
import com.raytheon.uf.edex.datadelivery.bandwidth.dao.SubscriptionRetrieval;
import com.raytheon.uf.edex.datadelivery.bandwidth.dao.SubscriptionRetrievalAttributes;
import com.raytheon.uf.edex.datadelivery.retrieval.db.IRetrievalDao;
import com.raytheon.uf.edex.datadelivery.retrieval.db.RetrievalRequestRecord;
import com.raytheon.uf.edex.datadelivery.retrieval.db.RetrievalRequestRecord.State;
import com.raytheon.uf.edex.datadelivery.retrieval.db.RetrievalRequestRecordPK;

/**
 * Test {@link SubscriptionRetrievalAgent}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 30, 2013 1543       djohnson     Initial creation
 * Jul 10, 2013 2106       djohnson     Inject providerHandler.
 * Jan 15, 2014 2678       bgonzale     Added Queue.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(locations = { SpringFiles.UNIT_TEST_DB_BEANS_XML,
        SpringFiles.BANDWIDTH_DATADELIVERY_DAOS_XML,
        SpringFiles.RETRIEVAL_DATADELIVERY_DAOS_XML })
public class SubscriptionRetrievalAgentTest {

    @Autowired
    @Qualifier(value = "retrievalDao")
    private IRetrievalDao retrievalDao;

    private final ConcurrentLinkedQueue<RetrievalRequestRecordPK> retrievalQueue = new ConcurrentLinkedQueue<RetrievalRequestRecordPK>();

    @Before
    public void setUp() throws RegistryHandlerException {
        PathManagerFactoryTest.initLocalization();
        RegistryObjectHandlersUtil.initMocks();
        when(DataDeliveryHandlers.getProviderHandler().getByName(anyString()))
                .thenReturn(ProviderFixture.INSTANCE.get());
        when(
                DataDeliveryHandlers.getDataSetMetaDataHandler().getById(
                        anyString())).thenReturn(
                OpenDapGriddedDataSetMetaDataFixture.INSTANCE.get());
    }

    @Test
    public void opsnetRoutedSubscriptionsCreateRetrievalsInPendingState()
            throws EdexException, SerializationException {
        testRetrievalIsPlacedInCorrectState(Network.OPSNET, State.PENDING);
    }

    @Test
    public void sbnRoutedSubscriptionsCreateRetrievalsInPendingState()
            throws EdexException, SerializationException {
        testRetrievalIsPlacedInCorrectState(Network.SBN, State.PENDING);
    }

    private void testRetrievalIsPlacedInCorrectState(final Network route,
            final State expectedState) throws SerializationException,
            EdexException, DataAccessLayerException {
        Subscription subscription = new SubscriptionBuilder().withRoute(route)
                .build();
        final SubscriptionRetrieval subscriptionRetrieval = new SubscriptionRetrieval();
        subscriptionRetrieval.setNetwork(subscription.getRoute());

        SubscriptionRetrievalAttributes attributes = new SubscriptionRetrievalAttributes();
        attributes.setSubscriptionRetrieval(subscriptionRetrieval);
        attributes.setSubscription(subscription);

        IBandwidthDao bandwidthDao = mock(IBandwidthDao.class);
        when(
                bandwidthDao
                        .getSubscriptionRetrievalAttributes(subscriptionRetrieval))
                .thenReturn(attributes);

        SubscriptionRetrievalAgent agent = new SubscriptionRetrievalAgent(
                route, "someUri", new Object(), 1, null, bandwidthDao,
                retrievalDao, DataDeliveryHandlers.getProviderHandler(),
                retrievalQueue) {
            @Override
            void wakeRetrievalTasks() throws EdexException {
                // Do nothing
            }
        };
        agent.processAllocation(subscriptionRetrieval);

        final List<RetrievalRequestRecord> requests = retrievalDao
                .getRequests(subscription.getName());
        assertThat(requests,
                is(not(emptyCollectionOf(RetrievalRequestRecord.class))));
        final RetrievalRequestRecord request = requests.iterator().next();

        assertThat(request.getState(), is(expectedState));
    }
}
