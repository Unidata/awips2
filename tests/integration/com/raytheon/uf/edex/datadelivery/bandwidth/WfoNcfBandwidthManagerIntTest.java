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
package com.raytheon.uf.edex.datadelivery.bandwidth;

import static org.hamcrest.Matchers.emptyCollectionOf;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.not;
import static org.junit.Assert.assertThat;

import java.util.Arrays;
import java.util.List;

import org.junit.Test;
import org.springframework.context.ApplicationContext;
import org.springframework.context.support.ClassPathXmlApplicationContext;

import com.raytheon.uf.common.datadelivery.bandwidth.BandwidthRequest;
import com.raytheon.uf.common.datadelivery.bandwidth.BandwidthRequest.RequestType;
import com.raytheon.uf.common.datadelivery.registry.Network;
import com.raytheon.uf.common.datadelivery.registry.Subscription;
import com.raytheon.uf.common.datadelivery.request.DataDeliveryConstants;
import com.raytheon.uf.common.serialization.comm.IRequestRouter;
import com.raytheon.uf.common.serialization.comm.IServerRequest;
import com.raytheon.uf.common.serialization.comm.RequestRouterTest;
import com.raytheon.uf.common.util.SpringFiles;
import com.raytheon.uf.common.util.registry.RegistryException;
import com.raytheon.uf.edex.datadelivery.bandwidth.WfoBandwidthManagerCreator.WfoBandwidthManager;
import com.raytheon.uf.edex.datadelivery.bandwidth.dao.BandwidthAllocation;
import com.raytheon.uf.edex.datadelivery.bandwidth.dao.BandwidthSubscription;
import com.raytheon.uf.edex.datadelivery.bandwidth.dao.IBandwidthDao;

/**
 * Test interactions between a {@link WfoBandwidthManager} and a
 * {@link NcfBandwidthManager}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 27, 2013 1644       djohnson     Initial creation
 * Jul 10, 2013 2106       djohnson     Dependency inject registry handlers.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
public class WfoNcfBandwidthManagerIntTest extends
        AbstractWfoBandwidthManagerIntTest {

    private BandwidthManager ncfBandwidthManager;

    private IBandwidthDao ncfBandwidthDao;

    @Override
    public void setUp() {
        super.setUp();

        // Set up the NCF bandwidth manager
        ApplicationContext ncfBandwidthManagerCtx = new ClassPathXmlApplicationContext(
                new String[] {
                        SpringFiles.UNIT_TEST_DB_BEANS_XML,
                        SpringFiles.MEMORY_DATADELIVERY_HANDLERS_XML,
                        SpringFiles.RETRIEVAL_DATADELIVERY_DAOS_XML,
                        SpringFiles.BANDWIDTH_DATADELIVERY_DAOS_XML,
                        SpringFiles.BANDWIDTH_DATADELIVERY_XML,
                        SpringFiles.BANDWIDTH_DATADELIVERY_NCF_XML,
                        SpringFiles.BANDWIDTH_DATADELIVERY_INTEGRATION_TEST_XML,
                        SpringFiles.BANDWIDTH_DATADELIVERY_INTEGRATION_TEST_NCF_XML });
        ncfBandwidthManager = (BandwidthManager) ncfBandwidthManagerCtx
                .getBean("bandwidthManager");
        ncfBandwidthDao = (IBandwidthDao) ncfBandwidthManagerCtx
                .getBean("bandwidthDao");

        // Add the router to the NCF bandwidth manager
        RequestRouterTest.clearRegistry();
        try {
            RequestRouterTest.register(
                    DataDeliveryConstants.NCF_BANDWIDTH_MANAGER_SERVICE,
                    new IRequestRouter() {
                        @Override
                        public Object route(IServerRequest request)
                                throws Exception {
                            return ncfBandwidthManager
                                    .handleRequest((BandwidthRequest) request);
                        }
                    });
        } catch (RegistryException e) {
            throw new RuntimeException(e);
        }
    }

    @Test
    public void proposeScheduleSbnSubscriptionIsScheduledForNcf()
            throws Exception {
        proposeScheduleSbnSubscriptionThatFits();

        final List<BandwidthAllocation> ncfSbnBandwidthAllocations = ncfBandwidthDao
                .getBandwidthAllocations(Network.SBN);
        assertThat(ncfSbnBandwidthAllocations,
                is(not(emptyCollectionOf(BandwidthAllocation.class))));
    }

    @Test
    public void proposeScheduleSbnSubscriptionIsScheduledForWfo()
            throws Exception {
        proposeScheduleSbnSubscriptionThatFits();

        final List<BandwidthAllocation> wfoSbnBandwidthAllocations = bandwidthDao
                .getBandwidthAllocations(Network.SBN);
        assertThat(wfoSbnBandwidthAllocations,
                is(not(emptyCollectionOf(BandwidthAllocation.class))));
    }

    @Test
    public void tooLargeSbnSubscriptionIsNotScheduledForNcf() throws Exception {
        proposeScheduleSbnSubscriptionThatDoesNotFit();

        final List<BandwidthAllocation> ncfSbnBandwidthAllocations = ncfBandwidthDao
                .getBandwidthAllocations(Network.SBN);
        assertThat(ncfSbnBandwidthAllocations,
                is(emptyCollectionOf(BandwidthAllocation.class)));
    }

    @Test
    public void tooLargeSbnSubscriptionIsNotScheduledForWfo() throws Exception {
        proposeScheduleSbnSubscriptionThatDoesNotFit();

        final List<BandwidthAllocation> wfoSbnBandwidthAllocations = bandwidthDao
                .getBandwidthAllocations(Network.SBN);
        assertThat(wfoSbnBandwidthAllocations,
                is(emptyCollectionOf(BandwidthAllocation.class)));
    }

    @Test
    public void smallEnoughSbnSubscriptionCanBeScheduledForNcf()
            throws Exception {
        scheduleSbnSubscriptionThatFits();

        final List<BandwidthSubscription> subscriptions = ncfBandwidthDao
                .getBandwidthSubscriptions();
        assertThat(subscriptions,
                is(not(emptyCollectionOf(BandwidthSubscription.class))));
    }

    @Test
    public void smallEnoughSbnSubscriptionCanBeScheduledForWfo()
            throws Exception {
        scheduleSbnSubscriptionThatFits();

        final List<BandwidthSubscription> subscriptions = bandwidthDao
                .getBandwidthSubscriptions();
        assertThat(subscriptions,
                is(not(emptyCollectionOf(BandwidthSubscription.class))));
    }

    /**
     * Propose schedules an SBN routed subscription that will fit within the
     * bandwidth requirements.
     * 
     * @throws Exception
     */
    private void proposeScheduleSbnSubscriptionThatFits() throws Exception {
        scheduleSbnSubscription(createSubscriptionThatFillsAThirdOfABucket(),
                RequestType.PROPOSE_SCHEDULE_SUBSCRIPTION);
    }

    /**
     * Propose schedules an SBN routed subscription that will NOT fit within the
     * bandwidth requirements.
     * 
     * @throws Exception
     */
    private void proposeScheduleSbnSubscriptionThatDoesNotFit()
            throws Exception {
        scheduleSbnSubscription(createSubscriptionThatFillsUpTwoBuckets(),
                RequestType.PROPOSE_SCHEDULE_SUBSCRIPTION);
    }

    /**
     * Schedules an SBN routed subscription that will fit within the bandwidth
     * requirements.
     * 
     * @throws Exception
     */
    private void scheduleSbnSubscriptionThatFits() throws Exception {
        scheduleSbnSubscription(createSubscriptionThatFillsAThirdOfABucket(),
                RequestType.SCHEDULE_SUBSCRIPTION);
    }

    /**
     * Schedules an SBN routed subscription that will NOT fit within the
     * bandwidth requirements.
     * 
     * @param requestType
     * 
     * @throws Exception
     */
    private void scheduleSbnSubscription(Subscription subscription,
            RequestType requestType) throws Exception {
        BandwidthRequest request = new BandwidthRequest();
        request.setRequestType(requestType);
        request.setNetwork(subscription.getRoute());
        request.setSubscriptions(Arrays.asList(subscription));

        bandwidthManager.handleRequest(request);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected Network getRouteToUseForSubscription() {
        return Network.SBN;
    }

}
