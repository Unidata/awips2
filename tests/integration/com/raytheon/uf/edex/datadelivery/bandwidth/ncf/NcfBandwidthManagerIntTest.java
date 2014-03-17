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
package com.raytheon.uf.edex.datadelivery.bandwidth.ncf;

import org.junit.Test;
import org.springframework.test.context.ContextConfiguration;

import com.raytheon.uf.common.datadelivery.bandwidth.IBandwidthRequest;
import com.raytheon.uf.common.datadelivery.bandwidth.IBandwidthRequest.RequestType;
import com.raytheon.uf.common.datadelivery.registry.Network;
import com.raytheon.uf.common.datadelivery.registry.Subscription;
import com.raytheon.uf.common.util.SpringFiles;
import com.raytheon.uf.edex.datadelivery.bandwidth.AbstractBandwidthManagerIntTest;
import com.raytheon.uf.edex.datadelivery.bandwidth.BandwidthManager;

/**
 * Test an NCF {@link BandwidthManager}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 19, 2013 1543       djohnson     Initial creation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
@ContextConfiguration(inheritLocations = true, locations = {
        SpringFiles.BANDWIDTH_DATADELIVERY_NCF_XML,
        SpringFiles.BANDWIDTH_DATADELIVERY_INTEGRATION_TEST_NCF_XML })
public class NcfBandwidthManagerIntTest extends AbstractBandwidthManagerIntTest {

    @Test
    public void testSchedulesSbnSubscriptionForRetrieval() {
        Subscription subscription = createSubscriptionThatFillsAThirdOfABucket();

        // bandwidthManager.schedule(subscription);
        //
        // final List<SubscriptionRetrieval> subRetrievals = bandwidthDao
        // .getSubscriptionRetrievals(subscription.getProvider(),
        // subscription.getDataSetName());
        // assertThat(subRetrievals, is(not(empty())));
        //
        // for (SubscriptionRetrieval subRetrieval : subRetrievals) {
        // assertThat(subRetrieval.getStatus(), is(RetrievalStatus.SCHEDULED));
        // }
    }

    @Test
    public void testDoesNotScheduleOpsnetSubscriptionForRetrieval() {
        Subscription subscription = createSubscriptionThatFillsAThirdOfABucket();
        subscription.setRoute(Network.OPSNET);

        // bandwidthManager.schedule(subscription);
        //
        // final List<SubscriptionRetrieval> subRetrievals = bandwidthDao
        // .getSubscriptionRetrievals(subscription.getProvider(),
        // subscription.getDataSetName());
        // assertThat(subRetrievals, is(empty()));
    }

    @Test
    public void testReinitializeUsesCorrectSpringFiles() throws Exception {
        final IBandwidthRequest request = new IBandwidthRequest();
        request.setRequestType(RequestType.REINITIALIZE);
        bandwidthManager.handleRequest(request);

        verifyRetrievalPlanDoesNotExistForRoute(Network.OPSNET);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected Network getRouteToUseForSubscription() {
        return Network.SBN;
    }

}
