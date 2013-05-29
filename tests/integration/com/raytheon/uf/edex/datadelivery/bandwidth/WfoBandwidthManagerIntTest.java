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

import static org.hamcrest.Matchers.empty;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.not;
import static org.junit.Assert.assertThat;

import java.util.List;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.test.annotation.DirtiesContext;
import org.springframework.test.annotation.DirtiesContext.ClassMode;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

import com.raytheon.uf.common.datadelivery.bandwidth.IBandwidthRequest;
import com.raytheon.uf.common.datadelivery.bandwidth.IBandwidthRequest.RequestType;
import com.raytheon.uf.common.datadelivery.registry.Network;
import com.raytheon.uf.common.datadelivery.registry.Subscription;
import com.raytheon.uf.common.util.SpringFiles;
import com.raytheon.uf.edex.database.dao.DatabaseUtil;
import com.raytheon.uf.edex.datadelivery.bandwidth.dao.SubscriptionRetrieval;
import com.raytheon.uf.edex.datadelivery.bandwidth.retrieval.RetrievalStatus;

/**
 * Test a WFO {@link BandwidthManager}.
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
@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(locations = { DatabaseUtil.UNIT_TEST_DB_BEANS_XML,
        SpringFiles.EVENTBUS_COMMON_XML,
        SpringFiles.RETRIEVAL_DATADELIVERY_DAOS_XML,
        SpringFiles.BANDWIDTH_DATADELIVERY_DAOS_XML,
        SpringFiles.BANDWIDTH_DATADELIVERY_XML,
        SpringFiles.BANDWIDTH_DATADELIVERY_WFO_XML,
        SpringFiles.BANDWIDTH_DATADELIVERY_INTEGRATION_TEST_XML,
        SpringFiles.BANDWIDTH_DATADELIVERY_INTEGRATION_TEST_WFO_XML })
@DirtiesContext(classMode = ClassMode.AFTER_EACH_TEST_METHOD)
public class WfoBandwidthManagerIntTest extends AbstractBandwidthManagerIntTest {

    @Test
    public void testSchedulesSbnSubscriptionForRetrieval() {
        Subscription subscription = createSubscriptionThatFillsAThirdOfABucket();
        subscription.setRoute(Network.SBN);

        bandwidthManager.schedule(subscription);

        final List<SubscriptionRetrieval> subRetrievals = bandwidthDao
                .getSubscriptionRetrievals(subscription.getProvider(),
                        subscription.getDataSetName());
        assertThat(subRetrievals, is(not(empty())));

        for (SubscriptionRetrieval subRetrieval : subRetrievals) {
            assertThat(subRetrieval.getStatus(), is(RetrievalStatus.SCHEDULED));
        }
    }

    @Test
    public void testSchedulesOpsnetSubscriptionForRetrieval() {
        Subscription subscription = createSubscriptionThatFillsAThirdOfABucket();

        bandwidthManager.schedule(subscription);

        final List<SubscriptionRetrieval> subRetrievals = bandwidthDao
                .getSubscriptionRetrievals(subscription.getProvider(),
                        subscription.getDataSetName());
        assertThat(subRetrievals, is(not(empty())));

        for (SubscriptionRetrieval subRetrieval : subRetrievals) {
            assertThat(subRetrieval.getStatus(), is(RetrievalStatus.SCHEDULED));
        }
    }

    @Test
    public void testReinitializeUsesCorrectSpringFiles() throws Exception {
        final IBandwidthRequest request = new IBandwidthRequest();
        request.setRequestType(RequestType.REINITIALIZE);
        bandwidthManager.handleRequest(request);

        verifyRetrievalPlanExistsForRoute(Network.OPSNET);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected Network getRouteToUseForSubscription() {
        return Network.OPSNET;
    }

}
