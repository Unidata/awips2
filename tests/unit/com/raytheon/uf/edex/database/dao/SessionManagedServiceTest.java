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
package com.raytheon.uf.edex.database.dao;

import static org.hamcrest.Matchers.emptyCollectionOf;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.not;
import static org.junit.Assert.assertThat;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import com.raytheon.uf.edex.core.EDEXUtil;
import com.raytheon.uf.edex.datadelivery.bandwidth.dao.BandwidthSubscription;
import com.raytheon.uf.edex.datadelivery.bandwidth.dao.SubscriptionDaoFixture;
import com.raytheon.uf.edex.datadelivery.bandwidth.dao.SubscriptionRetrieval;
import com.raytheon.uf.edex.datadelivery.bandwidth.dao.SubscriptionRetrievalFixture;
import com.raytheon.uf.edex.datadelivery.bandwidth.hibernate.HibernateBandwidthDao;

/**
 * Test {@link SessionManagedService}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 08, 2013 1543       djohnson     Initial creation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
public class SessionManagedServiceTest {

    private final BandwidthSubscription subscription = SubscriptionDaoFixture.INSTANCE
            .get();

    private final SubscriptionRetrieval subscriptionRetrieval = SubscriptionRetrievalFixture.INSTANCE
            .get();
    {
        subscriptionRetrieval.setBandwidthSubscription(subscription);
    }

    private HibernateBandwidthDao bandwidthService;

    private MockService service;

    @Before
    public void setUp() {
        DatabaseUtil.start();

        service = (MockService) EDEXUtil.getESBComponent("mockService");
        bandwidthService = (HibernateBandwidthDao) EDEXUtil
                .getESBComponent("hibernateBandwidthDao");
    }

    @After
    public void tearDown() {
        DatabaseUtil.shutdown();
    }

    @Test
    public void exceptionThrownInDaoWillRollbackTransaction() {
        try {
            service.storeStuffThenThrowException(subscription);
        } catch (RuntimeException e) {
            // Expected
        }

        assertThat(bandwidthService.getBandwidthSubscriptions(),
                is(emptyCollectionOf(BandwidthSubscription.class)));
    }

    @Test
    public void noExceptionThrownInDaoWillCommitTransaction() {
        service.storeStuffAndNotThrowException(subscription);

        assertThat(bandwidthService.getBandwidthSubscriptions(),
                is(not(emptyCollectionOf(BandwidthSubscription.class))));
    }

}
