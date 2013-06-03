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

import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

import com.raytheon.uf.common.util.SpringFiles;
import com.raytheon.uf.edex.datadelivery.bandwidth.dao.BandwidthSubscription;
import com.raytheon.uf.edex.datadelivery.bandwidth.dao.IBandwidthDao;
import com.raytheon.uf.edex.datadelivery.bandwidth.dao.SubscriptionDaoFixture;
import com.raytheon.uf.edex.datadelivery.bandwidth.dao.SubscriptionRetrieval;
import com.raytheon.uf.edex.datadelivery.bandwidth.dao.SubscriptionRetrievalFixture;

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
@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(locations = { SpringFiles.UNIT_TEST_DB_BEANS_XML,
        SpringFiles.BANDWIDTH_DATADELIVERY_DAOS_XML,
        SpringFiles.RETRIEVAL_DATADELIVERY_DAOS_XML,
        "sessionManagedServiceTest.xml" })
public class SessionManagedServiceTest {

    private final BandwidthSubscription subscription = SubscriptionDaoFixture.INSTANCE
            .get();

    private final SubscriptionRetrieval subscriptionRetrieval = SubscriptionRetrievalFixture.INSTANCE
            .get();
    {
        subscriptionRetrieval.setBandwidthSubscription(subscription);
    }

    @Autowired
    private IBandwidthDao bandwidthService;

    @Autowired
    private IMockService service;

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
