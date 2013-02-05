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

import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

import com.raytheon.uf.edex.datadelivery.bandwidth.dao.BandwidthAllocation;
import com.raytheon.uf.edex.datadelivery.bandwidth.dao.BandwidthSubscription;
import com.raytheon.uf.edex.datadelivery.bandwidth.hibernate.HibernateBandwidthDao;

/**
 * Mock service which uses another service.
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
@Transactional
@Repository
public class MockService extends SessionManagedDao {

    private HibernateBandwidthDao bandwidthService;

    /**
     * @param config
     */
    public MockService() {
    }

    /**
     * Stores a {@link BandwidthSubscription} then attempts an invalid transactable
     * operation.
     * 
     * @param subscription
     *            the subscription
     */
    public void storeStuffThenThrowException(BandwidthSubscription subscription) {
        SpringTransactionUtils
                .transactionRequired("storeStuffThenThrowException");

        storeStuff(subscription);
        // This will throw the exception
        bandwidthService.store((BandwidthAllocation) null);
    }

    /**
     * Stores a {@link BandwidthSubscription}, not throwing an exception as long as
     * the subscription object is valid.
     * 
     * @param subscription
     *            the subscription
     */
    public void storeStuffAndNotThrowException(BandwidthSubscription subscription) {
        SpringTransactionUtils
                .transactionRequired("storeStuffAndNotThrowException");

        storeStuff(subscription);
    }

    private void storeStuff(BandwidthSubscription subscription) {
        bandwidthService.store(subscription);
    }

    /**
     * @param bandwidthService
     *            the bandwidthService to set
     */
    public void setBandwidthService(HibernateBandwidthDao bandwidthService) {
        this.bandwidthService = bandwidthService;
    }

}
