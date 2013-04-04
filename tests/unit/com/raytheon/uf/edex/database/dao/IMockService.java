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

import com.raytheon.uf.edex.datadelivery.bandwidth.dao.BandwidthSubscription;
import com.raytheon.uf.edex.datadelivery.bandwidth.dao.IBandwidthDao;

/**
 * DAO interface for MockService.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 13, 2013 1543       djohnson     Generated from MockService.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
public interface IMockService {

    /**
     * Stores a {@link BandwidthSubscription} then attempts an invalid transactable
     * operation.
     * 
     * @param subscription
     *            the subscription
     */
    void storeStuffThenThrowException(BandwidthSubscription subscription);

    /**
     * Stores a {@link BandwidthSubscription}, not throwing an exception as long as
     * the subscription object is valid.
     * 
     * @param subscription
     *            the subscription
     */
    void storeStuffAndNotThrowException(BandwidthSubscription subscription);

    /**
     * @param bandwidthService
     *            the bandwidthService to set
     */
    void setBandwidthService(IBandwidthDao bandwidthService);

}