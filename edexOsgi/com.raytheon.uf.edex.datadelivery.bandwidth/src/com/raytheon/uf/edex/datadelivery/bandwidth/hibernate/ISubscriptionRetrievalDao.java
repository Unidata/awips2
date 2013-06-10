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
package com.raytheon.uf.edex.datadelivery.bandwidth.hibernate;

import java.util.Calendar;
import java.util.List;

import org.hibernate.dialect.Dialect;
import org.hibernate.jdbc.Work;

import com.raytheon.uf.edex.datadelivery.bandwidth.dao.SubscriptionRetrieval;

/**
 * DAO for {@link SubscriptionRetrieval} instances.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 13, 2013 1543       djohnson     Initial creation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
interface ISubscriptionRetrievalDao extends
        IBaseBandwidthAllocationDao<SubscriptionRetrieval> {

    /**
     * Get by provider name, dataset name, and base reference time.
     * 
     * @param provider
     * @param dataSetName
     * @param baseReferenceTime
     * @return
     */
    List<SubscriptionRetrieval> getByProviderDataSetReferenceTime(
            String provider, String dataSetName, Calendar baseReferenceTime);

    /**
     * Get by provider and dataset names.
     * 
     * @param provider
     * @param dataSetName
     * @return
     */
    List<SubscriptionRetrieval> getByProviderDataSet(String provider,
            String dataSetName);

    /**
     * Do arbitrary work.
     * 
     * @param work
     *            work
     */
    // TODO: It would be nice to remove this method, if possible
    void doWork(Work work);

    /**
     * Get the hibernate dialect.
     * 
     * @return
     */
    // TODO: It would be nice to remove this method, if possible
    Dialect getDialect();

}