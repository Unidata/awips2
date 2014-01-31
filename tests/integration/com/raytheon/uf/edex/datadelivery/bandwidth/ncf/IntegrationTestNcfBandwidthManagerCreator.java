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

import com.raytheon.uf.common.datadelivery.registry.handlers.IAdhocSubscriptionHandler;
import com.raytheon.uf.common.datadelivery.registry.handlers.IDataSetMetaDataHandler;
import com.raytheon.uf.common.datadelivery.registry.handlers.ISubscriptionHandler;
import com.raytheon.uf.common.datadelivery.service.ISubscriptionNotificationService;
import com.raytheon.uf.edex.datadelivery.bandwidth.EdexBandwidthContextFactory.IEdexBandwidthManagerCreator;
import com.raytheon.uf.edex.datadelivery.bandwidth.IBandwidthManager;
import com.raytheon.uf.edex.datadelivery.bandwidth.dao.IBandwidthDao;
import com.raytheon.uf.edex.datadelivery.bandwidth.dao.IBandwidthDbInit;
import com.raytheon.uf.edex.datadelivery.bandwidth.retrieval.RetrievalManager;
import com.raytheon.uf.edex.datadelivery.bandwidth.util.BandwidthDaoUtil;

/**
 * Creates {@link IntegrationTestNcfBandwidthManager} instances.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 20, 2013 1543       djohnson     Initial creation
 * Jul 10, 2013 2106       djohnson     Dependency inject registry handlers.
 * Nov 08, 2013 2506       bgonzale     Added notification service to bandwidth manager.
 * Jan 14, 2014 2692       dhladky      AdhocSubscription handler 
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
public class IntegrationTestNcfBandwidthManagerCreator implements
        IEdexBandwidthManagerCreator {

    /**
     * {@inheritDoc}
     */
    @Override
    public IBandwidthManager getBandwidthManager(IBandwidthDbInit dbInit,
            IBandwidthDao bandwidthDao, RetrievalManager retrievalManager,
            BandwidthDaoUtil bandwidthDaoUtil,
            IDataSetMetaDataHandler dataSetMetaDataHandler,
            ISubscriptionHandler subscriptionHandler,
            IAdhocSubscriptionHandler adhocSubscriptionHandler,
            ISubscriptionNotificationService subscriptionNotificationService) {
        return new IntegrationTestNcfBandwidthManager(dbInit, bandwidthDao,
                retrievalManager, bandwidthDaoUtil, dataSetMetaDataHandler,
                subscriptionHandler, adhocSubscriptionHandler, subscriptionNotificationService);
    }
}
