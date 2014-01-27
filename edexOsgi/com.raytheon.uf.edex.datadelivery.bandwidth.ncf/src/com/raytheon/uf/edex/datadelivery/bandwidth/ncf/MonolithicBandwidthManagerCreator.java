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

import com.raytheon.uf.common.datadelivery.registry.Coverage;
import com.raytheon.uf.common.datadelivery.registry.Time;
import com.raytheon.uf.common.datadelivery.registry.handlers.IDataSetMetaDataHandler;
import com.raytheon.uf.common.datadelivery.registry.handlers.ISubscriptionHandler;
import com.raytheon.uf.common.datadelivery.service.ISubscriptionNotificationService;
import com.raytheon.uf.edex.datadelivery.bandwidth.BandwidthManager;
import com.raytheon.uf.edex.datadelivery.bandwidth.EdexBandwidthContextFactory.IEdexBandwidthManagerCreator;
import com.raytheon.uf.edex.datadelivery.bandwidth.IBandwidthManager;
import com.raytheon.uf.edex.datadelivery.bandwidth.dao.IBandwidthDao;
import com.raytheon.uf.edex.datadelivery.bandwidth.dao.IBandwidthDbInit;
import com.raytheon.uf.edex.datadelivery.bandwidth.retrieval.RetrievalManager;
import com.raytheon.uf.edex.datadelivery.bandwidth.util.BandwidthDaoUtil;

/**
 * {@link IEdexBandwidthManagerCreator} A bandwidth manager creator for
 * bandwidth managers that act standalone. They do not have parent or client
 * registries, but handle the scheduling themselves. This creator will configure
 * the manager with the spring files to configure SBN and OPSNET retrievals.
 * 
 * This is primarily for a development EDEX mode for registry so that testing
 * can be on a standalone box.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 13, 2013 2545       bgonzale    Initial creation
 * Dec 04, 2013 2566       bgonzale    use bandwidthmanager method to retrieve spring files.
 * 
 * </pre>
 * 
 * @author bgonzale
 * @version 1.0
 */
public class MonolithicBandwidthManagerCreator<T extends Time, C extends Coverage>
        extends NcfBandwidthManagerCreator<T, C> {

    /**
     * NCF {@link BandwidthManager} implementation.
     */
    static class MonolithicBandwidthManager<T extends Time, C extends Coverage>
            extends NcfBandwidthManager<T, C> {

        public static final String MODE_NAME = "devRegistry";

        private static final String[] BANDWIDTH_MANAGER_FILES = getSpringFileNamesForMode(MODE_NAME);

        /**
         * Constructor.
         * 
         * @param dbInit
         * @param bandwidthDao
         * @param retrievalManager
         * @param bandwidthDaoUtil
         */
        public MonolithicBandwidthManager(IBandwidthDbInit dbInit,
                IBandwidthDao<T, C> bandwidthDao, RetrievalManager retrievalManager,
                BandwidthDaoUtil<T, C> bandwidthDaoUtil,
                IDataSetMetaDataHandler dataSetMetaDataHandler,
                ISubscriptionHandler subscriptionHandler,
                ISubscriptionNotificationService subscriptionNotificationService) {
            super(dbInit, bandwidthDao, retrievalManager, bandwidthDaoUtil,
                    dataSetMetaDataHandler, subscriptionHandler,
                    subscriptionNotificationService);
        }

        @Override
        protected String[] getSpringFilesForNewInstance() {
            return BANDWIDTH_MANAGER_FILES;
        }

    }

    /**
     * {@inheritDoc}
     */
    @Override
    public IBandwidthManager<T, C> getBandwidthManager(IBandwidthDbInit dbInit,
            IBandwidthDao bandwidthDao, RetrievalManager retrievalManager,
            BandwidthDaoUtil bandwidthDaoUtil,
            IDataSetMetaDataHandler dataSetMetaDataHandler,
            ISubscriptionHandler subscriptionHandler,
            ISubscriptionNotificationService subscriptionNotificationService) {
        return new MonolithicBandwidthManager(dbInit, bandwidthDao,
                retrievalManager,
                bandwidthDaoUtil, dataSetMetaDataHandler, subscriptionHandler,
                subscriptionNotificationService);
    }

}
