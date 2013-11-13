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

import java.util.List;
import java.util.Set;

import com.raytheon.uf.common.datadelivery.bandwidth.IBandwidthService;
import com.raytheon.uf.common.datadelivery.bandwidth.IProposeScheduleResponse;
import com.raytheon.uf.common.datadelivery.registry.Coverage;
import com.raytheon.uf.common.datadelivery.registry.Subscription;
import com.raytheon.uf.common.datadelivery.registry.Time;
import com.raytheon.uf.common.datadelivery.registry.handlers.IDataSetMetaDataHandler;
import com.raytheon.uf.common.datadelivery.registry.handlers.ISubscriptionHandler;
import com.raytheon.uf.common.datadelivery.service.ISubscriptionNotificationService;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.util.JarUtil;
import com.raytheon.uf.edex.datadelivery.bandwidth.EdexBandwidthContextFactory.IEdexBandwidthManagerCreator;
import com.raytheon.uf.edex.datadelivery.bandwidth.dao.IBandwidthDao;
import com.raytheon.uf.edex.datadelivery.bandwidth.dao.IBandwidthDbInit;
import com.raytheon.uf.edex.datadelivery.bandwidth.retrieval.RetrievalManager;
import com.raytheon.uf.edex.datadelivery.bandwidth.util.BandwidthDaoUtil;

/**
 * {@link IEdexBandwidthManagerCreator} for a WFO bandwidth manager.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 20, 2013 1543       djohnson     Initial creation
 * Feb 27, 2013 1644       djohnson     Schedule SBN subscriptions by routing to the NCF bandwidth manager.
 * Mar 11, 2013 1645       djohnson     Add missing Spring file.
 * May 15, 2013 2000       djohnson     Include daos.
 * Jul 10, 2013 2106       djohnson     Dependency inject registry handlers.
 * Oct 2,  2013 1797       dhladky      Generics
 * Oct 28, 2013 2506       bgonzale     SBN (Shared) Scheduled at the central registry.
 *                                      Added subscription notification service to bandwidth manager.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
public class WfoBandwidthManagerCreator<T extends Time, C extends Coverage> implements IEdexBandwidthManagerCreator {

    /**
     * WFO {@link BandwidthManager} implementation.
     */
    static class WfoBandwidthManager<T extends Time, C extends Coverage> extends EdexBandwidthManager<T, C> {

        private static final String[] WFO_BANDWIDTH_MANAGER_FILES = new String[] {
                JarUtil.getResResourcePath("/spring/bandwidth-datadelivery-wfo-edex-impl.xml"),
                JarUtil.getResResourcePath("/spring/bandwidth-datadelivery-edex-impl.xml"),
                JarUtil.getResResourcePath("/spring/bandwidth-datadelivery.xml"),
                JarUtil.getResResourcePath("/spring/bandwidth-datadelivery-daos.xml"),
                JarUtil.getResResourcePath("/spring/bandwidth-datadelivery-eventbus.xml"),
                JarUtil.getResResourcePath("/spring/thrift-bandwidth.xml"),
                JarUtil.getResResourcePath("/spring/bandwidth-datadelivery-wfo.xml") };

        // TODO: Change to be DIed in Spring
        private final IBandwidthService<T, C> ncfBandwidthService = new NcfBandwidthService();

        /**
         * Constructor.
         * 
         * @param dbInit
         * @param bandwidthDao
         * @param retrievalManager
         * @param bandwidthDaoUtil
         * @param subscriptionNotificationService
         */
        public WfoBandwidthManager(IBandwidthDbInit dbInit,
                IBandwidthDao bandwidthDao, RetrievalManager retrievalManager,
                BandwidthDaoUtil bandwidthDaoUtil,
                IDataSetMetaDataHandler dataSetMetaDataHandler,
                ISubscriptionHandler subscriptionHandler,
                ISubscriptionNotificationService subscriptionNotificationService) {
            super(dbInit, bandwidthDao, retrievalManager, bandwidthDaoUtil,
                    dataSetMetaDataHandler, subscriptionHandler,
                    subscriptionNotificationService);
        }

        @Override
        protected String[] getSpringFilesForNewInstance() {
            return WFO_BANDWIDTH_MANAGER_FILES;
        }

        /**
         * {@inheritDoc}
         */
        @Override
        protected IProposeScheduleResponse proposeScheduleSbnSubscription(
                List<Subscription<T, C>> subscriptions) throws Exception {

            final IProposeScheduleResponse proposeResponse = ncfBandwidthService
                    .proposeSchedule(subscriptions);

            // If the NCF bandwidth manager says they fit without
            // unscheduling anything, then schedule them at the NCF level
            if (proposeResponse.getUnscheduledSubscriptions().isEmpty()) {
                ncfBandwidthService.schedule(subscriptions);
            }

            return proposeResponse;
        }

        /**
         * {@inheritDoc}
         */
        @Override
        protected Set<String> scheduleSbnSubscriptions(
                List<Subscription<T, C>> subscriptions) throws SerializationException {

            return ncfBandwidthService.schedule(subscriptions);
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
        return new WfoBandwidthManager<T, C>(dbInit, bandwidthDao,
                retrievalManager, bandwidthDaoUtil, dataSetMetaDataHandler,
                subscriptionHandler, subscriptionNotificationService);
    }

}
