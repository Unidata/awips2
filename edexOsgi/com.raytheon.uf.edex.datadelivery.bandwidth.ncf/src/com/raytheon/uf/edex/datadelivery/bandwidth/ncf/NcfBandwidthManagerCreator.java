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

import java.util.List;
import java.util.Set;

import com.raytheon.uf.common.datadelivery.bandwidth.ProposeScheduleResponse;
import com.raytheon.uf.common.datadelivery.registry.Subscription;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.util.JarUtil;
import com.raytheon.uf.edex.datadelivery.bandwidth.BandwidthManager;
import com.raytheon.uf.edex.datadelivery.bandwidth.EdexBandwidthContextFactory.IEdexBandwidthManagerCreator;
import com.raytheon.uf.edex.datadelivery.bandwidth.IBandwidthManager;
import com.raytheon.uf.edex.datadelivery.bandwidth.dao.IBandwidthDao;
import com.raytheon.uf.edex.datadelivery.bandwidth.dao.IBandwidthDbInit;
import com.raytheon.uf.edex.datadelivery.bandwidth.retrieval.RetrievalManager;
import com.raytheon.uf.edex.datadelivery.bandwidth.util.BandwidthDaoUtil;

/**
 * {@link IEdexBandwidthManagerCreator} for an NCF bandwidth manager.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 20, 2013 1543       djohnson     Initial creation
 * Feb 27, 2013 1644       djohnson     Schedule SBN subscriptions.
 * Mar 11, 2013 1645       djohnson     Add missing Spring file.
 * May 15, 2013 2000       djohnson     Include daos.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
public class NcfBandwidthManagerCreator implements IEdexBandwidthManagerCreator {

    /**
     * NCF {@link BandwidthManager} implementation.
     */
    static class NcfBandwidthManager extends BandwidthManager {

        private static final String[] NCF_BANDWIDTH_MANAGER_FILES = new String[] {
                JarUtil.getResResourcePath("/spring/bandwidth-datadelivery-ncf-edex-impl.xml"),
                JarUtil.getResResourcePath("/spring/bandwidth-datadelivery-edex-impl.xml"),
                JarUtil.getResResourcePath("/spring/bandwidth-datadelivery.xml"),
                JarUtil.getResResourcePath("/spring/bandwidth-datadelivery-daos.xml"),
                JarUtil.getResResourcePath("/spring/bandwidth-datadelivery-eventbus.xml"),
                JarUtil.getResResourcePath("/spring/thrift-bandwidth.xml"),
                JarUtil.getResResourcePath("/spring/bandwidth-datadelivery-ncf.xml") };

        /**
         * Constructor.
         * 
         * @param dbInit
         * @param bandwidthDao
         * @param retrievalManager
         * @param bandwidthDaoUtil
         */
        public NcfBandwidthManager(IBandwidthDbInit dbInit,
                IBandwidthDao bandwidthDao, RetrievalManager retrievalManager,
                BandwidthDaoUtil bandwidthDaoUtil) {
            super(dbInit, bandwidthDao, retrievalManager, bandwidthDaoUtil);
        }

        @Override
        protected String[] getSpringFilesForNewInstance() {
            return NCF_BANDWIDTH_MANAGER_FILES;
        }

        /**
         * {@inheritDoc}
         */
        @Override
        protected ProposeScheduleResponse proposeScheduleSbnSubscription(
                List<Subscription> subscriptions) throws Exception {
            return proposeScheduleSubscriptions(subscriptions);
        }

        /**
         * {@inheritDoc}
         */
        @Override
        protected Set<String> scheduleSbnSubscriptions(
                List<Subscription> subscriptions) throws SerializationException {
            return scheduleSubscriptions(subscriptions);
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public IBandwidthManager getBandwidthManager(IBandwidthDbInit dbInit,
            IBandwidthDao bandwidthDao, RetrievalManager retrievalManager,
            BandwidthDaoUtil bandwidthDaoUtil) {
        return new NcfBandwidthManager(dbInit, bandwidthDao, retrievalManager,
                bandwidthDaoUtil);
    }

}
