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

import com.raytheon.uf.common.datadelivery.bandwidth.ProposeScheduleResponse;
import com.raytheon.uf.common.datadelivery.registry.Coverage;
import com.raytheon.uf.common.datadelivery.registry.Subscription;
import com.raytheon.uf.common.datadelivery.registry.Time;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.edex.datadelivery.bandwidth.dao.BandwidthAllocation;
import com.raytheon.uf.edex.datadelivery.bandwidth.dao.IBandwidthDao;
import com.raytheon.uf.edex.datadelivery.bandwidth.dao.IBandwidthDbInit;
import com.raytheon.uf.edex.datadelivery.bandwidth.interfaces.BandwidthInitializer;
import com.raytheon.uf.edex.datadelivery.bandwidth.retrieval.RetrievalManager;
import com.raytheon.uf.edex.datadelivery.bandwidth.util.BandwidthDaoUtil;

/**
 * An in-memory {@link IBandwidthManager} that does not communicate with an
 * actual database. Intentionally package-private.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 30, 2012 1286       djohnson     Initial creation
 * Feb 20, 2013 1543       djohnson     For now assume all in-memory bandwidth managers are WFOs.
 * Feb 27, 2013 1644       djohnson     Schedule SBN subscriptions.
 * Apr 16, 2013 1906       djohnson     Implements RegistryInitializedListener.
 * Jun 25, 2013 2106       djohnson     init() now takes a {@link RetrievalManager} as well.
 * Jul 09, 2013 2106       djohnson     Add shutdownInternal().
 * Oct 2,  2013 1797       dhladky      Generics
 * Dec 04, 2013 2566       bgonzale     use bandwidthmanager method to retrieve spring files.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
class InMemoryBandwidthManager<T extends Time, C extends Coverage> extends BandwidthManager<T, C> {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(InMemoryBandwidthManager.class);

    private static final String MODE_NAME = "inMemoryBandwidthManager";

    // NOTE: NEVER add the bandwidth-datadelivery-eventbus.xml file to this
    // array, in-memory versions should not coordinate with the event bus in any
    // fashion
    public static final String[] IN_MEMORY_BANDWIDTH_MANAGER_FILES = getSpringFileNamesForMode(MODE_NAME);

    /**
     * {@link BandwidthInitializer} which will make a copy of the current
     * running EDEX {@link BandwidthManager} data.
     */
    public static class InMemoryBandwidthInitializer implements
            BandwidthInitializer {

        /**
         * {@inheritDoc}
         */
        @Override
        public boolean init(IBandwidthManager instance,
                IBandwidthDbInit dbInit, RetrievalManager retrievalManager) {
            BandwidthManager edexBandwidthManager = EdexBandwidthContextFactory
                    .getInstance();
            if (instance instanceof InMemoryBandwidthManager) {
                List<BandwidthAllocation> unscheduled = ((InMemoryBandwidthManager) instance)
                        .copyState(edexBandwidthManager);
                return true;
            } else {
                statusHandler
                        .error("Skipping init(), this initializer should only be used "
                                + "on an in-memory BandwidthManager!"
                                + "  This is a configuration error.");
                return false;
            }
        }

        /**
         * {@inheritDoc}
         */
        @Override
        public void executeAfterRegistryInit() {
            // Nothing to do
        }
    }

    /**
     * Constructor.
     * 
     * @param dbInit
     * @param bandwidthDao
     * @param retrievalManager
     * @param bandwidthDaoUtil
     */
    public InMemoryBandwidthManager(IBandwidthDbInit dbInit,
            IBandwidthDao<T,C> bandwidthDao, RetrievalManager retrievalManager,
            BandwidthDaoUtil bandwidthDaoUtil) {
        super(dbInit, bandwidthDao, retrievalManager, bandwidthDaoUtil);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected String[] getSpringFilesForNewInstance() {
        return IN_MEMORY_BANDWIDTH_MANAGER_FILES;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected ProposeScheduleResponse proposeScheduleSbnSubscription(
            List<Subscription<T,C>> subscriptions) throws Exception {
        return proposeScheduleSubscriptions(subscriptions);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected Set<String> scheduleSbnSubscriptions(
            List<Subscription<T,C>> subscriptions) throws SerializationException {
        return scheduleSubscriptions(subscriptions);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected void shutdownInternal() {
        // Nothing to do for in-memory version
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected void unscheduleSubscriptionsForAllocations(
            List<BandwidthAllocation> unscheduled) {
        // Nothing to do for in-memory version
    }

}
