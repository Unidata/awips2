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

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.util.JarUtil;
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
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
class InMemoryBandwidthManager extends BandwidthManager {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(InMemoryBandwidthManager.class);

    public static final String[] IN_MEMORY_BANDWIDTH_MANAGER_FILES = new String[] {
            JarUtil.getResResourcePath("/spring/bandwidth-datadelivery-inmemory-impl.xml"),
            JarUtil.getResResourcePath("/spring/bandwidth-datadelivery.xml") };

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
        public boolean init(IBandwidthManager instance, IBandwidthDbInit dbInit) {
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
            IBandwidthDao bandwidthDao, RetrievalManager retrievalManager,
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

}
