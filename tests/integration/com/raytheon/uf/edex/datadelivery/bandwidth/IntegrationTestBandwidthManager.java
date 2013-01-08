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

import com.raytheon.uf.common.util.TestUtil;
import com.raytheon.uf.edex.datadelivery.bandwidth.dao.IBandwidthDao;
import com.raytheon.uf.edex.datadelivery.bandwidth.dao.IBandwidthDbInit;
import com.raytheon.uf.edex.datadelivery.bandwidth.retrieval.RetrievalManager;
import com.raytheon.uf.edex.datadelivery.bandwidth.util.BandwidthDaoUtil;

/**
 * An {@link IBandwidthManager} that runs as an integration test, outside of the
 * EDEX container.
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

public class IntegrationTestBandwidthManager extends BandwidthManager {

    static final String[] INTEGRATION_TEST_SPRING_FILES = new String[] {
            "/bandwidth/bandwidth-datadelivery-integrationtest-impl.xml",
        TestUtil.getResResourcePath("/spring/bandwidth-datadelivery.xml") };

    /**
     * Constructor.
     * 
     * @param dbInit
     * @param bandwidthDao
     * @param retrievalManager
     * @param bandwidthDaoUtil
     */
    public IntegrationTestBandwidthManager(IBandwidthDbInit dbInit,
            IBandwidthDao bandwidthDao, RetrievalManager retrievalManager,
            BandwidthDaoUtil bandwidthDaoUtil) {
        super(dbInit, bandwidthDao, retrievalManager, bandwidthDaoUtil);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected String[] getSpringFilesForNewInstance() {
        return INTEGRATION_TEST_SPRING_FILES;
    }
}
