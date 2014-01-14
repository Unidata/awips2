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
import com.raytheon.uf.common.util.JarUtil;
import com.raytheon.uf.common.util.SpringFiles;
import com.raytheon.uf.edex.datadelivery.bandwidth.IBandwidthManager;
import com.raytheon.uf.edex.datadelivery.bandwidth.dao.IBandwidthDao;
import com.raytheon.uf.edex.datadelivery.bandwidth.dao.IBandwidthDbInit;
import com.raytheon.uf.edex.datadelivery.bandwidth.ncf.NcfBandwidthManagerCreator.NcfBandwidthManager;
import com.raytheon.uf.edex.datadelivery.bandwidth.retrieval.RetrievalManager;
import com.raytheon.uf.edex.datadelivery.bandwidth.util.BandwidthDaoUtil;

/**
 * An NCF {@link IBandwidthManager} that runs as an integration test, outside of
 * the EDEX container.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 18, 2013 1543       djohnson     Initial creation
 * Feb 27, 2013 1644       djohnson     Extend NCF bandwidth manager.
 * Jul 10, 2013 2106       djohnson     Dependency inject registry handlers.
 * Nov 08, 2013 2506       bgonzale     Added notification service to bandwidth manager.
 * Jan 14, 2014 2692       dhladky      AdhocSubscription handler 
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public class IntegrationTestNcfBandwidthManager extends NcfBandwidthManager {

    static final String[] INTEGRATION_TEST_SPRING_FILES = new String[] {
            "/bandwidth/bandwidth-datadelivery-integrationtest-impl.xml",
            JarUtil.getResResourcePath("/spring/bandwidth-datadelivery-daos.xml"),
            JarUtil.getResResourcePath(SpringFiles.BANDWIDTH_DATADELIVERY_XML),
            JarUtil.getResResourcePath(SpringFiles.BANDWIDTH_DATADELIVERY_EVENTBUS_XML),
            JarUtil.getResResourcePath(SpringFiles.BANDWIDTH_DATADELIVERY_NCF_XML) };

    /**
     * Constructor.
     * 
     * @param dbInit
     * @param bandwidthDao
     * @param retrievalManager
     * @param bandwidthDaoUtil
     */
    public IntegrationTestNcfBandwidthManager(IBandwidthDbInit dbInit,
            IBandwidthDao bandwidthDao, RetrievalManager retrievalManager,
            BandwidthDaoUtil bandwidthDaoUtil,
            IDataSetMetaDataHandler dataSetMetaDataHandler,
            ISubscriptionHandler subscriptionHandler,
            IAdhocSubscriptionHandler adhocSubscriptionHandler,
            ISubscriptionNotificationService subscriptionNotificationService) {
        super(dbInit, bandwidthDao, retrievalManager, bandwidthDaoUtil,
                dataSetMetaDataHandler, subscriptionHandler, adhocSubscriptionHandler,
                subscriptionNotificationService);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected String[] getSpringFilesForNewInstance() {
        return INTEGRATION_TEST_SPRING_FILES;
    }
}
