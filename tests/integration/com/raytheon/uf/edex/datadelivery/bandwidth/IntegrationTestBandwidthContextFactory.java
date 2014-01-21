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

import java.io.File;

import com.raytheon.uf.common.datadelivery.registry.handlers.IAdhocSubscriptionHandler;
import com.raytheon.uf.common.datadelivery.registry.handlers.IDataSetMetaDataHandler;
import com.raytheon.uf.common.datadelivery.registry.handlers.ISubscriptionHandler;
import com.raytheon.uf.common.datadelivery.service.ISubscriptionNotificationService;
import com.raytheon.uf.edex.datadelivery.bandwidth.dao.BandwidthContextFactory;
import com.raytheon.uf.edex.datadelivery.bandwidth.dao.IBandwidthBucketDao;
import com.raytheon.uf.edex.datadelivery.bandwidth.dao.IBandwidthDao;
import com.raytheon.uf.edex.datadelivery.bandwidth.dao.IBandwidthDbInit;

/**
 * The {@link BandwidthContextFactory} implementation for integration tests.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 24, 2012 1286       djohnson     Initial creation
 * Feb 20, 2013 1543       djohnson     Pass additional super-class constructor arguments.
 * Jun 25, 2013 2106       djohnson     Add {@link IBandwidthBucketDao}.
 * Jul 10, 2013 2106       djohnson     Dependency inject registry handlers.
 * Nov 07, 2013 2506       bgonzale     Added notification handler to bandwidth context.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
public class IntegrationTestBandwidthContextFactory extends
        EdexBandwidthContextFactory {

    /**
     * Constructor, intentionally package-private.
     * 
     * @param bandwidthDao
     *            the bandwidthDao
     * @param bandwidthBucketsDao
     * @param bandwidthManagerCreator
     *            the creator for the bandwidth manager instance
     * @param dbInit
     *            the database initializer
     * @param dataSetMetaDataHandler
     * @param subscriptionHandler
     */
    IntegrationTestBandwidthContextFactory(IBandwidthDao bandwidthDao,
            IBandwidthBucketDao bandwidthBucketsDao,
            IEdexBandwidthManagerCreator bandwidthManagerCreator,
            IBandwidthDbInit dbInit,
            IDataSetMetaDataHandler dataSetMetaDataHandler,
            ISubscriptionHandler subscriptionHandler,
            IAdhocSubscriptionHandler adhocSubscriptionHandler,
            
            ISubscriptionNotificationService notificationService) {
        super(bandwidthDao, bandwidthBucketsDao,
                new IntegrationTestBandwidthInitializer(),
                bandwidthManagerCreator, dbInit, dataSetMetaDataHandler,
                subscriptionHandler, adhocSubscriptionHandler, notificationService);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public IBandwidthDbInit getBandwidthDbInit() {
        return new IntegrationTestDbInit();
    }

    /**
     * Get the integration test bandwidth map config file.
     * 
     * @return the file
     */
    public static File getIntegrationTestBandwidthMapConfigFile() {
        return new IntegrationTestBandwidthContextFactory(null, null, null,
                null, null, null, null, null).getBandwidthMapConfigFile();
    }
}
