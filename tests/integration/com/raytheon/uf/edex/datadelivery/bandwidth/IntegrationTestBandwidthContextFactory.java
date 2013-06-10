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

import com.raytheon.uf.edex.datadelivery.bandwidth.dao.BandwidthContextFactory;
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
     */
    IntegrationTestBandwidthContextFactory(IBandwidthDao bandwidthDao,
            IEdexBandwidthManagerCreator bandwidthManagerCreator,
            IBandwidthDbInit dbInit) {
        super(bandwidthDao, new IntegrationTestBandwidthInitializer(),
                bandwidthManagerCreator, dbInit);
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
        return new IntegrationTestBandwidthContextFactory((IBandwidthDao) null,
                (IEdexBandwidthManagerCreator) null, (IBandwidthDbInit) null)
                .getBandwidthMapConfigFile();
    }
}
