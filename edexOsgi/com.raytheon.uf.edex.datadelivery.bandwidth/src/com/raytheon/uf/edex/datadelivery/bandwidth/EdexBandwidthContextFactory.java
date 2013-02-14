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

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.edex.datadelivery.bandwidth.dao.BandwidthContextFactory;
import com.raytheon.uf.edex.datadelivery.bandwidth.dao.IBandwidthDao;
import com.raytheon.uf.edex.datadelivery.bandwidth.dao.IBandwidthDbInit;
import com.raytheon.uf.edex.datadelivery.bandwidth.hibernate.HibernateBandwidthDao;
import com.raytheon.uf.edex.datadelivery.bandwidth.hibernate.HibernateBandwidthDbInit;
import com.raytheon.uf.edex.datadelivery.bandwidth.hibernate.HibernateBandwidthInitializer;
import com.raytheon.uf.edex.datadelivery.bandwidth.interfaces.BandwidthInitializer;
import com.raytheon.uf.edex.datadelivery.bandwidth.retrieval.RetrievalManager;
import com.raytheon.uf.edex.datadelivery.bandwidth.util.BandwidthDaoUtil;

/**
 * {@link BandwidthContextFactory} for running in EDEX. Intentionally
 * package-private to hide implementation details.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 24, 2012 1286       djohnson     Initial creation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
class EdexBandwidthContextFactory implements BandwidthContextFactory {

    private static BandwidthManager instance;

    /**
     * Intentionally package-private constructor, as it is created from Spring
     * which is able to reflectively instantiate.
     * 
     * @param instance
     *            the {@link BandwidthManager} instance
     */
    EdexBandwidthContextFactory() {
    }

    /**
     * Intentionally package-private constructor, as it is created from Spring
     * which is able to reflectively instantiate.
     */
    @SuppressWarnings("unused")
    private EdexBandwidthContextFactory(BandwidthManager instance) {
        EdexBandwidthContextFactory.instance = instance;
    }

    /**
     * Intentionally package-private, the instance should only be retrieved from
     * classes in the same package.
     * 
     * 
     * @return the instance
     */
    static BandwidthManager getInstance() {
        return instance;
    }

    /**
     * Retrieve the actual bandwidth map configuration file.
     */
    public static File getBandwidthMapConfig() {
        // TODO: Change to be site specific
        IPathManager pm = PathManagerFactory.getPathManager();
        LocalizationContext lc = pm.getContext(LocalizationType.COMMON_STATIC,
                LocalizationLevel.BASE);

        LocalizationFile lf = pm.getLocalizationFile(lc,
                "datadelivery/bandwidthmap.xml");
        File file = lf.getFile();
        return file;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public IBandwidthDbInit getBandwidthDbInit() {
        return new HibernateBandwidthDbInit(HibernateBandwidthDao.getInstance());
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public IBandwidthDao getBandwidthDao() {
        return HibernateBandwidthDao.getInstance();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public BandwidthInitializer getBandwidthInitializer() {
        return new HibernateBandwidthInitializer();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public File getBandwidthMapConfigFile() {
        return getBandwidthMapConfig();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public IBandwidthManager getBandwidthManager(IBandwidthDbInit dbInit,
            IBandwidthDao bandwidthDao, RetrievalManager retrievalManager,
            BandwidthDaoUtil bandwidthDaoUtil) {
        return new EdexBandwidthManager(dbInit, bandwidthDao, retrievalManager,
                bandwidthDaoUtil);
    }
}
