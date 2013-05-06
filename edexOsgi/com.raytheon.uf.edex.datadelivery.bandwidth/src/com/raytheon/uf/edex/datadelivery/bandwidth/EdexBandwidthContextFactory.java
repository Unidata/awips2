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
 * Feb 20, 2013 1543       djohnson     Add IEdexBandwidthManagerCreator.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
public class EdexBandwidthContextFactory implements BandwidthContextFactory {

    /**
     * Pluggable strategy for how to create the {@link BandwidthManager}.
     * Intentionally package-private.
     */
    public static interface IEdexBandwidthManagerCreator {

        /**
         * Get the bandwidth manaager.
         * 
         * @param dbInit
         * @param bandwidthDao
         * @param retrievalManager
         * @param bandwidthDaoUtil
         * @return the bandwidth manager
         */
        IBandwidthManager getBandwidthManager(IBandwidthDbInit dbInit,
                IBandwidthDao bandwidthDao, RetrievalManager retrievalManager,
                BandwidthDaoUtil bandwidthDaoUtil);
    }

    private static BandwidthManager instance;

    private final IBandwidthDao bandwidthDao;

    private final BandwidthInitializer bandwidthInitializer;

    private final IEdexBandwidthManagerCreator bandwidthManagerCreator;

    private final IBandwidthDbInit dbInit;

    /**
     * Intentionally package-private constructor, as it is created from Spring
     * which is able to reflectively instantiate.
     * 
     * @param bandwidthDao
     * @param findSubscriptionStrategy
     */
    EdexBandwidthContextFactory(IBandwidthDao bandwidthDao,
            BandwidthInitializer bandwidthInitializer,
            IEdexBandwidthManagerCreator bandwidthManagerCreator,
            IBandwidthDbInit dbInit) {
        this.bandwidthDao = bandwidthDao;
        this.bandwidthInitializer = bandwidthInitializer;
        this.bandwidthManagerCreator = bandwidthManagerCreator;
        this.dbInit = dbInit;
    }

    /**
     * Intentionally private constructor, as it is created from Spring which is
     * able to reflectively instantiate. It is only used to set the
     * {@link BandwidthManager} instance.
     * 
     * @param instance
     *            the {@link BandwidthManager} instance
     */
    EdexBandwidthContextFactory(BandwidthManager instance) {
        EdexBandwidthContextFactory.instance = instance;
        this.bandwidthDao = null;
        this.bandwidthInitializer = null;
        this.bandwidthManagerCreator = null;
        this.dbInit = null;
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
     * 
     * @return the file reference to the bandwidth map config file, the file may
     *         or may not exist
     */
    public static File getBandwidthMapConfig() {
        LocalizationFile lf = getBandwidthMapLocalizationFile();
        File file = lf.getFile();
        return file;
    }

    /**
     * Retrieve the actual bandwidth map localization file.
     * 
     * @return the localization file
     */
    public static LocalizationFile getBandwidthMapLocalizationFile() {
        // TODO: Change to be site specific
        IPathManager pm = PathManagerFactory.getPathManager();
        LocalizationContext lc = pm.getContext(LocalizationType.COMMON_STATIC,
                LocalizationLevel.BASE);

        LocalizationFile lf = pm.getLocalizationFile(lc,
                "datadelivery/bandwidthmap.xml");
        return lf;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public IBandwidthDbInit getBandwidthDbInit() {
        return dbInit;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public IBandwidthDao getBandwidthDao() {
        return bandwidthDao;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public BandwidthInitializer getBandwidthInitializer() {
        return bandwidthInitializer;
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
        return bandwidthManagerCreator.getBandwidthManager(dbInit,
                bandwidthDao, retrievalManager, bandwidthDaoUtil);
    }
}
