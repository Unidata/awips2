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

import com.raytheon.uf.common.datadelivery.registry.Coverage;
import com.raytheon.uf.common.datadelivery.registry.Time;
import com.raytheon.uf.common.datadelivery.registry.handlers.IAdhocSubscriptionHandler;
import com.raytheon.uf.common.datadelivery.registry.handlers.IDataSetMetaDataHandler;
import com.raytheon.uf.common.datadelivery.registry.handlers.ISubscriptionHandler;
import com.raytheon.uf.common.datadelivery.service.ISubscriptionNotificationService;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.edex.datadelivery.bandwidth.dao.BandwidthContextFactory;
import com.raytheon.uf.edex.datadelivery.bandwidth.dao.IBandwidthBucketDao;
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
 * Jul 10, 2013 2106       djohnson     Dependency inject registry handlers.
 * Oct 03, 2013 1797       dhladky      Some generics
 * Nov 07, 2013 2506       bgonzale     Added notification handler to bandwidth context.
 * Jan 14, 2014 2692       dhladky      AdhocSubscription handler 
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
public class EdexBandwidthContextFactory<T extends Time, C extends Coverage> implements BandwidthContextFactory {

    /**
     * Pluggable strategy for how to create the {@link BandwidthManager}.
     * Intentionally package-private.
     */
    public static interface IEdexBandwidthManagerCreator<T extends Time, C extends Coverage> {

        /**
         * Get the bandwidth manaager.
         * 
         * @param dbInit
         * @param bandwidthDao
         * @param retrievalManager
         * @param bandwidthDaoUtil
         * @param dataSetMetaDataHandler
         * @param subscriptionHandler
         * @return the bandwidth manager
         */
        IBandwidthManager<T, C> getBandwidthManager(IBandwidthDbInit dbInit,
                IBandwidthDao<T, C> bandwidthDao, RetrievalManager retrievalManager,
                BandwidthDaoUtil<T, C> bandwidthDaoUtil,
                IDataSetMetaDataHandler dataSetMetaDataHandler,
                ISubscriptionHandler subscriptionHandler,
                IAdhocSubscriptionHandler adhocSubscriptionHandler,
                ISubscriptionNotificationService notificationService);
    }

    private static EdexBandwidthManager instance;

    private final IBandwidthDao<T, C> bandwidthDao;

    private final IBandwidthBucketDao bandwidthBucketDao;

    private final BandwidthInitializer bandwidthInitializer;

    private final IEdexBandwidthManagerCreator<T, C> bandwidthManagerCreator;

    private final IBandwidthDbInit dbInit;

    private final IDataSetMetaDataHandler dataSetMetaDataHandler;

    private final ISubscriptionHandler subscriptionHandler;
    
    private final IAdhocSubscriptionHandler adhocSubscriptionHandler;

    private final ISubscriptionNotificationService notificationService;

    /**
     * Intentionally package-private constructor, as it is created from Spring
     * which is able to reflectively instantiate.
     * 
     * @param bandwidthDao
     * @param bandwidthBucketDao
     * @param bandwidthInitializer
     * @param bandwidthManagerCreator
     * @param dbInit
     * @param dataSetMetaDataHandler
     * @param subscriptionHandler
     * @param adhocSubscriptionHandler
     * @param notificationService
     */
    EdexBandwidthContextFactory(IBandwidthDao<T, C> bandwidthDao,
            IBandwidthBucketDao bandwidthBucketDao,
            BandwidthInitializer bandwidthInitializer,
            IEdexBandwidthManagerCreator<T, C> bandwidthManagerCreator,
            IBandwidthDbInit dbInit,
            IDataSetMetaDataHandler dataSetMetaDataHandler,
            ISubscriptionHandler subscriptionHandler,
            IAdhocSubscriptionHandler adhocSubscriptionHandler,
            ISubscriptionNotificationService notificationService) {
        this.bandwidthDao = bandwidthDao;
        this.bandwidthBucketDao = bandwidthBucketDao;
        this.bandwidthInitializer = bandwidthInitializer;
        this.bandwidthManagerCreator = bandwidthManagerCreator;
        this.dbInit = dbInit;
        this.dataSetMetaDataHandler = dataSetMetaDataHandler;
        this.subscriptionHandler = subscriptionHandler;
        this.adhocSubscriptionHandler = adhocSubscriptionHandler;
        this.notificationService = notificationService;

    }

    /**
     * Intentionally private constructor, as it is created from Spring which is
     * able to reflectively instantiate. It is only used to set the
     * {@link BandwidthManager} instance.
     * 
     * @param instance
     *            the {@link BandwidthManager} instance
     */
    EdexBandwidthContextFactory(EdexBandwidthManager<T, C> instance) {
        this(null, null, null, null, null, null, null, null, null);
        EdexBandwidthContextFactory.instance = instance;
    }

    /**
     * Intentionally package-private, the instance should only be retrieved from
     * classes in the same package.
     * 
     * 
     * @return the instance
     */
    static EdexBandwidthManager getInstance() {
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
    public IBandwidthDao<T, C> getBandwidthDao() {
        return bandwidthDao;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public IBandwidthBucketDao getBandwidthBucketDao() {
        return bandwidthBucketDao;
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
    public IBandwidthManager<T, C> getBandwidthManager(IBandwidthDbInit dbInit,
            IBandwidthDao bandwidthDao, RetrievalManager retrievalManager,
            BandwidthDaoUtil bandwidthDaoUtil) {
        return bandwidthManagerCreator.getBandwidthManager(dbInit,
                bandwidthDao, retrievalManager, bandwidthDaoUtil,
                dataSetMetaDataHandler, subscriptionHandler, adhocSubscriptionHandler,
                notificationService);
    }
}
