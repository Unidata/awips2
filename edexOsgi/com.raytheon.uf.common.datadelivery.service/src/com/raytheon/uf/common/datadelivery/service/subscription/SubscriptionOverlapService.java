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
package com.raytheon.uf.common.datadelivery.service.subscription;

import java.util.HashMap;
import java.util.Map;
import java.util.MissingResourceException;

import javax.xml.bind.JAXBException;

import com.raytheon.uf.common.datadelivery.registry.Coverage;
import com.raytheon.uf.common.datadelivery.registry.DataType;
import com.raytheon.uf.common.datadelivery.registry.Subscription;
import com.raytheon.uf.common.datadelivery.registry.Time;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.exception.LocalizationException;
import com.raytheon.uf.common.serialization.JAXBManager;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;

/**
 * Checks subscriptions to see if they would be considered duplicates.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 07, 2013 2000       djohnson     Initial creation
 * Jun 04, 2013  223       mpduff       Get base file if site doesn't exist.
 * Sept 23, 2013 2283      dhladky      Updated for multiple configs
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public class SubscriptionOverlapService<T extends Time, C extends Coverage> implements ISubscriptionOverlapService<T, C> {

    /**
     * Base response object implementing {@link ISubscriptionOverlapResponse}.
     */
    public class SubscriptionOverlapResponse implements
            ISubscriptionOverlapResponse {

        private final boolean duplicate;

        private final boolean overlapping;

        /**
         * Constructor.
         * 
         * @param duplicate
         * @param overlapping
         */
        public SubscriptionOverlapResponse(boolean duplicate,
                boolean overlapping) {
            this.duplicate = duplicate;
            this.overlapping = overlapping;
        }

        /**
         * {@inheritDoc}
         */
        @Override
        public boolean isDuplicate() {
            return duplicate;
        }

        /**
         * {@inheritDoc}
         */
        @Override
        public boolean isOverlapping() {
            return overlapping;
        }

    }

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(SubscriptionOverlapService.class);

    private static final String UNABLE_TO_UNMARSHAL = "Unable to unmarshal the configuration file.  "
            + "No subscriptions will be considered to overlap!";

    private static final String SUBSCRIPTION_OVERLAP_CONFIG_FILE_ROOT = "SubscriptionOverlapRules.xml";
    
    private static final String SUBSCRIPTION_OVERLAP_CONFIG_FILE_PATH = "datadelivery/";

    private final ISubscriptionDuplicateChecker<T, C> duplicateChecker;

    private final JAXBManager jaxbManager;

    /**
     * Constructor.
     * 
     * @param duplicateChecker
     */
    public SubscriptionOverlapService(
            ISubscriptionDuplicateChecker<T,C> duplicateChecker) {
        this.duplicateChecker = duplicateChecker;

        try {
            @SuppressWarnings("rawtypes")
            Class[] clazzes = new Class[]{SubscriptionOverlapConfig.class,
                    GridSubscriptionOverlapConfig.class,
                    PointSubscriptionOverlapConfig.class};
            jaxbManager = new JAXBManager(clazzes);
        } catch (JAXBException e) {
            throw new ExceptionInInitializerError(e);
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ISubscriptionOverlapResponse isOverlapping(Subscription<T, C> sub1,
            Subscription<T, C> sub2) {
        // Ignore requests to compare with itself
        if (sub1.getName().equals(sub2.getName())) {
            return new SubscriptionOverlapResponse(false, false);
        }
        
        // Ignore requests where the two subscriptions are of different types
        if (!sub1.getDataSetType().equals(sub2.getDataSetType())) {
            return new SubscriptionOverlapResponse(false, false);
        }
        
        SubscriptionOverlapConfig config = getConfigFile(sub1.getDataSetType());
        
        return getOverlap(config, sub1, sub2);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void writeConfig(SubscriptionOverlapConfig config)
            throws LocalizationException {
        final IPathManager pathManager = PathManagerFactory.getPathManager();
        LocalizationContext context = pathManager.getContext(
                LocalizationType.COMMON_STATIC, LocalizationLevel.SITE);
        
        String fileName = null;
        
        if (config instanceof PointSubscriptionOverlapConfig) {
            fileName = SUBSCRIPTION_OVERLAP_CONFIG_FILE_PATH+DataType.POINT.name()+SUBSCRIPTION_OVERLAP_CONFIG_FILE_ROOT;
        } else if (config instanceof GridSubscriptionOverlapConfig) {
            fileName = SUBSCRIPTION_OVERLAP_CONFIG_FILE_PATH+DataType.GRID.name()+SUBSCRIPTION_OVERLAP_CONFIG_FILE_ROOT;
        } else {
            throw new IllegalArgumentException(config.getClass()+" Doesn't have any implementation in use");
        }
        
        final LocalizationFile configFile = pathManager
                .getLocalizationFile(
                        context,
                        fileName);
        configFile.jaxbMarshal(config, jaxbManager);
    }

    /**
     * {@inheritDoc}
     * 
     * @throws LocalizationException
     */
    @Override
    public Map<DataType, SubscriptionOverlapConfig> readConfig()
            throws LocalizationException {

        HashMap<DataType, SubscriptionOverlapConfig> configs = new HashMap<DataType, SubscriptionOverlapConfig>();

        for (DataType type : DataType.values()) {
            SubscriptionOverlapConfig config = getConfigFile(type);
            if (config != null) {
                configs.put(type, config);
            }
        }

        return configs;
    }

    /**
     * Process a set of Gridded subscriptions for duplication;
     * @param config
     * @param sub1
     * @param sub2
     * @return
     */
    private SubscriptionOverlapResponse processGriddedSubscriptionOverlap(GridSubscriptionOverlapConfig config, Subscription<T, C> sub1, Subscription<T, C> sub2) {
       
        final int parameterDuplicationPercent = duplicateChecker
                .getParameterDuplicationPercent(sub1, sub2);
        final int forecastHourDuplicationPercent = duplicateChecker
                .getForecastHourDuplicationPercent(sub1, sub2);
        final int cycleDuplicationPercent = duplicateChecker
                .getCycleDuplicationPercent(sub1, sub2);
        final int spatialDuplicationPercent = duplicateChecker
                .getSpatialDuplicationPercent(sub1, sub2);

        final boolean overlaps = config.isOverlapping(
                parameterDuplicationPercent, forecastHourDuplicationPercent,
                cycleDuplicationPercent, spatialDuplicationPercent);

        final boolean duplicate = (parameterDuplicationPercent == ONE_HUNDRED_PERCENT)
                && (forecastHourDuplicationPercent == ONE_HUNDRED_PERCENT)
                && (cycleDuplicationPercent == ONE_HUNDRED_PERCENT)
                && (spatialDuplicationPercent == ONE_HUNDRED_PERCENT);

        return new SubscriptionOverlapResponse(duplicate, overlaps);
    }
    
    /***
     * Process a set of Point subscriptions for duplication
     * @param config
     * @param sub1
     * @param sub2
     * @return
     */
    private SubscriptionOverlapResponse processPointSubscriptionOverlap(PointSubscriptionOverlapConfig config, Subscription<T, C> sub1, Subscription<T, C> sub2) {
        
        final int parameterDuplicationPercent = duplicateChecker
                .getParameterDuplicationPercent(sub1, sub2);
        final int timeDuplicationPercent = duplicateChecker
                .getTimeDuplicationPercent(sub1, sub2);
        final int spatialDuplicationPercent = duplicateChecker
                .getSpatialDuplicationPercent(sub1, sub2);

        final boolean overlaps = config.isOverlapping(
                parameterDuplicationPercent, timeDuplicationPercent,
                0, spatialDuplicationPercent);

        final boolean duplicate = (parameterDuplicationPercent == ONE_HUNDRED_PERCENT)
                && (timeDuplicationPercent == ONE_HUNDRED_PERCENT)
                && (spatialDuplicationPercent == ONE_HUNDRED_PERCENT);

        return new SubscriptionOverlapResponse(duplicate, overlaps);
    }
    
    /**
     * Gets the overlap config file by type
     * @param type
     * @return
     */
    private SubscriptionOverlapConfig getConfigFile(DataType type) {
        
        final IPathManager pathManager = PathManagerFactory.getPathManager();
        LocalizationFile localizationFile = null;
        SubscriptionOverlapConfig config = null;
        localizationFile = pathManager
                .getStaticLocalizationFile(SUBSCRIPTION_OVERLAP_CONFIG_FILE_PATH+type.name()+SUBSCRIPTION_OVERLAP_CONFIG_FILE_ROOT);
       
        try {
            if (!localizationFile.exists()) {
                throw new MissingResourceException(localizationFile.getName()
                        + " does not exist.",
                        SubscriptionOverlapConfig.class.getName(), "Not yet implemented!");
            }

            if (type == DataType.GRID) {
                config = localizationFile.jaxbUnmarshal(
                        GridSubscriptionOverlapConfig.class, jaxbManager);

            } else if (type == DataType.POINT) {
                config = localizationFile.jaxbUnmarshal(
                        PointSubscriptionOverlapConfig.class, jaxbManager);

            } 
            
        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM, UNABLE_TO_UNMARSHAL, e.getLocalizedMessage());
            // this a fall back so at least some checking gets done
            if (type == DataType.GRID) {
                config = new GridSubscriptionOverlapConfig().getNeverOverlaps();
            } else if (type == DataType.POINT) {
                config = new PointSubscriptionOverlapConfig().getNeverOverlaps();
            } 
        }
        
        return config;
    }
    
    /**
     * Gets the SubscriptionOverlapResponse by type
     * @param config
     * @param sub1
     * @param sub2
     * @return
     */
    private SubscriptionOverlapResponse getOverlap(SubscriptionOverlapConfig config, Subscription<T, C> sub1, Subscription<T,C> sub2) {
        
        SubscriptionOverlapResponse response = null;
        DataType type = sub1.getDataSetType();
        
        if (type == DataType.GRID) {
            response = processGriddedSubscriptionOverlap(
                    (GridSubscriptionOverlapConfig) config, sub1, sub2);
        } else if (type == DataType.POINT) {
            response = processPointSubscriptionOverlap(
                    (PointSubscriptionOverlapConfig) config, sub1, sub2);
        } else {
            throw new IllegalArgumentException(type
                    + " Config not yet Implemented!");
        }
        
        return response;
    }
}
