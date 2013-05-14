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

import java.util.MissingResourceException;

import javax.xml.bind.JAXBException;

import com.google.common.annotations.VisibleForTesting;
import com.raytheon.uf.common.datadelivery.registry.Subscription;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.serialization.JAXBManager;
import com.raytheon.uf.common.serialization.SerializationException;
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
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public class SubscriptionOverlapService implements ISubscriptionOverlapService {

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

    private static final String SUBSCRIPTION_OVERLAP_CONFIG_FILE_PATH = "datadelivery/subscriptionOverlapRules.xml";

    private final ISubscriptionDuplicateChecker duplicateChecker;

    private final JAXBManager jaxbManager;

    /**
     * Constructor.
     * 
     * @param duplicateChecker
     */
    public SubscriptionOverlapService(
            ISubscriptionDuplicateChecker duplicateChecker) {
        this.duplicateChecker = duplicateChecker;

        try {
            jaxbManager = new JAXBManager(SubscriptionOverlapConfig.class);
        } catch (JAXBException e) {
            throw new ExceptionInInitializerError(e);
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ISubscriptionOverlapResponse isOverlapping(Subscription sub1,
            Subscription sub2) {
        // Ignore requests to compare with itself
        if (sub1.getName().equals(sub2.getName())) {
            return new SubscriptionOverlapResponse(false, false);
        }

        final IPathManager pathManager = PathManagerFactory.getPathManager();

        final LocalizationFile localizationFile = pathManager
                .getStaticLocalizationFile(SUBSCRIPTION_OVERLAP_CONFIG_FILE_PATH);

        SubscriptionOverlapConfig config;
        try {
            if (!localizationFile.exists()) {
                throw new MissingResourceException(localizationFile.getName()
                        + " does not exist.",
                        SubscriptionOverlapConfig.class.getName(), "");
            }
            config = localizationFile.jaxbUnmarshal(
                    SubscriptionOverlapConfig.class, jaxbManager);
        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM, UNABLE_TO_UNMARSHAL, e);
            config = SubscriptionOverlapConfig.NEVER_OVERLAPS;
        }

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

    /**
     * Writes the new configuration.
     * 
     * @param config
     *            the configuration
     * @throws SerializationException
     *             on error serializing the configuration
     */
    @VisibleForTesting
    void writeNewConfig(SubscriptionOverlapConfig config)
            throws SerializationException {
        final LocalizationFile configFile = PathManagerFactory
                .getPathManager()
                .getStaticLocalizationFile(
                        SubscriptionOverlapService.SUBSCRIPTION_OVERLAP_CONFIG_FILE_PATH);
        this.jaxbManager.jaxbMarshalToXmlFile(config, configFile.getFile()
                .getAbsolutePath());
    }

}
