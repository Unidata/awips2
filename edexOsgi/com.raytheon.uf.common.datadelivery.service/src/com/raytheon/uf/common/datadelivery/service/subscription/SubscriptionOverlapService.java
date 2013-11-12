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

import java.io.File;
import java.util.MissingResourceException;

import javax.xml.bind.JAXBException;

import com.raytheon.uf.common.datadelivery.registry.Coverage;
import com.raytheon.uf.common.datadelivery.registry.DataType;
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
import com.raytheon.uf.common.util.FileUtil;

/**
 * Read/Write subscription overlap config files.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 07, 2013  2000      djohnson     Initial creation
 * Jun 04, 2013  223       mpduff       Get base file if site doesn't exist.
 * Sept 23, 2013 2283      dhladky      Updated for multiple configs
 * Oct 03, 2013  2386      mpduff       Moved the subscription overlap rules files into the rules directory.
 * Oct 25, 2013  2292      mpduff       Move overlap checks to edex.
 * Nov 12, 2013  2361      njensen      Made JAXBManager static and initialized on first use
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public class SubscriptionOverlapService<T extends Time, C extends Coverage>
        implements ISubscriptionOverlapService<T, C> {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(SubscriptionOverlapService.class);

    private static final String UNABLE_TO_UNMARSHAL = "Unable to unmarshal the configuration file.  "
            + "No subscriptions will be considered to overlap!";

    private static final String SUBSCRIPTION_OVERLAP_CONFIG_FILE_ROOT = "SubscriptionOverlapRules.xml";

    private static final String SUBSCRIPTION_OVERLAP_CONFIG_FILE_PATH = FileUtil
            .join("datadelivery", "systemManagement", "rules", File.separator);

    private static JAXBManager jaxbManager;

    private static synchronized JAXBManager getJaxbManager() {
        if (jaxbManager == null) {
            try {
                Class<?>[] clazzes = new Class<?>[] {
                        SubscriptionOverlapConfig.class,
                        GridSubscriptionOverlapConfig.class,
                        PointSubscriptionOverlapConfig.class };
                jaxbManager = new JAXBManager(clazzes);
            } catch (JAXBException e) {
                throw new ExceptionInInitializerError(e);
            }
        }
        return jaxbManager;
    }

    /**
     * Constructor.
     * 
     * @param duplicateChecker
     */
    public SubscriptionOverlapService() {

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
            fileName = SUBSCRIPTION_OVERLAP_CONFIG_FILE_PATH
                    + DataType.POINT.name()
                    + SUBSCRIPTION_OVERLAP_CONFIG_FILE_ROOT;
        } else if (config instanceof GridSubscriptionOverlapConfig) {
            fileName = SUBSCRIPTION_OVERLAP_CONFIG_FILE_PATH
                    + DataType.GRID.name()
                    + SUBSCRIPTION_OVERLAP_CONFIG_FILE_ROOT;
        } else {
            throw new IllegalArgumentException(config.getClass()
                    + " Doesn't have any implementation in use");
        }

        final LocalizationFile configFile = pathManager.getLocalizationFile(
                context, fileName);
        configFile.jaxbMarshal(config, getJaxbManager());
    }

    /**
     * Gets the overlap config file by type
     * 
     * @param type
     * @return
     */
    @Override
    public SubscriptionOverlapConfig getConfigFile(DataType type) {

        final IPathManager pathManager = PathManagerFactory.getPathManager();
        LocalizationFile localizationFile = null;
        SubscriptionOverlapConfig config = null;
        localizationFile = pathManager
                .getStaticLocalizationFile(SUBSCRIPTION_OVERLAP_CONFIG_FILE_PATH
                        + type.name() + SUBSCRIPTION_OVERLAP_CONFIG_FILE_ROOT);

        try {
            if (!localizationFile.exists()) {
                throw new MissingResourceException(localizationFile.getName()
                        + " does not exist.",
                        SubscriptionOverlapConfig.class.getName(),
                        "Not yet implemented!");
            }

            if (type == DataType.GRID) {
                config = localizationFile.jaxbUnmarshal(
                        GridSubscriptionOverlapConfig.class, getJaxbManager());

            } else if (type == DataType.POINT) {
                config = localizationFile.jaxbUnmarshal(
                        PointSubscriptionOverlapConfig.class, getJaxbManager());

            }

        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM, UNABLE_TO_UNMARSHAL,
                    e.getLocalizedMessage());
            // this a fall back so at least some checking gets done
            if (type == DataType.GRID) {
                config = new GridSubscriptionOverlapConfig().getNeverOverlaps();
            } else if (type == DataType.POINT) {
                config = new PointSubscriptionOverlapConfig()
                        .getNeverOverlaps();
            }
        }

        return config;
    }

}
