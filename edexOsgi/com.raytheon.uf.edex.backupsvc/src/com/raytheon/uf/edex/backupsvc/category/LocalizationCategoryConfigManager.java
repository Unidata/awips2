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
package com.raytheon.uf.edex.backupsvc.category;

import java.io.IOException;
import java.io.InputStream;
import java.util.List;

import jakarta.xml.bind.JAXBException;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.raytheon.uf.common.localization.ILocalizationFile;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.exception.LocalizationException;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.SingleTypeJAXBManager;

/**
 * Manager of the XML configuration file for localization categories.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 23, 2021 96321      Robert.Blum Initial creation
 *
 * </pre>
 *
 * @author Robert.Blum
 */

public class LocalizationCategoryConfigManager {

    private static final String CONFIG_FILE = "localizationcategory"
            + IPathManager.SEPARATOR + "localizationCategories.xml";

    private static final Logger logger = LoggerFactory
            .getLogger(LocalizationCategoryConfigManager.class);

    private static LocalizationCategoryConfigManager INSTANCE;

    private static final Object LOCK_INSTANCE = new Object();

    private SingleTypeJAXBManager<LocalizationCategoryConfig> jaxbManager;

    private volatile LocalizationCategoryConfig config;

    public static LocalizationCategoryConfigManager getInstance() {
        synchronized (LOCK_INSTANCE) {
            if (INSTANCE == null) {
                INSTANCE = new LocalizationCategoryConfigManager();
            }
            return INSTANCE;
        }
    }

    private LocalizationCategoryConfigManager() {
        reload();
        PathManagerFactory.getPathManager()
                .addLocalizationPathObserver(CONFIG_FILE, (file) -> {
                    reload();
                });
    }

    public synchronized void reload() {
        IPathManager pathManager = PathManagerFactory.getPathManager();
        LocalizationContext siteCtx = pathManager.getContext(
                LocalizationType.COMMON_STATIC, LocalizationLevel.SITE);
        ILocalizationFile file = PathManagerFactory.getPathManager()
                .getLocalizationFile(siteCtx, CONFIG_FILE);
        LocalizationCategoryConfig newConfig = null;
        try {
            if (file.exists()) {
                if (jaxbManager == null) {
                    jaxbManager = new SingleTypeJAXBManager<>(true,
                            LocalizationCategoryConfig.class);
                }
                try (InputStream is = file.openInputStream()) {
                    newConfig = jaxbManager.unmarshalFromInputStream(
                            LocalizationCategoryConfig.class, is);
                } catch (IOException e) {
                    logger.debug(
                            "Error on stream close in LocalizationCategoryConfig",
                            e);
                }
            } else {
                // Load the default config
                newConfig = new LocalizationCategoryConfig();
            }
        } catch (JAXBException | SerializationException
                | LocalizationException e) {
            logger.error(
                    "LocalizationCategoryConfig Failed to load settings from "
                            + file.getPath(),
                    e);
            logger.warn(
                    "LocalizationCategoryConfig Falling back to default config with no hosts");
            newConfig = new LocalizationCategoryConfig();
        }
        config = newConfig;
    }

    /**
     * Get the list of categories from the xml configuration.
     *
     * @return
     */
    public List<Category> getCategories() {
        return config.getCategories();
    }

    /**
     * Gets the include file list for the provided category or null if the
     * category doesn't exist.
     *
     * @param category
     * @return
     */
    public List<String> getIncludeListForCategory(String category) {
        for (Category cat : config.getCategories()) {
            if (cat.getCategoryName().equals(category)) {
                return cat.getIncludeFileList();
            }
        }
        return null;
    }
}
