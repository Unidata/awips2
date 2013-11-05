/**
 * Copyright 09/24/12 Raytheon Company.
 *
 * Unlimited Rights
 * This software was developed pursuant to Contract Number 
 * DTFAWA-10-D-00028 with the US Government. The US Government’s rights 
 * in and to this copyrighted software are as specified in DFARS
 * 252.227-7014 which was made part of the above contract. 
 */
package com.raytheon.uf.edex.wcs;

import java.io.File;
import java.io.FileInputStream;
import java.util.Properties;

import org.apache.commons.lang.BooleanUtils;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;

/**
 * WCS configuration
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 9, 2013            bclement     Initial creation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class WcsConfig {

    public static final String FORCE_KM_BBOX = "force.km.altitude";

    private static final IUFStatusHandler log = UFStatus
            .getHandler(WcsConfig.class);

    private static Properties props = null;

    static {
        LocalizationFile confFile = findConfigFile();
        if (confFile == null) {
            log.warn("Unable to find custom id mapping properties");
            props = new Properties();
        } else {
            props = loadConfig(confFile);
        }
    }

    /**
     * Find configuration file in localization
     * 
     * @return null if not found
     */
    private static LocalizationFile findConfigFile() {
        IPathManager pm = PathManagerFactory.getPathManager();
        LocalizationContext[] searchHierarchy = pm
                .getLocalSearchHierarchy(LocalizationType.EDEX_STATIC);

        for (LocalizationContext ctx : searchHierarchy) {
            LocalizationFile localizationFile = pm.getLocalizationFile(ctx,
                    "wcs" + IPathManager.SEPARATOR + "wcsConfig.properties");
            if (localizationFile.exists()) {
                return localizationFile;
            }
        }
        return null;
    }

    /**
     * Load map from config
     * 
     * @param idMapFile
     * @return
     */
    private static Properties loadConfig(LocalizationFile idMapFile) {
        Properties props = new Properties();
        File file = idMapFile.getFile();
        try {
            props.load(new FileInputStream(file));
        } catch (Exception e) {
            log.error("Unable to load WCS config: " + file, e);
        }
        return props;
    }

    /**
     * Get property from config
     * 
     * @param key
     * @return null if property not set
     */
    public static String getProperty(String key) {
        if (props == null) {
            return null;
        }
        return props.getProperty(key);
    }

    /**
     * Return boolean value of property
     * 
     * @param key
     * @return false if property not set
     */
    public static boolean isProperty(String key) {
        if (props == null) {
            return false;
        }
        return BooleanUtils.toBoolean(props.getProperty(key));
    }

}
