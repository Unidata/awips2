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
package com.raytheon.uf.edex.aviation.aag;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.Properties;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.raytheon.uf.common.localization.ILocalizationFile;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationUtil;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.exception.LocalizationException;

/**
 * Provide plain language weather descriptions from a config file
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 12, 2017 6110       tgurney     Initial creation
 *
 * </pre>
 *
 * @author tgurney
 */

public class WxDescriptionConfigFileProvider implements WxDescriptionProvider {

    private final Logger logger = LoggerFactory.getLogger(getClass());

    private static final String CFG_FILE_PATH = LocalizationUtil
            .join("aviation", "aag", "wxDescriptions.properties");

    private volatile Properties wxDescriptions = new Properties();

    public WxDescriptionConfigFileProvider() {
        if (Boolean.getBoolean(AAGForecastGenerator.AAG_ENABLED_PROPERTY)) {
            reloadConfig(CFG_FILE_PATH);
            PathManagerFactory.getPathManager()
                    .addLocalizationPathObserver(CFG_FILE_PATH, (file) -> {
                        reloadConfig(CFG_FILE_PATH);
                    });
        }
    }

    private void reloadConfig(String filePath) {
        Properties newWxDescriptions = new Properties();
        IPathManager pathMgr = PathManagerFactory.getPathManager();
        LocalizationContext[] contexts = pathMgr
                .getLocalSearchHierarchy(LocalizationType.COMMON_STATIC);
        for (LocalizationContext ctx : contexts) {
            ILocalizationFile cfgFile = pathMgr.getLocalizationFile(ctx,
                    CFG_FILE_PATH);
            if (cfgFile.exists()) {
                // incremental override
                try (InputStream is = cfgFile.openInputStream()) {
                    try (BufferedReader in = new BufferedReader(
                            new InputStreamReader(is))) {
                        newWxDescriptions.load(in);
                    }
                } catch (IOException | LocalizationException e) {
                    logger.warn("Failed to read weather descriptions from "
                            + filePath, e);
                }
            }
        }
        wxDescriptions = newWxDescriptions;
    }

    @Override
    public String getWxDescription(String wxShortCode) {
        return wxDescriptions.getProperty(wxShortCode, "unknown weather");
    }

}
