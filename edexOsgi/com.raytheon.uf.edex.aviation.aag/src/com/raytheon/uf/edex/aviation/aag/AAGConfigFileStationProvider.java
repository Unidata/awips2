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
import java.util.ArrayList;
import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.raytheon.uf.common.localization.ILocalizationFile;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationUtil;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.exception.LocalizationException;

/**
 * Provide a list of AAG stations from config file
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 11, 2017 6110       tgurney     Initial creation
 *
 * </pre>
 *
 * @author tgurney
 */

public class AAGConfigFileStationProvider implements AAGStationProvider {

    private final Logger logger = LoggerFactory.getLogger(getClass());

    private static final String CFG_FILE_PATH = LocalizationUtil
            .join("aviation", "aag", "aagStations.txt");

    private volatile List<String> stationIds = new ArrayList<>();

    public AAGConfigFileStationProvider() {
        if (Boolean.getBoolean(AAGForecastGenerator.AAG_ENABLED_PROPERTY)) {
            reloadConfig(CFG_FILE_PATH);
            PathManagerFactory.getPathManager()
                    .addLocalizationPathObserver(CFG_FILE_PATH, (file) -> {
                        reloadConfig(CFG_FILE_PATH);
                    });
        }
    }

    private void reloadConfig(String filePath) {
        List<String> newStationIds = new ArrayList<>();
        IPathManager pathMgr = PathManagerFactory.getPathManager();
        ILocalizationFile cfgFile = pathMgr.getStaticLocalizationFile(filePath);
        if (cfgFile == null) {
            // This only happens if the BASE file is missing
            String fmt = "AAG config file %s is missing. "
                    + "No AAG products will be created";
            logger.warn(String.format(fmt, filePath));
            stationIds = newStationIds;
            return;
        }
        try (InputStream is = cfgFile.openInputStream()) {
            try (BufferedReader in = new BufferedReader(
                    new InputStreamReader(is))) {
                String line = null;
                while ((line = in.readLine()) != null) {
                    line = line.trim();
                    if (line.isEmpty() || line.startsWith("#")) {
                        continue;
                    }
                    newStationIds.add(line.toUpperCase());
                }
            }
            stationIds = newStationIds;
            String fmt = "Loaded %d station IDs from %s";
            logger.info(String.format(fmt, stationIds.size(), filePath));
        } catch (IOException | LocalizationException e) {
            logger.warn("Failed to read station IDs from " + filePath, e);
        }

    }

    @Override
    public List<String> getStationIds() {
        return stationIds;
    }

}
