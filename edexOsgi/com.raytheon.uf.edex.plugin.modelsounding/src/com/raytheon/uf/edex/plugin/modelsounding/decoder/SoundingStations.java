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
package com.raytheon.uf.edex.plugin.modelsounding.decoder;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.HashMap;
import java.util.Map;

import com.raytheon.uf.common.localization.ILocalizationFile;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;

/**
 * Loads a map from a file (modelBufrStationList.txt) for mapping wmo numbers to
 * icaos.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Aug 10, 2009           jkorman   Initial creation
 * Dec 02, 2013  2537     bsteffen  Switch logger to ufstatus.
 * Jul 08, 2016  5744     mapeters  Config file moved from edex_static to
 *                                  common_static, cleanup
 * 
 * </pre>
 * 
 * @author jkorman
 */

public class SoundingStations {

    private static final String STATION_LIST = ModelSoundingDataAdapter.MODEL_STATION_LIST;

    private static final IUFStatusHandler logger = UFStatus
            .getHandler(SoundingStations.class);

    private static Map<String, String> stationMap;

    public SoundingStations() {
        stationMap = new HashMap<>();
        populateMap();
    }

    private void populateMap() {
        try {
            IPathManager pm = PathManagerFactory.getPathManager();
            String fullLocPath = "modelsounding" + IPathManager.SEPARATOR
                    + STATION_LIST;

            /*
             * Check SITE first, as the updated config file is stored there when
             * NDM files are ingested. If not found, use default BASE file.
             */
            LocalizationContext context = pm.getContext(
                    LocalizationType.COMMON_STATIC, LocalizationLevel.SITE);
            ILocalizationFile srcFile = pm.getLocalizationFile(context,
                    fullLocPath);
            if (srcFile == null || !srcFile.exists()) {
                context = pm.getContext(LocalizationType.COMMON_STATIC,
                        LocalizationLevel.BASE);
                srcFile = pm.getLocalizationFile(context, fullLocPath);
            }
            if (srcFile != null && srcFile.exists()) {
                try (BufferedReader bf = new BufferedReader(
                        new InputStreamReader(srcFile.openInputStream()))) {
                    int count = 0;
                    String line = null;
                    while ((line = bf.readLine()) != null) {
                        // filter out comments
                        if ((!line.startsWith("#")) && (line.length() > 22)) {
                            String wmo = line.substring(0, 13).trim();
                            String pICAO = line.substring(15, 19).trim();
                            stationMap.put(wmo, pICAO);
                            ++count;
                        }
                    }
                    logger.debug(String.format("Read %d stationIds from %s.",
                            count, STATION_LIST));
                } catch (IOException ioe) {
                    logger.handle(Priority.PROBLEM, ioe.getLocalizedMessage(),
                            ioe);
                }
            } else {
                logger.error("Localization file " + fullLocPath
                        + " does not exist");
            }

        } catch (Exception e) {
            logger.error("Error reading model sounding station list", e);
        }
    }

    /**
     * Map a modelsounding wmoId to a pseudoICAO identifier.
     * 
     * @param stationId
     * @return
     */
    public String mapId(String stationId) {
        return stationMap.get(stationId);
    }
}
