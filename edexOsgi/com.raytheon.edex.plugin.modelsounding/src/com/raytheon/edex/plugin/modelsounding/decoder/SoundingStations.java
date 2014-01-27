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
package com.raytheon.edex.plugin.modelsounding.decoder;

import static com.raytheon.uf.common.localization.LocalizationContext.LocalizationType.EDEX_STATIC;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.HashMap;
import java.util.Map;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;

/**
 * Loads a map from a file for mapping wmo numbers to icaos.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Aug 10, 2009           jkorman     Initial creation
 * Dec 02, 2013  2537     bsteffen    Switch logger to ufstatus.
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */

public class SoundingStations {

    private static final String STATION_LIST = "modelBufrStationList.txt";

    private static final IUFStatusHandler logger = UFStatus
            .getHandler(SoundingStations.class);

    private static Map<String, String> stationMap;

    public SoundingStations(String path) {
        stationMap = new HashMap<String, String>();
        populateMap(path);
    }

    private void populateMap(String path) {

        BufferedReader bf = null;
        try {
            IPathManager manager = PathManagerFactory.getPathManager();

            LocalizationContext baseContext = manager.getContext(EDEX_STATIC,
                    LocalizationLevel.BASE);

            File baseDir = manager.getFile(baseContext, "modelsounding");
            if (baseDir.exists()) {
                File srcFile = new File(baseDir, STATION_LIST);

                InputStream strm = getInputStream(srcFile);
                try {
                    int count = 0;
                    if (strm != null) {
                        bf = new BufferedReader(new InputStreamReader(strm));

                        String line = null;
                        while ((line = bf.readLine()) != null) {
                            if (line != null) {
                                // filter out comments
                                if ((!line.startsWith("#"))
                                        && (line.length() > 22)) {
                                    String wmo = line.substring(0, 13).trim();
                                    String pICAO = line.substring(15, 19)
                                            .trim();
                                    stationMap.put(wmo, pICAO);
                                }
                            }
                        }
                    }
                    logger.debug(String.format("Read %d stationIds from %s.",
                            count, path));
                } catch (IOException ioe) {
                    ioe.printStackTrace();
                } finally {
                    if (bf != null) {
                        try {
                            bf.close();
                        } catch (IOException ioe) {
                            ioe.printStackTrace();
                        }
                    }
                }
            } else {
                logger.error("File " + baseDir.getPath() + " does not exist");
            }

        } catch (Exception e) {
            logger.error("Error reading model sounding station list", e);
        }
    }

    /**
     * 
     * @param path
     * @return
     */
    public static SoundingStations instance(String path) {
        return new SoundingStations(path);
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

    /**
     * 
     * @param file
     * @return
     */
    private static FileInputStream getInputStream(File file) {
        FileInputStream fis = null;

        try {
            fis = new FileInputStream(file);
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        }
        return fis;
    }

}
