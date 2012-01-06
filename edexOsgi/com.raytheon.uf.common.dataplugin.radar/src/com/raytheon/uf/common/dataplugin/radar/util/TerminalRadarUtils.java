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
package com.raytheon.uf.common.dataplugin.radar.util;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.PathManagerFactory;

/**
 * Gives the terminal radar menus different elevations
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 24, 2010            mnash     Initial creation
 * 
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */

public class TerminalRadarUtils {

    private static Map<String, List<Double>> radarElevations;

    public static boolean isTerminalRadar(String icao) {
        if (radarElevations == null) {
            parseTerminalRadarFile();
        }
        return radarElevations.keySet().contains(icao.toLowerCase());
    }

    /**
     * Parses the file and puts into a hashmap
     * 
     * @return
     * @throws IOException
     */
    public static Map<String, List<Double>> parseTerminalRadarFile() {
        if (radarElevations == null) {
            radarElevations = new HashMap<String, List<Double>>();
        }
        File file = getElevationsFile();
        radarElevations = new HashMap<String, List<Double>>();
        String line = "";
        BufferedReader reader = null;
        try {
            reader = new BufferedReader(new FileReader(file));
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        }
        try {
            line = reader.readLine();
            Map<String, Set<Double>> mapSet = new HashMap<String, Set<Double>>();
            Map<Double, Double> theMap = new HashMap<Double, Double>();

            while (line != null) {
                theMap.clear();
                if (line.startsWith("#")) {
                    line = reader.readLine();
                    continue;
                } else {
                    Set<Double> tempSet = new HashSet<Double>();
                    String[] lineVals = line.split("\\s+");

                    for (int i = 0; i < lineVals.length - 4; i++) {
                        tempSet.add(Double.parseDouble(lineVals[i + 4]));
                    }
                    mapSet.put(lineVals[2], tempSet);

                    lineVals = reader.readLine().split("\\s+");

                    tempSet = new HashSet<Double>();
                    for (int i = 0; i < lineVals.length - 4; i++) {
                        tempSet.add(Double.parseDouble(lineVals[i + 4]));
                    }
                    mapSet.put(lineVals[2], tempSet);
                    for (Double x : mapSet.get("MON")) {
                        theMap.put(TiltAngleBin.getPrimaryElevationAngle(x), x);
                    }

                    for (Double y : mapSet.get("HAZ")) {
                        theMap.put(TiltAngleBin.getPrimaryElevationAngle(y), y);
                    }
                    // add the base case if necessary
                    if (!theMap.containsKey(0.0)) {
                        theMap.put(0.0, 0.0);
                    }
                    List<Double> list = new ArrayList<Double>(theMap.values());
                    Collections.sort(list);
                    radarElevations.put("t" + lineVals[0].toLowerCase(), list);
                    line = reader.readLine();
                }
            }
            reader.close();
        } catch (IOException e) {
            e.printStackTrace();
        }
        return radarElevations;
    }

    public static File getElevationsFile() {
        IPathManager pathMgr = PathManagerFactory.getPathManager();
        LocalizationContext context = pathMgr.getContext(
                LocalizationType.COMMON_STATIC, LocalizationLevel.BASE);
        return pathMgr.getLocalizationFile(context,
                "radar" + File.separator + "tdwrElevations.txt").getFile();
    }

    public static Map<Double, Double> getPrimarysMap(String site) {
        Map<Double, Double> map = new HashMap<Double, Double>();
        if (radarElevations == null) {
            radarElevations = parseTerminalRadarFile();
        }
        for (Double elev : radarElevations.get(site)) {
            map.put(TiltAngleBin.getPrimaryElevationAngle(elev), elev);
        }
        return map;
    }

    public static void main(String[] args) {
        parseTerminalRadarFile();
    }
}
