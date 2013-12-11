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
package com.raytheon.uf.common.dataplugin.warning.portions;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.raytheon.uf.common.dataplugin.warning.portions.GisUtil.Direction;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;

/**
 * Creates a map of all the site's area suppress files.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 2, 2010            jsanchez     Initial creation
 * Aug 15,2013  2177      jsanchez     Refactored.
 * Dec  4,2013  2604      jsanchez     Moved out of viz.warngen.
 * 
 * </pre>
 * 
 * @author jsanchez
 * @version 1.0
 */

public class SuppressMap {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(SuppressMap.class);

    private static SuppressMap instance = null;

    private static final String AREA_SUPPRESS_FILENAME = "warngen/area.suppress";

    private static final Pattern ugcPattern = Pattern
            .compile("[A-Z]{2}[CZ][0-9]{3}");

    private static final List<Direction> NORTH_SOUTH = Arrays.asList(
            Direction.NORTH, Direction.SOUTH);

    private static final List<Direction> EAST_WEST = Arrays.asList(
            Direction.EAST, Direction.WEST);

    private static final List<Direction> ALL = Arrays.asList(Direction.NORTH,
            Direction.SOUTH, Direction.EAST, Direction.WEST, Direction.CENTRAL,
            Direction.EXTREME);

    private static Map<String, Map<String, List<Direction>>> suppressMap = new HashMap<String, Map<String, List<Direction>>>();

    private SuppressMap() {

    }

    /**
     * 
     * @return an instance of the SuppressMap object.
     */
    public static SuppressMap getInstance() {
        if (instance == null) {
            instance = new SuppressMap();
        }
        return instance;
    }

    /**
     * Returns the suppressed area map for the current localized site.
     * 
     * @return
     */
    public Map<String, List<Direction>> getAreas(String threeLetterSiteID) {
        Map<String, List<Direction>> areas = suppressMap.get(threeLetterSiteID);

        if (areas == null) {
            areas = new HashMap<String, List<Direction>>();
            IPathManager pm = PathManagerFactory.getPathManager();
            LocalizationContext lc = pm.getContext(
                    LocalizationType.COMMON_STATIC, LocalizationLevel.SITE);
            File file = pm.getFile(lc, AREA_SUPPRESS_FILENAME);
            loadFile(file, areas);
        }

        return areas;
    }

    /**
     * Loads the areas map with the suppress information in the file
     * 
     * @param file
     * @param areas
     */
    private void loadFile(File file, Map<String, List<Direction>> areas) {
        if ((file != null) && file.exists()) {
            Matcher m = null;
            BufferedReader fis = null;
            try {
                fis = new BufferedReader(new InputStreamReader(
                        new FileInputStream(file)));

                String line = null;
                try {
                    while ((line = fis.readLine()) != null) {
                        m = ugcPattern.matcher(line);
                        if (m.find()) {
                            List<Direction> suppressedDirections = new ArrayList<Direction>();
                            String ugc = m.group();
                            if (line.indexOf("ns") > 5) {
                                suppressedDirections = NORTH_SOUTH;
                            } else if (line.indexOf("ew") > 5) {
                                suppressedDirections = EAST_WEST;
                            } else {
                                suppressedDirections = ALL;
                            }
                            areas.put(ugc, suppressedDirections);
                        }
                    }
                } catch (IOException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Could not read counties file: "
                                    + AREA_SUPPRESS_FILENAME, e);

                }
            } catch (FileNotFoundException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Failed to find counties file: "
                                + AREA_SUPPRESS_FILENAME, e);

            } finally {

                if (fis != null) {
                    try {
                        fis.close();
                    } catch (IOException e) {
                        statusHandler.handle(Priority.PROBLEM,
                                "Error trying to close buffered reader ", e);
                    }

                }
            }
        }
    }
}
