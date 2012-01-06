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
package com.raytheon.viz.warngen.suppress;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.HashMap;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 2, 2010            jsanchez     Initial creation
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

    public static final String NORTH_SOUTH = "NS";

    public static final String EAST_WEST = "EW";

    public static final String NONE = "NONE";

    public static final String ALL = "ALL";

    private Map<String, String> suppressMap = new HashMap<String, String>();

    public static SuppressMap getInstance() {
        if (instance == null) {
            instance = new SuppressMap();
        }
        return instance;
    }

    private SuppressMap() {
        readFile();
    }

    private void readFile() {
        // load site
        IPathManager pathMgr = PathManagerFactory.getPathManager();
        LocalizationContext lc = pathMgr.getContext(
                LocalizationType.COMMON_STATIC, LocalizationLevel.SITE);
        File file = pathMgr.getFile(lc, AREA_SUPPRESS_FILENAME);
        loadFile(file, suppressMap);
    }

    private void loadFile(File file, Map<String, String> aliasMap) {
        if ((file != null) && file.exists()) {
            Matcher m = null;
            try {
                BufferedReader fis = new BufferedReader(new InputStreamReader(
                        new FileInputStream(file)));
                String line = null;
                try {
                    while ((line = fis.readLine()) != null) {
                        m = ugcPattern.matcher(line);
                        if (m.find()) {
                            String dataKey = m.group();
                            String data = ALL;
                            if (line.indexOf("ns") > 5) {
                                data = NORTH_SOUTH;
                            } else if (line.indexOf("ew") > 5) {
                                data = EAST_WEST;
                            }
                            aliasMap.put(dataKey, data);
                        }
                    }
                } catch (IOException e) {
                    // TODO Auto-generated catch block. Please revise as
                    // appropriate.
                    statusHandler.handle(Priority.PROBLEM,
                            "Could not read counties file: "
                                    + AREA_SUPPRESS_FILENAME, e);

                }
            } catch (FileNotFoundException e) {
                // TODO Auto-generated catch block. Please revise as
                // appropriate.
                statusHandler.handle(Priority.PROBLEM,
                        "Failed to find counties file: "
                                + AREA_SUPPRESS_FILENAME, e);

            }
        }
    }

    public String getType(String key) {
        if (suppressMap.containsKey(key)) {
            return suppressMap.get(key);
        } else {
            return NONE;
        }
    }
}
