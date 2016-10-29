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
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManager;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.exception.LocalizationException;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;

/**
 * Provides easy interface to grab configured sites for radar and various other
 * things
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 14, 2010            mnash       Initial creation
 * Feb 15, 2016 5244       nabowle     Replace deprecated LocalizationFile methods.
 * 
 *
 * </pre>
 *
 * @author mnash
 * @version 1.0
 */

public class RadarsInUseUtil {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(RadarsInUseUtil.class);

    public static final String LOCAL_CONSTANT = "LOCAL_RADARS";

    public static final String ASR_CONSTANT = "ASR_RADARS";

    public static final String ARSR_CONSTANT = "ARSR_RADARS";

    public static final String MOSAIC_CONSTANT = "MOSAIC_RADARS";

    private static final IUFStatusHandler handler = UFStatus
            .getHandler(RadarsInUseUtil.class);

    private static Map<String, List<String>> siteMap = new HashMap<String, List<String>>();

    private static boolean parsed = false;

    // hold the currently parsed site
    private static String radarSite = "";

    private static synchronized void parseFile(String site) throws IOException {
        PathManager pm = (PathManager) PathManagerFactory.getPathManager();
        LocalizationContext context = null;

        // if the site isn't given, default to the regular site
        if (site == null) {
            context = pm.getContext(LocalizationType.COMMON_STATIC,
                    LocalizationLevel.SITE);
            site = context.getContextName();
        } else {
            context = pm
                    .getContextForSite(LocalizationType.COMMON_STATIC, site);
        }

        // if site (maybe default, maybe not) is equal to the last site parsed
        if (parsed && radarSite.equals(site)) {
            return;
        }

        // set radarSite to the site being parsed at this time
        radarSite = context.getContextName();
        parsed = true;
        siteMap.clear();

        LocalizationFile file = pm.getLocalizationFile(context, "radar"
                + File.separator + "radarsInUse.txt");

        if (!file.exists()) {
            LocalizationContext baseContext = pm.getContext(
                    LocalizationType.COMMON_STATIC, LocalizationLevel.BASE);
            file = pm.getLocalizationFile(baseContext, "radar" + File.separator
                    + "radarsInUse.txt");
            statusHandler.info("Site radarsInUse.txt file not configured for "
                    + radarSite + ".  Using the base file.");
        }
        if (file != null) {
            try (BufferedReader buf = new BufferedReader(new InputStreamReader(
                    file.openInputStream()))) {
                String temp = buf.readLine();
                temp = buf.readLine();
                String radarType = "";
                List<String> sites = new ArrayList<String>();
                while (temp != null) {
                    temp = temp.trim();
                    if (temp.startsWith("#")) {
                        sites = new ArrayList<String>();
                        radarType = temp.substring(1, temp.indexOf(" ", 2));
                        siteMap.put(radarType.trim(), sites);
                    } else if (!temp.trim().isEmpty()) {
                        sites.add(temp);
                    }
                    temp = buf.readLine();
                }
            } catch (LocalizationException e) {
                throw new IOException("Error while reading " + file.getPath(),
                        e);
            }
        }
    }

    /**
     * @param type
     * @return
     */
    public static List<String> getSite(String site, String type) {
        try {
            parseFile(site);
        } catch (IOException e) {
            handler.handle(Priority.ERROR,
                    "Error occurred looking up site radars", e);
        }
        List<String> siteList = new ArrayList<String>(siteMap.get(type));
        return siteList;
    }

    public static List<String> getAll(String site) {
        try {
            parseFile(site);
        } catch (IOException e) {
            handler.handle(Priority.ERROR, "Error occurred looking up radars",
                    e);
        }
        List<String> returnList = new ArrayList<String>();
        Set<String> setList = new HashSet<String>();
        for (List<String> list : siteMap.values()) {
            for (int i = 0; i < list.size(); i++) {
                setList.add(list.get(i));
            }
        }
        returnList.addAll(setList);
        return returnList;
    }

    public static void setParsed(boolean isParsed) {
        parsed = isParsed;
    }
}
