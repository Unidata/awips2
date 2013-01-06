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
package com.raytheon.edex.plugin.bufrua.util;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManager;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;

/**
 * Takes a file for customization for the upper air sites and parses it
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 15, 2010            mnash     Initial creation
 * 
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */

public class RaobSitesInUseUtil {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(RaobSitesInUseUtil.class);

    private static final String RAOB_REGEX = "\\s*(.\\w+)\\s+(.\\w+)\\s+(.*)";

    private static final Pattern raob_pattern = Pattern.compile(RAOB_REGEX,
            Pattern.CASE_INSENSITIVE | Pattern.MULTILINE);

    public static final String UPPER_AIR_CONSTANT = "UPPER_AIR";

    public static final String LOCAL_UPPER_AIR_CONSTANT = "LOCAL_UPPER_AIR";

    private static Map<String, List<UpperAirSite>> siteMap = new HashMap<String, List<UpperAirSite>>();

    private static boolean parsed = false;

    private static String raobSite = "";

    /**
     * Parse the file into necessary elements
     * 
     * @throws IOException
     */
    private static void parseFile(String site) throws IOException {
        if (parsed && raobSite.equals(site)) {
            return;
        }

        parsed = true;
        if (site != null) {
            raobSite = site;
        } else {
            raobSite = "";
        }

        siteMap.clear();
        PathManager pm = (PathManager) PathManagerFactory.getPathManager();

        LocalizationContext context = null;
        if (site != null) {
            context = pm
                    .getContextForSite(LocalizationType.COMMON_STATIC, site);
        } else {
            context = pm.getContext(LocalizationType.COMMON_STATIC,
                    LocalizationLevel.SITE);
        }

        LocalizationFile file = pm.getLocalizationFile(context, "upperair"
                + File.separator + "raobSitesInUse.txt");

        if (!file.exists()) {
            LocalizationContext baseContext = pm.getContext(
                    LocalizationType.COMMON_STATIC, LocalizationLevel.BASE);
            file = pm.getLocalizationFile(baseContext, "upperair"
                    + File.separator + "raobSitesInUse.txt");
            statusHandler
                    .info("Site raobSitesInUse.txt file not configured for "
                            + site + ".  Using the base file.");
        }
        if (file != null) {
            BufferedReader buf = new BufferedReader(new FileReader(
                    file.getFile()));
            System.out.println("temping");
            String temp = buf.readLine();
            temp = buf.readLine();
            String radarType = "";
            List<UpperAirSite> sites = new ArrayList<UpperAirSite>();
            while (temp != null) {
                temp = temp.trim();
                if (temp.startsWith("#")) {
                    sites = new ArrayList<UpperAirSite>();
                    radarType = temp.substring(1, temp.indexOf(" ", 2));
                    siteMap.put(radarType.trim(), sites);
                } else if (!temp.isEmpty()) {
                    Matcher m = raob_pattern.matcher(temp);
                    while (m.find()) {
                        UpperAirSite uaSite = new UpperAirSite();
                        uaSite.setIcao(m.group(1));
                        uaSite.setSiteId(m.group(2));
                        uaSite.setCity(m.group(3));
                        sites.add(uaSite);
                    }
                }
                temp = buf.readLine();
            }
            buf.close();
        }
    }

    /**
     * Use this with the constants to parse the file
     * 
     * @param type
     * @return
     */
    public static List<UpperAirSite> getSite(String site, String type) {
        try {
            parseFile(site);
        } catch (IOException e) {
            e.printStackTrace();
        }
        return siteMap.get(type);
    }

    /**
     * @param parsed
     *            the parsed to set
     */
    public static void setParsed(boolean parsed) {
        RaobSitesInUseUtil.parsed = parsed;
    }
}
