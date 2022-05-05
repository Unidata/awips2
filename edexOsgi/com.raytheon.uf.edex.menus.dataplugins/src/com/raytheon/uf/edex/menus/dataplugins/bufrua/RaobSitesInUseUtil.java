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
package com.raytheon.uf.edex.menus.dataplugins.bufrua;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.raytheon.uf.common.localization.ILocalizationFile;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.exception.LocalizationException;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;

/**
 * Takes a file for customization for the upper air sites and parses it
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- -------------------------------------
 * Jul 15, 2010  5571     mnash     Initial creation
 * Feb 16, 2016  5237     bsteffen  Replace deprecated localization API.
 * Apr 08, 2016  5435     bsteffen  Move to menus plugin.
 * 
 * </pre>
 * 
 * @author mnash
 */
public class RaobSitesInUseUtil {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(RaobSitesInUseUtil.class);

    private static final String RAOB_REGEX = "\\s*(.\\w+)\\s+(.\\w+)\\s+(.*)";

    private static final Pattern raob_pattern = Pattern.compile(RAOB_REGEX,
            Pattern.CASE_INSENSITIVE | Pattern.MULTILINE);

    public static final String UPPER_AIR_CONSTANT = "UPPER_AIR";

    public static final String LOCAL_UPPER_AIR_CONSTANT = "LOCAL_UPPER_AIR";

    private static Map<String, List<UpperAirSite>> siteMap = new HashMap<>();

    private static boolean parsed = false;

    private static String raobSite = "";

    /**
     * Parse the file into necessary elements
     * 
     * @throws IOException
     */
    private static void parseFile(String site) {
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
        IPathManager pm = PathManagerFactory.getPathManager();

        LocalizationContext context = null;
        if (site != null) {
            context = pm
                    .getContextForSite(LocalizationType.COMMON_STATIC, site);
        } else {
            context = pm.getContext(LocalizationType.COMMON_STATIC,
                    LocalizationLevel.SITE);
        }

        ILocalizationFile file = pm.getLocalizationFile(context, "upperair"
                + File.separator + "raobSitesInUse.txt");

        if (file == null || !file.exists()) {
            LocalizationContext baseContext = pm.getContext(
                    LocalizationType.COMMON_STATIC, LocalizationLevel.BASE);
            file = pm.getLocalizationFile(baseContext, "upperair"
                    + File.separator + "raobSitesInUse.txt");
            statusHandler
                    .info("Site raobSitesInUse.txt file not configured for "
                            + site + ".  Using the base file.");
        }
        if (file != null && file.exists()) {
            try (BufferedReader buf = new BufferedReader(new InputStreamReader(
                    file.openInputStream()))) {
                String temp = buf.readLine();
                temp = buf.readLine();
                String radarType = "";
                List<UpperAirSite> sites = new ArrayList<>();
                while (temp != null) {
                    temp = temp.trim();
                    if (temp.startsWith("#")) {
                        sites = new ArrayList<>();
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
            } catch (IOException | LocalizationException e) {
                statusHandler.error("Error reading " + file.getPath(), e);
            }
        }
    }

    /**
     * Use this with the constants to parse the file
     * 
     * @param type
     * @return
     */
    public static List<UpperAirSite> getSite(String site, String type) {
        parseFile(site);
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
