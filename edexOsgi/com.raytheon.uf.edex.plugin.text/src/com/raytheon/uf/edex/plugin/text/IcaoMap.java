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
package com.raytheon.uf.edex.plugin.text;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.HashMap;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.raytheon.edex.site.SiteUtil;
import com.raytheon.uf.common.localization.ILocalizationFile;
import com.raytheon.uf.common.localization.ILocalizationPathObserver;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.exception.LocalizationException;
import com.raytheon.uf.common.site.SiteMap;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;

/**
 * This class handles the mapping of a 3 or 4 letter site (station) to an
 * associated ICAO (WFO) ID.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar  7, 2016 4716       rferrel     Initial creation
 * Apr 05, 2016 5434       bkowal      Refactored NDM aspects into a separate
 *                                     plugin. Use localization path observer.
 * 
 * </pre>
 * 
 * @author rferrel
 * @version 1.0
 */

public class IcaoMap implements ILocalizationPathObserver {
    private static final IUFStatusHandler logger = UFStatus
            .getHandler(IcaoMap.class);

    private static final IcaoMap instance = new IcaoMap();

    /**
     * Keeps track of whether or not the read of an existing icao lookup file
     * was successful.
     */
    private volatile boolean initialized = false;

    public static final String ICAO_LOOKUP_FILENAME = "icao_lookup_table.dat";

    public static final String ICAO_LOCALIZATION_PATH = "textdb"
            + IPathManager.SEPARATOR + ICAO_LOOKUP_FILENAME;

    /**
     * Comment is a blank line or line where the first non-space character is #.
     */
    private static final Pattern COMMENT_PATTERN = Pattern
            .compile("^\\s*(#.*)?$");

    /**
     * Line staring with 4 character ICAO ID followed by:
     * <ul>
     * <li>Rest of line is blank ICAO ID maps to itself.
     * <li>A 3 or 4 character site to map the ICAO ID to.
     * </ul>
     */
    private static final Pattern DATA_PATTERN = Pattern
            .compile("^([A-Z][A-Z0-9]{3})(\\s+([A-Z0-9]{3,4}))?\\s*$");

    /**
     * The mapping of sites to ICAO ID.
     */
    private volatile Map<String, String> siteToIcaoMap = new HashMap<>();

    private IcaoMap() {
        final IPathManager pathMgr = PathManagerFactory.getPathManager();
        pathMgr.addLocalizationPathObserver(ICAO_LOCALIZATION_PATH, this);
    }

    /**
     * Clear the map and load the base/configured map file and if it exists the
     * site's ICAO map files.
     * 
     * @throws LocalizationException
     * @throws IOException
     */
    public synchronized void readFiles() throws IOException,
            LocalizationException {

        Map<String, String> newSiteToIcaoMap = new HashMap<>();

        IPathManager pathMgr = PathManagerFactory.getPathManager();
        final LocalizationContext[] lcs = new LocalizationContext[] {
                pathMgr.getContext(LocalizationType.COMMON_STATIC,
                        LocalizationLevel.BASE),
                pathMgr.getContext(LocalizationType.COMMON_STATIC,
                        LocalizationLevel.CONFIGURED) };

        ILocalizationFile file = pathMgr.getStaticLocalizationFile(lcs,
                ICAO_LOCALIZATION_PATH);

        loadIcaoLookupFile(newSiteToIcaoMap, file);

        /*
         * Load site's icao lookup. Can overwrite or add to base/configuration
         * entries.
         */
        LocalizationContext lc = pathMgr.getContext(
                LocalizationType.COMMON_STATIC, LocalizationLevel.SITE);
        file = pathMgr.getLocalizationFile(lc, ICAO_LOCALIZATION_PATH);
        loadIcaoLookupFile(newSiteToIcaoMap, file);
        logger.info("Read " + newSiteToIcaoMap.size() + " ICAO lookup entries.");
        this.siteToIcaoMap = newSiteToIcaoMap;
    }

    /**
     * Load the ICAO map entries form the desired file.
     * 
     * @param file
     * @throws IOException
     * @throws LocalizationException
     */
    private void loadIcaoLookupFile(Map<String, String> map,
            ILocalizationFile file) throws IOException, LocalizationException {
        if ((file != null) && file.exists()) {
            try (InputStream in = file.openInputStream();
                    BufferedReader fis = new BufferedReader(
                            new InputStreamReader(in));) {
                String line = null;
                while ((line = fis.readLine()) != null) {
                    if (!COMMENT_PATTERN.matcher(line).matches()) {
                        Matcher m = DATA_PATTERN.matcher(line);
                        if (m.matches()) {
                            String icao = m.group(1);
                            String key = m.group(3);
                            if (key == null) {
                                key = icao;
                            }

                            String oldIcao = map.get(key);
                            if ((oldIcao != null) && !icao.equals(oldIcao)
                                    && logger.isPriorityEnabled(Priority.DEBUG)) {
                                /*
                                 * This can be the site file overriding the
                                 * base/configuration. This most likely means
                                 * the base/configuration file needs to be
                                 * corrected and the entry removed from the site
                                 * file.
                                 * 
                                 * It may also indicate a file where a given key
                                 * has multiple values.
                                 */
                                logger.debug("For " + key
                                        + " replacing icao id " + oldIcao
                                        + " with " + icao);
                            }
                            map.put(key, icao);
                        } else {
                            logger.warn("Ignoring bad data line in "
                                    + file.getPath() + ": " + line);
                        }
                    }
                }
            }
        }
    }

    /**
     * Get the ICAO ID for the 3 letter site. When the site is not valid 3
     * letter site or has no mapping the cccc will be used to create a default
     * mapping.
     * 
     * @param site
     * @param cccc
     * @return icao
     */
    public static String siteToIcaoId(String site, String cccc) {
        String icao = cccc;

        if (!instance.initialized) {
            instance.initialize();
        }

        if (instance.initialized) {
            boolean valid3LtrSite = site.trim().length() == 3;
            if (valid3LtrSite) {
                icao = instance.siteToIcaoMap.get(SiteMap.getInstance()
                        .getSite4LetterId(site));
                if (icao == null) {
                    icao = cccc;
                }
            }
        }

        /*
         * No cccc generate one based on local site.
         */
        if (icao == null) {
            icao = SiteMap.getInstance().getSite4LetterId(SiteUtil.getSite());
        }

        return icao;
    }

    /**
     * Triggers reading the icao lookup file in localization if it exists. Will
     * catch and handle any exceptions from the reading of the icao lookup file
     * when applicable.
     */
    private synchronized void initialize() {
        logger.info("Reading icao file: " + ICAO_LOOKUP_FILENAME + " ...");
        try {
            readFiles();
            logger.info("Successfully read icao file: " + ICAO_LOOKUP_FILENAME
                    + ".");
            initialized = true;
        } catch (Exception e) {
            logger.error("Failed to read icao file: " + ICAO_LOOKUP_FILENAME
                    + ".", e);
            initialized = false;
        }
    }

    @Override
    public void fileChanged(ILocalizationFile file) {
        instance.initialize();
    }
}