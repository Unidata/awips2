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
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.HashMap;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.raytheon.edex.site.SiteUtil;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.SaveableOutputStream;
import com.raytheon.uf.common.localization.exception.LocalizationException;
import com.raytheon.uf.common.site.SiteMap;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.edex.core.EDEXUtil;
import com.raytheon.uf.edex.core.EdexException;
import com.raytheon.uf.edex.ndm.ingest.INationalDatasetSubscriber;

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
 * 
 * </pre>
 * 
 * @author rferrel
 * @version 1.0
 */

public class IcaoMap implements INationalDatasetSubscriber {
    private static final IUFStatusHandler logger = UFStatus
            .getHandler(IcaoMap.class);

    private static IcaoMap instance;

    /**
     * Flag to prevent flooding the log with error messages when problems
     * reading the mapping files. Should only be reference in synchronized
     * methods.
     */
    private static boolean readError = false;

    private static final String ICAO_LOOKUP_FILENAME = "textdb/icao_lookup_table.dat";

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

    private static String urlName = "jms-generic:topic:AwipsFilesChanged";

    /**
     * The mapping of sites to ICAO ID.
     */
    private volatile Map<String, String> siteToIcaoMap = new HashMap<>();

    /**
     * Get an instance.
     * 
     * @return the instance or null when error reading mapping files.
     */
    public synchronized static IcaoMap getInstance(String urlName) {
        if (instance == null) {
            if (urlName != null) {
                IcaoMap.urlName = urlName;
            }
            try {
                instance = new IcaoMap();
            } catch (IOException | LocalizationException e) {
                if (!readError) {
                    logger.handle(Priority.PROBLEM,
                            "Unable to read icao mapping files:", e);
                    readError = true;
                }
                instance = null;
            }
        }

        if (readError && (instance != null)) {
            logger.handle(Priority.INFO, "Now able to read icao mapping files.");
            readError = false;
        }
        return instance;
    }

    private IcaoMap() throws IOException, LocalizationException {
        readFiles();
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
        LocalizationContext[] lcs = new LocalizationContext[2];
        lcs[0] = pathMgr.getContext(LocalizationType.COMMON_STATIC,
                LocalizationLevel.BASE);
        lcs[1] = pathMgr.getContext(LocalizationType.COMMON_STATIC,
                LocalizationLevel.CONFIGURED);

        LocalizationFile file = pathMgr.getStaticLocalizationFile(lcs,
                ICAO_LOOKUP_FILENAME);

        loadIcaoLookupFile(newSiteToIcaoMap, file);

        /*
         * Load site's icao lookup. Can overwrite or add to base/configuration
         * entries.
         */
        LocalizationContext lc = pathMgr.getContext(
                LocalizationType.COMMON_STATIC, LocalizationLevel.SITE);
        file = pathMgr.getLocalizationFile(lc, ICAO_LOOKUP_FILENAME);
        loadIcaoLookupFile(newSiteToIcaoMap, file);
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
            LocalizationFile file) throws IOException, LocalizationException {
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
        String icao = null;

        if (instance == null) {
            IcaoMap.getInstance(IcaoMap.urlName);
        }

        if (instance != null) {
            boolean valid3LtrSite = site.trim().length() == 3;
            if (valid3LtrSite) {
                icao = instance.siteToIcaoMap.get(SiteMap.getInstance()
                        .getSite4LetterId(site));
                if (icao == null) {
                    icao = cccc;
                }
            } else {
                icao = cccc;
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
     * This copies a file to CONFIGURED directory and performs a readFile.
     * 
     * @param file
     * @return true when file successfully copied and parsed file
     */
    public synchronized boolean copyToConfiguration(File file) {
        IPathManager pathMgr = PathManagerFactory.getPathManager();
        LocalizationContext lc = pathMgr.getContext(
                LocalizationType.COMMON_STATIC, LocalizationLevel.CONFIGURED);
        LocalizationFile destFile = pathMgr.getLocalizationFile(lc,
                ICAO_LOOKUP_FILENAME);

        boolean status = true;

        try (SaveableOutputStream out = destFile.openOutputStream()) {
            Path inPath = Paths.get(file.getAbsolutePath());
            Files.copy(inPath, out);
            out.save();
            EDEXUtil.getMessageProducer().sendAsyncUri(this.urlName, "");
        } catch (IOException | LocalizationException e) {
            status = false;
            logger.error("Unable to create " + destFile.getPath(), e);
        } catch (EdexException e) {
            logger.error("Unable to trigger reading new file:", e);
        }

        return status;
    }

    @Override
    public void notify(String fileName, File file) {
        if ("icao_lookup_table.dat".equals(fileName)) {
            if (!copyToConfiguration(file)) {
                logger.handle(Priority.PROBLEM,
                        "Could not copy file: " + file.getName());
            }
        } else {
            logger.warn("Ignoring ndm file: " + fileName);
        }
    }
}
