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
package com.raytheon.uf.common.site;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Unmarshaller;

import org.apache.commons.lang3.StringUtils;

import com.raytheon.uf.common.localization.ILocalizationFile;
import com.raytheon.uf.common.localization.ILocalizationPathObserver;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationUtil;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.exception.LocalizationException;
import com.raytheon.uf.common.site.xml.NwsSitesXML;
import com.raytheon.uf.common.site.xml.SiteIdXML;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;

/**
 * Contains several lookup tables for mapping ICAO and site IDs to various
 * values
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer        Description
 * ------------- -------- --------------- --------------------------------------
 * Jul 16, 2010           bfarmer         Initial creation
 * Apr 09, 2012  14765    mhuang          Map out correct CCCC site ID for
 *                                        backup sites.
 * May 15, 2013  1040     mpduff          Add awips_site_list.xml.
 * Mar 18, 2014  17173    D. Friedman     Re-implement DR 14765.
 * Apr 06, 2017  19619    MPorricelli     Have all edex servers made aware of
 *                                        ndm textdb file change
 * Jan 26, 2018  6863     dgilling        Allow site-level overrides to
 *                                        national_category_table.template,
 *                                        cleanup localization code.
 * Feb 07, 2019  7730     randerso        Moved files into textdb/config for
 *                                        localization perspective. Improved
 *                                        parsing to allow comments and be more
 *                                        forgiving of white space. Made
 *                                        overrides incremental.
 * Feb 11, 2019  15552    ryu             fix AlertViz error for TextWS
 * Jun 24, 2019  7864     randerso        Allow overrides to remove or
 *                                        conditionally add entries. Fixed
 *                                        getAFOSTableMap to return empty string
 *                                        if ccc not found.
 * Mar 23, 2020  21048    mgamazaychikov  Reduce duplicate metars in textDB
 * Apr 28, 2020  8151     randerso        Fix NullPointerException introduced by
 *                                        DCS 21048
 *
 * </pre>
 *
 * @author bfarmer
 */

public class SiteMap implements ILocalizationPathObserver {
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(SiteMap.class);

    private static final String TEXTDB_CONFIG_DIR = LocalizationUtil
            .join("textdb", "config");

    public static final String AFOS_LOOKUP_FILENAME = LocalizationUtil
            .join(TEXTDB_CONFIG_DIR, "afos_lookup_table.dat");

    public static final String NATIONAL_CATEGORY_TABLE_FILENAME = LocalizationUtil
            .join(TEXTDB_CONFIG_DIR, "national_category_table.template");

    private static final String SITE_OVERRIDE_FILENAME = LocalizationUtil
            .join(TEXTDB_CONFIG_DIR, "site3LetterTo4LetterOverride.dat");

    private static final String RFC_TABLE_FILENAME = LocalizationUtil
            .join(TEXTDB_CONFIG_DIR, "rfc_lookup_table.dat");

    private static final String DUPLICATE_NNN_FILENAME = LocalizationUtil
            .join(TEXTDB_CONFIG_DIR, "duplicate_nnn_table.dat");

    private static final String LOCATION_ID_FILENAME = "awips_site_list.xml";

    private static final String COMMENT_DELIMITERS = "#!";

    private static SiteMap instance = new SiteMap();

    private static class ParsedResult {
        enum Action {
            OVERRIDE(null), ADD('+'), REMOVE('-');

            private Character symbol;

            private Action(Character symbol) {
                this.symbol = symbol;
            }

            /**
             * Get action from symbol
             *
             * @param c
             * @return action
             */
            public static Action fromSymbol(char c) {
                for (Action action : values()) {
                    if (action.symbol == null) {
                        continue;
                    }

                    if (action.symbol.equals(c)) {
                        return action;
                    }
                }

                return OVERRIDE;
            }
        }

        private String[] tokens;

        private Action action;

        public ParsedResult() {
            tokens = null;
            action = Action.OVERRIDE;
        }
    }

    private final Set<String> rfcList = new HashSet<>();

    private final Map<String, String> siteToSiteMap = new HashMap<>();

    private final Map<String, String> nationalCategoryMap = new HashMap<>();

    private final Map<String, String> siteTo4LetterSite = new HashMap<>();

    private final Map<String, Set<String>> siteTo3LetterSite = new HashMap<>();

    private final Set<String> site3to4LetterOverrides = new HashSet<>();

    private final Map<String, SiteData> siteMap = new TreeMap<>();

    private Set<String> duplicateNNNSet;

    /** JAXB context */
    private JAXBContext jax;

    /** Unmarshaller object */
    private Unmarshaller unmarshaller;

    /**
     * Get an instance.
     *
     * @return the instance
     */
    public static SiteMap getInstance() {
        return instance;
    }

    private SiteMap() {
        try {
            jax = JAXBContext.newInstance(NwsSitesXML.class, SiteIdXML.class);
            this.unmarshaller = jax.createUnmarshaller();
        } catch (JAXBException e) {
            statusHandler.error("Error creating context for SiteMap", e);
            throw new ExceptionInInitializerError(
                    "Error creating context for SiteMap");
        }
        readFiles();

        PathManagerFactory.getPathManager()
                .addLocalizationPathObserver(TEXTDB_CONFIG_DIR, this);
    }

    /**
     * @param xxx
     * @return ccc
     */
    public synchronized String getCCCFromXXXCode(String xxx) {
        if (nationalCategoryMap.isEmpty() || siteToSiteMap.isEmpty()) {
            readFiles();
        }
        String retval = null;
        if (xxx != null) {
            if (xxx.length() == 3) {
                retval = siteToSiteMap.get(getSite4LetterId(xxx));
                if (retval == null) {
                    retval = nationalCategoryMap.get(getSite4LetterId(xxx));
                }
            } else if (xxx.length() == 4) {
                retval = siteToSiteMap.get(xxx);
                if (retval == null) {
                    retval = nationalCategoryMap.get(xxx);
                }
            }
        }

        return retval;
    }

    /**
     * Attempt to map a station id (icao?) to a cccid. Use the
     * afos_lookup_table.dat data only.
     *
     * @param xxx
     *            An id to map.
     * @return ccc or empty string if not found
     */
    public synchronized String getAFOSTableMap(String xxx) {
        if (siteToSiteMap.isEmpty()) {
            readFiles();
        }

        return siteToSiteMap.getOrDefault(xxx, "");
    }

    /**
     * @param icao
     * @return ccc
     */
    public synchronized String mapICAOToCCC(String icao) {
        if (nationalCategoryMap.isEmpty()) {
            readFiles();
        }
        return nationalCategoryMap.get(icao);
    }

    private synchronized void readFiles() {
        IPathManager pathMgr = PathManagerFactory.getPathManager();
        LocalizationContext[] searchOrder = pathMgr
                .getLocalSearchHierarchy(LocalizationType.COMMON_STATIC);

        List<LocalizationContext> reverseOrder = Arrays
                .asList(Arrays.copyOf(searchOrder, searchOrder.length));
        Collections.reverse(reverseOrder);
        ILocalizationFile lf;
        duplicateNNNSet = new HashSet<>();
        for (LocalizationContext ctx : reverseOrder) {
            lf = pathMgr.getLocalizationFile(ctx, AFOS_LOOKUP_FILENAME);
            loadAfosLookupFile(lf, siteToSiteMap);

            lf = pathMgr.getLocalizationFile(ctx,
                    NATIONAL_CATEGORY_TABLE_FILENAME);
            loadNationalCategoryFile(lf, nationalCategoryMap);

            lf = pathMgr.getLocalizationFile(ctx, RFC_TABLE_FILENAME);
            loadRFCLookupFile(lf, rfcList);
            lf = pathMgr.getLocalizationFile(ctx, DUPLICATE_NNN_FILENAME);
            loadDuplicateNNNLookupFile(lf, duplicateNNNSet);
        }

        // Load site list
        LocalizationContext lc = pathMgr.getContext(
                LocalizationType.COMMON_STATIC, LocalizationLevel.SITE);
        File file = pathMgr.getFile(lc, LOCATION_ID_FILENAME);
        if (file == null || !file.exists()) {
            lc = pathMgr.getContext(LocalizationType.COMMON_STATIC,
                    LocalizationLevel.BASE);
            file = pathMgr.getFile(lc, LOCATION_ID_FILENAME);
            statusHandler.info(
                    "Loaded location ID file [" + LOCATION_ID_FILENAME + "]");
        }
        loadSiteListFile(file);

        // post-process the nationalCategoryMap to generate the 3 to 4 letter
        // mapping
        for (String icao : nationalCategoryMap.keySet()) {
            if (icao.trim().length() == 4) {
                String threeId = icao.substring(1);
                String prefixCode = icao.substring(0, 1);
                String foundId = siteTo4LetterSite.get(threeId);

                /*
                 * TODO: 1. this doesn't work because the file contains upper
                 * case and we're looking for lower case k.
                 *
                 * 2. Do we really want K to take precedence at all sites. I
                 * thought we added a preference order some time ago.
                 */
                // US contiguous prefix code "K" takes precedence
                if (foundId == null || "k".equals(prefixCode)) {
                    siteTo4LetterSite.put(threeId, icao);
                }

                Set<String> reverse = siteTo3LetterSite.get(icao);
                if (reverse == null) {
                    reverse = new TreeSet<>();
                    siteTo3LetterSite.put(icao, reverse);
                }
                reverse.add(icao.substring(1));
            }
        }

        // load site 3 letter to 4 letter override
        for (LocalizationContext ctx : reverseOrder) {
            lf = pathMgr.getLocalizationFile(ctx, SITE_OVERRIDE_FILENAME);
            loadSite3LetterTo4LetterOverrideFile(lf, siteTo4LetterSite,
                    siteTo3LetterSite);
        }
    }

    /**
     * @param line
     * @return
     */
    private ParsedResult parseLine(String line) {
        ParsedResult result = new ParsedResult();

        line = line.trim();
        if (!line.isEmpty()) {
            String c = line.substring(0, 1);
            if (!COMMENT_DELIMITERS.contains(c)) {
                result.action = ParsedResult.Action.fromSymbol(c.charAt(0));
                if (result.action != ParsedResult.Action.OVERRIDE) {
                    line = line.substring(1).trim();
                }

                if (!line.isEmpty()) {
                    result.tokens = line.split("\\s+");
                }
            }
        }

        return result;
    }

    private synchronized void loadAfosLookupFile(ILocalizationFile file,
            Map<String, String> aliasMap) {
        if ((file != null) && file.exists()) {
            try (BufferedReader fis = new BufferedReader(
                    new InputStreamReader(file.openInputStream()))) {

                String line = null;
                int lineNumber = 0;
                while ((line = fis.readLine()) != null) {
                    lineNumber++;
                    ParsedResult result = parseLine(line);
                    if (result.tokens == null) {
                        continue;
                    }

                    if (result.tokens.length != 2
                            || result.tokens[0].length() != 4
                            || result.tokens[1].length() != 3) {
                        statusHandler.error(String.format(
                                "Invalid syntax at line %d of %s. Expected line of format: \"IIII CCC\"",
                                lineNumber, file));
                    } else {
                        switch (result.action) {
                        case ADD:
                            if (aliasMap.containsKey(result.tokens[0])) {
                                break;
                            }
                            // fall through to override

                        case OVERRIDE:
                            aliasMap.put(result.tokens[0], result.tokens[1]);
                            break;

                        case REMOVE:
                            aliasMap.remove(result.tokens[0]);
                            break;

                        }
                    }
                }
            } catch (IOException | LocalizationException e) {
                statusHandler.error("Could not read AFOS Lookup File " + file,
                        e);
            }
        }
    }

    private synchronized void loadNationalCategoryFile(ILocalizationFile file,
            Map<String, String> aliasMap) {
        if ((file != null) && file.exists()) {
            try (BufferedReader fis = new BufferedReader(
                    new InputStreamReader(file.openInputStream()))) {

                String line = null;
                int lineNumber = 0;
                while ((line = fis.readLine()) != null) {
                    lineNumber++;
                    ParsedResult result = parseLine(line);
                    if (result.tokens == null) {
                        continue;
                    }

                    if (result.tokens.length != 2
                            || result.tokens[0].length() < 3
                            || result.tokens[1].length() != 3) {
                        statusHandler.error(String.format(
                                "Invalid syntax at line %d of %s. Expected line of format: \"IIII CCC\"",
                                lineNumber, file));
                    } else {
                        // pad token[0] to 4 characters
                        result.tokens[0] = StringUtils
                                .rightPad(result.tokens[0], 4);
                        switch (result.action) {
                        case ADD:
                            if (aliasMap.containsKey(result.tokens[0])) {
                                break;
                            }
                            // fall through to override

                        case OVERRIDE:
                            aliasMap.put(result.tokens[0], result.tokens[1]);
                            break;

                        case REMOVE:
                            aliasMap.remove(result.tokens[0]);
                            break;
                        }
                    }
                }
            } catch (IOException | LocalizationException e) {
                statusHandler.error(
                        "Could not read National Category Table " + file, e);
            }
        }
    }

    private synchronized void loadSite3LetterTo4LetterOverrideFile(
            ILocalizationFile file, Map<String, String> site3To4LetterMap,
            Map<String, Set<String>> site4To3LetterMap) {
        if ((file != null) && file.exists()) {
            try (BufferedReader fis = new BufferedReader(
                    new InputStreamReader(file.openInputStream()))) {
                String line = null;
                int lineNumber = 0;
                while ((line = fis.readLine()) != null) {
                    lineNumber++;
                    ParsedResult result = parseLine(line);
                    if (result.tokens == null) {
                        continue;
                    }

                    if (result.tokens.length != 2
                            || result.tokens[0].length() != 3
                            || result.tokens[1].length() != 4) {
                        statusHandler.error(String.format(
                                "Invalid syntax at line %d of %s. Expected line of format: \"XXX XXXX\"",
                                lineNumber, file));
                    } else {
                        String site3 = result.tokens[0];
                        String site4 = result.tokens[1];

                        switch (result.action) {
                        case ADD:
                            if (site3To4LetterMap.containsKey(site3)) {
                                break;
                            }
                            // fall through to override

                        case OVERRIDE: {
                            // Currently, only the 3-letter IDs are used (in
                            // getSite4LetterId(), but it should be possible
                            // to also add the 4-letter IDs to this set.
                            site3to4LetterOverrides.add(site3);

                            site3To4LetterMap.put(site3, site4);

                            // Add the entry to the reverse lookup map
                            Set<String> site3s = site4To3LetterMap.get(site4);
                            if (site3s == null) {
                                site3s = new TreeSet<>();
                                site4To3LetterMap.put(site4, site3s);
                            }
                            site3s.add(site3);
                            break;
                        }

                        case REMOVE: {
                            site3to4LetterOverrides.remove(site3);
                            site3To4LetterMap.remove(site3);
                            Set<String> site3s = site4To3LetterMap.get(site4);
                            if (site3s != null) {
                                site3s.remove(site3);
                            }
                            break;
                        }
                        }
                    }
                }
            } catch (IOException | LocalizationException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Could not read Site 3 Letter To 4 Letter Override "
                                + file,
                        e);
            }
        }

    }

    private synchronized void loadRFCLookupFile(ILocalizationFile file,
            Set<String> aliasList) {
        if ((file != null) && file.exists()) {
            try (BufferedReader fis = new BufferedReader(
                    new InputStreamReader(file.openInputStream()))) {
                String line = null;
                int lineNumber = 0;
                while ((line = fis.readLine()) != null) {
                    lineNumber++;
                    ParsedResult result = parseLine(line);
                    if (result.tokens == null) {
                        continue;
                    }

                    if (result.tokens.length != 1
                            || result.tokens[0].length() < 3
                            || result.tokens[0].length() > 4) {
                        statusHandler.error(String.format(
                                "Invalid syntax at line %d of %s. Expected line of format: \"XXXX\"",
                                lineNumber, file));
                    } else {
                        switch (result.action) {
                        case ADD:
                            if (aliasList.contains(result.tokens[0])) {
                                break;
                            }
                            // fall through to override

                        case OVERRIDE:
                            aliasList.add(result.tokens[0]);
                            break;

                        case REMOVE:
                            aliasList.remove(result.tokens[0]);
                            break;
                        }
                        aliasList.add(line.trim());
                    }
                }
            } catch (IOException | LocalizationException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Could not read RFC Lookup File " + file, e);
            }
        }
    }

    private synchronized void loadSiteListFile(File file) {
        if (file != null && file.exists()) {
            NwsSitesXML siteXml;
            try {
                siteXml = (NwsSitesXML) unmarshaller.unmarshal(file);
                for (SiteIdXML xml : siteXml.getSiteIds()) {
                    String id = xml.getId();
                    SiteData sd = new SiteData(id, xml.getType());
                    this.siteMap.put(id, sd);
                }
            } catch (JAXBException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Problem reading in Site Id File ["
                                + LOCATION_ID_FILENAME + "]",
                        e);
            }
        }
    }

    private void loadDuplicateNNNLookupFile(ILocalizationFile file,
            Set<String> dupSet) {
        if ((file != null) && file.exists()) {
            try (BufferedReader fis = new BufferedReader(
                    new InputStreamReader(file.openInputStream()))) {
                String line = null;
                int lineNumber = 0;
                while ((line = fis.readLine()) != null) {
                    lineNumber++;
                    ParsedResult result = parseLine(line);
                    if (result.tokens == null) {
                        continue;
                    }

                    if (result.tokens.length != 1
                            || result.tokens[0].length() < 3) {
                        statusHandler.error(String.format(
                                "Invalid syntax at line %d of %s. Expected line of format: \"XXX\"",
                                lineNumber, file));
                    } else {
                        switch (result.action) {
                        case ADD:
                            // fall through to override

                        case OVERRIDE:
                            dupSet.add(result.tokens[0]);
                            break;

                        case REMOVE:
                            dupSet.remove(result.tokens[0]);
                            break;
                        }
                    }
                }
            } catch (IOException | LocalizationException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Could not read duplicate NNN Lookup File " + file, e);
            }
        }
    }

    /**
     * Converts a 3 letter site ID into a 4 letter ID, e.g. OAX to KOAX
     *
     * @param site3LetterId
     *            the 3 letter site id
     * @return the 4 letter site id
     */
    public synchronized String getSite4LetterId(String site3LetterId) {
        if (siteTo4LetterSite.isEmpty()) {
            readFiles();
        }
        String site = siteTo4LetterSite.get(site3LetterId);
        /*
         * TODO: more K stuff
         *
         * If site not found default to K + 3-letter-ID.
         *
         * Or, if the 4-letter site ID that was looked up does not start with a
         * 'K' and did not come from site3LetterTo4LetterOverride.dat, also
         * return K + 3-letter-ID.
         */
        if (site == null || (site.length() > 0 && site.charAt(0) != 'K'
                && !site3to4LetterOverrides.contains(site3LetterId))) {
            site = "K" + site3LetterId;
        }

        return site;
    }

    /**
     * Convert a 4 letter site ID into the 3 letter site IDs that convert to it,
     * e.g. KOAX to OAX. Some 3 letter sites convert to the same 4 letter site,
     * so this reverse lookup has to return a collection.
     *
     * @param site4LetterId
     * @return the 3 letter sites that map to the 4 letter site
     */
    public synchronized Set<String> getSite3LetterIds(String site4LetterId) {
        if (siteTo3LetterSite.isEmpty()) {
            readFiles();
        }
        Set<String> site3LetterIds = siteTo3LetterSite.get(site4LetterId);
        if (site3LetterIds == null) {
            site3LetterIds = new TreeSet<>();
            if (site4LetterId == null) {
                // return empty set
            } else if (site4LetterId.length() <= 3) {
                site3LetterIds.add(site4LetterId);
            } else {
                site3LetterIds.add(site4LetterId.substring(1));
            }
        }
        return site3LetterIds;
    }

    /**
     * @param site
     * @return true if site is an RFC site
     */
    public synchronized boolean isRFCSite(String site) {
        if (rfcList.isEmpty()) {
            readFiles();
        }
        return rfcList.contains(site);
    }

    /**
     * Get the site data objects.
     *
     * @return site data objects
     */
    public synchronized Map<String, SiteData> getSiteData() {
        if (siteMap.isEmpty()) {
            readFiles();
        }
        return siteMap;
    }

    public synchronized boolean isDuplicateNNN(String nnnid) {
        if (duplicateNNNSet == null) {
            readFiles();
        }
        return duplicateNNNSet.contains(nnnid);
    }

    @Override
    public synchronized void fileChanged(ILocalizationFile file) {
        siteToSiteMap.clear();
        nationalCategoryMap.clear();
        siteTo4LetterSite.clear();
        siteTo3LetterSite.clear();
        site3to4LetterOverrides.clear();
        rfcList.clear();
        siteMap.clear();
        duplicateNNNSet = null;

        readFiles();
    }
}
