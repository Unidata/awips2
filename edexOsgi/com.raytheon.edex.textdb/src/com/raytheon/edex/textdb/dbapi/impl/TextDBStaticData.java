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
package com.raytheon.edex.textdb.dbapi.impl;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.atomic.AtomicBoolean;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.site.SiteMap;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.util.CollectionUtil;
import com.raytheon.uf.common.util.FileUtil;
import com.raytheon.uf.common.util.StringUtil;

/**
 * A singleton class that maintains static data used by the TextDB (e.g., the
 * mappings needed to perform lookups for AFOS and WMO IDs). Data is lazily
 * loaded by data file.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 02, 2008 1538       jkorman     Initial creation
 * Jul 10, 2009 2191       rjpeter     Added additional methods.
 * Apr 01, 2014 2915       dgilling    Major re-factor, all methods are now
 *                                     static.
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */

public class TextDBStaticData {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(TextDBStaticData.class);

    private static final int COLLECTIVE_TABLE_KEY_LEN = 6;

    private static final int UPAIR_KEY_LEN = 6;

    private static final int STATION_TABLE_KEY_LEN = 5;

    private static final int BIT_TABLE_KEY_LEN = 3;

    private static final int ISPAN_KEY_LEN = 10;

    private static final int STD_SEPARATOR_LEN = 1;

    private static final int ISPAN_SEPARATOR_LEN = 2;

    private static boolean exclusionFileInplay = System
            .getenv("STD_TEXT_EXCLUSION") != null;

    private static final String TEXTDB = "textdb";

    private static final String COLLECTIVE_TABLE_PATH = FileUtil.join(TEXTDB,
            "collective_table.dat");

    private static final String UPAIR_TABLE_PATH = FileUtil.join(TEXTDB,
            "upair_table.dat");

    private static final String STATION_TABLE_PATH = FileUtil.join(TEXTDB,
            "station_table.dat");

    private static final String EXCLUSION_PRODUCT_LIST_PATH = FileUtil.join(
            TEXTDB, "exclusionProductList.dat");

    private static final String CHECK_PRODUCT_FILE_PATH = FileUtil.join(TEXTDB,
            "checkProductFile.dat");

    private static final String ISPAN_TABLE_PATH = FileUtil.join(TEXTDB,
            "ispan_table.dat");

    private static final String BIT_TABLE_PATH = FileUtil.join(TEXTDB,
            "bit_table.dat");

    private static final AtomicBoolean stdCollectiveLoaded = new AtomicBoolean(
            false);

    private static final AtomicBoolean uaCollectiveLoaded = new AtomicBoolean(
            false);

    private static final AtomicBoolean stationTableLoaded = new AtomicBoolean(
            false);

    private static final AtomicBoolean ispanTableLoaded = new AtomicBoolean(
            false);

    private static final AtomicBoolean bitTableLoaded = new AtomicBoolean(false);

    private static final AtomicBoolean exclusionListLoaded = new AtomicBoolean(
            false);

    private static final AtomicBoolean dupCheckListLoaded = new AtomicBoolean(
            false);

    private static final Map<String, String> stdCollectiveMap = new HashMap<>();

    private static final Map<String, String> uaCollectiveMap = new HashMap<>();

    private static final Map<String, String> stationTable = new HashMap<>();

    private static final Map<String, String> ispanTable = new HashMap<>();

    private static final Map<String, String> bitTable = new HashMap<>();

    private static final Collection<String> exclusionList = new HashSet<>();

    private static final Collection<String> duplicateCheckList = new HashSet<>();

    private TextDBStaticData() {
        // prevent default constructor from being created.
        throw new AssertionError();
    }

    public static void setDirty() {
        stdCollectiveLoaded.set(false);
        uaCollectiveLoaded.set(false);
        stationTableLoaded.set(false);
        ispanTableLoaded.set(false);
        bitTableLoaded.set(false);
        exclusionListLoaded.set(false);
        dupCheckListLoaded.set(false);
    }

    /**
     * 
     * @param dataDes
     * @return
     */
    public static String matchStdCollective(String dataDes) {
        if (!stdCollectiveLoaded.get()) {
            populateMap(stdCollectiveMap, stdCollectiveLoaded,
                    COLLECTIVE_TABLE_PATH, COLLECTIVE_TABLE_KEY_LEN,
                    STD_SEPARATOR_LEN);
        }

        String retValue = null;
        synchronized (stdCollectiveMap) {
            retValue = stdCollectiveMap.get(dataDes);
        }
        return retValue;
    }

    /**
     * 
     * @param dataDes
     * @return
     */
    public static String matchUACollective(String dataDes) {
        if (!uaCollectiveLoaded.get()) {
            populateMap(uaCollectiveMap, uaCollectiveLoaded, UPAIR_TABLE_PATH,
                    UPAIR_KEY_LEN, STD_SEPARATOR_LEN);
        }

        String retValue = null;
        synchronized (uaCollectiveMap) {
            retValue = uaCollectiveMap.get(dataDes);
        }
        return retValue;
    }

    /**
     * Map a wmo identifier to its associated ICAO i.e. "72558" >> "KOAX"
     * 
     * @param WMOId
     * @return
     */
    public static String mapWMOToICAO(String WMOId) {
        if (!stationTableLoaded.get()) {
            populateMap(stationTable, stationTableLoaded, STATION_TABLE_PATH,
                    STATION_TABLE_KEY_LEN, STD_SEPARATOR_LEN);
        }

        String retValue = null;
        synchronized (stationTable) {
            retValue = stationTable.get(WMOId);
        }
        return retValue;
    }

    /**
     * Map a wmo identifier to its associated ICAO i.e. "72558" >> "KOAX"
     * 
     * @param WMOId
     * @return
     */
    public static String mapICAOToCCC(String icao) {
        return SiteMap.getInstance().mapICAOToCCC(icao);
    }

    /**
     * Map a wmo identifier to its associated CCC via the mapping through the
     * national_category table i.e. "72558" >> "KOAX" >> "OMA"
     * 
     * @param WMOId
     * @return
     */
    public static String mapWMOToCCC(String icao) {
        return mapICAOToCCC(mapWMOToICAO(icao));
    }

    /**
     * 
     * @param ispanId
     * @return
     */
    public static boolean isMappedISpanId(String ispanId) {
        if (!ispanTableLoaded.get()) {
            populateMap(ispanTable, ispanTableLoaded, ISPAN_TABLE_PATH,
                    ISPAN_KEY_LEN, ISPAN_SEPARATOR_LEN);
        }

        boolean containsKey = false;
        synchronized (ispanTable) {
            containsKey = ispanTable.containsKey(ispanId);
        }
        return containsKey;
    }

    /**
     * 
     * @param afosId
     * @return
     */
    public static boolean isExcluded(String afosId) {
        if (!exclusionListLoaded.get()) {
            if (exclusionFileInplay) {
                populateCollection(exclusionList, exclusionListLoaded,
                        EXCLUSION_PRODUCT_LIST_PATH);
            } else {
                synchronized (exclusionList) {
                    exclusionList.clear();
                    exclusionListLoaded.set(true);
                }
            }
        }

        synchronized (exclusionList) {
            if (!CollectionUtil.isNullOrEmpty(exclusionList)) {
                // If a product ID in the exclusion file list that matches the
                // incoming product's ID is found, end the process; otherwise,
                // continue searching through the end of the list.

                // with CCC_id as wild card
                String expandProductId = "000"
                        + afosId.substring(3,
                                (afosId.length() > 9 ? 9 : afosId.length()));
                if (exclusionList.contains(expandProductId)) {
                    return true;
                }

                // with NNN_id as wild card
                expandProductId = afosId.substring(0, 3) + "000"
                        + afosId.substring(6);
                if (exclusionList.contains(expandProductId)) {
                    return true;
                }

                // with XXX_id as wild card
                expandProductId = afosId.substring(0, 6) + "000";
                if (exclusionList.contains(expandProductId)) {
                    return true;
                }

                // with both NNN_id and XXX_id as wild card
                expandProductId = afosId.substring(0, 3) + "000000";
                if (exclusionList.contains(expandProductId)) {
                    return true;
                }

                // with both CCC_id and XXX_id as wild card
                expandProductId = "000" + afosId.substring(3, 6) + "000";
                if (exclusionList.contains(expandProductId)) {
                    return true;
                }

                // with both CCC_id and NNN_id as wild card
                expandProductId = "000000" + afosId.substring(6);
                if (exclusionList.contains(expandProductId)) {
                    return true;
                }

                // search full product id
                return exclusionList.contains(afosId);
            }
        }

        return false;
    }

    /**
     * 
     * @param afosId
     * @return
     */
    public static boolean checkForDuplicate(String afosId) {
        if (!dupCheckListLoaded.get()) {
            populateCollection(duplicateCheckList, dupCheckListLoaded,
                    CHECK_PRODUCT_FILE_PATH);
        }

        boolean retVal = false;
        synchronized (duplicateCheckList) {
            if (!CollectionUtil.isNullOrEmpty(duplicateCheckList)) {
                retVal = duplicateCheckList.contains(afosId);
            }
        }
        return retVal;
    }

    /**
     * 
     * @param ispanId
     * @return
     */
    public static String getProductId(String ispanId) {
        if (!ispanTableLoaded.get()) {
            populateMap(ispanTable, ispanTableLoaded, ISPAN_TABLE_PATH,
                    ISPAN_KEY_LEN, ISPAN_SEPARATOR_LEN);
        }

        String retVal = null;
        synchronized (ispanTable) {
            retVal = ispanTable.get(ispanId);
        }
        return retVal;
    }

    /**
     * 
     * @param nnnId
     * @return
     */
    public static String getSiteIdFromNNN(String nnnId, String siteId) {
        if (!bitTableLoaded.get()) {
            populateMap(bitTable, bitTableLoaded, BIT_TABLE_PATH,
                    BIT_TABLE_KEY_LEN, STD_SEPARATOR_LEN);
        }

        String value = null;
        synchronized (bitTable) {
            value = bitTable.get(nnnId);
        }
        String retVal = ("AAA".equals(value)) ? SiteMap.getInstance()
                .getCCCFromXXXCode(siteId) : value;
        return retVal;
    }

    /**
     * 
     * @param wmoCccc
     * @return
     */
    public static String getAfosCCC(String wmoCccc) {
        return SiteMap.getInstance().getCCCFromXXXCode(wmoCccc);
    }

    /**
     * 
     * @param v
     * @return
     */
    public static String getAFOSTableMap(String v) {
        return SiteMap.getInstance().getAFOSTableMap(v);
    }

    private static void populateMap(final Map<String, String> dataMap,
            final AtomicBoolean loadedFlag, final String filePath,
            final int keyLen, final int separatorLen) {
        synchronized (dataMap) {
            if (loadedFlag.get()) {
                return;
            }

            try {
                Map<String, String> newData = loadFileToMap(filePath, keyLen,
                        separatorLen);
                dataMap.clear();
                dataMap.putAll(newData);
                loadedFlag.set(true);
            } catch (IOException e) {
                statusHandler.error(
                        "Could not load TextDBStaticData from file ["
                                + filePath + "]", e);
            }
        }
    }

    private static void populateCollection(final Collection<String> dataList,
            final AtomicBoolean loadedFlag, final String filePath) {
        synchronized (dataList) {
            if (loadedFlag.get()) {
                return;
            }

            try {
                Set<String> newData = loadFileToSet(filePath);
                dataList.clear();
                dataList.addAll(newData);
                loadedFlag.set(true);
            } catch (IOException e) {
                statusHandler.error(
                        "Could not load TextDBStaticData from file ["
                                + filePath + "]", e);
            }
        }
    }

    /**
     * Read in the desired static file, parse its contains and place results in
     * the map. This assumes that each line of the file is in the following
     * format: (KEY)(DELIMETER)(VALUE)
     * <ul>
     * <li>(KEY) is a string of <code>keyLen</code> characters that may include
     * spaces.</li>
     * <li>(DELIMETER) is a string of <code>separatorLen</code> chars (usually,
     * a space or equal sign or multiple spaces).</li>
     * <li>(VALUE) the reset of the line that is mapped to (KEY).</li>
     * </ul>
     * 
     * @param filename
     *            File to parse
     * @param keyLen
     *            Length of the file's keys
     * @param separatorLen
     *            Length of the separator between keys and values.
     * @return A <code>Map</code> pairing the keys to their values.
     * @throws IOException
     *             If any errors occur reading the specified input file.
     * @throws FileNotFoundException
     *             If the specified file cannot be located in the localization
     *             hierarchy or if the file cannot be opened for reading.
     */
    private static Map<String, String> loadFileToMap(final String filename,
            final int keyLen, final int separatorLen)
            throws FileNotFoundException, IOException {
        File file = locateFile(filename);
        if (file == null) {
            throw new FileNotFoundException("Could not locate file ["
                    + filename + "] in localization hierarchy.");
        }

        Map<String, String> retVal = new HashMap<>();
        try (BufferedReader inFile = new BufferedReader(new FileReader(file))) {
            String line = null;
            while ((line = inFile.readLine()) != null) {
                if ((line.length() < (keyLen + separatorLen + 1))
                        || (line.startsWith("#"))) {
                    continue;
                }

                /*
                 * A note on why we're performing this selective trimming: The
                 * format of the current NDM files on the web are formatted
                 * slightly differently than those in the current A2 baseline.
                 * 
                 * The A2 baseline files have a single separator char between
                 * key and value, and no excess whitespace between the separator
                 * char and the value. The new updated files on the web,
                 * however, vary between a single separator char and using 2
                 * spaces as a separator.
                 * 
                 * To allow both formats to work we expect a fixed-length key,
                 * and left-trim the value string in case we get an extra
                 * leading space char from the newer format files.
                 */
                String dataKey = line.substring(0, keyLen);
                String tblData = StringUtil.ltrim(line.substring(keyLen
                        + separatorLen));
                retVal.put(dataKey, tblData);
            }
        }

        return retVal;
    }

    /**
     * Read in the desired static file, parse its contains and place results in
     * a <code>Set</code>.
     * 
     * @param filename
     *            The path to the input file in the localization hierarchy.
     * @return A <code>Set</code> containing all the valid values in the input
     *         file.
     * @throws FileNotFoundException
     *             If the specified file cannot be located in the localization
     *             hierarchy or if the file cannot be opened for reading.
     * @throws IOException
     *             If any errors occur reading the specified input file.
     */
    private static Set<String> loadFileToSet(final String filename)
            throws FileNotFoundException, IOException {
        File file = locateFile(filename);
        if (file == null) {
            throw new FileNotFoundException("Could not locate file ["
                    + filename + "] in localization hierarchy.");
        }

        Set<String> retVal = new HashSet<>();
        try (BufferedReader inFile = new BufferedReader(new FileReader(file))) {
            String line = null;
            while ((line = inFile.readLine()) != null) {
                line = line.trim();
                if (!line.isEmpty() && !line.startsWith("#")
                        && !"000000000".equals(line)) {
                    retVal.add(line);
                }
            }
        }

        return retVal;
    }

    private static File locateFile(final String filename) {
        IPathManager pathMgr = PathManagerFactory.getPathManager();
        Map<LocalizationLevel, LocalizationFile> tieredFile = pathMgr
                .getTieredLocalizationFile(LocalizationType.COMMON_STATIC,
                        filename);
        LocalizationContext[] contexts = pathMgr
                .getLocalSearchHierarchy(LocalizationType.COMMON_STATIC);

        File file = null;
        for (LocalizationContext context : contexts) {
            LocalizationLevel level = context.getLocalizationLevel();
            LocalizationFile lFile = tieredFile.get(level);
            if (lFile != null) {
                file = lFile.getFile();
                break;
            }
        }

        return file;
    }
}
