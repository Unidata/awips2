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
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.site.SiteMap;

/**
 * TODO Add Description
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 2, 2008        1538 jkorman     Initial creation
 * Jul 10, 2009 2191       rjpeter     Added additional methods.
 * Mar 17, 2014 DR 16449   D. Friedman Fix reload/populate.
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */

public class TextDBStaticData {

    private static final int COLLECTIVE_TABLE_KEY_LEN = 6;

    private static final int UPAIR_KEY_LEN = 6;

    private static final int STATION_TABLE_KEY_LEN = 5;

    private static final int BIT_TABLE_KEY_LEN = 3;

    private static boolean exclusionFileInplay = System
            .getenv("STD_TEXT_EXCLUSION") != null;

    private static Map<String, TextDBStaticData> instanceMap = new HashMap<String, TextDBStaticData>();

    private boolean tablesLoaded = false;

    private Map<String, String> stdCollectiveMap = new HashMap<String, String>();

    private Map<String, String> uaCollectiveMap = new HashMap<String, String>();

    private Map<String, String> stationTable = new HashMap<String, String>();

    private Map<String, String> nationalTable = new HashMap<String, String>();

    private Map<String, String> ispanTable = new HashMap<String, String>();

    private Map<String, String> bitTable = new HashMap<String, String>();

    private Set<String> exclusionList = new HashSet<String>();

    private Set<String> duplicateCheckList = new HashSet<String>();

    private String siteId = null;

    /**
     * 
     */
    private TextDBStaticData(String siteId) {
        this.siteId = siteId;
        populateTables();
    }

    public boolean areTablesLoaded() {
        return tablesLoaded;
    }

    /**
     * 
     * @return
     */
    public synchronized static TextDBStaticData instance(String siteId) {
        TextDBStaticData instance = instanceMap.get(siteId);
        if (instance == null) {
            instance = new TextDBStaticData(siteId);
            instanceMap.put(siteId, instance);
        }
        return instance;
    }

    public synchronized static void setDirty() {
        for (String key : instanceMap.keySet()) {
            if (null != instanceMap.get(key)) {
                instanceMap.get(key).makeDirty();
            }
        }
    }

    public synchronized void makeDirty() {
        tablesLoaded = false;
    }

    /**
     * 
     * @param dataDes
     * @return
     */
    public synchronized String matchStdCollective(String dataDes) {
        if (!tablesLoaded) {
            reload();
        }
        String retValue = null;
        if (stdCollectiveMap != null) {
            retValue = stdCollectiveMap.get(dataDes);
        }
        return retValue;
    }

    /**
     * 
     * @param dataDes
     * @return
     */
    public synchronized String matchUACollective(String dataDes) {
        if (!tablesLoaded) {
            reload();
        }

        String retValue = null;
        if (uaCollectiveMap != null) {
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
    public synchronized String mapWMOToICAO(String WMOId) {
        if (!tablesLoaded) {
            reload();
        }
        String retValue = null;
        if (stationTable != null) {
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
    public synchronized String mapICAOToCCC(String icao) {
        return SiteMap.getInstance().mapICAOToCCC(icao);
    }

    /**
     * Map a wmo identifier to its associated CCC via the mapping through the
     * national_category table i.e. "72558" >> "KOAX" >> "OMA"
     * 
     * @param WMOId
     * @return
     */
    public synchronized String mapWMOToCCC(String icao) {
        return mapICAOToCCC(mapWMOToICAO(icao));
    }

    /**
     * 
     * @param ispanId
     * @return
     */
    public synchronized boolean isMappedISpanId(String ispanId) {
        if (!tablesLoaded) {
            reload();
        }
        return ispanTable.containsKey(ispanId);
    }

    /**
     * 
     * @param afosId
     * @return
     */
    public synchronized boolean isExcluded(String afosId) {
        if (!tablesLoaded) {
            reload();
        }
        if (exclusionList != null && exclusionList.size() > 0) {
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

        return false;
    }

    /**
     * 
     * @param afosId
     * @return
     */
    public synchronized boolean checkForDuplicate(String afosId) {
        boolean retVal = false;
        if (duplicateCheckList != null && duplicateCheckList.size() > 0) {
            retVal = duplicateCheckList.contains(afosId);
        }

        return retVal;
    }

    /**
     * 
     * @param ispanId
     * @return
     */
    public synchronized String getProductId(String ispanId) {
        if (!tablesLoaded) {
            reload();
        }
        return ispanTable.get(ispanId);
    }

    /**
     * 
     * @param nnnId
     * @return
     */
    public synchronized String getSiteIdFromNNN(String nnnId) {
        if (!tablesLoaded) {
            reload();
        }
        return bitTable.get(nnnId);
    }

    /**
     * 
     * @param wmoCccc
     * @return
     */
    public synchronized String getAfosCCC(String wmoCccc) {
        return SiteMap.getInstance().getCCCFromXXXCode(wmoCccc);
    }

    /**
     * 
     * @param v
     * @return
     */
    public synchronized String getAFOSTableMap(String v) {
        return SiteMap.getInstance().getAFOSTableMap(v);
    }
    
    /**
     * Cause the internal tables to be reloaded. <code>
     *   TextDBStaticData.instance(site).reload();
     * </code>
     */
    public void reload() {
        synchronized (this) {
            // Drop the old maps.
            stdCollectiveMap = null;
            uaCollectiveMap = null;
            stationTable = null;
            nationalTable = null;
            ispanTable = null;
            bitTable = null;
            exclusionList = null;
            duplicateCheckList = null;
            // and repopulate.
            populateTables();
        }
    }

    /**
     * Populate the internal data maps.
     */
    private void populateTables() {
        tablesLoaded = true;

        // ******************************
        stdCollectiveMap = new HashMap<String, String>();
        tablesLoaded &= loadFile("textdb/collective_table.dat",
                stdCollectiveMap, COLLECTIVE_TABLE_KEY_LEN);
        // ******************************
        uaCollectiveMap = new HashMap<String, String>();
        tablesLoaded &= loadFile("textdb/upair_table.dat", uaCollectiveMap,
                UPAIR_KEY_LEN);
        // ******************************
        stationTable = new HashMap<String, String>();
        tablesLoaded &= loadFile("textdb/station_table.dat", stationTable,
                STATION_TABLE_KEY_LEN);
        // ******************************
        exclusionList = new HashSet<String>();
        if (exclusionFileInplay) {
            tablesLoaded &= loadFile("textdb/exclusionProductList.dat",
                    exclusionList);
        }
        // ******************************
        duplicateCheckList = new HashSet<String>();
        tablesLoaded &= loadFile("textdb/checkProductFile.dat",
                duplicateCheckList);
        // ******************************
        ispanTable = new HashMap<String, String>();
        tablesLoaded &= loadISpanFile("textdb/ispan_table.dat", ispanTable);

        // ******************************
        bitTable = new HashMap<String, String>();
        tablesLoaded &= loadFile("textdb/bit_table.dat", bitTable,
                BIT_TABLE_KEY_LEN);
        String node = SiteMap.getInstance().getCCCFromXXXCode(siteId);
        for (String s : bitTable.keySet()) {
            if ("AAA".equals(bitTable.get(s))) {
                bitTable.put(s, node);
            }
        }
    }

    /**
     * Read in the desired static file, parse its contains and place results in
     * the map. This assumes that each line of the file is in the following
     * format: <li>(KEY)(DEL_CHAR)(VALUE)</li> <li>(KEY) is a string of keyLen
     * characters that may include spaces</li> <li>(DEL_CHAR) a single character
     * delimiter normally a space or equal sign</li> <li>(VALUE) the reset of
     * the line that is mapped to (KEY)</li>
     * 
     * @param filename
     *            - file to parse
     * @param map
     *            - parse results
     * @param keyLen
     *            - Length of the file's keys
     * @return - true when file read and parsed otherwise false
     */
    private boolean loadFile(String filename, Map<String, String> map,
            final int keyLen) {
        boolean loaded = false;

        InputStream strm = null;
        BufferedReader bf = null;
        IPathManager pathMgr = PathManagerFactory.getPathManager();
        LocalizationContext lc = pathMgr.getContext(
                LocalizationType.COMMON_STATIC, LocalizationLevel.BASE);
        File file = pathMgr.getFile(lc, filename);

        HashMap<String, String> dataMap = new HashMap<String, String>();

        try {
            try {
                strm = new FileInputStream(file);
                if (strm != null) {
                    bf = new BufferedReader(new InputStreamReader(strm));

                    String line = null;
                    while ((line = bf.readLine()) != null) {

                        String dataKey = line.substring(0, keyLen);
                        String tblData = line.substring(keyLen + 1);

                        String tData = dataMap.get(tblData);
                        if (tData == null) {
                            dataMap.put(tblData, tblData);
                            tData = tblData;
                        }
                        map.put(dataKey, tData);
                    }
                    loaded = true;
                }
            } catch (IOException ioe) {
                ioe.printStackTrace();
            }
        } finally {
            if (bf != null) {
                try {
                    bf.close();
                } catch (IOException ioe) {
                    ioe.printStackTrace();
                }
            }
            if (dataMap != null) {
                dataMap.clear();
            }
        }

        return loaded;
    }

    /**
     * 
     * @param filename
     * @param list
     * @return
     */
    private boolean loadFile(String filename, Set<String> list) {
        boolean loaded = false;

        InputStream strm = null;
        BufferedReader bf = null;
        IPathManager pathMgr = PathManagerFactory.getPathManager();
        LocalizationContext lc = pathMgr.getContext(
                LocalizationType.COMMON_STATIC, LocalizationLevel.BASE);
        File file = pathMgr.getFile(lc, filename);

        try {
            try {
                strm = new FileInputStream(file);

                if (strm != null) {
                    bf = new BufferedReader(new InputStreamReader(strm));

                    String line = null;
                    while ((line = bf.readLine()) != null) {
                        line = line.trim();
                        if (line.length() > 0 && !line.startsWith("#")
                                && !"000000000".equals(line)) {
                            list.add(line);
                        }
                    }
                    loaded = true;
                }
            } catch (IOException ioe) {
                ioe.printStackTrace();
            }
        } finally {
            if (bf != null) {
                try {
                    bf.close();
                } catch (IOException ioe) {
                    ioe.printStackTrace();
                }
            }
        }

        return loaded;
    }

    /**
     * 
     * @param filename
     * @param map
     * @return
     */
    private boolean loadISpanFile(String filename, Map<String, String> map) {
        boolean loaded = false;

        InputStream strm = null;
        BufferedReader bf = null;
        IPathManager pathMgr = PathManagerFactory.getPathManager();
        LocalizationContext lc = pathMgr.getContext(
                LocalizationType.COMMON_STATIC, LocalizationLevel.BASE);
        File file = pathMgr.getFile(lc, filename);

        try {
            try {
                strm = new FileInputStream(file);

                if (strm != null) {
                    bf = new BufferedReader(new InputStreamReader(strm));

                    String line = null;
                    while ((line = bf.readLine()) != null) {
                        map.put(line.substring(0, 10), line.substring(12));
                    }
                    loaded = true;
                }
            } catch (IOException ioe) {
                ioe.printStackTrace();
            }
        } finally {
            if (bf != null) {
                try {
                    bf.close();
                } catch (IOException ioe) {
                    ioe.printStackTrace();
                }
            }
        }

        return loaded;
    }
}
