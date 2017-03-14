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

package com.raytheon.viz.texteditor;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.SortedSet;
import java.util.TreeSet;

import com.raytheon.uf.common.dataplugin.radar.util.RadarTextProductUtil;
import com.raytheon.uf.common.dataplugin.radar.util.RadarsInUseUtil;
import com.raytheon.uf.common.localization.ILocalizationFile;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.exception.LocalizationException;
import com.raytheon.uf.common.site.SiteMap;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.localization.LocalizationManager;
import com.raytheon.viz.texteditor.util.AFOS_CLASS;
import com.raytheon.viz.texteditor.util.AFOS_ORIGIN;

/**
 * Singleton class that contains information related to querying for or updating
 * of text products as well as listing menu contents in the Afos Browser.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 11/7/2007    501         grichard    Initial creation.
 * 11/8/2007    520         grichard    Implemented build 11 features.
 * 11/26/2007   520         grichard    Implemented SuperSite in preparation for JiBX'ng.
 * 12/14/2007   582         grichard    Implemented build 12 features.
 * 07/24/2012   939         jkorman     Modified parseAfosMasterPil() handle blank lines as well
 *                                      as lines with trailing whitespace.
 * 02/17/2016   5391        randerso    Added displayHelp
 * 02/12/2016   4716        rferrel     Refactor to extend {@link AbstractBrowserModel}.
 * 11/02/2016   5692        rferrel     Refactor ccc help to {@link AbstractBrowserModel}.
 * </pre>
 * 
 * @author grichard
 */
public final class AfosBrowserModel extends AbstractBrowserModel {
    // Need at least this many characters in an afosMasterPIL entry
    // Need the CCCNNN and at least a 1 character XXX
    private static final int MIN_MASTPIL_LEN = 7;

    // but no more than 9 characters.
    private static final int MAX_MASTPIL_LEN = 9;

    private static final int CCC_PIL_POS = 0;

    private static final int NNN_PIL_POS = 3;

    private static final int XXX_PIL_POS = 6;

    private static final String SITE_WILDCARD = "@@@";

    private static final String ORIGIN_TABLE = "textdb/textOriginTable.txt";

    private static final String AFOS_MASTER_PIL = "textdb/afosMasterPIL.txt";

    private static final Map<String, String> displayHelp = new HashMap<>();

    static {
        displayHelp.put("A:", "Display current hour versions");
        displayHelp.put("ALL:", "Display all versions");
    }

    /**
     * The static singleton instance.
     */
    private static AfosBrowserModel instance;

    /**
     * Associated a set of ccc with a given origin. Used a display use, should
     * be sorted.
     */
    private Map<AFOS_ORIGIN, SortedSet<String>> originMap;

    /**
     * Associates a category class to the list of nnn that are associated with
     * it. Only used for internal indexing, no need to be sorted.
     */
    private Map<Integer, Set<String>> categoryClass;

    private Map<String, Map<String, SortedSet<String>>> masterPil;

    private String localSite;

    /**
     * Private constructor: Use getInstance().
     */
    private AfosBrowserModel() {
        super();
        this.cccHelp = new HashMap<>();
        this.originMap = new HashMap<>();
        this.categoryClass = new HashMap<>();
        this.masterPil = new HashMap<>();
        this.localSite = null;
        setup();
        cleanup();
    }

    @Override
    protected void setup() {
        super.setup();

        // read in origin into map of ccc to allow for overriding
        Map<String, List<AFOS_ORIGIN>> tmpOrigin = new HashMap<>();
        for (LocalizationContext lc : lcArray) {
            tmpOrigin.putAll(parseOrigin(
                    pathManager.getLocalizationFile(lc, ORIGIN_TABLE)));
        }

        // generate into map by origin
        for (Entry<String, List<AFOS_ORIGIN>> entry : tmpOrigin.entrySet()) {
            for (AFOS_ORIGIN origin : entry.getValue()) {
                SortedSet<String> cccList = originMap.get(origin);
                if (cccList == null) {
                    cccList = new TreeSet<>();
                    originMap.put(origin, cccList);
                }
                cccList.add(entry.getKey());
            }
        }

        // read in categories into map of nnn to allow for overriding
        Map<String, String> tmpCategory = new HashMap<>();
        for (LocalizationContext lc : lcArray) {
            parseStringValues(
                    pathManager.getLocalizationFile(lc, CATEGORY_CLASS),
                    tmpCategory);
        }

        // generate into map by category
        for (Entry<String, String> entry : tmpCategory.entrySet()) {
            for (String category : entry.getValue().split(" ")) {
                try {
                    Integer cat = new Integer(category);
                    Set<String> nnnList = categoryClass.get(cat);
                    if (nnnList == null) {
                        nnnList = new HashSet<>();
                        categoryClass.put(cat, nnnList);
                    }
                    nnnList.add(entry.getKey());
                } catch (NumberFormatException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Error occurred processing category class nnn ["
                                    + entry.getKey() + "] category [" + category
                                    + "] is not a number",
                            e);
                }
            }
        }

        for (LocalizationContext lc : lcArray) {
            parseAfosMasterPil(
                    pathManager.getLocalizationFile(lc, AFOS_MASTER_PIL));
        }
        addRadarToMasterPil();
    }

    /**
     * Adds radar CCCNNNXXX codes to the master pil archive so that they can be
     * found via the AFOS browser.
     */
    private void addRadarToMasterPil() {
        String curSite = LocalizationManager.getInstance().getCurrentSite();
        List<String> radarIDList = RadarsInUseUtil.getSite(curSite,
                RadarsInUseUtil.ARSR_CONSTANT);
        radarIDList.addAll(
                RadarsInUseUtil.getSite(curSite, RadarsInUseUtil.ASR_CONSTANT));
        radarIDList.addAll(RadarsInUseUtil.getSite(curSite,
                RadarsInUseUtil.MOSAIC_CONSTANT));
        radarIDList.addAll(RadarsInUseUtil.getSite(curSite,
                RadarsInUseUtil.LOCAL_CONSTANT));
        List<String> radarIDListTrunc = new ArrayList<>();
        List<String> radarList = RadarTextProductUtil.getRadarTableEntries();
        List<String> radarTypeList = new ArrayList<>();

        // Preprocessing: Must strip the "WSR" off the front of each entry in
        // radarTypeList.
        for (String radar : radarList) {
            radarTypeList.add(radar.substring(3).toUpperCase());
        }
        // Preprocessing: Must get the radar ID list entries down to 3 letter
        // codes.
        for (String radarID : radarIDList) {
            radarIDListTrunc.add(radarID.substring(1).toUpperCase());
        }

        String ccc = "WSR";

        // Set up the WSR entry if it isn't already there.
        Map<String, SortedSet<String>> nnnxxx = masterPil.get(ccc);
        if (nnnxxx == null) {
            nnnxxx = new HashMap<>();
            masterPil.put(ccc, nnnxxx);
        }

        for (String nnn : radarTypeList) {
            SortedSet<String> xxxList = nnnxxx.get(nnn);
            if (xxxList == null) {
                xxxList = new TreeSet<>();
                nnnxxx.put(nnn, xxxList);
            }

            xxxList.addAll(radarIDListTrunc);
            if ("RCM".equals(nnn)) {
                xxxList.remove("OKC");
            }
        }

    }

    private Map<String, List<AFOS_ORIGIN>> parseOrigin(
            ILocalizationFile fileToParse) {
        Map<String, List<AFOS_ORIGIN>> cccOriginMap = new HashMap<>();
        if ((fileToParse != null) && fileToParse.exists()) {
            try (InputStream in = fileToParse.openInputStream();
                    BufferedReader br = new BufferedReader(
                            new InputStreamReader(in));) {
                String line = null;

                while ((line = br.readLine()) != null) {
                    // skip comments.
                    if (line.startsWith(COMMENT_DELIM)) {
                        continue;
                    }

                    // Get the category id
                    String ccc = line.substring(0, 3);
                    String key = line.substring(4, 5);
                    AFOS_ORIGIN origin = AFOS_ORIGIN.getFromOriginKey(key);
                    List<AFOS_ORIGIN> origins = cccOriginMap.get(ccc);
                    if (origins == null) {
                        origins = new ArrayList<>(5);
                        cccOriginMap.put(ccc, origins);
                    }
                    origins.add(origin);
                } // while
            } catch (IOException | LocalizationException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Error occurred parsing file [" + fileToParse.getPath()
                                + "]",
                        e);
            }
        }

        return cccOriginMap;
    }

    /**
     * Read and parse an afos PIL list. In the event of processing multiple
     * files, the most recent entry overwrites a current entry.
     * 
     * @param fileToParse
     *            File reference containing the PIL list.
     */
    private void parseAfosMasterPil(ILocalizationFile fileToParse) {
        if (fileToParse != null && fileToParse.exists()) {

            String localCCC = SiteMap.getInstance()
                    .getCCCFromXXXCode(localSite);
            try (InputStream in = fileToParse.openInputStream();
                    BufferedReader br = new BufferedReader(
                            new InputStreamReader(in));) {
                String line = null;

                while ((line = br.readLine()) != null) {
                    // Remove any trailing spaces.
                    line = line.trim();
                    // skip blank lines or comments.
                    if (line.length() == 0 || line.startsWith(COMMENT_DELIM)) {
                        continue;
                    }
                    if (line.length() >= MIN_MASTPIL_LEN) {
                        String ccc = line.substring(CCC_PIL_POS, NNN_PIL_POS);
                        if (ccc.equals(SITE_WILDCARD)) {
                            ccc = localCCC;
                        }
                        String nnn = line.substring(NNN_PIL_POS, XXX_PIL_POS);
                        String xxx;
                        if (line.length() > MAX_MASTPIL_LEN) {
                            // Only take the first 9 characters of the line.
                            // Trim in case there are any internal spaces.
                            xxx = line
                                    .substring(XXX_PIL_POS, MAX_MASTPIL_LEN + 1)
                                    .trim();
                        } else {
                            // Just grab the remainder of the input line.
                            // Its already been trimmed.
                            xxx = line.substring(6);
                        }

                        Map<String, SortedSet<String>> nnnxxx = masterPil
                                .get(ccc);
                        if (nnnxxx == null) {
                            nnnxxx = new HashMap<>();
                            masterPil.put(ccc, nnnxxx);
                        }

                        SortedSet<String> xxxList = nnnxxx.get(nnn);
                        if (xxxList == null) {
                            xxxList = new TreeSet<>();
                            nnnxxx.put(nnn, xxxList);
                        }

                        xxxList.add(xxx);
                    } else {
                        String msg = String.format(
                                "Line [%s] in file %s incorrect", line,
                                fileToParse.getPath());

                        statusHandler.handle(Priority.SIGNIFICANT, msg);
                    }

                } // while
            } catch (IOException | LocalizationException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Error occurred parsing file [" + fileToParse.getPath()
                                + "]",
                        e);
            }
        }
    }

    /**
     * Get an instance of the AFOS browser data model.
     * 
     * @return The AFOS browser data model.
     */
    public static synchronized AfosBrowserModel getInstance() {
        if (instance == null) {
            instance = new AfosBrowserModel();
        }

        return instance;
    }

    /**
     * 
     * @param ccc
     * @param categoryClassName
     * @return
     */
    public SortedSet<String> getFilteredCategoryList(String ccc,
            String categoryClassName) {
        // Get the category map for a site "ccc"
        Map<String, SortedSet<String>> cccCatMap = masterPil.get(ccc);
        SortedSet<String> retList = new TreeSet<>();

        if (cccCatMap != null) {
            // Now iterate the category list for the selected class
            Set<String> categories = getCategoryClassList(categoryClassName);
            if (categories != null) {
                Set<String> setToCheck = null;
                Set<String> setToIterate = null;

                if (categories.size() < cccCatMap.size()) {
                    setToCheck = cccCatMap.keySet();
                    setToIterate = categories;
                } else {
                    setToCheck = categories;
                    setToIterate = cccCatMap.keySet();
                }

                for (String cat : setToIterate) {
                    if (setToCheck.contains(cat)) {
                        retList.add(cat);
                    }
                }
            }
        }
        return retList;
    }

    public SortedSet<String> getUnfilteredCategoryList(String ccc) {
        Map<String, SortedSet<String>> cccCatMap = masterPil.get(ccc);
        SortedSet<String> retList = new TreeSet<>();
        retList.addAll(cccCatMap.keySet());
        return retList;
    }

    /**
     * 
     * @param origin
     * @return
     */
    public SortedSet<String> getOriginNodeList(String origin) {
        AFOS_ORIGIN key = AFOS_ORIGIN.valueOf(origin);

        return originMap.get(key);
    }

    /**
     * Find the origin that corresponds to the given node.
     * 
     * @param node
     * @return
     */
    public AFOS_ORIGIN getDefaultOrigin(String node) {
        AFOS_ORIGIN defaultOrigin = AFOS_ORIGIN.Regional;

        for (Entry<AFOS_ORIGIN, SortedSet<String>> origin : originMap
                .entrySet()) {
            if (origin.getValue().contains(node)) {
                defaultOrigin = origin.getKey();
                break;
            }
        }

        return defaultOrigin;
    }

    /**
     * 
     * @param ccc
     * @param nnn
     * @return
     */
    public SortedSet<String> getDesignatorList(String ccc, String nnn) {
        SortedSet<String> designatorList = null;

        Map<String, SortedSet<String>> catMap = masterPil.get(ccc);
        if (catMap != null) {
            designatorList = catMap.get(nnn);
        }
        return designatorList;
    }

    /**
     * Get the help text for a single node.
     * 
     * @param node
     * @return
     */
    public String getNodeHelp(String node) {
        return cccHelp.get(node);
    }

    public boolean contains(String nnn, String xxx) {
        return contains(localSite, nnn, xxx);
    }

    public boolean contains(String ccc, String nnn, String xxx) {
        boolean rval = false;
        Map<String, SortedSet<String>> catMap = masterPil.get(ccc);
        if (catMap != null && xxx != null) {
            SortedSet<String> desList = catMap.get(nnn);
            if (desList != null) {
                rval = desList.contains(xxx);
            }
        }
        return rval;
    }

    /**
     * Get the help text for a single category.
     * 
     * @param designator
     * @return
     */
    public String getDesignatorHelp(String designator) {
        if (designator.length() < 3) {
            // need to right justify the designator before lookup!
            StringBuilder sb = new StringBuilder(designator.trim());
            while (sb.length() < 3) {
                sb.insert(0, " ");
            }
        }
        return cccHelp.get(designator);
    }

    /**
     * Get the help for a display command
     * 
     * @param display
     *            the display command
     * @return help text
     */
    public String getDisplayHelp(String display) {
        return displayHelp.get(display);
    }

    /**
     * 
     * @param categoryName
     * @return
     */
    private Set<String> getCategoryClassList(String categoryName) {
        Integer key = AFOS_CLASS.getFromClassKey(categoryName).ordinal();

        return categoryClass.get(key + 1);
    }

}
