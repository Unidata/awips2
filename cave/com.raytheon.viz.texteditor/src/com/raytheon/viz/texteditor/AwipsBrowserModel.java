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
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.localization.LocalizationManager;
import com.raytheon.viz.texteditor.util.AFOS_CLASS;

/**
 * Singleton class that contains information related to querying for or updating
 * of text products as well as listing menu contents in the Awips Browser.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * Aug 10, 2015 4716        rferrel     Initial creation.
 * Nov 02, 2016 5975        rferrel     Designator help now uses cccHelp map.
 * </pre>
 * 
 * @author rferrel
 * @version 1.0
 */
public final class AwipsBrowserModel extends AbstractBrowserModel {
    // Need at least this many characters in an awipsMasterPIL entry
    // Need the NNN and at least a 1 character XXX
    private static final int MIN_MASTPIL_LEN = 4;

    // but no more than 6 characters.
    private static final int MAX_MASTPIL_LEN = 6;

    private static final int NNN_PIL_POS = 0;

    private static final int XXX_PIL_POS = 3;

    private static final String AWIPS_MASTER_PIL = "textdb/awipsMasterPIL.txt";

    /**
     * The static singleton instance.
     */
    private static AwipsBrowserModel instance;

    /**
     * Associates a category class to the list of nnn that are associated with
     * it. Only used for internal indexing, no need to be sorted.
     */
    private final Map<Integer, Set<String>> categoryClass;

    private final Map<String, SortedSet<String>> awipsMasterPil;

    /**
     * Private constructor: Use getInstance().
     */
    private AwipsBrowserModel() {
        super();
        this.categoryClass = new HashMap<>();
        this.awipsMasterPil = new HashMap<>();
        setup();
        cleanup();
    }

    @Override
    protected void setup() {
        super.setup();
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
                        nnnList = new TreeSet<>();
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
            parseAwipsMasterPil(
                    pathManager.getLocalizationFile(lc, AWIPS_MASTER_PIL));
        }
        addRadarToMasterPil();
    }

    /**
     * Adds radar NNNXXX codes to the awips master pil archive so that they can
     * be found via the AWIPS browser.
     */
    private void addRadarToMasterPil() {
        String curSite = LocalizationManager.getInstance().getCurrentSite();
        List<String> radarIDList = RadarsInUseUtil.getSite(curSite,
                RadarsInUseUtil.ARSR_CONSTANT);
        radarIDList.addAll(
                RadarsInUseUtil.getSite(curSite, RadarsInUseUtil.ASR_CONSTANT));
        radarIDList.addAll(RadarsInUseUtil.getSite(curSite,
                RadarsInUseUtil.MOSAIC_CONSTANT));
        List<String> radarIDListTrunc = new ArrayList<>();
        List<String> radarList = RadarTextProductUtil.getRadarTableEntries();
        List<String> radarTypeList = new ArrayList<>();

        /*
         * Preprocessing: Must strip the "WSR" off the front of each entry in
         * radarTypeList.
         */
        for (String radar : radarList) {
            radarTypeList.add(radar.substring(3).toUpperCase());
        }

        /*
         * Preprocessing: Must get the radar ID list entries down to 3 letter
         * codes.
         */
        for (String radarID : radarIDList) {
            radarIDListTrunc.add(radarID.substring(1).toUpperCase());
        }

        for (String nnn : radarTypeList) {
            SortedSet<String> xxxList = awipsMasterPil.get(nnn);
            if (xxxList == null) {
                xxxList = new TreeSet<>();
                awipsMasterPil.put(nnn, xxxList);
            }

            xxxList.addAll(radarIDListTrunc);
            if ("RCM".equals(nnn)) {
                xxxList.remove("OKC");
            }
        }

    }

    /**
     * Read and parse an awips PIL list. In the event of processing multiple
     * files, the most recent entry overwrites a current entry.
     * 
     * @param fileToParse
     *            File reference containing the PIL list.
     */
    private void parseAwipsMasterPil(ILocalizationFile fileToParse) {
        if (fileToParse != null && fileToParse.exists()) {

            try (InputStream in = fileToParse.openInputStream();
                    BufferedReader br = new BufferedReader(
                            new InputStreamReader(in));) {

                String line = null;
                while ((line = br.readLine()) != null) {
                    // Remove any trailing spaces.
                    line = line.trim();
                    // skip blank lines or comments.
                    if ((line.length() == 0)
                            || line.startsWith(COMMENT_DELIM)) {
                        continue;
                    }
                    if (line.length() >= MIN_MASTPIL_LEN) {
                        String nnn = line.substring(NNN_PIL_POS, XXX_PIL_POS);
                        String xxx = null;
                        if (line.length() > MAX_MASTPIL_LEN) {
                            // Only take the first 6 characters of the line.
                            // Trim in case there are any internal spaces.
                            xxx = line
                                    .substring(XXX_PIL_POS, MAX_MASTPIL_LEN + 1)
                                    .trim();
                        } else {
                            // Just grab the remainder of the input line.
                            // Its already been trimmed.
                            xxx = line.substring(XXX_PIL_POS);
                        }

                        SortedSet<String> xxxList = awipsMasterPil.get(nnn);
                        if (xxxList == null) {
                            xxxList = new TreeSet<>();
                            awipsMasterPil.put(nnn, xxxList);
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
     * Get an instance of the AWIPS browser data model.
     * 
     * @return The AWIPS browser data model.
     */
    public static synchronized AwipsBrowserModel getInstance() {
        if (instance == null) {
            instance = new AwipsBrowserModel();
        }

        return instance;
    }

    /**
     * 
     * @param ccc
     * @param categoryClassName
     * @return
     */
    public SortedSet<String> getFilteredCategoryList(String categoryClassName) {
        SortedSet<String> retList = new TreeSet<>();

        Set<String> categories = getCategoryClassList(categoryClassName);
        if (categories != null) {
            retList.addAll(categories);
        }
        return retList;
    }

    /**
     * 
     * @param nnn
     * @return
     */
    public SortedSet<String> getDesignatorList(String nnn) {
        SortedSet<String> designatorList = null;

        Set<String> destSet = awipsMasterPil.get(nnn);
        if ((destSet != null) && !destSet.isEmpty()) {
            designatorList = new TreeSet<>(destSet);
        }
        return designatorList;
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
     * 
     * @param categoryName
     * @return
     */
    private Set<String> getCategoryClassList(String categoryName) {
        Integer key = AFOS_CLASS.getFromClassKey(categoryName).ordinal();

        return categoryClass.get(key + 1);
    }
}
