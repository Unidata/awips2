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
package com.raytheon.uf.common.monitor.scan.config;

import java.io.File;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.Set;

import com.raytheon.uf.common.localization.ILocalizationFile;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.SaveableOutputStream;
import com.raytheon.uf.common.monitor.scan.xml.SCANTrendSetXML;
import com.raytheon.uf.common.monitor.scan.xml.SCANTrendSetsXML;
import com.raytheon.uf.common.serialization.SingleTypeJAXBManager;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;

/**
 *
 * Manages the Trend Sets configuration. This will handle either CELL or DMD
 * trend sets.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 3, 2009  #3039      lvenable    Initial creation
 * Oct 2, 2013   2361      njensen     Use JAXBManager for XML
 * Feb 15, 2016 5244       nabowle     Replace deprecated LocalizationFile methods.
 *                                     Add statusHandler.
 * Aug 09, 2017 6373       tgurney     Move configs to common_static
 *
 * </pre>
 *
 * @author lvenable
 */
public class TrendSetConfigMgr {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(TrendSetConfigMgr.class);

    private static final SingleTypeJAXBManager<SCANTrendSetsXML> jaxb = SingleTypeJAXBManager
            .createWithoutException(SCANTrendSetsXML.class);

    /**
     * Configuration XML.
     */
    private String configXml = null;

    /**
     * Trend sets XML.
     */
    private SCANTrendSetsXML trendSets;

    /**
     * Linked hash map of trend set names and the associated attributes.
     */
    private LinkedHashMap<String, String> trendSetMap;

    /**
     * Constructor.
     *
     * @param defCfgXML
     *            Default configuration XML name.
     */
    public TrendSetConfigMgr(String defCfgXML) {
        this.configXml = defCfgXML;

        init();
    }

    /**
     * Initialize method.
     */
    private void init() {
        trendSetMap = new LinkedHashMap<>();
        readDefaultConfig();

        updateTrendSetHashMap();
    }

    /**
     * Read in the default configuration trend set XML.
     */
    private void readDefaultConfig() {
        try {
            IPathManager pm = PathManagerFactory.getPathManager();
            ILocalizationFile file = pm
                    .getStaticLocalizationFile(getFullConfigFileNameStr());
            try (InputStream is = file.openInputStream()) {
                trendSets = jaxb.unmarshalFromInputStream(is);
            }
        } catch (Exception e) {
            statusHandler.handle(Priority.WARN, "Error reading default config.",
                    e);
        }
    }

    /**
     * Update the trend sets hashmap by re-reading in the data.
     */
    private void updateTrendSetHashMap() {
        trendSetMap.clear();

        ArrayList<SCANTrendSetXML> trendSetArray = trendSets.getTrendSets();

        for (int i = 0; i < trendSetArray.size(); i++) {
            trendSetMap.put(trendSetArray.get(i).getName(),
                    trendSetArray.get(i).getAttributes());
        }
    }

    /**
     * Get the full path/file name of the configuration XML.
     *
     * @return Full file name and path.
     */
    private String getFullConfigFileNameStr() {
        String fs = String.valueOf(File.separatorChar);
        String fileNameStr = "scan" + fs + "config" + fs + "trendSets" + fs
                + configXml;

        return fileNameStr;
    }

    /**
     * Get the trend sets map.
     *
     * @return The trend sets map.
     */
    public final LinkedHashMap<String, String> getTrendSetMap() {
        return trendSetMap;
    }

    /**
     * Get the trend sets configuration XML.
     *
     * @return The trend sets configuration XML.
     */
    public SCANTrendSetsXML getTrendSets() {
        return trendSets;
    }

    /**
     * Save the trend sets.
     */
    public void saveTrendSets() {
        ArrayList<SCANTrendSetXML> updateTrendSetArray = trendSets
                .getTrendSets();
        updateTrendSetArray.clear();

        Set<String> keys = trendSetMap.keySet();

        for (String key : keys) {
            SCANTrendSetXML newTrendSet = new SCANTrendSetXML();
            newTrendSet.setName(key);
            newTrendSet.setAttributes(trendSetMap.get(key));

            updateTrendSetArray.add(newTrendSet);
        }

        trendSets.setTrendSets(updateTrendSetArray);

        /*
         * Generate the array to save from the hashmap.
         */

        IPathManager pm = PathManagerFactory.getPathManager();
        LocalizationContext context = pm.getContext(
                LocalizationType.COMMON_STATIC, LocalizationLevel.SITE);
        ILocalizationFile locFile = pm.getLocalizationFile(context,
                getFullConfigFileNameStr());

        try (SaveableOutputStream sos = locFile.openOutputStream()) {
            jaxb.marshalToStream(trendSets, sos);
            sos.save();
        } catch (Exception e) {
            statusHandler.handle(Priority.WARN, "Error saving trend sets.", e);
        }
    }

    /**
     * Remove a trend set.
     *
     * @param name
     *            Trend set name to remove.
     */
    public void removeTrendSet(String name) {
        trendSetMap.remove(name);
    }

    /**
     * Add/Update a trend set.
     *
     * @param name
     * @param attibute
     */
    public void addUpdateTrendSet(String name, String attibute) {
        trendSetMap.put(name, attibute);
    }

    /**
     * Get a list of trend sets that do not have at least 2 attributes.
     *
     * @return Array on trend set names.
     */
    public String[] getInvalidAttributeNumber() {
        ArrayList<String> invalidTrends = new ArrayList<>();
        Set<String> keys = trendSetMap.keySet();

        for (String key : keys) {
            String[] attrs = trendSetMap.get(key).split(",");

            if (attrs.length < 2) {
                invalidTrends.add(key);
            }
        }

        return invalidTrends.toArray(new String[0]);
    }

    /**
     * Get a string array of attributes for the specified trend set name.
     *
     * @param trendName
     *            Trend set name.
     * @return String array of attributes.
     */
    public String[] getAttributes(String trendName) {
        if (trendSetMap.containsKey(trendName)) {
            String[] attrs = trendSetMap.get(trendName).split(",");
            return attrs;
        }

        return new String[] {};
    }

    /**
     * Get an array on trend set names.
     *
     * @return Array of trend set names.
     */
    public String[] getTrendSetNames() {
        ArrayList<String> trendNames = new ArrayList<>();

        Set<String> keys = trendSetMap.keySet();

        for (String key : keys) {
            trendNames.add(key);
        }

        return trendNames.toArray(new String[0]);
    }

    /**
     * Reload the configuration.
     */
    public void reloadConfiguration() {
        readDefaultConfig();
        updateTrendSetHashMap();
    }
}
