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
package com.raytheon.uf.viz.monitor.scan.config;

import java.awt.Point;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.SortedMap;
import java.util.TreeMap;

import javax.xml.bind.JAXBException;

import com.raytheon.uf.common.localization.ILocalizationFile;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.monitor.scan.config.SCANConfigEnums.ScanThresholdColor;
import com.raytheon.uf.common.monitor.scan.xml.SCANAbstractXML;
import com.raytheon.uf.common.monitor.scan.xml.SCANAttributesXML;
import com.raytheon.uf.common.monitor.scan.xml.SCANConfigCellXML;
import com.raytheon.uf.common.monitor.scan.xml.SCANConfigDmdXML;
import com.raytheon.uf.common.monitor.scan.xml.SCANConfigMesoXML;
import com.raytheon.uf.common.monitor.scan.xml.SCANConfigTvsXML;
import com.raytheon.uf.common.serialization.JAXBManager;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.monitor.config.CommonTableConfig.SortDirection;

/**
 * Abstract class used for common configuration data for the CELL, DMD, MESO,
 * and TVS configuration managers.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 21, 2009 3039       lvenable    Initial creation
 * Apr 25, 2013 1926       njensen     Improved initialization speed
 * Oct 02, 2013 2361       njensen     Use JAXBManager for XML
 * Aug 09, 2017 6373       tgurney     Move config to common_static
 * Mar 15, 2018 6797       mduff       Alarm values are now double so decimal values can be used.
 * Jul 09, 2018 6673       tgurney     Change config loading to incremental override
 * </pre>
 *
 * @author lvenable
 */
public abstract class AbsConfigMgr {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(AbsConfigMgr.class);

    protected static final JAXBManager jaxb = initializeJAXB();

    /**
     * Map of attribute names and SCANAttributesXML.
     */
    protected Map<String, SCANAttributesXML> attrMap;

    /**
     * Map of attribute names and column index.
     */
    protected Map<String, Integer> indexMap;

    /**
     * Default XML name.
     */
    protected String currentConfigFileName = null;

    private static JAXBManager initializeJAXB() {
        JAXBManager retVal = null;
        try {
            retVal = new JAXBManager(SCANConfigCellXML.class,
                    SCANConfigDmdXML.class, SCANConfigMesoXML.class,
                    SCANConfigTvsXML.class);
        } catch (JAXBException e) {
            statusHandler.error(
                    "Error initializing JAXBManager for SCAN configs", e);
        }
        return retVal;
    }

    /**
     * Constructor.
     *
     * @param defCfgXML
     *            Default XML name.
     */
    public AbsConfigMgr() {
        attrMap = new HashMap<>();
        indexMap = new HashMap<>();

        init();
    }

    /**
     * Create a map of attributes data.
     *
     * @param attrArray
     *            Array of attributes to be put into a map.
     */
    protected void createAttributeMap(List<SCANAttributesXML> attrArray) {
        int index = 0;

        attrMap.clear();
        indexMap.clear();

        for (SCANAttributesXML attrXML : attrArray) {
            attrMap.put(attrXML.getAttrName(), attrXML);
            indexMap.put(attrXML.getAttrName(), new Integer(index));
            ++index;
        }
    }

    /**
     * Read in the default configuration.
     *
     * @return Abstract XML that can be cast to a specific SCAN configuration
     *         data.
     */
    public SCANAbstractXML readDefaultConfig() {
        SCANAbstractXML cfgXML = null;

        IPathManager pm = PathManagerFactory.getPathManager();
        SortedMap<LocalizationLevel, LocalizationFile> files = new TreeMap<>(
                pm.getTieredLocalizationFile(LocalizationType.COMMON_STATIC,
                        getFullDefaultConfigName()));

        for (LocalizationFile lf : files.values()) {
            if (lf != null) {
                try (InputStream is = lf.openInputStream()) {
                    SCANAbstractXML tmpCfgXml = null;
                    tmpCfgXml = jaxb.unmarshalFromInputStream(
                            SCANAbstractXML.class, is);
                    if (cfgXML == null) {
                        cfgXML = tmpCfgXml;
                    } else {
                        cfgXML.combine(tmpCfgXml);
                    }
                } catch (Exception e) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Failed to read config file "
                                    + getFullDefaultConfigName(),
                            e);
                }
            }
        }
        return cfgXML;

    }

    /**
     * Read in an existing configuration file.
     *
     * @return The configuration XML data.
     */
    public SCANAbstractXML readExistingConfig() {
        try {
            String newConfigFile = getExistingConfigFilePath();

            SCANAbstractXML defaultConfig = readDefaultConfig();
            if (newConfigFile == null) {
                return defaultConfig;
            }

            SCANAbstractXML cfgXML = null;

            IPathManager pm = PathManagerFactory.getPathManager();
            ILocalizationFile lfile = pm
                    .getStaticLocalizationFile(newConfigFile);

            try (InputStream is = lfile.openInputStream()) {
                cfgXML = jaxb.unmarshalFromInputStream(SCANAbstractXML.class,
                        is);
            }
            defaultConfig.combine(cfgXML);
            return defaultConfig;
        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Failed to read config file " + getExistingConfigFilePath(),
                    e);
            return null;
        }
    }

    /**
     * Get a string array of column names.
     *
     * @return String array of column names.
     */
    public String[] getColumnNames() {
        int size = getAttributes().size();

        String[] colNames = new String[size];

        for (int i = 0; i < size; i++) {
            colNames[i] = getAttributes().get(i).getAttrName();
        }

        return colNames;
    }

    /**
     * Get the sort direction.
     *
     * @param colName
     *            Column/Attribute name.
     * @return Sort direction - SWT.UP for ascending, SWT.DOWN for descending,
     *         or SWT.NONE for no sort.
     */
    public int getSortDirection(String colName) {
        SCANAttributesXML attrXML = attrMap.get(colName);

        if (attrXML != null) {
            if (attrXML.getRank().compareTo("ascending") == 0) {
                return SortDirection.Ascending.getSortDir();
            } else if (attrXML.getRank().compareTo("descending") == 0) {
                return SortDirection.Decending.getSortDir();
            }
        }

        return SortDirection.None.getSortDir();
    }

    /**
     * Get the threshold color.
     *
     * @param attrName
     *            Attribute name.
     * @param value
     *            Value to determine the threshold color.
     * @return The SCAN threshold color enumeration identifier.
     */
    public ScanThresholdColor getThresholdColor(String attrName, double value) {
        if (Double.isNaN(value)) {
            return ScanThresholdColor.Default;
        }

        SCANAttributesXML attrXML = attrMap.get(attrName);

        if (!attrXML.getColored()) {
            return ScanThresholdColor.Default;
        }

        if (value >= attrXML.getUpper()) {
            return ScanThresholdColor.Upper;
        } else if (value >= attrXML.getMid()) {
            return ScanThresholdColor.Mid;
        } else if (value >= attrXML.getLow()) {
            return ScanThresholdColor.Lower;
        }

        return ScanThresholdColor.Default;
    }

    /**
     * Get the column index of the specified attribute name.
     *
     * @param attrName
     * @return
     */
    public int getColumnIndex(String attrName) {
        if (indexMap.containsKey(attrName)) {
            return indexMap.get(attrName);
        }

        return -1;
    }

    /**
     * Get the column/attribute names that can be ranked.
     *
     * @return Array of column/attribute names.
     */
    public String[] getRankColumns() {
        List<String> strArray = new ArrayList<>();
        List<SCANAttributesXML> attrArray = getAttributes();

        for (SCANAttributesXML attrXML : attrArray) {
            if (attrXML.getRank().compareTo("none") != 0) {
                strArray.add(attrXML.getAttrName());
            }
        }

        return strArray.toArray(new String[0]);
    }

    /**
     * Set the columns/attributes on the table to be visible or hidden. The
     * boolean array passed in are the visible states for all of the columns in
     * the table. True indicated visible, false is hidden.
     *
     * @param visibleAttrs
     *            Boolean array of visible states. True is visible, false is
     *            hidden.
     */
    public void setVisibleAttributes(boolean[] visibleAttrs) {
        List<SCANAttributesXML> attrArray = getAttributes();

        for (int i = 0; i < attrArray.size(); i++) {
            attrArray.get(i).setInTable(visibleAttrs[i]);
        }
    }

    /**
     * Get a linked hash map (ordered) of threshold attribute names and the
     * units.
     *
     * @return Linked hash map of attribute names and units.
     */
    public LinkedHashMap<String, String> getThreshAttributes() {
        LinkedHashMap<String, String> attrUnitsMap = new LinkedHashMap<>();
        List<SCANAttributesXML> attrArray = getAttributes();

        for (SCANAttributesXML attrXML : attrArray) {
            if (attrXML.getColored()) {
                attrUnitsMap.put(attrXML.getAttrName(), attrXML.getUnits());
            }
        }

        return attrUnitsMap;
    }

    public LinkedHashMap<String, String> getTimeHeightAttributes() {
        LinkedHashMap<String, String> attrUnitsMap = new LinkedHashMap<>();
        List<SCANAttributesXML> attrArray = getAttributes();

        for (SCANAttributesXML attrXML : attrArray) {
            if (attrXML.getTimeHeight()) {
                attrUnitsMap.put(attrXML.getAttrName(), attrXML.getUnits());
            }
        }

        return attrUnitsMap;
    }

    /**
     * Get the minimum and maximum values for the attribute. The values are
     * stored in a point object. x = min, y = max
     *
     * @param attrName
     *            Attribute name.
     * @return Min/Max in a point.
     */
    public Point getMinMaxValues(String attrName) {
        int min = (int) attrMap.get(attrName).getMin();
        int range = (int) attrMap.get(attrName).getRange();

        if (range > 0) {
            return new Point(min, min + range);
        }

        return null;
    }

    /**
     * Get the upper value for the specified attribute name.
     *
     * @param attrName
     *            Attribute name.
     * @return Upper value.
     */
    public double getUpper(String attrName) {
        return attrMap.get(attrName).getUpper();
    }

    /**
     * Get the mid value for the specified attribute name.
     *
     * @param attrName
     *            Attribute name.
     * @return Mid value.
     */
    public double getMid(String attrName) {
        return attrMap.get(attrName).getMid();
    }

    /**
     * Get the lower value for the specified attribute name.
     *
     * @param attrName
     *            Attribute name.
     * @return Lower value.
     */
    public double getLower(String attrName) {
        return attrMap.get(attrName).getLow();
    }

    /**
     * Set the threshold values.
     *
     * @param attrName
     *            Attribute name.
     * @param upper
     *            Upper value.
     * @param mid
     *            Mid value.
     * @param lower
     *            Lower value.
     */
    public void setThresholds(String attrName, double upper, double mid,
            double lower) {
        if (!indexMap.containsKey(attrName)) {
            return;
        }

        List<SCANAttributesXML> attrArray = getAttributes();

        attrArray.get(indexMap.get(attrName)).setUpper(upper);
        attrArray.get(indexMap.get(attrName)).setMid(mid);
        attrArray.get(indexMap.get(attrName)).setLow(lower);
    }

    /**
     * Get the attributes XML data.
     *
     * @param attrName
     *            Attribute name.
     * @return The attributes XML data.
     */
    public SCANAttributesXML getArrtibuteXML(String attrName) {
        return attrMap.get(attrName);
    }

    /**
     * Get the trend flag indicating if a trend graph can be displayed.
     *
     * @param colName
     *            Column/Attribute name to check against.
     * @return True if a trend graph can be displayed, false otherwise.
     */
    public boolean getTrend(String colName) {
        return attrMap.get(colName).getTrend();
    }

    /**
     * Get the time-height flag indicating if a time-height graph can be
     * displayed.
     *
     * @param colName
     *            Column/Attribute name to check against.
     * @return True if a time-height graph can be displayed, false otherwise.
     */
    public boolean getTimeHeight(String colName) {
        return attrMap.get(colName).getTimeHeight();
    }

    /**
     * Get an array of attribute names that can have an alarm.
     *
     * @return String array of attribute names.
     */
    public String[] getAlarmAttributes() {
        List<String> strArray = new ArrayList<>();
        List<SCANAttributesXML> attrArray = getAttributes();

        for (SCANAttributesXML attrXML : attrArray) {
            if (attrXML.getHasAlarm()) {
                strArray.add(attrXML.getAttrName());
            }
        }

        return strArray.toArray(new String[0]);
    }

    /**
     * Get an array of attribute names that can display trend graphs.
     *
     * @return Array of attribute names.
     */
    public String[] getTrendAttributeNames() {
        List<String> strArray = new ArrayList<>();
        List<SCANAttributesXML> attrArray = getAttributes();

        for (SCANAttributesXML attrXML : attrArray) {
            if (attrXML.getTrend()) {
                strArray.add(attrXML.getAttrName());
            }
        }

        return strArray.toArray(new String[0]);
    }

    /**
     * Get attribute data for the specified trend (if it can display a trend).
     *
     * @param attrName
     *            Attribute name.
     * @return Attribute data.
     */
    public SCANAttributesXML getTrendAttrData(String attrName) {
        if (attrMap.containsKey(attrName)) {
            if (attrMap.get(attrName).getTrend()) {
                return attrMap.get(attrName);
            }
        }

        return null;
    }

    /**
     * Get the absolute alarm value for the specified attribute name.
     *
     * @param attrName
     *            Attribute name.
     * @return Absolute alarm value.
     */
    public double getAbsoluteValue(String attrName) {
        if (attrMap.containsKey(attrName)) {
            return attrMap.get(attrName).getAbsAlarm();
        }
        return 0;
    }

    /**
     * Set the absolute alarm value.
     *
     * @param attrName
     *            Attribute name.
     * @param value
     *            Absolute alarm value.
     */
    public void setAbsoluteValue(String attrName, double value) {
        if (!indexMap.containsKey(attrName)) {
            return;
        }

        List<SCANAttributesXML> attrArray = getAttributes();

        attrArray.get(indexMap.get(attrName)).setAbsAlarm(value);
    }

    /**
     * Get the rate of change value for the specified attribute name.
     *
     * @param attrName
     *            Attribute name.
     * @return Rate of change value.
     */
    public double getRateOfChange(String attrName) {
        if (attrMap.containsKey(attrName)) {
            return attrMap.get(attrName).getAlarm();
        }
        return 0;
    }

    /**
     * Set the rate of change value for the specified attribute name.
     *
     * @param attrName
     *            Attribute name.
     * @param value
     *            Rate of change value.
     */
    public void setRateOfChangeAlarm(String attrName, double value) {
        if (!indexMap.containsKey(attrName)) {
            return;
        }

        List<SCANAttributesXML> attrArray = getAttributes();

        attrArray.get(indexMap.get(attrName)).setAlarm(value);
    }

    /**
     * Get the display unit for the specified attribute name.
     *
     * @param attrName
     *            Attribute name.
     * @return Display unit.
     */
    public String getUnit(String attrName) {
        if (attrMap.containsKey(attrName)) {
            attrMap.get(attrName).getUnits();
        }

        return "";
    }

    /**
     * Check if the current configuration being used is the default
     * configuration.
     *
     * @return True if the current configuration is the default, false
     *         otherwise.
     */
    public boolean currentConfigIsDefault() {
        if (currentConfigFileName.compareTo(getDefaultConfigName()) == 0) {
            return true;
        }

        return false;
    }

    /**
     * Get the file and path of the current configuration.
     *
     * @return The file and path of the current configuration.
     */
    public String getExistingConfigFilePath() {
        if (currentConfigFileName == null) {
            return getFullDefaultConfigName();
        }

        return getConfigPath() + currentConfigFileName;
    }

    /**
     * Save the configuration as a new name.
     *
     * @param newName
     *            New configuration name.
     */
    public void saveConfigAs(String newName) {
        currentConfigFileName = newName;

        saveConfig();
    }

    public String getCurrentConfigFileName() {
        return currentConfigFileName;
    }

    protected abstract void init();

    public abstract String getDefaultConfigName();

    public abstract String getFullDefaultConfigName();

    public abstract String getConfigPath();

    public abstract boolean showTips();

    public abstract void setShowTips(boolean showFlag);

    public abstract void loadNewConfig(String newCfgName);

    public abstract void loadDefaultConfig();

    public abstract void saveConfig();

    public abstract List<SCANAttributesXML> getAttributes();
}
