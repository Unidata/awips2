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

import java.io.File;
import java.util.ArrayList;
import java.util.LinkedHashMap;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.monitor.scan.xml.SCANAttributesXML;
import com.raytheon.uf.common.monitor.scan.xml.SCANConfigCellXML;

/**
 * 
 * Configuration manager for the CELL table.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 2, 2009  #3039      lvenable    Initial creation
 * Oct 2, 2013  2361       njensen     Use JAXBManager for XML
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */
public class CellConfigMgr extends AbsConfigMgr {
    /**
     * CELL configuration manager XML.
     */
    private SCANConfigCellXML cellCfgXML;

    /**
     * Default configuration file name.
     */
    private final String defaultConfigFileName = "SCANconfig_cellTable.xml";

    /**
     * Constructor.
     */
    public CellConfigMgr() {
        super();
    }

    /**
     * Initialize method.
     */
    @Override
    protected void init() {
        currentConfigFileName = defaultConfigFileName;
        loadDefaultConfig();

        if (cellCfgXML == null) {
            System.out.println("cellCfgXML is null");
        } else {
            System.out.println("--- " + cellCfgXML.getDefaultRank());
        }
    }

    /**
     * Get the attributes XML.
     */
    @Override
    public ArrayList<SCANAttributesXML> getAttributes() {
        return cellCfgXML.getAttributesData();
    }

    /**
     * Load the default configuration.
     */
    @Override
    public void loadDefaultConfig() {
        currentConfigFileName = defaultConfigFileName;
        cellCfgXML = (SCANConfigCellXML) readDefaultConfig();
        createAttributeMap(getAttributes());
    }

    /**
     * Load a new configuration.
     */
    @Override
    public void loadNewConfig(String newCfgName) {
        currentConfigFileName = newCfgName;
        cellCfgXML = (SCANConfigCellXML) readExistingConfig();
        createAttributeMap(getAttributes());
    }

    /**
     * Save the current configuration.
     */
    @Override
    public void saveConfig() {
        IPathManager pm = PathManagerFactory.getPathManager();
        LocalizationContext context = pm.getContext(
                LocalizationType.CAVE_STATIC, LocalizationLevel.SITE);
        String newFileName = getExistingConfigFilePath();
        LocalizationFile locFile = pm.getLocalizationFile(context, newFileName);

        if (locFile.getFile().getParentFile().exists() == false) {
            System.out.println("Creating new directory");

            if (locFile.getFile().getParentFile().mkdirs() == false) {
                System.out.println("Could not create new directory...");
            }
        }

        try {
            System.out.println("Saving -- "
                    + locFile.getFile().getAbsolutePath());
            jaxb.marshalToXmlFile(cellCfgXML, locFile.getFile()
                    .getAbsolutePath());
            locFile.save();
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    /**
     * Check if the tips should be shown.
     */
    @Override
    public boolean showTips() {
        return cellCfgXML.getTipsOption();
    }

    /**
     * Set the show tips flag.
     */
    @Override
    public void setShowTips(boolean showFlag) {
        cellCfgXML.setTipsOption(showFlag);
    }

    /**
     * Get the path to the configuration files.
     */
    @Override
    public String getConfigPath() {
        String fs = String.valueOf(File.separatorChar);
        StringBuilder sb = new StringBuilder();

        sb.append("scan").append(fs);
        sb.append("config").append(fs);
        sb.append("cellTableConfig").append(fs);

        return sb.toString();
    }

    /**
     * Get the full default configuration file name.
     */
    @Override
    public String getFullDefaultConfigName() {
        return getConfigPath() + defaultConfigFileName;
    }

    /**
     * Get the default configuration name.
     */
    @Override
    public String getDefaultConfigName() {
        return defaultConfigFileName;
    }

    /**
     * Get the clutter control attribute name.
     * 
     * @return The clutter control attribute name.
     */
    public String getClutterControl() {
        return cellCfgXML.getClutterControl();
    }

    /**
     * Get the radius interpolation.
     * 
     * @return The radius interpolation.
     */
    public String getRadVar() {
        return cellCfgXML.getRadVar();
    }

    /**
     * Get a linked map of clutter control attribute and the associated units.
     * 
     * @return A linked map of clutter control attribute and the associated
     *         units.
     */
    public LinkedHashMap<String, String> getClutterAttributes() {
        LinkedHashMap<String, String> attrUnitsMap = new LinkedHashMap<String, String>();
        ArrayList<SCANAttributesXML> attrArray = getAttributes();

        for (SCANAttributesXML attrXML : attrArray) {
            if (attrXML.getClutter() == true) {
                attrUnitsMap.put(attrXML.getAttrName(), attrXML.getUnits());
            }
        }

        return attrUnitsMap;
    }

    /**
     * Get the SCAN CELL configuration data.
     * 
     * @return SCAN CELL configuration data.
     */
    public SCANConfigCellXML getScanCellCfgXML() {
        return cellCfgXML;
    }

    public void setAlarmsDisabled(boolean flag) {
        cellCfgXML.setAlarmsDisabled(flag);
    }

    public boolean getAlarmsDisabled() {
        return cellCfgXML.getAlarmsDisabled();
    }

    public void setAlarmBell(boolean flag) {
        cellCfgXML.setAlarmBell(flag);
    }

    public boolean getAlarmBell() {
        return cellCfgXML.getAlarmBell();
    }
}
