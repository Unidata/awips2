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
import java.util.LinkedHashMap;
import java.util.List;

import com.raytheon.uf.common.localization.ILocalizationFile;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.SaveableOutputStream;
import com.raytheon.uf.common.monitor.scan.xml.SCANAttributesXML;
import com.raytheon.uf.common.monitor.scan.xml.SCANConfigCellXML;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;

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
 * Aug 09, 2017 6373       tgurney     Move config to common_static
 * Jul 23, 2018 6673       tgurney     Cleanup
 *
 * </pre>
 *
 * @author lvenable
 */
public class CellConfigMgr extends AbsConfigMgr {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(CellConfigMgr.class);

    /**
     * CELL configuration manager XML.
     */
    private SCANConfigCellXML cellCfgXML;

    /**
     * Default configuration file name.
     */
    private static final String defaultConfigFileName = "SCANconfig_cellTable.xml";

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
            statusHandler.info("cellCfgXML is null");
        } else {
            statusHandler.info("--- " + cellCfgXML.getDefaultRank());
        }
    }

    /**
     * Get the attributes XML.
     */
    @Override
    public List<SCANAttributesXML> getAttributes() {
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
                LocalizationType.COMMON_STATIC, LocalizationLevel.SITE);
        String newFileName = getExistingConfigFilePath();
        ILocalizationFile locFile = pm.getLocalizationFile(context,
                newFileName);

        try {
            statusHandler.info("Saving -- " + locFile.getPath());
            try (SaveableOutputStream os = locFile.openOutputStream()) {
                jaxb.marshalToStream(cellCfgXML, os);
                os.save();
            }
        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Failed to save " + getExistingConfigFilePath(), e);
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
        LinkedHashMap<String, String> attrUnitsMap = new LinkedHashMap<>();
        List<SCANAttributesXML> attrArray = getAttributes();

        for (SCANAttributesXML attrXML : attrArray) {
            if (attrXML.getClutter()) {
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
