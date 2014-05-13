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

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.monitor.scan.xml.SCANAttributesXML;
import com.raytheon.uf.common.monitor.scan.xml.SCANConfigTvsXML;

/**
 * 
 * Configuration manager for the TVS table.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 3, 2009  #3039      lvenable    Initial creation
 * Oct 2, 2013  2361       njensen     Use JAXBManager for XML
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */
public class TvsConfigMgr extends AbsConfigMgr {
    /**
     * TVS configuration manager XML.
     */
    private SCANConfigTvsXML tvsCfgXML;

    /**
     * Default configuration file name.
     */
    private final String defaultConfigFileName = "SCANconfig_tvsTable.xml";

    /**
     * Constructor.
     */
    public TvsConfigMgr() {
        super();
    }

    /**
     * Initialize method.
     */
    @Override
    protected void init() {
        currentConfigFileName = defaultConfigFileName;
        loadDefaultConfig();
    }

    /**
     * Get the attributes XML.
     */
    @Override
    public ArrayList<SCANAttributesXML> getAttributes() {
        return tvsCfgXML.getAttributesData();
    }

    /**
     * Load the default configuration.
     */
    @Override
    public void loadDefaultConfig() {
        currentConfigFileName = defaultConfigFileName;
        tvsCfgXML = (SCANConfigTvsXML) readDefaultConfig();
        createAttributeMap(getAttributes());
    }

    /**
     * Load a new configuration.
     */
    @Override
    public void loadNewConfig(String newCfgName) {
        currentConfigFileName = newCfgName;
        tvsCfgXML = (SCANConfigTvsXML) readExistingConfig();
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
            System.out.println("TVS - Creating new directory");

            if (locFile.getFile().getParentFile().mkdirs() == false) {
                System.out.println("TVS - Could not create new directory...");
            }
        }

        try {
            System.out.println("Saving -- "
                    + locFile.getFile().getAbsolutePath());
            jaxb.marshalToXmlFile(tvsCfgXML, locFile.getFile()
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
        return tvsCfgXML.getTipsOption();
    }

    /**
     * Set the show tips flag.
     */
    @Override
    public void setShowTips(boolean showFlag) {
        tvsCfgXML.setTipsOption(showFlag);
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
        sb.append("tvsTableConfig").append(fs);

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
     * Get the SCAN TVS configuration data.
     * 
     * @return SCAN TVS configuration data.
     */
    public SCANConfigTvsXML getScanTvsCfgXML() {
        return tvsCfgXML;
    }
}
