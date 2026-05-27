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
import java.util.List;

import com.raytheon.uf.common.localization.ILocalizationFile;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.SaveableOutputStream;
import com.raytheon.uf.common.monitor.scan.xml.SCANAttributesXML;
import com.raytheon.uf.common.monitor.scan.xml.SCANConfigMesoXML;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;

/**
 *
 * Configuration manager for the MESO table.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 3, 2009  #3039      lvenable    Initial creation
 * Oct 2, 2013  2361       njensen     Use JAXBManager for XML
 * Aug 09, 2017 6373       tgurney     Move config to common_static
 * Jul 23, 2018 6673       tgurney     Cleanup
 *
 * </pre>
 *
 * @author lvenable
 */
public class MesoConfigMgr extends AbsConfigMgr {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(MesoConfigMgr.class);

    /**
     * MESO configuration manager XML.
     */
    private SCANConfigMesoXML mesoCfgXML;

    /**
     * Default configuration file name.
     */
    private static final String defaultConfigFileName = "SCANconfig_mesoTable.xml";

    /**
     * Constructor.
     */
    public MesoConfigMgr() {
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
    public List<SCANAttributesXML> getAttributes() {
        return mesoCfgXML.getAttributesData();
    }

    /**
     * Load the default configuration.
     */
    @Override
    public void loadDefaultConfig() {
        currentConfigFileName = defaultConfigFileName;
        mesoCfgXML = (SCANConfigMesoXML) readDefaultConfig();
        createAttributeMap(getAttributes());
    }

    /**
     * Load a new configuration.
     */
    @Override
    public void loadNewConfig(String newCfgName) {
        currentConfigFileName = newCfgName;
        mesoCfgXML = (SCANConfigMesoXML) readExistingConfig();
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
                jaxb.marshalToStream(mesoCfgXML, os);
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
        return mesoCfgXML.getTipsOption();
    }

    /**
     * Set the show tips flag.
     */
    @Override
    public void setShowTips(boolean showFlag) {
        mesoCfgXML.setTipsOption(showFlag);
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
        sb.append("mesoTableConfig").append(fs);

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
     * Get the SCAN MESO configuration data.
     *
     * @return SCAN MESO configuration data.
     */
    public SCANConfigMesoXML getScanMesoCfgXML() {
        return mesoCfgXML;
    }
}
