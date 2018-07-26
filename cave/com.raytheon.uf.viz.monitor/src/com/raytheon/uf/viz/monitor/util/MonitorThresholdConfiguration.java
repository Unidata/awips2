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
package com.raytheon.uf.viz.monitor.util;

import java.io.File;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.NoSuchElementException;

import org.apache.commons.configuration.CombinedConfiguration;
import org.apache.commons.configuration.ConfigurationException;
import org.apache.commons.configuration.XMLConfiguration;
import org.apache.commons.configuration.tree.OverrideCombiner;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.osgi.framework.Bundle;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.exception.LocalizationException;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 16, 2009 2076       avarani     Initial creation
 * 
 * </pre>
 * 
 * @author avarani
 * @version 1.0
 */
public class MonitorThresholdConfiguration {

    private CombinedConfiguration combinedConfiguration;

    private XMLConfiguration userConfiguration;

    private XMLConfiguration siteConfiguration;

    private XMLConfiguration baseConfiguration;

    private XMLConfiguration configFile;

    private String userFile;

    private String siteFile;

    private String baseFile;

    private static final String USER_FILE = "Files.userFile";

    private static final String SITE_FILE = "Files.siteFile";

    private static final String BASE_FILE = "Files.baseFile";

    // Safe Seas threshold configuration filename
    public static final String SAFESEAS_THRESHOLD_CONFIG = "SSconfig.xml";

    // Snow threshold configuration filename
    public static final String SNOW_THRESHOLD_CONFIG = "SNOWconfig.xml";

    // Fog Monitor threshold configuration filename
    public static final String FOG_THRESHOLD_CONFIG = "FMconfig.xml";

    // Default area identifier
    public static final String DEFAULT_AREA_ID = "Default";

    public static String EMPTY_CONFIGURATION = "<?xml version=\"1.0\" encoding=\"ISO-8859-1\" standalone=\"no\"?>\n"
            + "<configuration>\n" + "</configuration>\n";

    /**
     * Constructor
     * 
     * @param configFileName
     */
    public MonitorThresholdConfiguration(String configFileName) {
        try {
            configFile = new XMLConfiguration(configFileName);
            baseFile = configFile.getString(BASE_FILE);
            siteFile = configFile.getString(SITE_FILE);
            userFile = configFile.getString(USER_FILE);
            reload();
        } catch (ConfigurationException e) {
            // TODO Auto-generated catch block
            System.out.println(e.getStackTrace());
        }
    }

    /**
     * Constructor
     * 
     * @param configFileName
     * @param context
     * @param userScope
     * @throws LocalizationException
     */
    public MonitorThresholdConfiguration(AbstractUIPlugin activator,
            String configFileName, String context, String siteScope,
            String userScope) {
        Bundle bundle = activator.getBundle();

        try {

            configFile = new XMLConfiguration(configFileName);
            this.baseFile = configFile.getString(BASE_FILE);
            this.siteFile = configFile.getString(SITE_FILE);
            this.userFile = configFile.getString(USER_FILE);

            IPathManager pm = PathManagerFactory.getPathManager();
            LocalizationContext userCtx = pm.getContext(
                    LocalizationType.CAVE_STATIC, LocalizationLevel.USER);
            userCtx.setContextName(userScope);

            LocalizationContext siteCtx = pm.getContext(
                    LocalizationType.CAVE_STATIC, LocalizationLevel.SITE);
            siteCtx.setContextName(siteScope);

            LocalizationContext baseCtx = pm.getContext(
                    LocalizationType.CAVE_STATIC, LocalizationLevel.BASE);

            bundle = activator.getBundle();
            if (bundle == null) {
                throw new LocalizationException("Bundle is null");
            }

            this.userFile = pm.getFile(userCtx,
                    context + File.separator + configFile.getString(USER_FILE))
                    .getAbsolutePath();
            this.siteFile = pm.getFile(siteCtx,
                    context + File.separator + configFile.getString(SITE_FILE))
                    .getAbsolutePath();
            System.out.println(context + File.separator
                    + configFile.getString(BASE_FILE));

            String configFileParent = configFile.getFile().getParent(); // XXconfig.xml
                                                                        // and
                                                                        // XXThresholds.xml
                                                                        // are
                                                                        // in
                                                                        // the
                                                                        // same
                                                                        // directory
            this.baseFile = pm.getFile(
                    baseCtx,
                    configFileParent.substring(configFileParent
                            .lastIndexOf(File.separator))
                            + File.separator
                            + configFile.getString(BASE_FILE))
                    .getAbsolutePath();

            // Create empty files if necessary
            setupEmptyFile(new File(this.userFile));
            setupEmptyFile(new File(this.siteFile));
            // Reload the configuration
            reload();
        } catch (ConfigurationException e) {
            // TODO Auto-generated catch block
        } catch (LocalizationException e) {
            // TODO Auto-generated catch block
        }
    }

    public void reload() {
        try {
            baseConfiguration = new XMLConfiguration(baseFile);
            siteConfiguration = new XMLConfiguration(siteFile);
            userConfiguration = new XMLConfiguration(userFile);

            OverrideCombiner combiner = new OverrideCombiner();

            combinedConfiguration = new CombinedConfiguration(combiner);
            combinedConfiguration.setForceReloadCheck(true);

            combinedConfiguration.addConfiguration(userConfiguration);
            combinedConfiguration.addConfiguration(siteConfiguration);
            combinedConfiguration.addConfiguration(baseConfiguration);

        } catch (ConfigurationException e) {
            // TODO Auto-generated catch block
        }
    }

    /**
     * Set up an empty configuration file if one does not exist
     * 
     * @param siteStr
     */
    private void setupEmptyFile(File file) {
        if (!file.exists()) {
            File parent = file.getParentFile();
            if (!parent.exists()) {
                parent.mkdirs();
            }

            PrintWriter pw = null;
            try {
                pw = new PrintWriter(file);
                pw.write(EMPTY_CONFIGURATION);

            } catch (IOException e) {
                // ignore
            } finally {
                if (pw != null) {
                    pw.close();
                }
            }
        }
    }

    /**
     * Set the userFile filename.
     * 
     * @param filename
     */
    public void setUserFile(String filename) {
        this.userFile = filename;
    }

    /**
     * Set the siteFile filename.
     * 
     * @param filename
     */
    public void setSiteFile(String filename) {
        this.siteFile = filename;
    }

    /**
     * Set the baseFile filename.
     * 
     * @param filename
     */
    public void setBaseFile(String filename) {
        this.baseFile = filename;
    }

    /**
     * Save the current userFile filename as the default userFile.
     */
    public void setUserFileAsDefault() {
        try {
            configFile.setProperty("Files.userFile", this.userFile);
            configFile.save();
        } catch (ConfigurationException e) {
            // TODO Auto-generated catch block
        }
    }

    /**
     * Save the current siteFile filename as the default siteFile.
     */
    public void setSiteFileAsDefault() {
        try {
            configFile.setProperty("Files.siteFile", this.siteFile);
            configFile.save();
        } catch (ConfigurationException e) {
            // TODO Auto-generated catch block
        }
    }

    /**
     * Save the current baseFile filename as the default baseFile.
     */
    public void setBaseFileAsDefault() {
        try {
            configFile.setProperty("Files.baseFile", this.baseFile);
            configFile.save();
        } catch (ConfigurationException e) {
            // TODO Auto-generated catch block
        }
    }

    /**
     * Save the user configuration with the default file name.
     */
    public void save() {
        save(userFile, true);
    }

    /**
     * Save the configuration with the default file name.
     * 
     * @param userConfig
     *            Specifies which configuration to save: true saves the user
     *            configuration and false saves the site configuration.
     */
    public void save(boolean userConfig) {
        if (userConfig) {
            save(userFile, userConfig);
        } else {
            save(siteFile, userConfig);
        }
    }

    /**
     * Save the user configuration to the specified file.
     * 
     * @param filename
     *            File name to use when saving the configuration.
     */
    public void save(String filename) {
        save(filename, true);
    }

    /**
     * Save the configuration to the specified file.
     * 
     * @param filename
     *            File name to use when saving the configuration.
     * @param userConfig
     *            Specifies which configuration to save: true saves the user
     *            configuration and false saves the site configuration.
     */
    public void save(String filename, boolean userConfig) {
        try {
            if (userConfig) {
                userConfiguration.save(filename);
            } else {
                siteConfiguration.save(filename);
            }
        } catch (ConfigurationException e) {
            // TODO: Add error handling.
        }
    }

    /**
     * Get a point object containing the red and yellow threshold data for the
     * specified Area ID and path.
     * 
     * @param areaID
     *            Area ID
     * @param path
     *            Path
     * 
     * @return RYData object
     */
    public RYData getValue(String areaID, String path) {

        String key = "zone." + areaID + "." + path;
        String r = combinedConfiguration.getString(key + ".red");
        String y = combinedConfiguration.getString(key + ".yellow");

        if (r == null || y == null) {
            key = "zone.Default." + path;
            r = combinedConfiguration.getString(key + ".red");
            y = combinedConfiguration.getString(key + ".yellow");
        }

        return new RYData(r, y);
    }

    /**
     * Get a point object containing the default red and yellow threshold data
     * for the specified path.
     * 
     * @param path
     *            Path
     * 
     * @return RYData object
     */
    public RYData getDefaultValue(String path) {
        String r, y;
        String key = "zone.Default." + path;

        r = baseConfiguration.getString(key + ".red");
        y = baseConfiguration.getString(key + ".yellow");

        return new RYData(r, y);
    }

    /**
     * Sets the threshold for the specified Area ID and path.
     * 
     * @param areaID
     *            Area ID
     * @param path
     *            Path
     * @param p
     *            RYData object
     */
    public void setUserValue(String areaID, String path, RYData p) {
        boolean pathExists;
        String key = "zone." + areaID + "." + path;

        try {
            userConfiguration.getInt(key + ".red");
            pathExists = true;
        } catch (NoSuchElementException e) {
            pathExists = false;
        }

        if (pathExists) {
            userConfiguration.setProperty(key + ".red", p.getRValue());
            userConfiguration.setProperty(key + ".yellow", p.getYValue());
        } else {
            userConfiguration.addProperty(key + ".red", p.getRValue());
            userConfiguration.addProperty(key + ".yellow", p.getYValue());
        }
    }
}
