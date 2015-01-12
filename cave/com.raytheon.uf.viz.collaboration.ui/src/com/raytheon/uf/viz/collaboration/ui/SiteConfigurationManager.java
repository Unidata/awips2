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
package com.raytheon.uf.viz.collaboration.ui;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;

import javax.xml.bind.DataBindingException;
import javax.xml.bind.JAXB;
import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;

import org.apache.commons.collections.map.LRUMap;

import com.raytheon.uf.common.localization.FileUpdatedMessage;
import com.raytheon.uf.common.localization.ILocalizationFileObserver;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.LocalizationFileOutputStream;
import com.raytheon.uf.common.localization.PathManager;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.exception.LocalizationException;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.collaboration.comm.identity.info.HostConfig;
import com.raytheon.uf.viz.collaboration.comm.identity.info.SiteConfig;
import com.raytheon.uf.viz.collaboration.comm.identity.info.SiteConfig.ListEntry;
import com.raytheon.uf.viz.collaboration.comm.identity.info.SiteConfig.ListType;
import com.raytheon.uf.viz.collaboration.comm.identity.info.SiteConfigInformation;

/**
 * Parse a file to grab attributes about a user
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 12, 2012            mnash       Initial creation
 * Jan 08, 2014 2563       bclement    duplicate code elimination
 *                                     added methods to partially modify user config
 * Jan 27, 2014 2700       bclement    fixed null list from jaxb object
 * Oct 10, 2014 3708       bclement    refactored to support blacklisting
 *                                      moved color config to SiteColorConfigManager
 *                                      site level now combined with site config
 * Oct 10, 2014 3708       bclement    fixed possible NPE in getSiteConfig() getSiteVisibilityConfig()
 * 
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */
public class SiteConfigurationManager {

    private static final IUFStatusHandler log = UFStatus
            .getHandler(SiteConfigurationManager.class);

    private static final String PRIMARY_CONF_FILE = "config.xml";

    private static SiteConfigInformation _siteInstance;

    private static LocalizationFile siteConfigInfoFile;

    private static SiteConfigInformation _userInstance;

    private static LocalizationFile userConfigInfoFile;

    @SuppressWarnings("unchecked")
    private static final Map<String, SiteVisiblityConfig> siteVisibilityMap = new LRUMap(
            2);

    private static final ILocalizationFileObserver siteConfigObserver = new ILocalizationFileObserver() {
        @Override
        public void fileUpdated(FileUpdatedMessage message) {
            clear();
        }
    };

    // level hierarchy for site config
    private static final LocalizationLevel[] siteLevels;

    static {
        // load site levels with all levels except for USER
        PathManager pm = (PathManager) PathManagerFactory.getPathManager();
        LocalizationLevel[] available = pm.getAvailableLevels();
        List<LocalizationLevel> levels = new ArrayList<LocalizationLevel>(
                available.length - 1);
        for (int i = available.length - 1; i >= 0; --i) {
            if (!available[i].equals(LocalizationLevel.USER)) {
                levels.add(available[i]);
            }
        }
        siteLevels = levels.toArray(new LocalizationLevel[levels.size()]);
    }

    /**
     * Get first localization file for SiteConfigInformation found in levels
     * 
     * @param filename
     *            config file name
     * @param levels
     *            localization levels to search in order
     * @return null if none found
     */
    private static LocalizationFile findConfigLocalizationFile(String filename,
            LocalizationLevel[] levels) {
        LocalizationFile rval = null;
        for (LocalizationLevel level : levels) {
            rval = getConfigLocalizationFile(filename, level);
            if (rval != null && rval.exists()) {
                break;
            }
        }
        return rval;
    }

    /**
     * Get LocalizationFile for SiteConfigInformation at specified level.
     * 
     * @param filename
     *            config file name
     * @param level
     * @return
     */
    private static LocalizationFile getConfigLocalizationFile(String filename,
            LocalizationLevel level) {
        PathManager pm = (PathManager) PathManagerFactory.getPathManager();
        LocalizationContext localContext = pm.getContext(
                LocalizationType.CAVE_STATIC, level);
        return pm.getLocalizationFile(localContext, "collaboration"
                + File.separator + filename);
    }

    /**
     * Read configuration file from file system. Must be externally
     * synchronized.
     * 
     * @return null if configuration file wasn't found or an error occurred
     */
    private static SiteConfigInformation readConfigInformation(
            LocalizationFile file) {
        SiteConfigInformation rval = null;
        if (file != null && file.exists()) {
            InputStream in = null;
            try {
                in = file.openInputStream();
                rval = JAXB.unmarshal(in, SiteConfigInformation.class);
            } catch (DataBindingException e) {
                log.error("Unable to parse collaboration configuration file '"
                        + file.getName() + "' at localization level '"
                        + file.getContext().getLocalizationLevel() + "'", e);
            } catch (LocalizationException e) {
                // unlikely to happen since we checked that the file exists
                log.error(e.getLocalizedMessage(), e);
            } finally {
                if (in != null) {
                    try {
                        in.close();
                    } catch (IOException e) {
                        log.error(e.getLocalizedMessage(), e);
                    }
                }
            }
        }
        return rval;
    }

    /**
     * Write configuration out to specified localization level. Must be
     * externally synchronized.
     * 
     * @param level
     * @param config
     * @throws JAXBException
     * @throws LocalizationException
     */
    private static void writeConfigInformation(LocalizationLevel level,
            SiteConfigInformation config) throws JAXBException,
            LocalizationException {
        LocalizationFile file = getConfigLocalizationFile(PRIMARY_CONF_FILE,
                level);
        LocalizationFileOutputStream out = null;
        try {
            out = file.openOutputStream();
            JAXBContext context = JAXBContext
                    .newInstance(SiteConfigInformation.class);
            Marshaller marshaller = context.createMarshaller();
            marshaller.setProperty(Marshaller.JAXB_FORMATTED_OUTPUT,
                    new Boolean(true));
            marshaller.marshal(config, out);
        } finally {
            if (out != null) {
                try {
                    out.closeAndSave();
                } catch (IOException e) {
                    log.error(e.getLocalizedMessage(), e);
                }
            }
        }
    }

    /**
     * Read site level configuration file
     * 
     * @return null if no file was found or file is invalid
     */
    private static SiteConfigInformation getSiteConfigInformation() {
        if (_siteInstance == null) {
            LocalizationFile file = findConfigLocalizationFile(
                    PRIMARY_CONF_FILE, siteLevels);
            SiteConfigInformation info = readConfigInformation(file);
            if (isValid(info)) {
                _siteInstance = info;
                file.addFileUpdatedObserver(siteConfigObserver);
                siteConfigInfoFile = file;
            } else {
                log.handle(Priority.PROBLEM,
                        "Misconfigured config.xml file at site level. "
                                + "Does not contain required "
                                + SiteConfigInformation.SITE_NAME + " and "
                                + SiteConfigInformation.ROLE_NAME
                                + " attributes.");
            }
        }
        return _siteInstance;
    }

    /**
     * Read user level configuration file. Creates new file if not found.
     * 
     * @return
     */
    private static SiteConfigInformation getUserConfigInformation() {
        if (_userInstance == null) {
            LocalizationFile file = getConfigLocalizationFile(
                    PRIMARY_CONF_FILE, LocalizationLevel.USER);
            SiteConfigInformation info = readConfigInformation(file);
            if (info == null) {
                // user config doesn't exist, create a new one
                info = new SiteConfigInformation();
                file.addFileUpdatedObserver(siteConfigObserver);
                userConfigInfoFile = file;
            }
            _userInstance = info;
        }
        return _userInstance;
    }

    /**
     * Overwrite user enabled sites list. Persists to localization file.
     * 
     * @param userEnabledSites
     */
    private static void writeUserConfiguredVisibility(
            SiteVisiblityConfig visibility) {
        try {
            SiteConfigInformation userInstance = getUserConfigInformation();
            Map<String, ListEntry> userSpecificConfigs = visibility
                    .getUserSpecificConfigs();
            if (!userSpecificConfigs.isEmpty()) {
                String site = visibility.getActingSite();
                SiteConfig siteConfig = getSiteConfig(userInstance, site);
                if (siteConfig == null) {
                    siteConfig = new SiteConfig();
                    siteConfig.setSite(site);
                    List<SiteConfig> configs = userInstance.getConfig();
                    if (configs == null) {
                        configs = new ArrayList<>(1);
                        userInstance.setConfig(configs);
                    }
                    configs.add(siteConfig);
                }
                ListEntry[] entries = userSpecificConfigs.values().toArray(
                        new ListEntry[0]);
                siteConfig.setListEntries(entries);

                // update on file system
                writeConfigInformation(LocalizationLevel.USER, userInstance);
            }
        } catch (Exception e) {
            log.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
        }
    }

    /**
     * Add host configuration specific to this user. Persists to localization
     * file.
     * 
     * @param config
     */
    synchronized public static void addUserHostConfig(HostConfig config) {
        updateUserHostConfig(config, false);
    }

    /**
     * Update user level host overrides
     * 
     * @param config
     * @param remove
     *            true if the host entry specifies that the user doesn't want to
     *            see the host in the UI
     */
    private static void updateUserHostConfig(HostConfig config, boolean remove) {
        try {
            SiteConfigInformation userInstance = getUserConfigInformation();
            List<HostConfig> servers = userInstance.getServer();
            if (servers == null) {
                servers = new ArrayList<HostConfig>(1);
                userInstance.setServer(servers);
            }
            HostConfig existing = findHost(servers, config);
            if (existing == null) {
                config.setRemoved(remove);
                servers.add(config);
                writeConfigInformation(LocalizationLevel.USER, userInstance);
            } else if (existing.isRemoved() != remove) {
                existing.setRemoved(remove);
                existing.setPrettyName(config.getPrettyName());
                writeConfigInformation(LocalizationLevel.USER, userInstance);
            }
        } catch (Exception e) {
            log.error(e.getLocalizedMessage(), e);
        }
    }

    /**
     * @param servers
     * @param config
     * @return null if matching host config not found
     */
    private static HostConfig findHost(List<HostConfig> servers,
            HostConfig config) {
        for (HostConfig hc : servers) {
            if (hc.getHostname().equalsIgnoreCase(config.getHostname())) {
                return hc;
            }
        }
        return null;
    }

    /**
     * Remove any host configuration that has serverAddress from current user's
     * localization file.
     * 
     * @param serverAddress
     */
    public static void removeUserHostConfig(String serverAddress) {
        updateUserHostConfig(new HostConfig(serverAddress), true);
    }

    /**
     * Constructs a list of host configs from the site level configs and the
     * user level overrides
     * 
     * @return empty list if none found
     */
    synchronized public static Collection<HostConfig> getHostConfigs() {
        List<HostConfig> userConfigured = getUserHostConfigs();
        SiteConfigInformation siteConfigInformation = getSiteConfigInformation();
        if (siteConfigInformation == null) {
            return Collections.emptyList();
        }
        List<HostConfig> siteConfigured = siteConfigInformation.getServer();
        if (siteConfigured == null) {
            siteConfigured = new ArrayList<>(0);
        }

        Collection<HostConfig> rval;
        if (userConfigured.isEmpty()) {
            rval = siteConfigured;
        } else {
            rval = new HashSet<HostConfig>(siteConfigured);
            for (HostConfig user : userConfigured) {
                if (user.isRemoved()) {
                    rval.remove(user);
                } else {
                    rval.add(user);
                }
            }
        }
        return rval;
    }

    /**
     * Get host configuration added by this user. Must be externally read
     * locked.
     * 
     * @return empty list if none are found
     */
    private static List<HostConfig> getUserHostConfigs() {
        List<HostConfig> rval;
        SiteConfigInformation userInstance = getUserConfigInformation();
        List<HostConfig> servers = userInstance.getServer();
        if (servers == null) {
            rval = new ArrayList<HostConfig>(0);
        } else {
            rval = new ArrayList<HostConfig>(servers);
        }
        return rval;
    }

    /**
     * @param info
     * @return true if info is not null and has at least one site configuration
     */
    private static boolean isValid(SiteConfigInformation info) {
        if (info != null) {
            for (SiteConfig i : info.getConfig()) {
                if (i.getSite() != null && !i.getSite().isEmpty()
                        && i.getRoles() != null && i.getRoles().length > 0) {
                    return true;
                }
            }
        }
        return false;
    }

    /**
     * Gets configuration objects for sites. Does not include user level
     * overrides.
     * 
     * @return
     */
    synchronized public static Collection<SiteConfig> getSiteConfigs() {
        Collection<SiteConfig> rval;
        SiteConfigInformation info = getSiteConfigInformation();
        if (info == null) {
            rval = Collections.emptyList();
        } else {
            List<SiteConfig> configs = info.getConfig();
            if (configs == null) {
                rval = Collections.emptyList();
            } else {
                rval = configs;
            }
        }
        return rval;
    }

    /**
     * @param actingSite
     * @param otherSite
     * @return true if the configuration for the acting site shows the other
     *         site as visible (takes into account user level overrides)
     */
    synchronized public static boolean isVisible(String actingSite,
            String otherSite) {
        SiteVisiblityConfig config = getSiteVisibilityConfig(actingSite);
        return config.isVisible(otherSite);
    }

    /**
     * Add a user level override for the acting site config to set the other
     * site as visible
     * 
     * @param actingSite
     * @param otherSite
     */
    synchronized public static void showSite(String actingSite, String otherSite) {
        SiteVisiblityConfig config = getSiteVisibilityConfig(actingSite);
        config.show(otherSite);
        writeUserConfiguredVisibility(config);
    }

    /**
     * Add a user level override for the acting site config to set the other
     * site as not visible
     * 
     * @param actingSite
     * @param otherSite
     */
    synchronized public static void hideSite(String actingSite, String otherSite) {
        SiteVisiblityConfig config = getSiteVisibilityConfig(actingSite);
        config.hide(otherSite);
        writeUserConfiguredVisibility(config);
    }

    /**
     * Get site visibility configuration for acting site
     * 
     * @param site
     * @return
     */
    private static SiteVisiblityConfig getSiteVisibilityConfig(String site) {
        SiteVisiblityConfig rval = null;
        rval = siteVisibilityMap.get(site);
        if (rval == null) {
            Map<String, ListEntry> userSpecificConfigs = getUserSpecificConfigs(site);
            SiteConfigInformation siteInstance = getSiteConfigInformation();
            if (siteInstance != null && siteInstance.getConfig() != null) {
                for (SiteConfig config : siteInstance.getConfig()) {
                    String configSite = config.getSite();
                    if (configSite != null && configSite.equals(site)) {
                        rval = new SiteVisiblityConfig(config,
                                userSpecificConfigs);
                        break;
                    }
                }
            }
            if (rval == null) {
                /*
                 * this shouldn't happen since you have to have an entry in the
                 * site configuration file to log in
                 */
                log.warn("No configuration found for site '" + site
                        + "'. Defaulting to showing all sites.");
                rval = new SiteVisiblityConfig(site, new HashSet<String>(0),
                        ListType.BLACKLIST, userSpecificConfigs);
            } else {
                siteVisibilityMap.put(site, rval);
            }
        }

        return rval;
    }

    /**
     * Get user level overrides for site visibility
     * 
     * @param site
     * @return site configs indexed by name
     */
    private static Map<String, ListEntry> getUserSpecificConfigs(String site) {
        Map<String, ListEntry> rval = new HashMap<>();
        SiteConfigInformation userInstance = getUserConfigInformation();
        SiteConfig siteConfig = getSiteConfig(userInstance, site);
        if (siteConfig != null) {
            ListEntry[] entries = siteConfig.getListEntries();
            if (entries != null) {
                for (ListEntry entry : entries) {
                    rval.put(entry.getValue(), entry);
                }
            }
        }
        return rval;
    }

    /**
     * @param info
     * @param site
     * @return null if no config exists for privided site in info
     */
    private static SiteConfig getSiteConfig(SiteConfigInformation info,
            String site) {
        SiteConfig rval = null;
        if (info.getConfig() != null) {
            for (SiteConfig config : info.getConfig()) {
                String configSite = config.getSite();
                if (configSite != null && configSite.equals(site)) {
                    rval = config;
                    break;
                }
            }
        }
        return rval;
    }

    /**
     * reset in-memory configuration
     */
    synchronized private static void clear() {
        if (siteConfigInfoFile != null) {
            siteConfigInfoFile.removeFileUpdatedObserver(siteConfigObserver);
            siteConfigInfoFile = null;
        }
        _siteInstance = null;
        if (userConfigInfoFile != null) {
            userConfigInfoFile.removeFileUpdatedObserver(siteConfigObserver);
            userConfigInfoFile = null;
        }
        _userInstance = null;
        siteVisibilityMap.clear();
    }
}
