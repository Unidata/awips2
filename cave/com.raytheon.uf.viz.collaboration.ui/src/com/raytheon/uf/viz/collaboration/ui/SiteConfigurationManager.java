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
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.concurrent.locks.ReentrantLock;

import javax.xml.bind.DataBindingException;
import javax.xml.bind.JAXB;
import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;
import javax.xml.bind.Unmarshaller;

import com.raytheon.uf.common.localization.IPathManager;
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
import com.raytheon.uf.viz.collaboration.comm.identity.info.SiteConfigInformation;
import com.raytheon.uf.viz.collaboration.comm.identity.info.SiteConfigInformation.HostConfig;
import com.raytheon.uf.viz.collaboration.comm.identity.info.SiteConfigInformation.SiteConfig;
import com.raytheon.uf.viz.collaboration.ui.SiteColorInformation.SiteColor;

/**
 * Parse a file to grab attributes about a user
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 12, 2012            mnash     Initial creation
 * Jan 08, 2014 2563       bclement    duplicate code elimination
 *                                     added methods to partially modify user config
 * 
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */
public class SiteConfigurationManager {
    
    private static final IUFStatusHandler log = UFStatus.getHandler(SiteConfigurationManager.class);

    private static SiteConfigInformation instance;

    private static SiteConfigInformation userInstance;

    private static final ReentrantLock userConfigLock = new ReentrantLock();

    private static SiteColorInformation colorInfo;

    // level hierarchy for site config
    private static final LocalizationLevel[] siteLevels;
    
    static{
        // load site levels with all levels except for USER
        PathManager pm = (PathManager) PathManagerFactory.getPathManager();
        LocalizationLevel[] available = pm.getAvailableLevels();
        List<LocalizationLevel> levels = new ArrayList<LocalizationLevel>(available.length -1);
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
     * @param levels
     *            localization levels to search in order
     * @return null if none found
     */
    private static LocalizationFile findConfigLocalizationFile(
            LocalizationLevel[] levels) {
        LocalizationFile rval = null;
        for (LocalizationLevel level : levels) {
            rval = getConfigLocalizationFile(level);
            if (rval != null && rval.exists()) {
                break;
            }
        }
        return rval;
    }

    /**
     * Get LocalizationFile for SiteConfigInformation at specified level.
     * 
     * @param level
     * @return
     */
    private static LocalizationFile getConfigLocalizationFile(
            LocalizationLevel level) {
        PathManager pm = (PathManager) PathManagerFactory.getPathManager();
        LocalizationContext localContext = pm.getContext(
                LocalizationType.CAVE_STATIC, level);
        return pm.getLocalizationFile(localContext, "collaboration"
                + File.separator + "config.xml");
    }

    /**
     * Read configuration file from file system. Must be externally
     * synchronized.
     * 
     * @param levels
     *            localization levels to search in order
     * @return null if configuration file wasn't found or an error occurred
     */
    private static SiteConfigInformation readConfigInformation(
            LocalizationLevel[] levels) {
        LocalizationFile file = findConfigLocalizationFile(levels);
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
        LocalizationFile file = getConfigLocalizationFile(level);
        LocalizationFileOutputStream out = null;
        try {
            out = file.openOutputStream();
            JAXBContext context = JAXBContext
                    .newInstance(SiteConfigInformation.class);
            Marshaller marshaller = context.createMarshaller();
            marshaller.setProperty(Marshaller.JAXB_FORMATTED_OUTPUT,
                    new Boolean(true));
            marshaller.marshal(userInstance, out);
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
     * Go to the config.xml file and grab the user information, for use in
     * determining what kind of user you are in collaboration
     * 
     * @return
     */
    public static synchronized SiteConfigInformation getSiteConfigInformation() {
        if (instance == null) {
            SiteConfigInformation info = readConfigInformation(siteLevels);
            if (isValid(info)) {
                instance = info;
            } else {
                log.handle(Priority.PROBLEM,
                        "Misconfigured config.xml file at site level. "
                                + "Does not contain required "
                                + SiteConfigInformation.SITE_NAME + " and "
                                + SiteConfigInformation.ROLE_NAME
                                + " attributes.");
            }
        }
        return instance;
    }

    /**
     * Write the colorInfo.xml file out to user localization so that the user
     * can retrieve it on CAVE restart
     * 
     * @param information
     */
    public static void writeSiteColorInformation(
            SiteColorInformation information) {
        colorInfo = information;
        IPathManager pathMgr = PathManagerFactory.getPathManager();
        LocalizationContext lContext = pathMgr.getContext(
                LocalizationType.CAVE_STATIC, LocalizationLevel.USER);
        LocalizationFile file = pathMgr.getLocalizationFile(lContext,
                "collaboration" + File.separator + "colorInfo.xml");
        try {
            JAXBContext context = JAXBContext
                    .newInstance(SiteColorInformation.class);
            Marshaller marshaller = context.createMarshaller();
            marshaller.setProperty(Marshaller.JAXB_FORMATTED_OUTPUT,
                    new Boolean(true));
            marshaller.marshal(information, file.getFile());
            file.save();
        } catch (Exception e) {
            Activator.statusHandler.handle(Priority.PROBLEM,
                    e.getLocalizedMessage(), e);
        }
    }

    /**
     * Allows users to have their own extra configured sites, but does not check
     * for validity as we don't have to have a role or a site specified. Must be
     * externally synchronized.
     */
    private static void readUserSiteConfigInformation() {
        // we always want to read/write the same localization file, only give
        // one level to search
        userInstance = readConfigInformation(new LocalizationLevel[] { LocalizationLevel.USER });
        if (userInstance == null) {
            // user config doesn't exist, create a new one
            userInstance = new SiteConfigInformation();
        }
    }

    /**
     * Overwrite user enabled sites list. Persists to localization file.
     * 
     * @param userEnabledSites
     */
    public static void writeUserEnabledSites(String[] userEnabledSites) {
        userConfigLock.lock();
        try {
            if (userInstance == null) {
                readUserSiteConfigInformation();
            }
            // update object
            List<SiteConfig> configs = userInstance.getConfig();
            SiteConfig config;
            if (configs == null || configs.isEmpty()) {
                configs = new ArrayList<SiteConfigInformation.SiteConfig>(1);
                config = new SiteConfig();
                configs.add(config);
                userInstance.setConfig(configs);
            } else {
                config = configs.get(0);
            }
            config.setSubscribedSites(userEnabledSites);

            // update on file system
            writeConfigInformation(LocalizationLevel.USER, userInstance);
        } catch (Exception e) {
            log.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
        } finally {
            userConfigLock.unlock();
        }
    }

    /**
     * Add host configuration specific to this user. Persists to localization
     * file.
     * 
     * @param config
     */
    public static void addUserHostConfig(HostConfig config){
        userConfigLock.lock();
        try{
            if(userInstance == null){
                readUserSiteConfigInformation();
            }
            List<HostConfig> servers = userInstance.getServer();
            if (servers == null){
                servers = new ArrayList<SiteConfigInformation.HostConfig>(1);
                userInstance.setServer(servers);
            }
            if (!hasHost(servers, config)) {
                servers.add(config);
                writeConfigInformation(LocalizationLevel.USER, userInstance);
            }
        } catch (Exception e) {
            log.error(e.getLocalizedMessage(), e);
        } finally {
            userConfigLock.unlock();
        }
    }

    /**
     * @param servers
     * @param config
     * @return true if the hostname in config matches any in servers
     */
    private static boolean hasHost(List<HostConfig> servers, HostConfig config) {
        for (HostConfig hc : servers) {
            if (hc.getHostname().equalsIgnoreCase(config.getHostname())) {
                return true;
            }
        }
        return false;
    }

    /**
     * Remove any host configuration that has serverAddress from current user's
     * localization file.
     * 
     * @param serverAddress
     */
    public static void removeUserHostConfig(String serverAddress) {
        userConfigLock.lock();
        try {
            if (userInstance == null) {
                readUserSiteConfigInformation();
            }
            List<HostConfig> servers = userInstance.getServer();
            if (servers != null) {
                List<HostConfig> newServers = new ArrayList<HostConfig>();
                for (HostConfig hc : servers) {
                    if (!hc.getHostname().equalsIgnoreCase(serverAddress)) {
                        newServers.add(hc);
                    }
                }
                userInstance.setServer(newServers);
                writeConfigInformation(LocalizationLevel.USER, userInstance);
            }
        } catch (Exception e) {
            log.error(e.getLocalizedMessage(), e);
        } finally {
            userConfigLock.unlock();
        }
    }

    /**
     * Get host configuration added by this user.
     * 
     * @return empty list if none are found
     */
    public static List<HostConfig> getUserHostConfig() {
        List<HostConfig> rval;
        userConfigLock.lock();
        try {
            if (userInstance == null) {
                readUserSiteConfigInformation();
            }
            List<HostConfig> servers = userInstance.getServer();
            if (servers == null) {
                rval = new ArrayList<HostConfig>(0);
            } else {
                rval = new ArrayList<HostConfig>(servers);
            }
        } finally {
            userConfigLock.unlock();
        }
        return rval;
    }

    /**
     * Instantiate the colorInfo object so that the colors can be read in from
     * the colorInfo.xml file and retrieved from localization
     * 
     * @return
     */
    public static SiteColorInformation getSiteColorInformation() {
        if (colorInfo == null) {
            PathManager pm = (PathManager) PathManagerFactory.getPathManager();
            Map<LocalizationLevel, LocalizationFile> files = pm
                    .getTieredLocalizationFile(LocalizationType.CAVE_STATIC,
                            "collaboration" + File.separator + "colorInfo.xml");
            LocalizationLevel[] levels = LocalizationLevel.values();

            for (int i = levels.length - 1; i >= 0 && colorInfo == null; --i) {
                LocalizationLevel level = levels[i];
                if (level == LocalizationLevel.SITE
                        || level == LocalizationLevel.USER) {
                    LocalizationFile file = files.get(level);
                    if (file != null) {
                        InputStream in = null;
                        try {
                            in = file.openInputStream();
                            JAXBContext context = JAXBContext
                                    .newInstance(SiteColorInformation.class);
                            Unmarshaller unmarshaller = context
                                    .createUnmarshaller();
                            colorInfo = (SiteColorInformation) unmarshaller
                                    .unmarshal(in);
                        } catch (Exception e) {
                            Activator.statusHandler.handle(Priority.PROBLEM,
                                    e.getLocalizedMessage(), e);
                        }
                        if (in != null) {
                            try {
                                in.close();
                            } catch (IOException e) {
                                Activator.statusHandler.handle(
                                        Priority.PROBLEM,
                                        e.getLocalizedMessage(), e);
                            }
                        }
                    }
                }
            }
        }
        return colorInfo;
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
     * Get list of subscribed sites from site configuration
     * 
     * @param site
     * @return
     */
    public static List<String> getSubscribeList(String site) {
        List<String> subscribed = new ArrayList<String>();
        for (SiteConfig config : instance.getConfig()) {
            if (config.getSite().equals(site)) {
                subscribed = Arrays.asList(config.getSubscribedSites());
                break;
            }
        }
        return subscribed;
    }

    /**
     * Reads the user subscribe list
     * 
     * @return
     */
    public static List<String> getUserSubscribeList() {
        List<String> subscribed = new ArrayList<String>();
        userConfigLock.lock();
        try {
            if (userInstance == null) {
                readUserSiteConfigInformation();
            }
            if (userInstance != null) {
                for (SiteConfig config : userInstance.getConfig()) {
                    if (config.getSubscribedSites() != null) {
                        subscribed = new ArrayList<String>();
                        for (String item : config.getSubscribedSites()) {
                            subscribed.add(item);
                        }
                        break;
                    }
                }
            }
        } finally {
            userConfigLock.unlock();
        }
        return subscribed;
    }

    /**
     * @return list of colors from site information config
     */
    public static List<SiteColor> getSiteColors() {
        SiteColorInformation colorInfo = getSiteColorInformation();
        if (colorInfo != null) {
            return getSiteColorInformation().getColors();
        } else {
            return null;
        }
    }

    /**
     * reset in-memory configuration
     */
    public static void nullifySiteConfigInstance() {
        instance = null;
        userInstance = null;
    }
}
