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

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;
import javax.xml.bind.Unmarshaller;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManager;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.exception.LocalizationException;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.collaboration.comm.identity.info.SiteConfigInformation;
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
 * 
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */
public class SiteConfigurationManager {

    private static SiteConfigInformation instance;

    private static SiteConfigInformation userInstance;

    private static SiteColorInformation colorInfo;

    /**
     * Go to the config.xml file and grab the user information, for use in
     * determining what kind of user you are in collaboration
     * 
     * @return
     */
    public static synchronized SiteConfigInformation getSiteConfigInformation() {
        if (instance == null) {
            PathManager pm = (PathManager) PathManagerFactory.getPathManager();
            Map<LocalizationLevel, LocalizationFile> files = pm
                    .getTieredLocalizationFile(LocalizationType.CAVE_STATIC,
                            "collaboration" + File.separator + "config.xml");

            LocalizationLevel[] levels = LocalizationLevel.values();
            for (int i = levels.length - 1; i >= 0 && instance == null; --i) {
                LocalizationLevel level = levels[i];
                if (level.isSystemLevel() || level == LocalizationLevel.SITE) {
                    LocalizationFile file = files.get(level);
                    if (file != null) {
                        InputStream in = null;
                        try {
                            in = file.openInputStream();
                            JAXBContext context = JAXBContext
                                    .newInstance(SiteConfigInformation.class);
                            Unmarshaller unmarshaller = context
                                    .createUnmarshaller();
                            SiteConfigInformation info = (SiteConfigInformation) unmarshaller
                                    .unmarshal(in);
                            if (isValid(info)) {
                                instance = info;
                            } else {
                                Activator.statusHandler
                                        .handle(Priority.PROBLEM,
                                                "Misconfigured config.xml file at "
                                                        + level
                                                        + " level. "
                                                        + "Does not contain required "
                                                        + SiteConfigInformation.SITE_NAME
                                                        + " and "
                                                        + SiteConfigInformation.ROLE_NAME
                                                        + " attributes.");
                            }
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
     * for validity as we don't have to have a role or a site specified, only
     * subscribed sites
     */
    private static synchronized void readUserSiteConfigInformation() {
        LocalizationFile file = getUserConfigFile();
        if (file != null && file.exists()) {
            InputStream in = null;
            try {
                in = file.openInputStream();
                JAXBContext context = JAXBContext
                        .newInstance(SiteConfigInformation.class);
                Unmarshaller unmarshaller = context.createUnmarshaller();
                SiteConfigInformation info = (SiteConfigInformation) unmarshaller
                        .unmarshal(in);
                userInstance = info;
            } catch (JAXBException e) {
                Activator.statusHandler.handle(Priority.PROBLEM,
                        e.getLocalizedMessage(), e);
            } catch (LocalizationException e) {
                Activator.statusHandler.handle(Priority.PROBLEM,
                        e.getLocalizedMessage(), e);
            }
        }
    }

    public static void writeUserSiteConfigInformation(
            SiteConfigInformation information) {
        userInstance = information;
        LocalizationFile file = getUserConfigFile();
        try {
            JAXBContext context = JAXBContext
                    .newInstance(SiteConfigInformation.class);
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
     * Shortcut method for retrieving the user configuration file
     * 
     * @return
     */
    private static LocalizationFile getUserConfigFile() {
        IPathManager pathMgr = PathManagerFactory.getPathManager();
        LocalizationContext lContext = pathMgr.getContext(
                LocalizationType.CAVE_STATIC, LocalizationLevel.USER);
        LocalizationFile file = pathMgr.getLocalizationFile(lContext,
                "collaboration" + File.separator + "config.xml");
        return file;
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
     * @return
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
     * Reads and sets the user subscribe list
     * 
     * @return
     */
    public static List<String> getUserSubscribeList() {
        if (userInstance == null) {
            readUserSiteConfigInformation();
        }
        List<String> subscribed = new ArrayList<String>();
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
        return subscribed;
    }

    public static List<SiteColor> getSiteColors() {
        SiteColorInformation colorInfo = getSiteColorInformation();
        if (colorInfo != null) {
            return getSiteColorInformation().getColors();
        } else {
            return null;
        }
    }

    public static void nullifySiteConfigInstance() {
        instance = null;
        userInstance = null;
    }
}
