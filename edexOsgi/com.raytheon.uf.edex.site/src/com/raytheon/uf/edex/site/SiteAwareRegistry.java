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
package com.raytheon.uf.edex.site;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.util.LinkedHashSet;
import java.util.Set;
import java.util.concurrent.CopyOnWriteArraySet;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.exception.LocalizationException;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.util.registry.RegistryException;
import com.raytheon.uf.edex.core.EDEXUtil;
import com.raytheon.uf.edex.core.EdexException;
import com.raytheon.uf.edex.core.props.EnvProperties;
import com.raytheon.uf.edex.core.props.PropertiesFactory;
import com.raytheon.uf.edex.site.SiteActivationMessage.Action;

/**
 *
 * Site Aware Registry
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 30, 2010            rjpeter     Initial creation
 * Jul 31, 2012  #965      dgilling    Force ordering of sites in
 *                                     getActiveSites().
 * Nov 1, 2012   15417     ryu         Modified getActiveSites to include
 *                                     home site only if activated.
 * Dec 11, 2012  14360     ryu         No printing stack trace on activation exception
 *
 * </pre>
 *
 * @author rjpeter
 * @version 1.0
 */
public class SiteAwareRegistry {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(SiteAwareRegistry.class);

    public static final String ACTIVE_SITES = "config/activeSites.txt";

    private static SiteAwareRegistry instance = new SiteAwareRegistry();

    private Set<String> activeSites = new CopyOnWriteArraySet<String>();

    private Set<ISiteActivationListener> activationListeners = new CopyOnWriteArraySet<ISiteActivationListener>();

    private String routeId;

    public static SiteAwareRegistry getInstance() {
        return instance;
    }

    private SiteAwareRegistry() {
        // read in the current activeSites
        loadActiveSites();

        // initialize default site
        EnvProperties env = PropertiesFactory.getInstance().getEnvProperties();
        String defaultSite = env.getEnvValue("SITENAME");
        if (!activeSites.contains(defaultSite)) {
            activeSites.add(defaultSite);
        }
    }

    /**
     * registers/adds site activation listeners
     *
     * @param sa
     *            the listener to register / add to the list
     */
    public Object register(ISiteActivationListener sa) throws RegistryException {
        if (!activationListeners.add(sa)) {
            throw new RegistryException(
                    "SiteAwareRegistry Exception - duplicate site "
                            + sa.toString());
        }
        // inform of the current active sites
        for (String siteID : activeSites) {
            try {
                sa.activateSite(siteID);
            } catch (Exception e) {
                // Stack trace is not printed per requirement for DR14360
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage());
            }
        }

        sa.registered();
        return this;
    }

    public String getRouteId() {
        return routeId;
    }

    public void setRouteId(String routeId) {
        this.routeId = routeId;
    }

    /**
     * get the set of strings for the active sites
     *
     * @return the requested array of Strings this is a string array to make it
     *         work with dwr frontend, most of the other stuff is Set<String>
     */
    public String[] getActiveSites() {
        // make a set of the strings for each listener site
        Set<String> tmp = new LinkedHashSet<String>();
        String mySite = PropertiesFactory.getInstance().getEnvProperties()
                .getEnvValue("SITENAME");
        for (ISiteActivationListener sa : activationListeners) {
            if (sa.getActiveSites().contains(mySite)) {
                tmp.add(mySite);
            }
        }
        for (ISiteActivationListener sa : activationListeners) {
            tmp.addAll(sa.getActiveSites());
        }
        return tmp.toArray(new String[] {});
    }

    /**
     * Checks to see if the given site is active
     *
     * @param site
     *            The site to check
     * @return True if the site is active, else false
     */
    public boolean isActive(String site) {

        String[] sites = getActiveSites();
        for (String siteInList : sites) {
            if (siteInList.equalsIgnoreCase(site)) {
                return true;
            }
        }
        return false;
    }

    public String validateConfig(String siteID) {
        StringBuffer retVal = new StringBuffer();
        for (ISiteActivationListener sa : activationListeners) {
            retVal.append(sa.validateConfig(siteID));
        }
        return retVal.toString();
    }

    /**
     * activate the site specified in each listener
     *
     * @param siteID
     */
    public void activateSite(String siteID) {
        try {
            SiteActivationMessage mess = new SiteActivationMessage();
            mess.setSiteId(siteID);
            mess.setAction(Action.ACTIVATE);
            routeMessage(mess);
            activeSites.add(siteID);
            saveActiveSites();
        } catch (Exception e) {
            statusHandler
                    .handle(Priority.PROBLEM,
                            "Failed to send site activation message for site "
                                    + siteID, e);
        }
    }

    /**
     * deactivate the site specified in each listener
     *
     * @param siteID
     */
    public void deactivateSite(String siteID) {
        try {
            SiteActivationMessage mess = new SiteActivationMessage();
            mess.setSiteId(siteID);
            mess.setAction(Action.DEACTIVATE);
            routeMessage(mess);
            activeSites.remove(siteID);
            saveActiveSites();
        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Failed to send site de-activation message for site "
                            + siteID, e);
        }
    }

    /**
     * cycle the site specified in each listener
     *
     * @param siteID
     */
    public void cycleSite(String siteID) {
        if (activeSites.contains(siteID)) {
            try {
                SiteActivationMessage mess = new SiteActivationMessage();
                mess.setSiteId(siteID);
                mess.setAction(Action.CYCLE);
                routeMessage(mess);
            } catch (Exception e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Failed to send site cycle message for site " + siteID,
                        e);
            }
        }
    }

    public synchronized void handleMessage(SiteActivationMessage message) {
        String siteID = message.getSiteId();
        Action action = message.getAction();
        for (ISiteActivationListener sa : activationListeners) {

            // CYCLE needs to deactive and then activate
            if (!Action.ACTIVATE.equals(action)) {
                try {
                    sa.deactivateSite(siteID);
                } catch (Exception e) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Failed to process site " + action + " for site "
                                    + siteID, e);
                    activeSites.add(siteID);
                    saveActiveSites();
                }
            }
            if (!Action.DEACTIVATE.equals(action)) {
                try {
                    sa.activateSite(siteID);
                } catch (Exception e) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Failed to process site " + action + " for site "
                                    + siteID, e);
                    activeSites.remove(siteID);
                    saveActiveSites();
                }
            }

        }
    }

    private void routeMessage(SiteActivationMessage mess) throws EdexException,
            SerializationException {
        EDEXUtil.getMessageProducer().sendAsync(routeId,
                SerializationUtil.transformToThrift(mess));
    }

    /**
     * load the active site list
     */
    private void loadActiveSites() {
        // add cluster locking
        activeSites.clear();
        BufferedReader in = null;
        try {
            IPathManager pathMgr = PathManagerFactory.getPathManager();
            LocalizationFile lf = pathMgr.getLocalizationFile(pathMgr
                    .getContext(LocalizationType.EDEX_STATIC,
                            LocalizationLevel.SITE), ACTIVE_SITES);
            File file = lf.getFile();
            if (file.exists()) {
                in = new BufferedReader(new FileReader(file));
                String site;
                while ((site = in.readLine()) != null) {
                    activeSites.add(site);
                }
            }
        } catch (IOException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error loading active sites", e);
        } finally {
            try {
                if (in != null) {
                    in.close();
                }
            } catch (IOException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Error loading active sites", e);
            }
        }
    }

    /**
     * save the active site list
     */
    private void saveActiveSites() {
        BufferedWriter out = null;
        IPathManager pathMgr = PathManagerFactory.getPathManager();
        LocalizationFile lf = pathMgr.getLocalizationFile(pathMgr.getContext(
                LocalizationType.EDEX_STATIC, LocalizationLevel.SITE),
                ACTIVE_SITES);
        File file = lf.getFile();
        try {
            file.getParentFile().mkdirs();
            out = new BufferedWriter(new FileWriter(file));
            for (String site : activeSites) {
                out.write(site + "\n");
            }
        } catch (IOException e) {
            statusHandler.handle(Priority.PROBLEM, "Error saving active sites",
                    e);
        } finally {
            try {
                if (out != null) {
                    out.close();
                }
            } catch (IOException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Error saving active sites", e);
            }
        }

        try {
            lf.save();
        } catch (LocalizationException e) {
            statusHandler.handle(Priority.PROBLEM, "Error saving active sites",
                    e);
        }
    }
}

/**
 * constants needed for the UFStatus logging
 */
class StatusConstants {
    protected static final String PLUGIN_ID = StatusConstants.class
            .getPackage().getName();

    protected static final String CATEGORY = "RegistryException";

    protected static final String SUBCATEGORY = "SiteAwareRegistry";
}
