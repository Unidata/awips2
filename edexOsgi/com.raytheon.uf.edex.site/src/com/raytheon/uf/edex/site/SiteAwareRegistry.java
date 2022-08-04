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
import java.io.IOException;
import java.io.InputStreamReader;
import java.time.Duration;
import java.util.LinkedHashSet;
import java.util.Set;
import java.util.concurrent.CopyOnWriteArraySet;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import com.raytheon.uf.common.localization.ILocalizationFile;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.SaveableOutputStream;
import com.raytheon.uf.common.localization.exception.LocalizationException;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.util.registry.RegistryException;
import com.raytheon.uf.edex.core.EDEXUtil;
import com.raytheon.uf.edex.core.EdexException;
import com.raytheon.uf.edex.database.cluster.ClusterLockUtils;
import com.raytheon.uf.edex.database.cluster.ClusterLockUtils.LockState;
import com.raytheon.uf.edex.database.cluster.ClusterTask;
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
 * Mar 10, 2014  2721      randerso    Fix error when activeSites.txt contains blank lines.
 * Jul 10, 2014  2914      garmendariz Remove EnvProperties
 * Nov 9, 2015   14734     yteng       Remove activeSites and add lock to synchronize access
 *                                     to activeSites.txt to eliminate race conditions
 * Dec 21, 2015  4262      dgilling    Execute startup ISiteActivationListeners asynchronously.
 * Jul 18, 2016  5744      mapeters    Use common_static instead of edex_static for
 *                                     saving/retrieving active sites
 * May 04, 2020 8151       randerso    Code cleanup
 *
 * </pre>
 *
 * @author rjpeter
 */
public class SiteAwareRegistry {
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(SiteAwareRegistry.class);

    public static final String ACTIVE_SITES = "config" + IPathManager.SEPARATOR
            + "activeSites.txt";

    private static SiteAwareRegistry instance = new SiteAwareRegistry();

    private final ExecutorService activationThreadPool = Executors
            .newCachedThreadPool();

    private final Set<ISiteActivationListener> activationListeners = new CopyOnWriteArraySet<>();

    private String routeId;

    public static SiteAwareRegistry getInstance() {
        return instance;
    }

    private SiteAwareRegistry() {
        // initialize default site
        Set<String> activeSites = getActiveSitesFromFile(true);
        String defaultSite = EDEXUtil.getEdexSite();
        if (!activeSites.contains(defaultSite)) {
            updateActiveSites(Action.ACTIVATE, defaultSite);
        }
    }

    /**
     * registers/adds site activation listeners
     *
     * @param sa
     *            the listener to register / add to the list
     */
    public Object register(final ISiteActivationListener sa)
            throws RegistryException {
        if (!activationListeners.add(sa)) {
            throw new RegistryException(
                    "SiteAwareRegistry Exception - duplicate site "
                            + sa.toString());
        }

        Set<String> activeSites = getActiveSitesFromFile(true);
        final CountDownLatch activationComplete = new CountDownLatch(
                activeSites.size());

        // inform of the current active sites
        for (final String siteID : activeSites) {
            Runnable activateSiteTask = new Runnable() {

                @Override
                public void run() {
                    try {
                        sa.activateSite(siteID);
                    } catch (Exception e) {
                        statusHandler.error(e.getLocalizedMessage(), e);
                    } finally {
                        activationComplete.countDown();
                    }
                }
            };
            activationThreadPool.submit(activateSiteTask);
        }

        Runnable siteActivationCompleteTask = new Runnable() {

            @Override
            public void run() {
                try {
                    activationComplete.await();
                } catch (InterruptedException e) {
                    statusHandler.error(e.getLocalizedMessage());
                } finally {
                    sa.registered();
                }
            }
        };
        activationThreadPool.submit(siteActivationCompleteTask);

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
        Set<String> tmp = new LinkedHashSet<>();
        String mySite = EDEXUtil.getEdexSite();
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
        StringBuilder retVal = new StringBuilder();
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
            updateActiveSites(Action.ACTIVATE, siteID);
        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Failed to send site activation message for site " + siteID,
                    e);
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
            updateActiveSites(Action.DEACTIVATE, siteID);
        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Failed to send site de-activation message for site "
                            + siteID,
                    e);
        }
    }

    /**
     * cycle the site specified in each listener
     *
     * @param siteID
     */
    public void cycleSite(String siteID) {
        Set<String> activeSites = getActiveSitesFromFile(true);
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
                                    + siteID,
                            e);
                    updateActiveSites(Action.ACTIVATE, siteID);
                }
            }
            if (!Action.DEACTIVATE.equals(action)) {
                try {
                    sa.activateSite(siteID);
                } catch (Exception e) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Failed to process site " + action + " for site "
                                    + siteID,
                            e);
                    updateActiveSites(Action.DEACTIVATE, siteID);
                }
            }

        }
    }

    private void routeMessage(SiteActivationMessage mess)
            throws EdexException, SerializationException {
        EDEXUtil.getMessageProducer().sendAsync(routeId,
                SerializationUtil.transformToThrift(mess));
    }

    /**
     * load the active site list
     */
    private Set<String> getActiveSitesFromFile(boolean useFileLock) {
        ClusterTask ct = null;
        if (useFileLock) {
            do {
                ct = ClusterLockUtils.lock("siteActivation", "readwrite",
                        Duration.ofMinutes(2).toMillis(), true);
            } while (!LockState.SUCCESSFUL.equals(ct.getLockState()));
        }

        Set<String> activeSites = new LinkedHashSet<>();
        IPathManager pathMgr = PathManagerFactory.getPathManager();
        ILocalizationFile lf = pathMgr.getLocalizationFile(
                pathMgr.getContext(LocalizationType.COMMON_STATIC,
                        LocalizationLevel.SITE),
                ACTIVE_SITES);
        if (lf != null && lf.exists()) {
            try (BufferedReader in = new BufferedReader(
                    new InputStreamReader(lf.openInputStream()))) {
                String site;
                while ((site = in.readLine()) != null) {
                    site = site.trim();
                    if (!site.isEmpty()) {
                        activeSites.add(site);
                    }
                }
            } catch (IOException | LocalizationException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Error loading active sites from file: " + lf.getPath(),
                        e);
            } finally {
                if (useFileLock) {
                    ClusterLockUtils.deleteLock(ct.getId().getName(),
                            ct.getId().getDetails());
                }
            }
        }

        return activeSites;
    }

    /**
     * save the active site list
     */
    private void saveActiveSites(Set<String> activeSites) {
        IPathManager pathMgr = PathManagerFactory.getPathManager();
        LocalizationContext ctx = pathMgr.getContext(
                LocalizationType.COMMON_STATIC, LocalizationLevel.SITE);
        ILocalizationFile lf = pathMgr.getLocalizationFile(ctx, ACTIVE_SITES);

        StringBuilder activeSitesStr = new StringBuilder();
        for (String site : activeSites) {
            activeSitesStr.append(site + "\n");
        }
        try (SaveableOutputStream os = lf.openOutputStream()) {
            os.write(activeSitesStr.toString().getBytes());
            os.save();
        } catch (IOException | LocalizationException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error saving active sites to file: " + lf.getPath(), e);
        }
    }

    /**
     * update the active site list
     *
     * @param action
     * @param siteID
     */
    private synchronized void updateActiveSites(Action action, String siteID) {
        ClusterTask ct = null;
        do {
            ct = ClusterLockUtils.lock("siteActivation", "readwrite",
                    Duration.ofMinutes(2).toMillis(), true);
        } while (!LockState.SUCCESSFUL.equals(ct.getLockState()));

        Set<String> activeSites = getActiveSitesFromFile(false);
        if (Action.ACTIVATE.equals(action)) {
            if (activeSites.add(siteID)) {
                saveActiveSites(activeSites);
            }
        } else if (Action.DEACTIVATE.equals(action)) {
            if (activeSites.remove(siteID)) {
                saveActiveSites(activeSites);
            }
        } else {
            statusHandler.handle(Priority.PROBLEM,
                    "Error updating active sites, invalid action type: "
                            + String.valueOf(action));
        }

        ClusterLockUtils.deleteLock(ct.getId().getName(),
                ct.getId().getDetails());
    }
}
