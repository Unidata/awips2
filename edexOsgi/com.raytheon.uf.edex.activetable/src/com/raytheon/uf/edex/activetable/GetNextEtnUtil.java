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
package com.raytheon.uf.edex.activetable;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.net.InetAddress;
import java.net.UnknownHostException;
import java.rmi.RemoteException;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collections;
import java.util.List;
import java.util.Properties;
import java.util.Queue;
import java.util.SortedMap;
import java.util.TreeMap;

import com.raytheon.edex.site.SiteUtil;
import com.raytheon.uf.common.activetable.ActiveTableMode;
import com.raytheon.uf.common.activetable.request.LockAndGetNextEtnRequest;
import com.raytheon.uf.common.activetable.request.UnlockAndSetNextEtnRequest;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.serialization.ExceptionWrapper;
import com.raytheon.uf.common.serialization.comm.IRequestRouter;
import com.raytheon.uf.common.serialization.comm.IServerRequest;
import com.raytheon.uf.common.serialization.comm.response.ServerErrorResponse;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.util.StringUtil;
import com.raytheon.uf.edex.auth.RemoteServerRequestRouter;
import com.raytheon.uf.edex.database.cluster.ClusterLockUtils;
import com.raytheon.uf.edex.database.cluster.ClusterLockUtils.LockState;
import com.raytheon.uf.edex.database.cluster.ClusterTask;

/**
 * Library module of functions to support retrieval of next ETN in sequence for
 * a given phensig and 4-character site identifier.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 29, 2013  #1843     dgilling     Initial creation
 * 
 * </pre>
 * 
 * @author dgilling
 * @version 1.0
 */
public final class GetNextEtnUtil {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(GetNextEtnUtil.class);

    private static final String CONFIG_FILE_NAME = "remote-etn-partners.properties";

    private static final String NEXT_ETN_LOCK = "ActiveTableNextEtn";

    /**
     * Dummy private constructor so this class can't be directly instantiated.
     * Every method is static.
     */
    private GetNextEtnUtil() {
        throw new AssertionError();
    }

    /**
     * Determine the next ETN in sequence given an office, phensig, and active
     * table. The next ETN will be determined by using the maximum of the last
     * ETN found for the given office and phensig in the appropriate active
     * table and in the cluster task for the same office, phensig, and active
     * table combination.
     * <p>
     * If the <code>performISC</code> flag is set then we will also query a list
     * of configured remote ETN partners. This configuration is stored in the
     * file edex_static/site/${AW_SITE_IDENTIFIER}/vtec/remote-etn-partners.
     * properties.
     * 
     * @param siteId
     *            The 4-character site identifier.
     * @param mode
     *            The active table to use.
     * @param phensig
     *            The phenomenon and significance combination (e.g., TO.W or
     *            DU.Y).
     * @param currentTime
     *            <code>Calendar</code> representing time (needed for DRT mode).
     * @param isLock
     *            Whether or not to return a unique ETN--one that has not and
     *            cannot be used by any other requestor.
     * @param performISC
     *            Whether or not to collaborate with neighboring sites to
     *            determine the next ETN. If this is true, but
     *            <code>isLock</code> is false, this flag is effectively false
     *            and your configured remote partners will not be contacted to
     *            determine the next ETN.
     * @return The next ETN to be used in sequence.
     */
    public static Integer getNextEtn(String siteId, ActiveTableMode mode,
            String phensig, Calendar currentTime, boolean isLock,
            boolean performISC) {
        List<IRequestRouter> hostsToQuery = Collections.emptyList();
        if (performISC) {
            hostsToQuery = GetNextEtnUtil.getRemoteEtnSources(siteId);
        }

        int nextEtn;
        if (performISC && isLock && (!hostsToQuery.isEmpty())) {
            nextEtn = GetNextEtnUtil.getNextEtnFromPartners(siteId, mode,
                    phensig, currentTime, hostsToQuery);
        } else {
            nextEtn = GetNextEtnUtil.getNextEtnFromLocal(siteId, mode, phensig,
                    currentTime, isLock);
        }

        return nextEtn;
    }

    private static List<IRequestRouter> getRemoteEtnSources(String siteId) {
        Properties etnBackupProps = new Properties();
        FileInputStream fis = null;
        try {
            IPathManager pathMgr = PathManagerFactory.getPathManager();
            LocalizationContext edexSiteCtx = pathMgr.getContextForSite(
                    LocalizationType.EDEX_STATIC, SiteUtil.getSite());
            File configFile = pathMgr.getFile(edexSiteCtx, "vtec"
                    + IPathManager.SEPARATOR + CONFIG_FILE_NAME);
            fis = new FileInputStream(configFile);
            etnBackupProps.load(fis);
        } catch (FileNotFoundException e) {
            statusHandler.error(CONFIG_FILE_NAME + " file does not exist!", e);
            return Collections.emptyList();
        } catch (IOException e) {
            statusHandler.error("Error reading " + CONFIG_FILE_NAME + " file!",
                    e);
            return Collections.emptyList();
        } finally {
            if (fis != null) {
                try {
                    fis.close();
                } catch (IOException e) {
                    statusHandler.handle(Priority.WARN, "Error closing "
                            + CONFIG_FILE_NAME + " file!", e);
                }
            }
        }

        String[] tokens = etnBackupProps.getProperty("BACKUP.HOSTS." + siteId,
                "").split(",");

        // To prevent deadlock, we ensure every system uses the same ordering by
        // use of this SortedMap. Configuration entries will be ordered
        // alphabetically by host name.
        SortedMap<String, IRequestRouter> sources = new TreeMap<String, IRequestRouter>();
        for (String token : tokens) {
            String host = token.trim().toLowerCase();
            if ("localhost".equals(host)) {
                try {
                    host = InetAddress.getLocalHost().getCanonicalHostName();
                } catch (UnknownHostException e) {
                    statusHandler.error(
                            "Unable to retrieve host name for localhost.", e);
                    statusHandler
                            .handle(Priority.CRITICAL,
                                    "ETN assignment will not be able to query local server for used ETNs. Please check your network configuration and "
                                            + CONFIG_FILE_NAME + ".");
                    continue;
                }
            }

            IRequestRouter reqHandler = new RemoteServerRequestRouter("http://"
                    + host + ":9581/services");
            sources.put(host, reqHandler);
        }

        return new ArrayList<IRequestRouter>(sources.values());
    }

    /**
     * Returns the EDEX cluster lock name for the given site and active table
     * mode.
     * 
     * @param siteId
     *            4-char site identifier
     * @param mode
     *            The active table mode
     * @return The cluster lock name for the given site and active table.
     */
    protected static String getEtnClusterLockName(String siteId,
            ActiveTableMode mode) {
        String lockName = GetNextEtnUtil.NEXT_ETN_LOCK + "_" + siteId + "_"
                + mode.name();
        return lockName;
    }

    /**
     * Will obtain a cluster task lock for the given active table, office id,
     * and phensig combination and use the current information in the active
     * table and that cluster task's metadata to determine the next ETN in
     * sequence.
     * <p>
     * It is the responsibility of the caller of this method to later call
     * <code>setNextEtnAndUnlock</code> to release the cluster task lock this
     * method creates.
     * 
     * @param siteId
     *            The 4-character site identifier.
     * @param mode
     *            The active table to use.
     * @param phensig
     *            The phenomenon and significance combination (e.g., TO.W or
     *            DU.Y).
     * @param currentTime
     *            <code>Calendar</code> representing time (needed for DRT mode).
     * @param isLock
     *            Whether or not to actually obtain the cluster task lock. Not
     *            needed if only determining a preliminary ETN. Required to be
     *            set to <code>true</code> if you want to actually move the
     *            sequence forward.
     * @return The next ETN to be used in sequence.
     */
    public static int lockAndGetNextEtn(String siteId, ActiveTableMode mode,
            String phensig, Calendar currentTime, boolean isLock) {
        String lockName = getEtnClusterLockName(siteId, mode);
        ClusterTask ct = null;
        if (isLock) {
            do {
                ct = ClusterLockUtils.lock(lockName, phensig, 15000, true);
            } while (!ct.getLockState().equals(LockState.SUCCESSFUL));
            statusHandler.info("Locking::[lockName = " + lockName
                    + ", phensig = " + phensig + "]");
        } else {
            ct = ClusterLockUtils.lookupLock(lockName, phensig);
        }

        Integer lastEtn = ActiveTable.getLastUsedEtn(siteId, mode, phensig,
                currentTime);
        int nextEtn = (lastEtn != null) ? lastEtn + 1 : 1;

        String year = Integer.toString(currentTime.get(Calendar.YEAR));
        String eInfo = ct.getExtraInfo();
        if ((!StringUtil.isEmptyString(eInfo)) && (eInfo.startsWith(year))) {
            // parse year info
            try {
                int ctNextEtn = Integer
                        .parseInt(eInfo.substring(year.length() + 1)) + 1;
                nextEtn = Math.max(nextEtn, ctNextEtn);
            } catch (NumberFormatException e) {
                statusHandler.error(
                        "Caught excetion parsing etn from cluster_task", e);
            }
        }

        return nextEtn;
    }

    /**
     * Will release the cluster lock for the given office, phensig, and active
     * table combination and also save the last used ETN for that given
     * combination.
     * 
     * @param siteId
     *            The 4-character site identifier.
     * @param mode
     *            The active table to use.
     * @param phensig
     *            The phenomenon and significance combination (e.g., TO.W or
     *            DU.Y).
     * @param year
     *            Year the next ETN is effective for.
     * @param nextEtn
     *            The ETN to persist.
     */
    public static void setNextEtnAndUnlock(String siteId, ActiveTableMode mode,
            String phensig, int year, int nextEtn) {
        String lockName = getEtnClusterLockName(siteId, mode);
        ClusterLockUtils.updateExtraInfo(lockName, phensig,
                Integer.toString(year) + ":" + nextEtn);
        ClusterLockUtils.unlock(lockName, phensig);
        statusHandler.info("Unlocking::[nextEtn = " + nextEtn + "]");
    }

    /**
     * Will retrieve the ETN in sequence for the given site, phensig, and active
     * table combination by only checking the internal active table and cluster
     * lock metadata. No remote partners will be contacted.
     * 
     * @param siteId
     *            The 4-character site identifier.
     * @param mode
     *            The active table to use.
     * @param phensig
     *            The phenomenon and significance combination (e.g., TO.W or
     *            DU.Y).
     * @param currentTime
     *            <code>Calendar</code> representing time (needed for DRT mode).
     * @param isLock
     *            Whether or not to return a unique ETN--one that has not and
     *            cannot be used by any other requestor.
     * @return The next ETN to be used in sequence.
     */
    public static Integer getNextEtnFromLocal(String siteId,
            ActiveTableMode mode, String phensig, Calendar currentTime,
            boolean isLock) {
        int nextEtn = lockAndGetNextEtn(siteId, mode, phensig, currentTime,
                isLock);
        if (isLock) {
            setNextEtnAndUnlock(siteId, mode, phensig,
                    currentTime.get(Calendar.YEAR), nextEtn);
        }
        return nextEtn;
    }

    /**
     * Will retrieve the ETN in sequence for the given site, phensig, and active
     * table combination by contacting the EDEX hosts defined in the file
     * edex_static
     * /site/${AW_SITE_IDENTIFIER}/vtec/remote-etn-partners.properties.
     * <p>
     * It is expected that configuration file will list a number of remote
     * servers to check, but the host name of the primary EDEX server hosting
     * the site being queried for should also be listed.
     * 
     * @param siteId
     *            The 4-character site identifier.
     * @param mode
     *            The active table to use.
     * @param phensig
     *            The phenomenon and significance combination (e.g., TO.W or
     *            DU.Y).
     * @param currentTime
     *            <code>Calendar</code> representing time (needed for DRT mode).
     * @param hostsToQuery
     *            The remote hosts to query. This should also include the local
     *            EDEX instance initiating this operation.
     * @return The next ETN to be used in sequence.
     */
    public static Integer getNextEtnFromPartners(String siteId,
            ActiveTableMode mode, String phensig, Calendar currentTime,
            List<IRequestRouter> hostsToQuery) {
        Queue<IRequestRouter> lockQueue = new ArrayDeque<IRequestRouter>(
                hostsToQuery);
        Queue<IRequestRouter> unlockQueue = Collections
                .asLifoQueue(new ArrayDeque<IRequestRouter>(hostsToQuery.size()));

        String mySiteId = SiteUtil.getSite();

        IServerRequest getAndLockReq = new LockAndGetNextEtnRequest(siteId,
                mySiteId, mode, phensig, currentTime);
        int nextEtn = 1;
        for (IRequestRouter router : lockQueue) {
            try {
                Integer partersNextEtn = (Integer) GetNextEtnUtil
                        .sendThriftRequest(router, getAndLockReq);
                nextEtn = Math.max(nextEtn, partersNextEtn);
                unlockQueue.add(router);
            } catch (RemoteException e) {
                statusHandler
                        .handle(Priority.WARN,
                                "Error occurred contacting one of the remote ETN partners.",
                                e);
            }
        }

        IServerRequest unlockReq = new UnlockAndSetNextEtnRequest(siteId,
                mySiteId, mode, currentTime.get(Calendar.YEAR), phensig,
                nextEtn);
        for (IRequestRouter router : unlockQueue) {
            try {
                GetNextEtnUtil.sendThriftRequest(router, unlockReq);
            } catch (RemoteException e) {
                statusHandler
                        .handle(Priority.WARN,
                                "Error occurred unlocking one of the remote ETN partners.",
                                e);
            }
        }

        return nextEtn;
    }

    private static Object sendThriftRequest(final IRequestRouter router,
            final IServerRequest request) throws RemoteException {
        Object retVal = null;

        try {
            retVal = router.route(request);
        } catch (Exception e) {
            throw new RemoteException(
                    "Unhandled exception occurred routing request type "
                            + request.getClass().toString(), e);
        }

        if (retVal instanceof ServerErrorResponse) {
            Throwable cause = ExceptionWrapper
                    .unwrapThrowable(((ServerErrorResponse) retVal)
                            .getException());
            throw new RemoteException(
                    "Unhandled exception occurred on remote server processing request type "
                            + request.getClass().toString(), cause);
        }

        return retVal;
    }
}
