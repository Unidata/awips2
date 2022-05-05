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
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TimeZone;
import java.util.concurrent.ExecutionException;

import org.hibernate.NonUniqueObjectException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.raytheon.edex.site.SiteUtil;
import com.raytheon.uf.common.activetable.ActiveTableKey;
import com.raytheon.uf.common.activetable.ActiveTableMode;
import com.raytheon.uf.common.activetable.ActiveTableRecord;
import com.raytheon.uf.common.activetable.MergeResult;
import com.raytheon.uf.common.activetable.OperationalActiveTableRecord;
import com.raytheon.uf.common.activetable.PracticeActiveTableRecord;
import com.raytheon.uf.common.activetable.VTECChange;
import com.raytheon.uf.common.activetable.VTECPartners;
import com.raytheon.uf.common.activetable.VTECTableChangeNotification;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.python.PyUtil;
import com.raytheon.uf.common.python.PythonScript;
import com.raytheon.uf.common.site.SiteMap;
import com.raytheon.uf.common.status.IPerformanceStatusHandler;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.PerformanceStatus;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.util.ITimer;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.common.util.CollectionUtil;
import com.raytheon.uf.common.util.FileUtil;
import com.raytheon.uf.edex.core.EDEXUtil;
import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.database.cluster.ClusterLockUtils;
import com.raytheon.uf.edex.database.cluster.ClusterLockUtils.LockState;
import com.raytheon.uf.edex.database.cluster.ClusterTask;
import com.raytheon.uf.edex.database.cluster.handler.CurrentTimeClusterLockHandler;
import com.raytheon.uf.edex.database.dao.CoreDao;
import com.raytheon.uf.edex.database.dao.DaoConfig;
import com.raytheon.uf.edex.database.query.DatabaseQuery;

import jep.JepConfig;
import jep.JepException;

/**
 * ActiveTable container and logic. The ActiveTable is a legacy GFE concept of
 * currently active VTEC products for the configured site. Uses legacy Python
 * scripts for most of the logic to determine if VTEC products have expired, are
 * the same product/ETN, or new products.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 17, 2009            njensen     Initial creation
 * Dec 21, 2009    4055    njensen     Queued thread for updates
 * Feb 26, 2013    1447    dgilling    Add routine to use MergeVTEC as basis
 *                                     for merge logic.
 * May 14, 2013    1842    dgilling    Also delete cluster locks when purging
 *                                     PRACTICE active table.
 * Jun 11, 2013    2083    randerso    Log active table changes
 * Aug 29, 2013    1843    dgilling    Move ETN related methods to
 *                                     GetNextEtnUtil.
 * Mar 06, 2014    2883    randerso    Pass siteId into python code
 * Apr 10, 2014    3004    dgilling    Remove ActiveTableMode parameter from
 *                                     clearPracticeTable().
 * May 15, 2014    3157    dgilling    Add support for multiple TPC and SPC
 *                                     issuing sites.
 * Jun 17, 2014    3296    randerso    Cached PythonScript. Moved active table
 *                                     backup and purging to a separate thread.
 *                                     Added performance logging
 * Nov 14, 2014    4953    randerso    Moved dumpProductToTempFile into PracticeVtecDecoder
 *                                     since it had no reason to be in this class
 * Feb 05, 2015    16942   ryu         Fix update of records for sites other than
 *                                     the home site and its neighbors.
 *                                     Pass issuance site id to getActiveTable()
 *                                     in updateActiveTable() so records will
 *                                     be updated correctly.
 * Feb 05, 2015    4099    randerso    Fixed latest ETN query for year-end
 * Feb 23, 2015    4127    dgilling    Use cluster locking to only allow 1 active
 *                                     table write at a time.
 * Mar 04, 2015    4129    randerso    Pass active table change logger to ingestAt and/or MergeVTEC
 * Apr 28, 2015  #4027     randerso    Expunged Calendar from ActiveTableRecord,
 *                                     fixed next ETN query to query for >= Jan 1
 * May 22, 2015 4522       randerso    Create proper primary key for ActiveTableRecord
 * Jul 09, 2015 4500       rjpeter     Fix SQL Injection concern.
 * Dec 14, 2015 5166       kbisanz     Update logging to use SLF4J
 * </pre>
 *
 * @author njensen
 * @version 1.0
 */

public class ActiveTable {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(ActiveTable.class);

    private static final Logger changeLog = LoggerFactory
            .getLogger("ActiveTableChange");

    private static final String ACTIVE_TABLE_LOCK_NAME = "ActiveTableWriteLock";

    private static final long DEFAULT_LOCK_TIMEOUT = 5 * TimeUtil.MILLIS_PER_MINUTE;

    private static CoreDao practiceDao = new CoreDao(
            DaoConfig.forClass(PracticeActiveTableRecord.class));

    private static CoreDao operationalDao = new CoreDao(
            DaoConfig.forClass(OperationalActiveTableRecord.class));

    /**
     * Default constructor
     */
    public ActiveTable() {
    }

    /**
     * Returns the active table for a particular site. This is a wrapper around
     * the 5-parameter method.
     *
     * @param siteId
     *            the four letter site id to retrieve the active table for
     * @param mode
     *            the active table mode (PRACTICE or OPERATIONAL)
     * @return the active table corresponding to the site
     * @throws ExecutionException
     */
    public static List<ActiveTableRecord> getActiveTable(String siteId,
            ActiveTableMode mode) throws ExecutionException {
        return getActiveTable(siteId, mode, null, null, null, null);
    }

    /**
     * Returns the active table for a particular site. This is a wrapper around
     * the 5-parameter method.
     *
     * @param siteId
     *            the four letter site id to retrieve the active table for
     * @param mode
     *            the active table mode (PRACTICE or OPERATIONAL)
     * @param phensigList
     *            phensigs to include. If null, all phensigs will be included.
     * @param act
     *            the VTEC action. If null all actions will be included
     * @param etn
     *            the ETN. If null all ETNs will be included
     * @param requestValidTimes
     *            true if only valid times are to be returned
     * @return the active table corresponding to the input parameters
     * @throws ExecutionException
     */
    public static List<ActiveTableRecord> getActiveTable(String siteId,
            ActiveTableMode mode, String phensigList, String act, String etn,
            boolean requestValidTimes) throws ExecutionException {
        return getActiveTable(siteId, mode, phensigList, act, etn, null);
    }

    /**
     * Returns the active table for a particular site
     *
     * @param siteId
     *            the four letter site id to retrieve the active table for
     * @param mode
     *            the active table mode (PRACTICE or OPERATIONAL)
     * @param phensigList
     *            phensigs to include. If null, all phensigs will be included.
     * @param act
     *            the VTEC action. If null all actions will be included
     * @param etn
     *            the ETN. If null all ETNs will be included
     * @param requestValidTimes
     *            true if only valid times are to be returned
     * @param wfos
     *            wfos to include. If null or empty, wfos from site's
     *            VTECPartners file will be used. Include "all" in the array for
     *            unrestricted WFOs.
     * @return the active table corresponding to the input parameters
     * @throws ExecutionException
     */
    public static List<ActiveTableRecord> getActiveTable(String siteId,
            ActiveTableMode mode, String phensigList, String act, String etn,
            String[] wfos) throws ExecutionException {

        if ((wfos == null) || !Arrays.asList(wfos).contains("all")) {

            if ((wfos == null) || (wfos.length == 0)) {
                // default to WFOs from VTECPartners

                Set<String> wfoSet = getDecoderSites(siteId);
                wfoSet.add(siteId);
                wfos = wfoSet.toArray(new String[0]);
            }

            // We have an array of 3- or 4-char WFOs to filter against.
            // We need a String "KMFL,KTBW,..." for the query.
            SiteMap siteMap = SiteMap.getInstance();
            StringBuilder wfosb = new StringBuilder();
            String sep = "";
            for (String wfo : wfos) {
                if (wfo.length() < 4) {
                    wfo = siteMap.getSite4LetterId(wfo);
                }
                wfosb.append(sep).append(wfo);
                sep = ",";
            }
            siteId = wfosb.toString();
        }

        return queryTable(siteId, mode, phensigList, act, etn, null, false,
                false);
    }

    private static Set<String> getDecoderSites(String siteId)
            throws ExecutionException {
        SiteMap siteMap = SiteMap.getInstance();

        // Use the 3-char site or VTEC_DECODER_SITES will be empty
        Set<String> site3s = siteMap.getSite3LetterIds(siteId);
        Set<String> wfoSet = new HashSet<>();
        for (String site3 : site3s) {
            VTECPartners vtecPartners = VTECPartners.getInstance(site3);
            @SuppressWarnings("unchecked")
            List<String> wfoList = (List<String>) vtecPartners
                    .getattr("VTEC_DECODER_SITES");
            wfoSet.addAll(wfoList);
            Collection<String> spcSite = vtecPartners.getSpcSites();
            wfoSet.addAll(spcSite);
            Collection<String> tpcSite = vtecPartners.getTpcSites();
            wfoSet.addAll(tpcSite);
        }
        return wfoSet;
    }

    /**
     * Updates the active table with the new warnings
     *
     * @param siteId
     *            the site to update the active table for
     * @param newRecords
     *            the new VTEC products
     * @throws ExecutionException
     */
    private void updateActiveTable(String siteId,
            List<ActiveTableRecord> newRecords, float offsetSecs)
            throws ExecutionException {
        if (!newRecords.isEmpty()) {
            ActiveTableMode mode = ActiveTableMode.PRACTICE;
            if (newRecords.get(0) instanceof OperationalActiveTableRecord) {
                mode = ActiveTableMode.OPERATIONAL;
            }

            String issueSiteId = newRecords.get(0).getOfficeid();

            IPerformanceStatusHandler perfStat = PerformanceStatus
                    .getHandler("ActiveTable");
            ITimer timer = TimeUtil.getTimer();
            MergeResult result = null;
            ClusterTask writeLock = null;
            try {
                boolean logFirst = true;
                timer.start();
                do {
                    if (logFirst) {
                        statusHandler
                                .info("updateActiveTable() waiting on lock ["
                                        + ACTIVE_TABLE_LOCK_NAME + ":"
                                        + mode.toString() + "].");
                        logFirst = false;
                    }
                    writeLock = ClusterLockUtils.lock(ACTIVE_TABLE_LOCK_NAME,
                            mode.toString(), new CurrentTimeClusterLockHandler(
                                    DEFAULT_LOCK_TIMEOUT, false), true);
                } while (!writeLock.getLockState().equals(LockState.SUCCESSFUL));
                statusHandler
                        .info("updateActiveTable() obtained lock ["
                                + ACTIVE_TABLE_LOCK_NAME + ":"
                                + mode.toString() + "].");
                timer.stop();
                perfStat.logDuration("getLock", timer.getElapsedTime());

                timer.reset();
                timer.start();
                List<ActiveTableRecord> activeTable = getActiveTable(
                        issueSiteId, mode);
                timer.stop();
                perfStat.logDuration("getActiveTable", timer.getElapsedTime());
                // get decoder sites to see if we need to backup active table
                Set<String> decoderSites = getDecoderSites(siteId);
                // if any new record is from one of the decoder sites
                // we need to queue a backup
                for (ActiveTableRecord rec : newRecords) {
                    if (decoderSites.contains(rec.getOfficeid())) {
                        ActiveTableBackup.queue(mode, activeTable);
                        break;
                    }
                }
                timer.reset();
                timer.start();
                result = filterTable(siteId, activeTable, newRecords, mode,
                        offsetSecs);
                timer.stop();
                perfStat.logDuration("filterTable", timer.getElapsedTime());
                timer.reset();
                timer.start();
                updateTable(result, mode);
                timer.stop();
                perfStat.logDuration("updateTable", timer.getElapsedTime());
            } finally {
                if (writeLock != null) {
                    statusHandler.info("updateActiveTable() released lock ["
                            + ACTIVE_TABLE_LOCK_NAME + ":" + mode.toString()
                            + "].");
                    ClusterLockUtils.unlock(writeLock, true);
                }
            }

            if (!result.changeList.isEmpty()) {
                sendNotification(mode, result.changeList, "VTECDecoder");
            }
        }
    }

    /**
     * Runs the new VTEC products against the legacy logic to update the active
     * table
     *
     * @param siteId
     *            the ID of the ingest site
     * @param activeTable
     *            the current active table
     * @param newRecords
     *            the new VTEC products
     * @return a list of size 2, with the first inner list being the updated
     *         active table and the second being the purged records
     */
    private MergeResult filterTable(String siteId,
            List<ActiveTableRecord> activeTable,
            List<ActiveTableRecord> newRecords, ActiveTableMode mode,
            float offsetSecs) {
        HashMap<String, Object> args = new HashMap<>(5, 1.0f);
        args.put("siteId", siteId);
        args.put("activeTable", activeTable);
        args.put("newRecords", newRecords);
        args.put("logger", changeLog);
        args.put("mode", mode.toString());
        args.put("offsetSecs", offsetSecs);
        MergeResult result = null;
        try (PythonScript python = createPythonScript()) {
            try {
                result = (MergeResult) python.execute("mergeFromJava", args);
            } catch (JepException e) {
                statusHandler.error("Error updating active table", e);
            }
        } catch (Exception e) {
            statusHandler.error("Error initializing active table python", e);
        }

        return result;
    }

    private static void sendNotification(ActiveTableMode mode,
            List<VTECChange> changes, String source) {
        Date modTime = new Date();
        // VTECTableChangeNotifier.send(mode, modTime, "VTECDecoder", changes);
        try {
            VTECTableChangeNotification notification = new VTECTableChangeNotification(
                    mode, modTime, source,
                    changes.toArray(new VTECChange[changes.size()]));
            // System.out.println("Sending VTECTableChangeNotification:"
            // + notification);
            EDEXUtil.getMessageProducer().sendAsync("vtecNotify", notification);
        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error Sending VTECTableChangeNotification", e);
        }
    }

    /**
     * Queries the active table for a particular site
     *
     * @param siteId
     *            the four letter site id
     * @param mode
     *            the active table you want
     * @return the active table for the site
     */
    @SuppressWarnings("unchecked")
    private static List<ActiveTableRecord> queryTable(String siteId,
            ActiveTableMode mode, String phensigList, String action,
            String etn, Date currentTime, boolean requestValidTimes,
            boolean latestEtn) {
        DatabaseQuery query = null;
        CoreDao dao = null;

        if (mode.equals(ActiveTableMode.OPERATIONAL)) {
            query = new DatabaseQuery(OperationalActiveTableRecord.class);
            dao = operationalDao;
        } else {
            query = new DatabaseQuery(PracticeActiveTableRecord.class);
            dao = practiceDao;
        }

        if (phensigList != null) {
            query.addQueryParam("phensig", phensigList, "in");
        }

        if (action != null) {
            query.addQueryParam("act", action, "in");
        }

        if (etn != null) {
            query.addQueryParam("key.etn", etn, "in");
        }

        if (requestValidTimes && (currentTime != null)) {
            // Current Time
            query.addQueryParam("endTime", currentTime, ">");
        }
        if (latestEtn && (currentTime != null)) {
            Calendar yearStart = Calendar.getInstance(TimeZone
                    .getTimeZone("GMT"));
            yearStart.setTime(currentTime);
            yearStart.set(yearStart.get(Calendar.YEAR), Calendar.JANUARY, 1, 0,
                    0, 0);
            yearStart.set(Calendar.MILLISECOND, 0);
            query.addQueryParam("issueTime", yearStart.getTime(), ">=");
            query.addOrder("key.etn", false);
            query.setMaxResults(1);
        }

        query.addQueryParam("key.officeid", siteId, "in");

        List<ActiveTableRecord> result = null;
        try {
            result = (List<ActiveTableRecord>) dao.queryByCriteria(query);
        } catch (DataAccessLayerException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error querying active table for site " + siteId, e);
        }
        return result;
    }

    /**
     * Updates the active table
     *
     * @param changes
     *            the updated table followed by the purged records
     */
    private static void updateTable(MergeResult changes, ActiveTableMode mode) {
        List<ActiveTableRecord> updated = changes.updatedList;
        List<ActiveTableRecord> purged = changes.purgedList;

        // Check for multiple updates for the same active table key
        Map<ActiveTableKey, ActiveTableRecord> updatesMap = new HashMap<>(
                updated.size(), 1.0f);
        for (ActiveTableRecord rec : updated) {
            ActiveTableKey key = rec.getKey();
            ActiveTableRecord prevRec = updatesMap.get(key);

            // if multiple updates select the one with the latest issueTime
            if (prevRec == null) {
                updatesMap.put(key, rec);
            } else {
                if (rec.getIssueTime().after(prevRec.getIssueTime())) {
                    updatesMap.put(key, rec);
                }

                SimpleDateFormat sdf = new SimpleDateFormat(
                        "yyyy-MM-dd HH:mm:ss");
                sdf.setTimeZone(TimeZone.getTimeZone("GMT"));

                statusHandler.warn("Multiple updates received for: "
                        + rec.getKey().toString() + "\n   "
                        + prevRec.getVtecstr() + " " + prevRec.getUgcZone()
                        + " " + sdf.format(prevRec.getIssueTime()) + "\n   "
                        + rec.getVtecstr() + " " + rec.getUgcZone()
                        + sdf.format(rec.getIssueTime()));
            }
        }

        // Don't try to delete purged records that are being updated
        List<ActiveTableRecord> toDelete = new ArrayList<>(purged.size());
        for (ActiveTableRecord rec : purged) {
            if (!updatesMap.containsKey(rec.getKey())) {
                toDelete.add(rec);
            }
        }

        CoreDao dao = (ActiveTableMode.OPERATIONAL.equals(mode)) ? operationalDao
                : practiceDao;
        try {
            dao.bulkSaveOrUpdateAndDelete(updatesMap.values(), toDelete);
        } catch (NonUniqueObjectException e) {
            StringBuilder msg = new StringBuilder(
                    "Error saving updates to activetable\nUpdates:");
            for (ActiveTableRecord rec : updatesMap.values()) {
                msg.append("\n").append(rec.getAct()).append(" ")
                        .append(rec.getKey());
            }
            msg.append("\nDeletes:");
            for (ActiveTableRecord rec : toDelete) {
                msg.append("\n").append(rec.getAct()).append(" ")
                        .append(rec.getKey());
            }
            statusHandler.error(msg.toString(), e);
        }
    }

    /**
     * Merge new records into the active table
     *
     * @param newRecords
     *            records to be merged
     * @return Exception if any occurs during merge
     */
    public Exception merge(List<ActiveTableRecord> newRecords) {
        return merge(newRecords, 0.0f);
    }

    /**
     * Merge new records into the active table
     *
     * @param newRecords
     *            records to be merged
     * @param timeOffset
     *            time offset for practice mode in displaced real time mode
     * @return Exception if any occurs during merge
     */
    public Exception merge(List<ActiveTableRecord> newRecords, float timeOffset) {
        Exception exc = null;
        try {
            if (newRecords != null) {
                String siteId = SiteUtil.getSite();
                siteId = SiteMap.getInstance().getSite4LetterId(siteId);
                updateActiveTable(siteId, newRecords, timeOffset);
            }
        } catch (Throwable t) {

            String msg = "Error processing new VTEC products for active table";
            statusHandler.handle(Priority.PROBLEM, msg, t);
            exc = new Exception(msg, t);
        }
        return exc;
    }

    /**
     * Merges the specified new active table records into the active table using
     * the legacy MergeVTEC logic (which is different than the legacy logic in
     * ActiveTable.py used elsewhere).
     *
     * @param siteId
     *            Site ID to perform the merge as.
     * @param tableName
     *            Table to merge the records into.
     * @param newRecords
     *            The incoming new records to merge.
     * @param timeOffset
     *            For DRT; number of seconds from current time to use as base
     *            time for the merge.
     * @param makeBackup
     *            Whether or not to make a backup of the active table prior to
     *            the merge.
     * @param runIngestAT
     *            Whether this merge will be performed as ingestAT or MergeVTEC
     *            (has minor logging effects).
     * @param xmlSource
     *            Only required when <code>runIngestAT</code> is true; XML data
     *            from MHS about source of new records.
     * @throws JepException
     *             If an unhandled exception is encountered in the python code
     *             executed.
     * @throws ExecutionException
     *             If an unhandled exception is encountered trying to retrieve
     *             properties from the site's VTECPartners.py file.
     */
    public static void mergeRemoteTable(String siteId,
            ActiveTableMode tableName, List<ActiveTableRecord> newRecords,
            float timeOffset, boolean makeBackup, boolean runIngestAT,
            String xmlSource) throws JepException, ExecutionException {
        String scriptName = runIngestAT ? "ingestAT.py" : "MergeVTEC.py";
        IPathManager pathMgr = PathManagerFactory.getPathManager();
        LocalizationContext commonCx = pathMgr.getContext(
                LocalizationType.COMMON_STATIC, LocalizationLevel.BASE);
        String scriptPath = pathMgr.getFile(commonCx,
                FileUtil.join(ActiveTablePyIncludeUtil.VTEC, scriptName))
                .getPath();
        String pythonIncludePath = PyUtil.buildJepIncludePath(
                ActiveTablePyIncludeUtil.getCommonPythonIncludePath(),
                ActiveTablePyIncludeUtil.getVtecIncludePath(siteId),
                ActiveTablePyIncludeUtil.getGfeConfigIncludePath(siteId),
                ActiveTablePyIncludeUtil.getIscScriptsIncludePath());

        MergeResult result = null;
        PythonScript script = null;
        ClusterTask writeLock = null;
        try {
            try {
                script = new PythonScript(scriptPath, pythonIncludePath,
                        ActiveTable.class.getClassLoader());
            } catch (JepException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Error initializing ingestAT or MergeVTEC python", e);
                throw e;
            }

            boolean logFirst = true;
            do {
                if (logFirst) {
                    statusHandler.info("mergeRemoteTable() waiting on lock ["
                            + ACTIVE_TABLE_LOCK_NAME + ":"
                            + tableName.toString() + "].");
                    logFirst = false;
                }
                writeLock = ClusterLockUtils.lock(ACTIVE_TABLE_LOCK_NAME,
                        tableName.toString(),
                        new CurrentTimeClusterLockHandler(DEFAULT_LOCK_TIMEOUT,
                                false), true);
            } while (!writeLock.getLockState().equals(LockState.SUCCESSFUL));
            statusHandler.info("mergeRemoteTable() obtained lock ["
                    + ACTIVE_TABLE_LOCK_NAME + ":" + tableName.toString()
                    + "].");

            try {
                String site4Char = SiteMap.getInstance().getSite4LetterId(
                        siteId);
                List<ActiveTableRecord> activeTable = getActiveTable(site4Char,
                        tableName);
                HashMap<String, Object> args = new HashMap<>();
                args.put("activeTable", activeTable);
                args.put("activeTableMode", tableName.toString());
                args.put("newRecords", newRecords);
                args.put("drt", timeOffset);
                args.put("makeBackups", makeBackup);
                args.put("atChangeLog", changeLog);
                if (runIngestAT) {
                    args.put("xmlIncoming", xmlSource);
                }

                String methodName = runIngestAT ? "runFromJava" : "merge";
                result = (MergeResult) script.execute(methodName, args);
            } catch (JepException | ExecutionException e) {
                statusHandler.error("Error merging active table", e);
                throw e;
            }

            if (result != null) {
                updateTable(result, tableName);
            }
        } finally {
            if (writeLock != null) {
                statusHandler.info("mergeRemoteTable() released lock ["
                        + ACTIVE_TABLE_LOCK_NAME + ":" + tableName.toString()
                        + "].");
                ClusterLockUtils.unlock(writeLock, true);
            }

            if (script != null) {
                script.dispose();
                script = null;
            }
        }

        if ((result != null) && (!result.changeList.isEmpty())) {
            sendNotification(tableName, result.changeList, "MergeVTEC");
        }
    }

    /**
     * Clear the practice active table for the requested site
     *
     * @param requestedSiteId
     *            site ID
     * @throws DataAccessLayerException
     */
    public static void clearPracticeTable(String requestedSiteId)
            throws DataAccessLayerException {
        CoreDao dao = practiceDao;
        String sql = "delete from practice_activetable;";
        dao.executeSQLUpdate(sql);

        sql = "delete from cluster_task where name = :name";
        dao.executeSQLUpdate(sql, "name", GetNextEtnUtil.getEtnClusterLockName(
                requestedSiteId, ActiveTableMode.PRACTICE));
    }

    /**
     * Get the last assigned ETN for the specified site and phensig combination.
     *
     * @param siteId
     *            The 4-character site identifier.
     * @param mode
     *            The active table to search.
     * @param phensig
     *            The phenomenon and significance combination to search for.
     * @param currentTime
     *            <code>Date</code> representing time to perform search from
     *            (needed for DRT mode).
     * @return The last ETN assigned to the particular site and phensig
     *         combination, or <code>null</code> if no ETNs have been assigned
     *         to this combination.
     */
    public static Integer getLastUsedEtn(String siteId, ActiveTableMode mode,
            String phensig, Date currentTime) {
        Integer lastEtn = null;
        List<ActiveTableRecord> records = ActiveTable.queryTable(siteId, mode,
                phensig, null, null, currentTime, false, true);
        if (!CollectionUtil.isNullOrEmpty(records)) {
            // queryTable will return ActiveTableRecords sorted by ETN in
            // descending order, so the first record's ETN will always be the
            // last used ETN.
            lastEtn = Integer.parseInt(records.get(0).getEtn());
        }

        return lastEtn;
    }

    private static PythonScript createPythonScript() throws JepException {
        ITimer timer = TimeUtil.getTimer();
        timer.start();
        IPathManager pathMgr = PathManagerFactory.getPathManager();
        LocalizationContext commonCx = pathMgr.getContext(
                LocalizationType.COMMON_STATIC, LocalizationLevel.BASE);
        String filePath = pathMgr
                .getFile(commonCx, "vtec" + File.separator + "ActiveTable.py")
                .getPath();
        String siteId = pathMgr.getContext(LocalizationType.COMMON_STATIC,
                LocalizationLevel.SITE).getContextName();
        String includePath = PyUtil.buildJepIncludePath(
                ActiveTablePyIncludeUtil.getCommonPythonIncludePath(),
                ActiveTablePyIncludeUtil.getVtecIncludePath(siteId),
                ActiveTablePyIncludeUtil.getGfeConfigIncludePath(siteId));

        PythonScript python = new PythonScript(
                new JepConfig().setIncludePath(includePath).setClassLoader(
                        ActiveTable.class.getClassLoader()),
                filePath);
        timer.stop();
        PerformanceStatus.getHandler("ActiveTable")
                .logDuration("create PythonScript", timer.getElapsedTime());
        return python;
    }
}
