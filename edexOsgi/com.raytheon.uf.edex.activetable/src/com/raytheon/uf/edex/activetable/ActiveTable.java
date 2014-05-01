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
import java.util.Arrays;
import java.util.Calendar;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;

import jep.JepException;

import org.apache.log4j.Logger;

import com.raytheon.edex.util.Util;
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
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.util.CollectionUtil;
import com.raytheon.uf.common.util.FileUtil;
import com.raytheon.uf.edex.core.EDEXUtil;
import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.database.dao.CoreDao;
import com.raytheon.uf.edex.database.dao.DaoConfig;
import com.raytheon.uf.edex.database.query.DatabaseQuery;

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
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class ActiveTable {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(ActiveTable.class);

    private static final Logger changeLog = Logger
            .getLogger("ActiveTableChange");

    private static String filePath;

    private static String includePath;

    private static CoreDao practiceDao = new CoreDao(
            DaoConfig.forClass(PracticeActiveTableRecord.class));

    private static CoreDao operationalDao = new CoreDao(
            DaoConfig.forClass(OperationalActiveTableRecord.class));

    private PythonScript python;

    static {
        IPathManager pathMgr = PathManagerFactory.getPathManager();
        LocalizationContext commonCx = pathMgr.getContext(
                LocalizationType.COMMON_STATIC, LocalizationLevel.BASE);
        filePath = pathMgr.getFile(commonCx,
                "vtec" + File.separator + "ActiveTable.py").getPath();
        String siteId = pathMgr.getContext(LocalizationType.COMMON_STATIC,
                LocalizationLevel.SITE).getContextName();
        String pythonPath = ActiveTablePyIncludeUtil
                .getCommonPythonIncludePath();
        String vtecPath = ActiveTablePyIncludeUtil.getVtecIncludePath(siteId);
        String configPath = ActiveTablePyIncludeUtil
                .getGfeConfigIncludePath(siteId);
        includePath = PyUtil.buildJepIncludePath(pythonPath, vtecPath,
                configPath);
    }

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
     */
    public static List<ActiveTableRecord> getActiveTable(String siteId,
            ActiveTableMode mode) {
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
     * @param requestValidTimes
     *            true if only valid times are to be returned
     * @return the active table corresponding to the input parameters
     */
    public static List<ActiveTableRecord> getActiveTable(String siteId,
            ActiveTableMode mode, String phensigList, String act, String etn,
            boolean requestValidTimes) {
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
     * @param requestValidTimes
     *            true if only valid times are to be returned
     * @param wfos
     *            wfos to include. If null or empty, wfos from site's
     *            VTECPartners file will be used. Include "all" in the array for
     *            unrestricted WFOs.
     * @return the active table corresponding to the input parameters
     */
    public static List<ActiveTableRecord> getActiveTable(String siteId,
            ActiveTableMode mode, String phensigList, String act, String etn,
            String[] wfos) {

        if (wfos == null || !Arrays.asList(wfos).contains("all")) {
            SiteMap siteMap = SiteMap.getInstance();

            if (wfos == null || wfos.length == 0) {
                // default to WFOs from VTECPartners

                // Use the 3-char site or VTEC_DECODER_SITES will be empty
                Set<String> site3s = siteMap.getSite3LetterIds(siteId);
                Set<String> wfoSet = new TreeSet<String>();
                for (String site3 : site3s) {
                    VTECPartners vtecPartners = VTECPartners.getInstance(site3);
                    List<String> wfoList = (List<String>) vtecPartners
                            .getattr("VTEC_DECODER_SITES");
                    wfoSet.addAll(wfoList);
                    String spcSite = (String) vtecPartners
                            .getattr("VTEC_SPC_SITE");
                    wfoSet.add(spcSite);
                    String tpcSite = (String) vtecPartners
                            .getattr("VTEC_TPC_SITE");
                    wfoSet.add(tpcSite);
                }
                wfoSet.add(siteId);
                wfos = wfoSet.toArray(new String[0]);
            }

            // We have an array of 3- or 4-char WFOs to filter against.
            // We need a String "KMFL,KTBW,..." for the query.
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

    /**
     * Updates the active table with the new warnings
     * 
     * @param siteId
     *            the site to update the active table for
     * @param newRecords
     *            the new VTEC products
     */
    private void updateActiveTable(String siteId,
            List<ActiveTableRecord> newRecords, float offsetSecs) {
        if (newRecords.size() > 0) {
            ActiveTableMode mode = ActiveTableMode.PRACTICE;
            if (newRecords.get(0) instanceof OperationalActiveTableRecord) {
                mode = ActiveTableMode.OPERATIONAL;
            }

            MergeResult result = filterTable(siteId,
                    getActiveTable(siteId, mode), newRecords, mode, offsetSecs);

            updateTable(siteId, result, mode);

            if (result.changeList.size() > 0) {
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
        HashMap<String, Object> args = new HashMap<String, Object>(5, 1.0f);
        args.put("siteId", siteId);
        args.put("activeTable", activeTable);
        args.put("newRecords", newRecords);
        args.put("logger", changeLog);
        args.put("mode", mode.toString());
        args.put("offsetSecs", offsetSecs);
        MergeResult result = null;
        try {
            try {
                python = new PythonScript(filePath, includePath,
                        ActiveTable.class.getClassLoader());
                try {
                    result = (MergeResult) python
                            .execute("mergeFromJava", args);
                } catch (JepException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Error updating active table", e);
                }
            } catch (JepException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Error initializing active table python", e);
            }
        } finally {
            if (python != null) {
                python.dispose();
                python = null;
            }
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
            String etn, Calendar currentTime, boolean requestValidTimes,
            boolean latestEtn) {
        synchronized (ActiveTable.class) {
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
                query.addQueryParam("etn", etn, "in");
            }

            if (requestValidTimes && currentTime != null) {
                // Current Time
                query.addQueryParam("endTime", currentTime, "greater_than");
            }
            if (latestEtn && currentTime != null) {
                Calendar yearStart = Calendar.getInstance();
                yearStart.set(currentTime.get(Calendar.YEAR), Calendar.JANUARY,
                        0, 0, 0);
                query.addQueryParam("startTime", yearStart, "greater_than");
                query.addOrder("etn", false);
                query.setMaxResults(1);
            }

            query.addQueryParam("officeid", siteId, "in");

            List<ActiveTableRecord> result = null;
            try {
                result = (List<ActiveTableRecord>) dao.queryByCriteria(query);
            } catch (DataAccessLayerException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Error querying active table for site " + siteId, e);
            }
            return result;
        }
    }

    /**
     * Replaces the active table for the site with the new active table
     * 
     * @param siteId
     *            the four letter site id to replace
     * @param changes
     *            the updated table followed by the purged records
     */
    private static void updateTable(String siteId, MergeResult changes,
            ActiveTableMode mode) {
        synchronized (ActiveTable.class) {
            List<ActiveTableRecord> updated = changes.updatedList;
            List<ActiveTableRecord> purged = changes.purgedList;

            CoreDao dao = null;
            if (mode.equals(ActiveTableMode.OPERATIONAL)) {
                dao = operationalDao;
            } else {
                dao = practiceDao;
            }
            for (ActiveTableRecord update : updated) {
                dao.saveOrUpdate(update);
            }
            for (ActiveTableRecord delete : purged) {
                dao.delete(delete);
            }
        }
    }

    public Exception merge(List<ActiveTableRecord> newRecords) {
        return merge(newRecords, 0.0f);
    }

    public Exception merge(List<ActiveTableRecord> newRecords, float timeOffset) {
        Exception exc = null;
        try {
            if (newRecords != null) {
                String siteId = newRecords.get(0).getOfficeid();
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
     */
    public static void mergeRemoteTable(String siteId,
            ActiveTableMode tableName, List<ActiveTableRecord> newRecords,
            float timeOffset, boolean makeBackup, boolean runIngestAT,
            String xmlSource) throws JepException {
        MergeResult result = null;
        PythonScript script = null;
        try {
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

            try {
                script = new PythonScript(scriptPath, pythonIncludePath,
                        ActiveTable.class.getClassLoader());
            } catch (JepException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Error initializing ingestAT or MergeVTEC python", e);
                throw e;
            }

            try {
                String site4Char = SiteMap.getInstance().getSite4LetterId(
                        siteId);
                List<ActiveTableRecord> activeTable = getActiveTable(site4Char,
                        tableName);
                HashMap<String, Object> args = new HashMap<String, Object>();
                args.put("activeTable", activeTable);
                args.put("activeTableMode", tableName.toString());
                args.put("newRecords", newRecords);
                args.put("drt", timeOffset);
                args.put("makeBackups", makeBackup);
                if (runIngestAT) {
                    args.put("xmlIncoming", xmlSource);
                }

                String methodName = runIngestAT ? "runFromJava" : "merge";
                result = (MergeResult) script.execute(methodName, args);
            } catch (JepException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Error merging active table", e);
                throw e;
            }
        } finally {
            if (script != null) {
                script.dispose();
                script = null;
            }
        }

        if (result != null) {
            updateTable(siteId, result, tableName);
            if (!result.changeList.isEmpty()) {
                sendNotification(tableName, result.changeList, "MergeVTEC");
            }
        }
    }

    public void dispose() {
        python.dispose();
    }

    public static void clearPracticeTable(String requestedSiteId,
            ActiveTableMode mode) throws DataAccessLayerException {
        CoreDao dao = practiceDao;
        String sql = "delete from practice_activetable;";
        dao.executeNativeSql(sql);

        sql = "delete from cluster_task where name ='"
                + GetNextEtnUtil.getEtnClusterLockName(requestedSiteId,
                        ActiveTableMode.PRACTICE) + "';";
        dao.executeNativeSql(sql);
    }

    public static File dumpProductToTempFile(String productText) {
        File file = Util.createTempFile(productText.getBytes(), "vtec");
        file.deleteOnExit();
        return file;
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
     *            <code>Calendar</code> representing time to perform search from
     *            (needed for DRT mode).
     * @return The last ETN assigned to the particular site and phensig
     *         combination, or <code>null</code> if no ETNs have been assigned
     *         to this combination.
     */
    public static Integer getLastUsedEtn(String siteId, ActiveTableMode mode,
            String phensig, Calendar currentTime) {
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
}
