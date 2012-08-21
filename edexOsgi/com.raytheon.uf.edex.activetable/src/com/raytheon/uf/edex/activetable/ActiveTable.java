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
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;

import jep.JepException;

import com.raytheon.edex.util.Util;
import com.raytheon.uf.common.activetable.ActiveTableMode;
import com.raytheon.uf.common.activetable.ActiveTableRecord;
import com.raytheon.uf.common.activetable.MergeResult;
import com.raytheon.uf.common.activetable.OperationalActiveTableRecord;
import com.raytheon.uf.common.activetable.PracticeActiveTableRecord;
import com.raytheon.uf.common.activetable.VTECChange;
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
import com.raytheon.uf.edex.core.EDEXUtil;
import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.database.cluster.ClusterLockUtils;
import com.raytheon.uf.edex.database.cluster.ClusterLockUtils.LockState;
import com.raytheon.uf.edex.database.cluster.ClusterTask;
import com.raytheon.uf.edex.database.cluster.handler.CurrentTimeClusterLockHandler;
import com.raytheon.uf.edex.database.dao.CoreDao;
import com.raytheon.uf.edex.database.dao.DaoConfig;
import com.raytheon.uf.edex.database.query.DatabaseQuery;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Geometry;

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
 * Dec 21, 2009    4055  njensen    Queued thread for updates
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class ActiveTable {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(ActiveTable.class);

    public static final String NATIONAL_CENTERS = ",KWNS,KNHC";

    private static final String NEXT_ETN_LOCK = "ActiveTableNextEtn";

    private static String filePath;

    private static String pythonPath;

    private static String vtecPath;

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
        pythonPath = pathMgr.getFile(commonCx, "python").getPath();
        vtecPath = pathMgr.getFile(commonCx, "vtec").getPath();
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

    public static Integer getNextEtn(String siteId, ActiveTableMode mode,
            String phensig, Calendar currentTime, boolean isLock) {
        String lockName = NEXT_ETN_LOCK + "_" + siteId + "_" + mode.name();
        ClusterTask ct = null;
        CurrentTimeClusterLockHandler lockHandler = null;
        if (isLock) {
            lockHandler = new CurrentTimeClusterLockHandler(15000, false);
            do {
                ct = ClusterLockUtils
                        .lock(lockName, phensig, lockHandler, true);
            } while (!ct.getLockState().equals(LockState.SUCCESSFUL));
            statusHandler.info("Locking::[lockName = " + lockName
                    + ", phensig = " + phensig + "]");
        } else {
            ct = ClusterLockUtils.lookupLock(lockName, phensig);
        }

        int nextEtn = 1;
        List<ActiveTableRecord> records = queryTable(siteId, mode, phensig,
                null, null, currentTime, false, true);

        if (records != null && records.size() > 0) {
            // should only be 1
            nextEtn = Integer.parseInt(records.get(0).getEtn()) + 1;
        }

        String year = "" + currentTime.get(Calendar.YEAR);
        String eInfo = ct.getExtraInfo();
        if (eInfo != null && eInfo.startsWith(year)) {
            // parse year info
            try {
                int ctNextEtn = Integer
                        .parseInt(eInfo.substring(year.length() + 1)) + 1;
                if (ctNextEtn > nextEtn) {
                    nextEtn = ctNextEtn;
                }
            } catch (Exception e) {
                statusHandler.error(
                        "Caught excetion parsing etn from cluster_task", e);
            }
        }

        if (isLock) {
            lockHandler.setExtraInfo(year + ":" + nextEtn);
            ClusterLockUtils.unlock(ct, false);
            statusHandler.info("Unlocking::[nextEtn = " + nextEtn + "]");
        }

        return new Integer(nextEtn);
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

            MergeResult result = filterTable(getActiveTable(siteId, mode),
                    newRecords, offsetSecs);

            updateTable(siteId, result, mode);

            if (result.changeList.size() > 0) {
                sendNotification(mode, result.changeList);
            }
        }
    }

    /**
     * Runs the new VTEC products against the legacy logic to update the active
     * table
     * 
     * @param activeTable
     *            the current active table
     * @param newRecords
     *            the new VTEC products
     * @return a list of size 2, with the first inner list being the updated
     *         active table and the second being the purged records
     */
    private MergeResult filterTable(List<ActiveTableRecord> activeTable,
            List<ActiveTableRecord> newRecords, float offsetSecs) {
        HashMap<String, Object> args = new HashMap<String, Object>(2);
        args.put("activeTable", activeTable);
        args.put("newRecords", newRecords);
        args.put("offsetSecs", offsetSecs);
        MergeResult result = null;
        try {
            try {
                python = new PythonScript(filePath, PyUtil.buildJepIncludePath(
                        pythonPath, vtecPath),
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

    private void sendNotification(ActiveTableMode mode, List<VTECChange> changes) {
        Date modTime = new Date();
        // VTECTableChangeNotifier.send(mode, modTime, "VTECDecoder", changes);
        try {
            VTECTableChangeNotification notification = new VTECTableChangeNotification(
                    mode, modTime, "VTECDecoder",
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
    private void updateTable(String siteId, MergeResult changes,
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

    public void dispose() {
        python.dispose();
    }

    public static void clearPracticeTable(String requestedSiteId,
            ActiveTableMode mode) throws DataAccessLayerException {
        CoreDao dao = practiceDao;
        String sql = "delete from practice_activetable;";
        dao.executeNativeSql(sql);
    }

    /**
     * Convert the active table to a list of Map<String, ?>s. Doing it directly
     * in Java eliminates the need for Python paths, handling JepExceptions, and
     * at least one Python/Java conversion of the active table.
     * 
     * @param records
     *            A list of ActiveTableRecords to convert to
     *            Map<String,Object>s.
     * @return records, converted to a list of Maps.
     * 
     *         TODO: move this method to a static utility class
     */
    public static List<Map<String, Object>> convertToDict(
            List<ActiveTableRecord> records, String site) {

        List<Map<String, Object>> dicts = new ArrayList<Map<String, Object>>(
                records.size());
        for (ActiveTableRecord atr : records) {
            Map<String, Object> template = new HashMap<String, Object>();
            template.put("vtecstr", atr.getVtecstr());
            template.put("etn", Integer.valueOf(atr.getEtn()));
            template.put("sig", atr.getSig());
            template.put("phen", atr.getPhen());
            if (atr.getSegText() != null) {
                template.put("segText", atr.getSegText());
            }
            if (atr.getOverviewText() != null) {
                template.put("overviewText", atr.getOverviewText());
                template.put("hdln", atr.getOverviewText());
            }
            template.put("phensig", atr.getPhensig());
            template.put("act", atr.getAct());
            template.put("seg", atr.getSeg());
            template.put("startTime",
                    atr.getStartTime().getTimeInMillis() / 1000);
            template.put("endTime", atr.getEndTime().getTimeInMillis() / 1000);
            template.put("ufn", atr.isUfn());
            template.put("officeid", atr.getOfficeid());
            template.put("purgeTime",
                    atr.getPurgeTime().getTimeInMillis() / 1000);
            template.put("issueTime",
                    atr.getIssueTime().getTimeInMillis() / 1000);
            template.put("state", "Decoded");
            template.put("xxxid", atr.getXxxid());

            template.put("pil",
                    remapPil(site, atr.getPhen(), atr.getSig(), atr.getPil()));
            template.put("productClass", atr.getProductClass());

            template.put("id", atr.getUgcZone());

            template.put("rawMessage", atr.getRawmessage());
            template.put("countyheader", atr.getCountyheader());
            Calendar floodBegin = atr.getFloodBegin();
            if (floodBegin != null) {
                long floodBeginMillis = floodBegin.getTimeInMillis();
                if (floodBeginMillis != 0) {
                    template.put("floodBegin", floodBeginMillis / 1000);
                }
            }
            template.put("wmoid", atr.getWmoid());

            // Warngen fields
            Calendar floodCrest = atr.getFloodCrest();
            if (floodCrest != null) {
                long floodCrestMillis = floodCrest.getTimeInMillis();
                if (floodCrestMillis != 0) {
                    template.put("floodCrest", floodCrestMillis / 1000);
                }
            }
            Calendar floodEnd = atr.getFloodEnd();
            if (floodEnd != null) {
                long floodEndMillis = floodEnd.getTimeInMillis();
                if (floodEndMillis != 0) {
                    template.put("floodBegin", floodEndMillis / 1000);
                }
            }
            String floodStatus = atr.getFloodRecordStatus();
            if (floodStatus != null && !"".equals(floodStatus.trim())) {
                template.put("floodrecordstatus", floodStatus);
            }
            String floodSeverity = atr.getFloodSeverity();
            if (floodSeverity != null && !"".equals(floodSeverity.trim())) {
                template.put("floodseverity", floodSeverity);
            }

            Geometry geometry = atr.getGeometry();
            if (geometry != null && !geometry.isEmpty()) {
                StringBuilder sb = new StringBuilder();
                String sep = "";
                long lat;
                long lon;
                for (Coordinate coordinate : geometry.getCoordinates()) {
                    sb.append(sep);
                    sep = " ";
                    lat = Math.round(Math.abs(coordinate.y) * 100.0);
                    lon = Math.round(Math.abs(coordinate.x) * 100.0);
                    sb.append(String.format("%d %d", lat, lon));
                }
                template.put("geometry", sb.toString());
            }

            String immediateCause = atr.getImmediateCause();
            if (immediateCause != null && !"".equals(immediateCause.trim())) {
                template.put("immediateCause", immediateCause);
            }

            String loc = atr.getLoc();
            if (loc != null && !"".equals(loc.trim())) {
                template.put("loc", loc);
            }

            String locationId = atr.getLocationID();
            if (locationId != null && !"".equals(locationId.trim())) {
                template.put("locationId", locationId);
            }

            Integer motdir = atr.getMotdir();
            if (motdir != null) {
                template.put("motdir", motdir);
            }

            Integer motspd = atr.getMotspd();
            if (motspd != null) {
                template.put("motspd", motspd);
            }

            dicts.add(template);
        }
        return dicts;
    }

    /**
     * Some events are issued in one PIL and cancelled or extended in another.
     * This finds the PIL needed.
     * 
     * @param siteID
     *            The site from which
     * @param phen
     *            The phenomenon code to look for
     * @param sig
     *            The significance code to look for
     * @param dft
     *            The PIL to use if the phensig is not remapped
     * @return The PIL after remapping.
     */
    @SuppressWarnings("unchecked")
    protected static String remapPil(String siteId, String phen, String sig,
            String dft) {
        String result = dft;
        Map<Object, String> MappedPils = (Map<Object, String>) VTECPartners
                .getInstance(siteId).getattr("VTEC_MAPPED_PILS");
        List<String> key = new ArrayList<String>(3);
        key.add(phen);
        key.add(sig);
        key.add(dft);
        String mPil = MappedPils.get(key);
        if (mPil != null) {
            result = mPil;
        }
        return result;
    }

    public static File dumpProductToTempFile(String productText) {
        File file = Util.createTempFile(productText.getBytes(), "vtec");
        file.deleteOnExit();
        return file;
    }

}
