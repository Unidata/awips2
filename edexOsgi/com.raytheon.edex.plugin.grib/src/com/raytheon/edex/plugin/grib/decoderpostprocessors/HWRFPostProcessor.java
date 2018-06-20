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

package com.raytheon.edex.plugin.grib.decoderpostprocessors;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import com.raytheon.edex.plugin.grib.exception.GribException;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.grid.GridInfoRecord;
import com.raytheon.uf.common.dataplugin.grid.GridRecord;
import com.raytheon.uf.common.dataquery.db.QueryResult;
import com.raytheon.uf.common.dataquery.db.QueryResultRow;
import com.raytheon.uf.common.gridcoverage.GridCoverage;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.edex.database.cluster.ClusterLockUtils;
import com.raytheon.uf.edex.database.cluster.ClusterLockUtils.LockState;
import com.raytheon.uf.edex.database.cluster.ClusterTask;
import com.raytheon.uf.edex.database.cluster.handler.CurrentTimeClusterLockHandler;
import com.raytheon.uf.edex.plugin.grid.dao.GridDao;
import com.vividsolutions.jts.geom.Geometry;

/**
 * HWRF post processor implementation.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * Sep 10, 2015 4819        rferrel     Initial Creation
 * Oct 07, 2015 3756        nabowle     Extends DecoderPostProcessor.
 *
 * </pre>
 *
 * @author rferrel
 * @version 1
 */
public class HWRFPostProcessor extends DecoderPostProcessor {

    /**
     * Format for datasetId prefix <model>-<dx><spacingUnit>-.
     */
    private static final String DS_ID_FORMAT = "HWRF-%.2f-%s-";

    /**
     * Cluster lock task name.
     */
    private static final String TASK_NAME = "HWRF_Post_Processor";

    /**
     * Cluster lock details.
     */
    private static final String DETAILS = "HWRF_process";

    /**
     * Wait 10 seconds for the cluster lock.
     */
    private static final int TIMEOUT = 10000;

    protected final IUFStatusHandler logger = UFStatus.getHandler(getClass());

    /**
     * Cache of girdInfo list for a given datasetId prefix.
     */
    private final Map<String, List<GridInfoRecord>> gridInfoLists = new ConcurrentHashMap<>();

    /**
     * Next scheduled time to clear the gridInfo Lists in order to remove purged
     * data.
     */
    private volatile long nextPurgeGridInfoLists = 0L;

    @Override
    public GridRecord[] process(GridRecord record) throws GribException {

        if (TimeUtil.currentTimeMillis() >= nextPurgeGridInfoLists) {
            nextPurgeGridInfoLists = TimeUtil.currentTimeMillis()
                    + TimeUtil.MILLIS_PER_HOUR;
            gridInfoLists.clear();
        }

        GridCoverage gc = record.getLocation();

        double dx = gc.getDx();
        String spacingUnit = gc.getSpacingUnit();
        String datasetIdPrefix = String.format(DS_ID_FORMAT, dx, spacingUnit);

        List<GridInfoRecord> giList = gridInfoLists.get(datasetIdPrefix);

        if (giList == null) {
            queryUpdateRecord(record, datasetIdPrefix);
        } else {
            String datasetId = findMatchingDatasetId(record.getInfo(), giList);
            if (datasetId == null) {
                /*
                 * Get new datasetId while locked just in case another process
                 * created it first.
                 */
                queryUpdateRecord(record, datasetIdPrefix);
            } else {
                record.getInfo().setDatasetId(datasetId);
            }
        }
        return new GridRecord[] { record };
    }

    /**
     * Add the count in ct's extrainfo to the dataset name prefix.
     *
     * @param ct
     * @param datasetIdPrefix
     * @return datasetId
     */
    private String createDatasetId(CurrentTimeClusterLockHandler handler,
            String datasetIdPrefix) {
        String stormCntString = handler.getExtraInfo();
        int stormCnt = 0;
        if ((stormCntString == null) || stormCntString.isEmpty()) {
            stormCnt = 1;
        } else {
            try {
                stormCnt = Integer.parseInt(stormCntString) + 1;
            } catch (NumberFormatException ex) {
                stormCnt = 1;
            }
        }
        String newStormCntString = Integer.toString(stormCnt);
        handler.setExtraInfo(newStormCntString);
        return datasetIdPrefix + newStormCntString;
    }

    /**
     * Find datasetId a result grid that has > 95% intersection with the record.
     *
     * @param record
     * @param result
     * @return datasetId or null when > 95% intersection not found
     */
    private String findMatchingDatasetId(GridInfoRecord recordInfo,
            List<GridInfoRecord> giList) {
        if (giList.isEmpty()) {
            return null;
        }

        GridCoverage coverage = recordInfo.getLocation();
        Geometry geo = coverage.getGeometry();
        double area = geo.getArea();

        for (GridInfoRecord gi : giList) {
            Geometry resultGeo = gi.getLocation().getGeometry();
            /*
             * TODO It is possible a storm might be over the dataline, I would
             * expect this approach of intersecting to fail horribly if a
             * hurricane approached and then crossed the dateline.
             */
            double intersect = geo.intersection(resultGeo).getArea();
            if ((intersect / area) > 0.95) {
                return gi.getDatasetId();
            }
        }
        return null;
    }

    /**
     * While locked get datasetId for the record's grid info and update the grid
     * info list associated with dsIdPrefix.
     *
     * @param record
     * @param datasetIdPrefix
     */
    private void queryUpdateRecord(GridRecord record, String datasetIdPrefix) {
        ClusterTask ct = null;

        /*
         * Perform lock
         */
        try {
            /*
             * The other lock methods clear the cluster tasks extraInfo where
             * the count is being kept.
             */
            CurrentTimeClusterLockHandler handler = new CurrentTimeClusterLockHandler(
                    TIMEOUT, false);
            do {
                ct = ClusterLockUtils.lock(TASK_NAME, DETAILS, handler, true);
            } while (!ct.getLockState().equals(LockState.SUCCESSFUL));

            handler.setExtraInfo(ct.getExtraInfo());

            /*
             * get list of unique grid_coverage to grid_info. records with
             * datasetId like 'HWRF-<dx>-<spacingUnit>-%' whose coverage is the
             * same type as this coverage.
             */
            GridDao dao = new GridDao();
            StringBuilder query = new StringBuilder();
            query.append("SELECT distinct(gi) FROM ");
            query.append(GridInfoRecord.class.getName()).append(" gi WHERE ");
            query.append("gi.datasetId LIKE :datasetId");

            Map<String, Object> params = new HashMap<>();
            params.put("datasetId", datasetIdPrefix + "%");
            QueryResult result = dao.executeHQLQuery(query.toString(), params);
            List<GridInfoRecord> giList = null;

            if (result.getResultCount() > 0) {
                // Make room for possible new entry
                giList = new ArrayList<>(result.getResultCount() + 1);
                for (QueryResultRow row : result.getRows()) {
                    giList.add((GridInfoRecord) row.getColumn(0));
                }
            } else {
                // Make room for new entry
                giList = new ArrayList<>(1);
            }
            gridInfoLists.put(datasetIdPrefix, giList);

            /*
             * If matches, and coverage has >95% intersection, use that
             * datasetId
             */
            GridInfoRecord recordInfo = record.getInfo();
            String datasetId = findMatchingDatasetId(recordInfo, giList);

            if (datasetId == null) {
                datasetId = createDatasetId(handler, datasetIdPrefix);
                recordInfo.setDatasetId(datasetId);

                if (dao.validateDataset(record) == false) {
                    logger.handle(Priority.WARN, "Unable to update record: "
                            + record);
                }
                giList.add(recordInfo);
            } else {
                recordInfo.setDatasetId(datasetId);
            }

        } catch (PluginException e) {
            logger.handle(Priority.PROBLEM,
                    "Problem with HWRF Post Processing ", e);
        } finally {
            // unlock
            if (ct != null) {
                ClusterLockUtils.unlock(ct, false);
            }
        }
    }
}
