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
package com.raytheon.viz.hydrocommon.datamanager;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.TimeZone;

import com.raytheon.uf.common.dataplugin.shef.data.Observation;
import com.raytheon.uf.common.dataplugin.shef.tables.Curpc;
import com.raytheon.uf.common.dataplugin.shef.tables.CurpcId;
import com.raytheon.uf.common.dataplugin.shef.tables.Curpp;
import com.raytheon.uf.common.dataplugin.shef.tables.CurppId;
import com.raytheon.uf.common.dataplugin.shef.tables.Riverstatus;
import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.time.SimulatedTime;
import com.raytheon.uf.viz.core.catalog.DirectDbQuery;
import com.raytheon.uf.viz.core.catalog.DirectDbQuery.QueryLanguage;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.hydrocommon.HydroConstants;
import com.raytheon.viz.hydrocommon.data.RainData;
import com.raytheon.viz.hydrocommon.data.RiverData;
import com.raytheon.viz.hydrocommon.data.RiverStat;
import com.raytheon.viz.hydrocommon.pdc.PDCConstants;
import com.raytheon.viz.hydrocommon.pdc.PDCConstants.TimeModeType;
import com.raytheon.viz.hydrocommon.pdc.PDCDBUtils;
import com.raytheon.viz.hydrocommon.pdc.PDCOptionData;
import com.raytheon.viz.hydrocommon.pdc.PDCOptions;
import com.raytheon.viz.hydrocommon.pdc.data.IngestFilter;
import com.raytheon.viz.hydrocommon.pdc.data.LocPDC;
import com.raytheon.viz.hydrocommon.pdc.data.PointDataPreset;
import com.raytheon.viz.hydrocommon.util.HydroUtils;
import com.raytheon.viz.hydrocommon.whfslib.PrecipUtil;

/**
 * Point Data Control Data Access Manager.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 22, 2008            mpduff      Initial creation
 * Jul 21, 2015 4500       rjpeter     Use Number in blind cast.
 * May 23, 2016 #5590      bkowal      {@link Observation} relocated to common.
 * Jun 29, 2018 6839       tgurney     getLocPDC() actually use the provided
 *                                     where clause
 * Jun 29, 2018 6839       tgurney     Clean up method signatures
 * Sep 07, 2018 6979       mduff       Clean up SonarQube comments.
 * Sep 21, 2018 7379       mduff       Support PDC Refactor.
 * </pre>
 *
 * @author mpduff
 */

public class PDCDataManager extends HydroDataManager {
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(PDCDataManager.class);

    private static PDCDataManager instance = null;

    private static Map<String, String> peMap = null;

    private static final String obsQueryHead = "select lid, pe, dur, ts, extremum, obstime, "
            + "value, shef_qual_code, quality_code, revision, product_id, producttime, postingtime from ";

    private static Map<String, List<IngestFilter>> ingestFilterMap = null;

    private static Map<String, RiverStat> riverStatMap = null;

    private String shefpost = null;

    private boolean postLatest = false;

    /**
     * Private constructor.
     */
    private PDCDataManager() {

    }

    public static synchronized PDCDataManager getInstance() {
        if (instance == null) {
            instance = new PDCDataManager();
        }

        return instance;
    }

    public List<String[]> getPeTs() {
        List<String[]> result = new ArrayList<>();

        String sql = "select distinct pe, ts from ingestfilter where ts like 'R%%' OR ts LIKE 'P%%'";
        List<Object[]> resultset = runQuery(sql);

        for (Object[] oa : resultset) {
            String[] sa = new String[2];
            sa[0] = (String) oa[0];
            sa[1] = (String) oa[1];
            result.add(sa);
        }
        return result;
    }

    public String[] getIngestFilterPE() throws VizException {
        List<String> peList = new ArrayList<>();

        String query = "select distinct(pe) from ingestfilter";
        List<Object[]> rs = DirectDbQuery.executeQuery(query,
                HydroConstants.IHFS, QueryLanguage.SQL);

        for (Object[] oa : rs) {
            peList.add((String) oa[0]);
        }

        return peList.toArray(new String[peList.size()]);
    }

    public synchronized Map<String, String> getPeNames() throws VizException {
        if ((peMap == null) || (peMap.size() < 1)) {
            peMap = new LinkedHashMap<>();
            populatePeMap();
        }

        return peMap;
    }

    private void populatePeMap() {
        String sql = "select pe, name from shefpe order by pe asc";

        List<Object[]> result = runQuery(sql);
        if (result != null) {
            for (Object[] oa : result) {
                peMap.put((String) oa[0], (String) oa[1]);
            }
        }
    }

    public List<PointDataPreset> getPresets() throws VizException {
        String sql = "select preset_id, descr, preset_rank, preset_string from pointdatapresets "
                + "order by preset_rank asc";
        List<Object[]> result = runQuery(sql);
        List<PointDataPreset> presetList = new ArrayList<>();

        for (Object[] oa : result) {
            PointDataPreset pdp = new PointDataPreset();
            pdp.setPresetId((String) oa[0]);
            pdp.setDescription((String) oa[1]);
            pdp.setPresetRank(((Number) oa[2]).intValue());
            pdp.setPresetString((String) oa[3]);
            presetList.add(pdp);
        }
        return presetList;
    }

    /**
     * Update the PointDataPresets table.
     *
     * @param id
     *            The Id
     * @param desc
     *            The Description
     * @param rank
     *            The Rank
     * @return true if successful, false otherwise
     * @throws VizException
     */
    public void updatePreset(PointDataPreset node) throws VizException {
        SqlBuilder sql = new SqlBuilder("pointdatapresets");
        sql.setSqlType(SqlBuilder.UPDATE);
        sql.addString("descr", node.getDescription());
        sql.addInt(" preset_rank", node.getPresetRank());
        sql.addString(" preset_string", node.getPresetString());
        sql.setWhereClause(" where preset_id = '" + node.getPresetId() + "'");

        runStatement(sql.toString());
    }

    /**
     * Insert into the PointDataPresets table.
     *
     * @param node
     *            Data to insert
     * @return true if successful, false otherwise
     * @throws VizException
     */
    public void insertPreset(PointDataPreset node) throws VizException {

        StringBuilder sql = new StringBuilder(
                "insert into pointdatapresets (preset_id, descr, preset_rank, preset_string) ");
        sql.append("values ('" + node.getPresetId() + "', '"
                + node.getDescription() + "', ");
        sql.append(
                node.getPresetRank() + ", '" + node.getPresetString() + "')");

        runStatement(sql.toString());
    }

    /**
     * Delete the record from the PointDataPresets table.
     *
     * @param id
     *            The ID to delete
     * @throws VizException
     */
    public void deletePreset(String id) throws VizException {
        String sql = "delete from pointdatapresets where preset_id = '" + id
                + "'";

        runStatement(sql);
    }

    /**
     * Get the type from the telmtype table.
     *
     * @param where
     *            The where clause to use, or null for no where clause
     * @return List<String> of telmtype values
     */
    public List<String> getTelmType(String where) {
        List<String> returnList = new ArrayList<>();
        SqlBuilder sql = new SqlBuilder("telmtype");
        sql.setColumn("type");
        sql.setSqlType(SqlBuilder.SELECT);

        if ((where != null) && (where.length() > 0)) {
            sql.setWhereClause(where);
        }

        List<Object[]> results = runQuery(sql.toString());

        returnList.add("Observer");
        returnList.add("DCP");

        for (Object[] oa : results) {
            returnList.add((String) oa[0]);
        }

        returnList.add("Undefined");

        return returnList;
    }

    /**
     * Get the HSA list.
     *
     * @param where
     *            The where clause
     * @return List<String> of HSA values
     * @throws VizException
     */
    public List<String> getHsaList(String where) throws VizException {
        List<String> returnList = new ArrayList<>();
        StringBuilder sql = new StringBuilder(
                "select distinct hsa from LocClass");

        if ((where != null) && (where.length() > 0)) {
            sql.append(" " + where);
        }

        List<Object[]> results = runQuery(sql.toString());

        for (Object[] oa : results) {
            returnList.add((String) oa[0]);
        }

        return returnList;
    }

    public synchronized RiverStat getRiverStatus(String lid) {
        RiverStat rsInfo = null;
        if (riverStatMap == null) {
            riverStatMap = new HashMap<>();
        }

        if ((lid != null) && (riverStatMap.get(lid) == null)) {
            String sql = "select lid, primary_pe, fq, fs, action_flow, wstg from riverstat where lid = '"
                    + lid + "'";

            List<Object[]> results = runQuery(sql);

            if (results != null && !results.isEmpty()) {
                rsInfo = new RiverStat(results.get(0));
                riverStatMap.put(lid, rsInfo);
            } else {
                riverStatMap.put(lid, null);
            }
        }

        return riverStatMap.get(lid);
    }

    /**
     * Query the Riverstatus table.
     *
     * @param where
     *            The where clause to use.
     * @return ArrayList of Riverstatus objects.
     * @throws VizException
     */
    public List<Riverstatus> getRiverStatusList(String where)
            throws VizException {
        StringBuilder query = new StringBuilder();
        query.append(
                "from " + com.raytheon.uf.common.dataplugin.shef.tables.Riverstatus.class
                        .getName());
        query.append(" " + where);

        List<Object[]> results = DirectDbQuery.executeQuery(query.toString(),
                HydroConstants.IHFS, QueryLanguage.HQL);

        List<Riverstatus> returnList = new ArrayList<>(results.size());
        for (Object[] item : results) {
            returnList.add((Riverstatus) item[0]);
        }

        return returnList;
    }

    /**
     * Gets a map of Lid/PE to TS Ranking map from the IngestFilter table.
     *
     * @return Map Lid-PE -> TS Ranking
     */
    public synchronized Map<String, List<IngestFilter>> getIngestFilterData() {
        if (ingestFilterMap == null) {
            ingestFilterMap = new HashMap<>();
            List<IngestFilter> filterList = new ArrayList<>();

            String sql = "select lid, pe, dur, ts, extremum, ts_rank from ingestFilter order by lid asc";
            List<Object[]> results = runQuery(sql);
            String lid = null;
            String prevLid = (String) results.get(0)[0];
            for (Object[] oa : results) {
                lid = (String) oa[0];
                if (!lid.equalsIgnoreCase(prevLid)) {
                    ingestFilterMap.put(prevLid, filterList);
                    prevLid = lid;
                    filterList = new ArrayList<>();
                }
                IngestFilter ingFilter = new IngestFilter(oa);
                filterList.add(ingFilter);
            }

        }

        return ingestFilterMap;
    }

    /**
     * Selects a distinct value from a table.
     *
     * @param field
     *            The distinct field
     * @param table
     *            The table to query
     * @param where
     *            The where clause for constraint
     * @return
     */
    public List<Object[]> getUnique(String field, String table, String where) {
        String sql = " SELECT DISTINCT " + field + " from " + table + " "
                + where;

        return runQuery(sql);
    }

    /**
     * Get the river data, which can be either height or discharge, either
     * observed or forecast, or can be any designated type-source.
     * 
     * @return
     */
    public RiverData getRiverData(PDCOptions pcOptions) {
        RiverData riverData = new RiverData();
        String where = null;

        /*
         * if not getting the Latest data, then need to go to the full PE tables
         * with the data - i.e. discharge and height tables.
         */

        if (pcOptions.getTimeMode() != TimeModeType.LATEST.getTimeMode()) {

            /*
             * if getting data for the "primary" river pe, then do two separate
             * retrievals, since the data may be in either table for a given
             * location.
             */

            if (pcOptions.getPrimary() == 1) {
                where = PDCDBUtils.buildRiverWhere("H");
                StringBuilder hBuffer = new StringBuilder();
                hBuffer.append(obsQueryHead);
                hBuffer.append(" height ");
                hBuffer.append(where);
                List<Object[]> heightResults = runQuery(hBuffer.toString());
                List<Observation> heightList = new ArrayList<>();
                for (Object[] oa : heightResults) {
                    Observation obs = new Observation(oa);
                    heightList.add(obs);
                }
                riverData.setObsHeightList(heightList);

                where = PDCDBUtils.buildRiverWhere("Q");

                StringBuilder qBuffer = new StringBuilder();
                qBuffer.append(obsQueryHead);
                qBuffer.append(" discharge ");
                qBuffer.append(where);
                List<Object[]> qResults = runQuery(qBuffer.toString());
                List<Observation> qList = new ArrayList<>();
                for (Object[] oa : qResults) {
                    Observation obs = new Observation();
                    qList.add(obs);
                }
                riverData.setObsDischargeList(qList);
            } else if (pcOptions.getSelectedAdHocElementString()
                    .startsWith("H")) {
                where = PDCDBUtils.buildRiverWhere("");

                StringBuilder qBuffer = new StringBuilder();
                qBuffer.append(obsQueryHead);
                qBuffer.append(" height ");
                qBuffer.append(where);
                List<Object[]> qResults = runQuery(qBuffer.toString());
                List<Observation> heightList = new ArrayList<>();
                for (Object[] oa : qResults) {
                    Observation obs = new Observation(oa);
                    heightList.add(obs);
                }
                riverData.setObsHeightList(heightList);
            } else if (pcOptions.getSelectedAdHocElementString()
                    .startsWith("Q")) {
                where = PDCDBUtils.buildRiverWhere("");

                StringBuilder qBuffer = new StringBuilder();
                qBuffer.append(obsQueryHead);
                qBuffer.append(" discharge ");
                qBuffer.append(where);
                List<Object[]> qResults = runQuery(qBuffer.toString());
                List<Observation> qList = new ArrayList<>();
                for (Object[] oa : qResults) {
                    Observation obs = new Observation();
                    qList.add(obs);
                }
                riverData.setObsDischargeList(qList);
            }
        } else {
            /*
             * if getting latest river data, always get the data from the
             * RiverStatus table. the where clause is built with knowledge of
             * whether getting primary data, and whether getting a specific pe
             * or ts
             */

            try {
                where = PDCDBUtils.buildRiverStatusWhere(pcOptions);
                List<Riverstatus> riverstatusList = getRiverStatusList(where);

                riverData.setRiverStatusList(riverstatusList);
            } catch (VizException e) {
                statusHandler.debug(
                        "Get river status list where " + where + " failed", e);
            }
        }

        return riverData;
    }

    /**
     * Load the PC and/or PP rain data.
     * 
     * @return
     */
    public RainData getRainData() {
        PDCOptionData pcOptions = PDCOptionData.getInstance();
        PrecipUtil precipUtil = PrecipUtil.getInstance();
        List<String> typeSourceArray = null;
        long minTime;
        long maxTime;
        List<Object[]> pcList = null;
        List<Object[]> ppList = null;
        List<Curpc> pcObjectList = new ArrayList<>();
        List<Curpp> ppObjectList = new ArrayList<>();

        typeSourceArray = pcOptions.getTypeSourceChosenList();

        /* get the appropriate data */
        Date now = SimulatedTime.getSystemTime().getTime();

        /* set the time window based on the time mode */
        if (pcOptions.getTimeMode() == PDCConstants.TimeModeType.LATEST
                .getTimeMode()) {
            minTime = now.getTime()
                    - (pcOptions.getDurHours() * PDCConstants.MILLIS_PER_HOUR);
            maxTime = now.getTime();
        } else {
            /*
             * min and max queries are not meaningful; use settime in that case
             */
            minTime = pcOptions.getValidTime().getTime()
                    - (pcOptions.getDurHours() * PDCConstants.MILLIS_PER_HOUR);
            maxTime = pcOptions.getValidTime().getTime()
                    + (pcOptions.getDurHours() * PDCConstants.MILLIS_PER_HOUR);
        }

        Calendar cal = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
        cal.setTimeInMillis(minTime);
        String beginTime = PDCConstants.DATE_FORMAT.format(cal.getTime());
        cal.setTimeInMillis(maxTime);
        String endTime = PDCConstants.DATE_FORMAT.format(cal.getTime());
        String lid = "";

        if (pcOptions.getPcAndpp() == 1) {
            pcList = precipUtil.loadPeRaw(beginTime, endTime, lid,
                    typeSourceArray, HydroConstants.PhysicalElement.PC);

            ppList = precipUtil.loadPeRaw(beginTime, endTime, lid,
                    typeSourceArray, HydroConstants.PhysicalElement.PP);
        } else {
            if (HydroConstants.PC.equalsIgnoreCase(
                    pcOptions.getSelectedAdHocElementString())) {
                pcList = precipUtil.loadPeRaw(beginTime, endTime, lid,
                        typeSourceArray, HydroConstants.PhysicalElement.PC);
            } else {
                ppList = precipUtil.loadPeRaw(beginTime, endTime, lid,
                        typeSourceArray, HydroConstants.PhysicalElement.PP);
            }
        }

        RainData rainData = new RainData();
        /* Save the data to the PointDataControlManager */
        if (pcList != null) {
            /*
             * select pc.lid, pc.pe, pc.dur, pc.ts, pc.extremum, pc.value,
             * pc.shef_qual_code, pc.quality_code, pc.revision, pc.product_id,
             * pc.producttime, pc.postingtime, pc.obstime, location.name from
             * location, curpc pc
             */
            for (int i = 0; i < pcList.size(); i++) {
                Object[] oa = pcList.get(i);
                Curpc pc = new Curpc();
                CurpcId id = new CurpcId();
                id.setLid((String) oa[0]);
                id.setTs((String) oa[3]);
                id.setExtremum((String) oa[4]);
                id.setObstime((Date) oa[12]);
                pc.setPe((String) oa[1]);
                pc.setDur(((Number) oa[2]).shortValue());
                pc.setValue((Double) oa[5]);
                pc.setShefQualCode((String) oa[6]);
                pc.setQualityCode(((Number) oa[7]).intValue());
                pc.setRevision(((Number) (oa[8])).shortValue());
                pc.setProductId((String) oa[9]);
                pc.setProducttime((Date) oa[10]);
                pc.setPostingtime((Date) oa[11]);
                pc.setId(id);

                pcObjectList.add(pc);
            }
            rainData.setPcList(pcObjectList);

        }

        if (ppList != null) {
            /*
             * select pc.lid, pc.pe, pc.dur, pc.ts, pc.extremum, pc.value,
             * pc.shef_qual_code, pc.quality_code, pc.revision, pc.product_id,
             * pc.producttime, pc.postingtime, pc.obstime, location.name from
             * location, curpc pc
             */
            for (int i = 0; i < ppList.size(); i++) {
                Object[] oa = ppList.get(i);
                Curpp pp = new Curpp();
                CurppId id = new CurppId();
                id.setLid((String) oa[0]);
                id.setDur(((Number) oa[2]).shortValue());
                id.setTs((String) oa[3]);
                id.setExtremum((String) oa[4]);
                id.setObstime((Date) oa[12]);
                pp.setPe((String) oa[1]);
                pp.setValue((Double) oa[5]);
                pp.setShefQualCode((String) oa[6]);
                pp.setQualityCode(((Number) oa[7]).intValue());
                pp.setRevision(((Number) oa[8]).shortValue());
                pp.setProductId((String) oa[9]);
                pp.setProducttime((Date) oa[10]);
                pp.setPostingtime((Date) oa[11]);
                pp.setId(id);

                ppObjectList.add(pp);
            }
            rainData.setPpList(ppObjectList);
        }

        return rainData;
    }

    public List<Observation> getSnowTempOtherData() {
        PDCOptionData pcOptions = PDCOptionData.getInstance();
        String typeSource = null;
        String tablename = null;
        String where = null;
        List<Object[]> obsResult = null;
        List<Observation> obsList = new ArrayList<>();

        /*
         * check whether the latest value is stored in the LatestObsValue table
         * and use it if it is since this table is usually much smaller than the
         * PE table, and the query will be faster.
         */
        boolean shefPostLatest = checkShefPostLatest();

        /*
         * set the where clause, which is independent of whether being read from
         * the PE table or the LatestObsValue table.
         */
        where = PDCDBUtils.buildSnowTempOtherWhere();

        /*
         * look in the applicable PE table if looking for a set time of data, or
         * the min or max, or if the latest obs are not being written to the
         * latestobsvalue table.
         */
        if ((pcOptions.getTimeMode() != TimeModeType.LATEST.getTimeMode())
                || !shefPostLatest) {
            if ((pcOptions.getTypeSourceChosenCount() < 1)
                    || (pcOptions.getFilterByTypeSource() == 0)) {
                pcOptions.setTypeSourceChosenCount(1);
                List<String> tsChosenList = pcOptions.getTypeSourceChosenList();
                List<String> newTsChosenList = new ArrayList<>();

                // Not sure why changing the list value here
                newTsChosenList.add("XX");
                for (int i = 1; i < tsChosenList.size(); i++) {
                    newTsChosenList.add(tsChosenList.get(i));
                }
                pcOptions.setTypeSourceChosenList(newTsChosenList);

                typeSource = pcOptions.getTypeSourceChosenList().get(0);

                tablename = HydroUtils.getTableName(
                        pcOptions.getSelectedAdHocElementString(), typeSource);

                obsResult = getObservationData(tablename, where);
            }
        } else {
            /*
             * even though the extremum code is in the key of the LatestObsValue
             * table, we can't look there for extremums because the settime
             * field may be set to find an old extremum; i.e. that is not the
             * latest.
             */
            obsResult = getLatestObsValue(where);
        }

        for (Object[] oa : obsResult) {
            Observation obs = new Observation(oa);
            obsList.add(obs);
        }

        return obsList;
    }

    /**
     * Query for an observation record.
     *
     * @param table
     *            The table name to query
     * @param where
     *            The where clause
     * @return An Observation object of data
     */
    public List<Object[]> getObservationData(String table, String where) {
        StringBuilder sb = new StringBuilder();
        sb.append("SELECT lid, pe, dur, ts, extremum, obstime, value, ");
        sb.append("shef_qual_code, quality_code, revision, product_id ");
        sb.append("FROM " + table + " " + where);

        return runQuery(sb.toString());
    }

    /**
     * Query for the lastestObsValue record.
     *
     * @param where
     *            The where clause
     * @return An Observation object of data
     */
    public List<Object[]> getLatestObsValue(String where) {
        StringBuilder sb = new StringBuilder();
        sb.append("SELECT lid, pe, dur, ts, extremum, obstime, value, ");
        sb.append("shef_qual_code, quality_code, revision, product_id, ");
        sb.append("producttime, postingtime ");
        sb.append("FROM latestObsValue " + where);

        return runQuery(sb.toString());
    }

    /**
     * Query the LocPDC view in the IHFS DB.
     *
     * @param where
     *            The where clause to use
     * @return List of LocPDC objects
     */
    public Map<String, LocPDC> getLocPDC(String where) {
        Map<String, LocPDC> returnMap = new HashMap<>();

        StringBuilder sql = new StringBuilder();
        sql.append(
                "Select lid, name, lat, lon, hsa, post, elev, primary_pe, fs, fq, ");
        sql.append("disp_class, is_dcp, is_observer, telem_type from locPDC ");
        sql.append(where);

        List<Object[]> results = runQuery(sql.toString());

        for (Object[] data : results) {
            LocPDC locPDC = new LocPDC();
            int i = 0;
            if (data[i] != null) { // LID
                locPDC.setLid((String) data[i]);
            }
            i++;
            if (data[i] != null) { // name
                locPDC.setName((String) data[i]);
            }
            i++;
            if (data[i] != null) { // lat
                locPDC.setLat((Double) data[i]);
            }
            i++;
            if (data[i] != null) { // lon
                locPDC.setLon((Double) data[i]);
            }
            i++;
            if (data[i] != null) { // hsa
                locPDC.setHsa((String) data[i]);
            }
            i++;
            if (data[i] != null) { // post
                locPDC.setPost(((Number) data[i]).intValue());
            }
            i++;
            if (data[i] != null) { // elev
                locPDC.setElev((Double) data[i]);
            }
            i++;
            if (data[i] != null) { // primary_pe
                locPDC.setPrimaryPe((String) data[i]);
            }
            i++;
            if (data[i] != null) { // fs
                locPDC.setFs((Double) data[i]);
            }
            i++;
            if (data[i] != null) { // fq
                locPDC.setFq((Double) data[i]);
            }
            // disp_class, is_dcp, is_observer, telem_type
            i++;
            if (data[i] != null) { // disp_class
                locPDC.setDispClass((String) data[i]);
            }
            i++;
            if (data[i] != null) {
                if ("F".equalsIgnoreCase(((String) data[i]))) {
                    locPDC.setDcp(false);
                } else {
                    locPDC.setDcp(true);
                }
            }
            i++;
            if (data[i] != null) {
                if ("F".equalsIgnoreCase(((String) data[i]))) {
                    locPDC.setObserver(false);
                } else {
                    locPDC.setObserver(true);
                }
            }
            i++;
            if (data[i] != null) { // Telem_type
                locPDC.setTelemType((String) data[i]);
            }

            returnMap.put(locPDC.getLid(), locPDC);
        }

        return returnMap;
    }

    private boolean checkShefPostLatest() {
        if (shefpost == null) {
            shefpost = AppsDefaults.getInstance().getToken("shef_post_latest");
            if ("Off".equalsIgnoreCase(shefpost)) {
                postLatest = false;
            } else {
                postLatest = true;
            }
        }

        return postLatest;
    }
}