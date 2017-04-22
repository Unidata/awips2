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
package com.raytheon.viz.mpe.core;

import java.awt.Rectangle;
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import javax.persistence.Table;

import org.apache.commons.lang3.builder.ToStringBuilder;
import org.opengis.metadata.spatial.PixelOrientation;

import com.raytheon.uf.common.dataplugin.shef.tables.Hourlypc;
import com.raytheon.uf.common.dataplugin.shef.tables.HourlypcId;
import com.raytheon.uf.common.dataplugin.shef.tables.Hourlypp;
import com.raytheon.uf.common.dataplugin.shef.tables.HourlyppId;
import com.raytheon.uf.common.dataplugin.shef.tables.Ingestfilter;
import com.raytheon.uf.common.dataplugin.shef.tables.Pseudogageval;
import com.raytheon.uf.common.dataplugin.shef.tables.Rawpp;
import com.raytheon.uf.common.hydro.CommonHydroConstants;
import com.raytheon.uf.common.hydro.data.PrecipTotal;
import com.raytheon.uf.common.mpe.util.PrecipUtil;
import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.common.util.FileUtil;
import com.raytheon.uf.common.xmrg.hrap.HRAP;
import com.raytheon.uf.common.xmrg.hrap.HRAPCoordinates;
import com.raytheon.uf.common.xmrg.hrap.HRAPSubGrid;
import com.raytheon.uf.viz.core.catalog.DirectDbQuery;
import com.raytheon.uf.viz.core.catalog.DirectDbQuery.QueryLanguage;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.hydrocommon.HydroConstants;
import com.raytheon.viz.hydrocommon.util.HydroQC;
import com.raytheon.viz.hydrocommon.whfslib.IHFSDbGenerated;
import com.raytheon.viz.mpe.core.MPEDataManager.MPERadarData.RadarAvailability;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * MPE Data Manager
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 1, 2008             randerso    Initial creation
 * Nov 6, 2008  1649       snaples     Added updatePseudogageval method
 * Nov 6, 2008  1649       snaples     Added new methods for getting and 
 *                                     updating RawPP records
 * Nov 24, 2008 1748       snaples     Added getters to MPEGageData
 * Jun 18, 2013 16053      snaples     Removed methods set and getRadarEditFlag
 * Dec 15 2013  DCS 167    cgobs       DualPol capabilities
 * Jul 29, 2015  17471     snaples     Added logging for Radar Bias results table query.
 * Aug 11, 2015 4500       rjpeter     Fix type casts.
 * Sep 29, 2015 17975      snaples     Fixed an issue with getDateMap query sometimes throwing errors.
 * Jul 25, 2016 4623       skorolev    Replaced total_precip with PrecipTotal. Cleanup.
 * 
 * </pre>
 * 
 * @author randerso
 */

public class MPEDataManager {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(MPEDataManager.class);

    private static final int TOP_OF_HOUR_WINDOW = 10;

    /**
     * MPE Date Information
     */
    public static class MPEDateInfo {
        private final Date lastSaveTime;

        private final Date lastExecTime;

        private final boolean autoSave;

        /**
         * Constructor.
         * 
         * @param lastSaveTime
         * @param lastExecTime
         * @param autoSave
         */
        public MPEDateInfo(Date lastSaveTime, Date lastExecTime,
                boolean autoSave) {
            this.lastSaveTime = lastSaveTime;
            this.lastExecTime = lastExecTime;
            this.autoSave = autoSave;
        }

        public Date getLastSaveTime() {
            return lastSaveTime;
        }

        public Date getLastExecTime() {
            return lastExecTime;
        }

        public boolean isAutoSave() {
            return autoSave;
        }

    }

    /**
     * MPE Radar Location
     */
    public static class MPERadarLoc {
        private String id;

        private Coordinate latLon;

        /**
         * Constructor.
         * 
         * @param id
         * @param lat
         * @param lon
         */
        public MPERadarLoc(String id, double lat, double lon) {
            this.id = id;
            latLon = new Coordinate(lon, lat);
        }

        public String getId() {
            return id;
        }

        public Coordinate getLatLon() {
            return latLon;
        }
    }

    /**
     * MPE Radar Data
     */
    public static class MPERadarData {
        public static enum RadarAvailability {
            AVAILABLE, MISSING, ZERO
        };

        private Date productDate;

        private short[] rawRadarData;

        private short[] unbiasedRadarData;

        private double rwBiasValUsed;

        private double memSpanUsed;

        private String editBias;

        private RadarAvailability radAvail;

        private boolean ignoreRadar;

        private int numGages;

        private boolean radarDataAvailable;

        /**
         * Constructor
         */
        public MPERadarData() {
            radAvail = RadarAvailability.MISSING;
            ignoreRadar = true;
            rwBiasValUsed = 1.0;
        }

        @Override
        public String toString() {
            return ToStringBuilder.reflectionToString(this);
        }

        public Date getProductDate() {
            return productDate;
        }

        public void setProductDate(Date productDate) {
            this.productDate = productDate;
        }

        public short[] getRawRadarData() {
            return rawRadarData;
        }

        public void setRawRadarData(short[] rawRadarData) {
            this.rawRadarData = rawRadarData;
        }

        public short[] getUnbiasedRadarData() {
            return unbiasedRadarData;
        }

        public void setUnbiasedRadarData(short[] unbiasedRadarData) {
            this.unbiasedRadarData = unbiasedRadarData;
        }

        public double getRwBiasValUsed() {
            return rwBiasValUsed;
        }

        public void setRwBiasValUsed(double rwBiasValUsed) {
            this.rwBiasValUsed = rwBiasValUsed;
        }

        /**
         * @return the memSpanUsed
         */
        public double getMemSpanUsed() {
            return memSpanUsed;
        }

        /**
         * @param memSpanUsed
         *            the memSpanUsed to set
         */
        public void setMemSpanUsed(double memSpanUsed) {
            this.memSpanUsed = memSpanUsed;
        }

        /**
         * @return the editBias
         */
        public String getEditBias() {
            return editBias;
        }

        /**
         * @param editBias
         *            the editBias to set
         */
        public void setEditBias(String editBias) {
            this.editBias = editBias;
        }

        public RadarAvailability getRadAvail() {
            return radAvail;
        }

        public void setRadAvail(RadarAvailability radAvail) {
            this.radAvail = radAvail;
        }

        public boolean isIgnoreRadar() {
            return ignoreRadar;
        }

        public void setIgnoreRadar(boolean ignoreRadar) {
            this.ignoreRadar = ignoreRadar;
        }

        public int getNumGages() {
            return numGages;
        }

        public void setNumGages(int numGages) {
            this.numGages = numGages;
        }

        public boolean isRadarDataAvailable() {
            return radarDataAvailable;
        }

        public void setRadarDataAvailable(boolean radarDataAvailable) {
            this.radarDataAvailable = radarDataAvailable;
        }

    }

    /**
     * MPE Gage Data
     */
    public static class MPEGageData {

        String id;

        String rid;

        String edit;

        float gval;

        float xmrgVal;

        float mval;

        float rval;

        float bval;

        float locVal;

        float gageOnly;

        float satVal;

        Coordinate latLon;

        Coordinate hrap; /* The national HRAP grid */

        Coordinate hrapLoc; /* The local HRAP grid */

        boolean td;

        boolean manedit;

        int qc; /* quality control */

        String ts;

        String pe;

        boolean reported_missing;

        boolean useInP3;

        boolean isBad;

        /**
         * @return the id
         */
        public String getId() {
            return id;
        }

        /**
         * @return the rid
         */
        public String getRid() {
            return rid;
        }

        /**
         * @return the edit
         */
        public String getEdit() {
            return edit;
        }

        /**
         * @return the gval
         */
        public float getGval() {
            return gval;
        }

        /**
         * @return the xmrgVal
         */
        public float getXmrgVal() {
            return xmrgVal;
        }

        /**
         * @return the mval
         */
        public float getMval() {
            return mval;
        }

        /**
         * @return the rval
         */
        public float getRval() {
            return rval;
        }

        /**
         * @return the bval
         */
        public float getBval() {
            return bval;
        }

        /**
         * @return the locVal
         */
        public float getLocVal() {
            return locVal;
        }

        /**
         * @return the gageOnly
         */
        public float getGageOnly() {
            return gageOnly;
        }

        /**
         * @return the satVal
         */
        public float getSatVal() {
            return satVal;
        }

        /**
         * @return the latLon
         */
        public Coordinate getLatLon() {
            return latLon;
        }

        /**
         * @return the hrap
         */
        public Coordinate getHrap() {
            return hrap;
        }

        /**
         * @return the hrap_loc
         */
        public Coordinate getHrapLoc() {
            return hrapLoc;
        }

        /**
         * @return the td
         */
        public boolean isTd() {
            return td;
        }

        /**
         * @return the manedit
         */
        public boolean isManedit() {
            return manedit;
        }

        /**
         * @return the qc
         */
        public int getQc() {
            return qc;
        }

        /**
         * @return the ts
         */
        public String getTs() {
            return ts;
        }

        /**
         * @return the reported_missing
         */
        public boolean isReported_missing() {
            return reported_missing;
        }

        /**
         * @return the useInP3
         */
        public boolean isUseInP3() {
            return useInP3;
        }

        /**
         * @param id
         *            the id to set
         */
        public void setId(String id) {
            this.id = id;
        }

        /**
         * @param rid
         *            the rid to set
         */
        public void setRid(String rid) {
            this.rid = rid;
        }

        /**
         * @param edit
         *            the edit to set
         */
        public void setEdit(String edit) {
            this.edit = edit;
        }

        /**
         * @param gval
         *            the gval to set
         */
        public void setGval(float gval) {
            this.gval = gval;
        }

        /**
         * @param xmrgVal
         *            the xmrgVal to set
         */
        public void setXmrgVal(float xmrgVal) {
            this.xmrgVal = xmrgVal;
        }

        /**
         * @param mval
         *            the mval to set
         */
        public void setMval(float mval) {
            this.mval = mval;
        }

        /**
         * @param rval
         *            the rval to set
         */
        public void setRval(float rval) {
            this.rval = rval;
        }

        /**
         * @param bval
         *            the bval to set
         */
        public void setBval(float bval) {
            this.bval = bval;
        }

        /**
         * @param locVal
         *            the locVal to set
         */
        public void setLocVal(float locVal) {
            this.locVal = locVal;
        }

        /**
         * @param gageOnly
         *            the gageOnly to set
         */
        public void setGageOnly(float gageOnly) {
            this.gageOnly = gageOnly;
        }

        /**
         * @param satVal
         *            the satVal to set
         */
        public void setSatVal(float satVal) {
            this.satVal = satVal;
        }

        /**
         * @param latLon
         *            the latLon to set
         */
        public void setLatLon(Coordinate latLon) {
            this.latLon = latLon;
        }

        /**
         * @param hrap
         *            the hrap to set
         */
        public void setHrap(Coordinate hrap) {
            this.hrap = hrap;
        }

        /**
         * @param hrapLoc
         *            the hrapLoc to set
         */
        public void setHrap_loc(Coordinate hrapLoc) {
            this.hrapLoc = hrapLoc;
        }

        /**
         * @param td
         *            the td to set
         */
        public void setTd(boolean td) {
            this.td = td;
        }

        /**
         * @param manedit
         *            the manedit to set
         */
        public void setManedit(boolean manedit) {
            this.manedit = manedit;
        }

        /**
         * @param qc
         *            the qc to set
         */
        public void setQc(int qc) {
            this.qc = qc;
        }

        /**
         * @param ts
         *            the ts to set
         */
        public void setTs(String ts) {
            this.ts = ts;
        }

        /**
         * @param reported_missing
         *            the reported_missing to set
         */
        public void setReported_missing(boolean reported_missing) {
            this.reported_missing = reported_missing;
        }

        /**
         * @param useInP3
         *            the useInP3 to set
         */
        public void setUseInP3(boolean useInP3) {
            this.useInP3 = useInP3;
        }

        /**
         * @return the pe
         */
        public String getPe() {
            return pe;
        }

        /**
         * @param pe
         *            the pe to set
         */
        public void setPe(String pe) {
            this.pe = pe;
        }

        public boolean isBad() {
            return isBad;
        }

        public void setBad(boolean isBad) {
            this.isBad = isBad;
        }

    }

    private static final int MAX_GAGEQC_DAYS = 10;

    private static MPEDataManager instance;

    private final AppsDefaults appsDefaults = AppsDefaults.getInstance();;

    private final String RFC;

    private List<MPERadarLoc> radarList;

    private Map<String, MPEGageData> editGages = new HashMap<>();

    private List<String> badGages = new ArrayList<>();

    private Map<Date, MPEDateInfo> dateMap;

    private Date latestAvailableDate = null;

    private Map<String, Coordinate> locationMap;

    private Rectangle HRAPExtent;

    private int dqcdays = Integer.parseInt(appsDefaults
            .getToken("mpe_dqc_num_days"));

    private HRAPSubGrid subGrid;

    private PrecipUtil pu = PrecipUtil.getInstance();

    /**
     * Retrieve singleton instance
     * 
     * @return singleton instance of MPEDataManager
     */
    public static synchronized MPEDataManager getInstance() {
        if (instance == null) {
            instance = new MPEDataManager();
        }

        return instance;
    }

    /**
     * private constructor for singleton
     */
    private MPEDataManager() {
        RFC = appsDefaults.getToken("st3_rfc");
        pu.setIngestList(this.getIngestInfoList());
    }

    /**
     * Gets Dates
     * 
     * @param update
     */
    private void getDates(boolean update) {
        String starttime = "";
        String endtime = "";
        if (update && latestAvailableDate != null) {
            starttime = TimeUtil.formatToSqlTimestamp(latestAvailableDate);
        } else {
            dateMap = new HashMap<>();
            starttime = TimeUtil.formatToSqlTimestamp(getEarliestDate());
        }
        endtime = TimeUtil.formatToSqlTimestamp(getLatestDate());
        String sqlQuery = "select obstime,last_save_time,last_exec_time,auto_save from rwresult where rfc='"
                + RFC
                + "' and obstime between '"
                + starttime
                + "' and '"
                + endtime + "'";

        try {
            List<Object[]> results = DirectDbQuery.executeQuery(sqlQuery,
                    HydroConstants.IHFS, QueryLanguage.SQL);
            for (Object[] item : results) {
                Date obstime = (Date) item[0];
                MPEDateInfo dateInfo = new MPEDateInfo((Date) item[1],
                        (Date) item[2], "T".equals(item[3]));
                dateMap.put(obstime, dateInfo);
                if (latestAvailableDate == null
                        || obstime.after(latestAvailableDate)) {
                    latestAvailableDate = obstime;
                }
            }

        } catch (VizException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Record not found in rwresult ", e);
        }
    }

    public Map<Date, MPEDateInfo> getDateMap(boolean update) {

        getDates(update);
        return dateMap;
    }

    /**
     * Reads Radar Location
     */
    public void readRadarLoc() {
        String sqlQuery = "select radid, lat, lon from radarloc where use_radar='T' order by radid asc";

        radarList = new ArrayList<>();
        try {
            List<Object[]> results = DirectDbQuery.executeQuery(sqlQuery,
                    HydroConstants.IHFS, QueryLanguage.SQL);
            for (Object[] item : results) {
                // note db stores west longitude as positive so must negate
                MPERadarLoc radarLoc = new MPERadarLoc((String) item[0],
                        ((Number) item[1]).doubleValue(),
                        -((Number) item[2]).doubleValue());
                radarList.add(radarLoc);
            }
        } catch (VizException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Record not found in radarloc ", e);
        }
    }

    /**
     * OrigReadRadarData
     * 
     * @param date
     * @return
     */
    public Map<String, MPERadarData> OrigReadRadarData(Date date) {
        getRadars();
        StringBuffer sqlQuery = new StringBuffer();
        sqlQuery.append("select radid,num_gages,rad_avail, rw_bias_val_used,mem_span_used, edit_bias, ignore_radar from rwradarresult where obstime='"
                + TimeUtil.formatToSqlTimestamp(date) + "' and radid in(");
        for (int i = 0; i < radarList.size(); i++) {
            sqlQuery.append("'");
            sqlQuery.append(radarList.get(i).getId());
            sqlQuery.append("'");
            if (i != radarList.size() - 1) {
                sqlQuery.append(",");
            }
        }
        sqlQuery.append(") order by radid asc");

        Map<String, MPERadarData> radarResultList = new HashMap<>(
                radarList.size());
        try {
            List<Object[]> results = DirectDbQuery
                    .executeQuery(sqlQuery.toString(), HydroConstants.IHFS,
                            QueryLanguage.SQL);
            Iterator<Object[]> iter = results.iterator();
            Object[] item = iter.hasNext() ? iter.next() : null;
            for (MPERadarLoc radarLoc : radarList) {
                MPERadarData radarData = new MPERadarData();

                int compareResult = item == null ? -1 : radarLoc.getId()
                        .compareTo((String) item[0]);

                if (compareResult < 0) {
                    radarData.setProductDate(date);

                } else if (compareResult == 0) {
                    radarData.setNumGages(((Number) item[1]).intValue());

                    RadarAvailability radAvail = RadarAvailability.MISSING;
                    if ("y".equals(item[2])) {
                        radAvail = RadarAvailability.AVAILABLE;
                    } else if ("z".equals(item[2])) {
                        radAvail = RadarAvailability.ZERO;
                    }
                    radarData.setRadAvail(radAvail);

                    radarData
                            .setRwBiasValUsed(((Number) item[3]).doubleValue());
                    radarData.setMemSpanUsed(((Number) item[4]).doubleValue());
                    radarData.setEditBias((String) item[5]);
                    radarData.setIgnoreRadar(!"n".equals(item[6]));

                    if (radAvail.equals(RadarAvailability.AVAILABLE)
                            || radAvail.equals(RadarAvailability.ZERO)) {
                        radarData.setProductDate(readProductDateTime(
                                radarLoc.getId(), date));
                    }

                    item = iter.hasNext() ? iter.next() : null;
                } else {
                    statusHandler.handle(
                            Priority.PROBLEM,
                            "Record not found in RWRadarResult table for "
                                    + radarLoc.getId() + " for time "
                                    + date.toString() + "Z");
                }

                radarResultList.put(radarLoc.getId(), radarData);
            }
        } catch (VizException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Record not found in RWRadarResult ", e);
        }

        return radarResultList;
    }

    public Map<String, MPERadarData> readSPRadarData(Date date) {
        // reads DPA radar data
        return readRadarData(date, "rwradarresult");

    }

    /**
     * Reads DPRadarData
     * 
     * @param date
     * @return
     */
    public Map<String, MPERadarData> readDPRadarData(Date date) {
        // reads DAA radar data
        return readRadarData(date, "daaradarresult");
    }

    /**
     * Reads Radar Data
     * 
     * @param date
     * @param tableName
     * @return
     */
    public Map<String, MPERadarData> readRadarData(Date date, String tableName) {
        getRadars();
        StringBuffer sqlQuery = new StringBuffer();
        sqlQuery.append("select radid,num_gages, rad_avail, rw_bias_val_used, mem_span_used, edit_bias, ignore_radar from "
                + tableName
                + " where obstime='"
                + TimeUtil.formatToSqlTimestamp(date) + "' and radid in(");
        statusHandler.handle(
                Priority.DEBUG,
                "Date string actually passed in query to radar table: "
                        + date.toString());
        for (int i = 0; i < radarList.size(); i++) {
            sqlQuery.append("'");
            sqlQuery.append(radarList.get(i).getId());
            sqlQuery.append("'");
            if (i != radarList.size() - 1) {
                sqlQuery.append(",");
            }
        }
        sqlQuery.append(") order by radid asc");

        Map<String, MPERadarData> radarResultList = new HashMap<>(
                radarList.size());
        try {
            List<Object[]> results = DirectDbQuery
                    .executeQuery(sqlQuery.toString(), HydroConstants.IHFS,
                            QueryLanguage.SQL);
            Iterator<Object[]> iter = results.iterator();
            Object[] item = iter.hasNext() ? iter.next() : null;
            for (MPERadarLoc radarLoc : radarList) {
                MPERadarData radarData = new MPERadarData();

                int compareResult = item == null ? -1 : radarLoc.getId()
                        .compareTo((String) item[0]);

                if (compareResult < 0) {
                    radarData.setProductDate(date);

                } else if (compareResult == 0) {
                    radarData.setNumGages(((Number) item[1]).intValue());

                    RadarAvailability radAvail = RadarAvailability.MISSING;
                    if ("y".equals(item[2])) {
                        radAvail = RadarAvailability.AVAILABLE;
                    } else if ("z".equals(item[2])) {
                        radAvail = RadarAvailability.ZERO;
                    }
                    radarData.setRadAvail(radAvail);

                    radarData
                            .setRwBiasValUsed(((Number) item[3]).doubleValue());
                    radarData.setMemSpanUsed(((Number) item[4]).doubleValue());
                    radarData.setEditBias((String) item[5]);
                    radarData.setIgnoreRadar(!"n".equals(item[6]));

                    if (radAvail.equals(RadarAvailability.AVAILABLE)
                            || radAvail.equals(RadarAvailability.ZERO)) {
                        radarData.setProductDate(readProductDateTime(
                                radarLoc.getId(), date));
                    }

                    item = iter.hasNext() ? iter.next() : null;
                } else {
                    statusHandler.handle(
                            Priority.PROBLEM,
                            "Record not found in " + tableName + " table for "
                                    + radarLoc.getId() + " for time "
                                    + date.toString() + "Z");
                }
                radarResultList.put(radarLoc.getId(), radarData);
            }
        } catch (VizException e) {
            statusHandler.handle(Priority.PROBLEM, "Record not found in "
                    + tableName + " ", e);
        }
        return radarResultList;
    }

    /**
     * Reads Product DateTime
     * 
     * @param radarId
     * @param date
     * @return
     */
    public Date readProductDateTime(String radarId, Date date) {
        Date productDate = null;

        StringBuilder query = new StringBuilder("select obstime");
        // query.append(",minoff,radid,abs(minoff) as abs");
        query.append(" from dparadar where radid = '");
        query.append(radarId);
        query.append("' and supplmess in (0,4) and obstime between '");

        Calendar cal = TimeUtil.newGmtCalendar(date);
        cal.add(Calendar.MINUTE, -TOP_OF_HOUR_WINDOW);
        Date start = cal.getTime();
        query.append(TimeUtil.formatToSqlTimestamp(start));
        query.append("' and '");

        cal.setTime(date);
        cal.add(Calendar.MINUTE, TOP_OF_HOUR_WINDOW);
        Date end = cal.getTime();
        query.append(TimeUtil.formatToSqlTimestamp(end));

        query.append("' order by abs(minoff) asc limit 1;");
        try {
            List<Object[]> results = DirectDbQuery.executeQuery(
                    query.toString(), HydroConstants.IHFS, QueryLanguage.SQL);

            if (results.size() > 0) {
                productDate = (Date) results.get(0)[0];
            }

        } catch (VizException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Could not read records from dparadar ", e);
        }
        return productDate;
    }

    /**
     * Gets Radar's locations from DB
     * 
     * @return
     */
    public List<MPERadarLoc> getRadars() {
        if (radarList == null) {
            readRadarLoc();
        }

        return radarList;
    }

    /**
     * Gets Latest Date
     * 
     * @return latest date
     */
    public Date getLatestDate() {
        Calendar cal = TimeUtil.newGmtCalendar();
        cal.set(Calendar.MINUTE, 0);
        cal.set(Calendar.SECOND, 0);
        cal.set(Calendar.MILLISECOND, 0);
        Date latestDate = cal.getTime();
        return latestDate;
    }

    /**
     * Gets Earliest Date
     * 
     * @return Earliest Date
     */
    public Date getEarliestDate() {
        Calendar cal = TimeUtil.newGmtCalendar(getLatestDate());
        cal.add(Calendar.DAY_OF_MONTH, -MAX_GAGEQC_DAYS);
        Date earliestDate = cal.getTime();
        return earliestDate;
    }

    /**
     * Gets HRAP SubGrid
     * 
     * @return
     */
    public HRAPSubGrid getHRAPSubGrid() {
        if (subGrid == null) {
            try {
                subGrid = HRAP.getInstance().getHRAPSubGrid(getHRAPExtent());
            } catch (Exception e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Error getting HRAP SubGrid ", e);
            }
        }
        return subGrid;
    }

    /**
     * Gets HRAP Extent
     * 
     * @return
     */
    public Rectangle getHRAPExtent() {
        if (HRAPExtent == null) {
            try {
                HRAPExtent = HRAPCoordinates.getHRAPCoordinates();
            } catch (Exception e) {
                statusHandler.handle(Priority.PROBLEM, "Error getting HRAP. ",
                        e);
            }
        }
        return HRAPExtent;
    }

    /**
     * Reads Gage Data
     * 
     * @param startTime
     * @param endTime
     * @return
     */
    public List<MPEGageData> readGageData(Date startTime, Date endTime) {

        /* read process PC token from .Apps_defaults */
        String processpc = appsDefaults.getToken("mpe_process_PC");

        boolean process_PC;
        if (processpc.equalsIgnoreCase("OFF")) {
            process_PC = false;
            statusHandler.handle(Priority.INFO, "Process PC Data = OFF ");
        } else {
            process_PC = true;
            statusHandler.handle(Priority.INFO, "Process PC Data = ON ");
        }

        // Read all records from the HourlyPC and HourlyPP tables for this
        // datetime.
        int[] pc_record_cnt = new int[] { 0 };
        List<Hourlypc> hourlyPCList = new ArrayList<>();
        if (process_PC == true) {
            hourlyPCList = loadPCHourly(startTime, endTime, null, null);
        } else {
            System.out.println("Processing of PC data turned off ");
        }
        pc_record_cnt[0] = hourlyPCList.size();

        int[] pp_record_cnt = new int[] { 0 };
        List<Hourlypp> hourlyPPList = loadPPHourly(startTime, endTime, null,
                null);
        pp_record_cnt[0] = hourlyPPList.size();

        // This is a PP/PC record count. It is not an actual count of stations
        // to process gage data for.
        int proc_count = pc_record_cnt[0] + pp_record_cnt[0];

        // read all records from PseudoGageVal table for this datetime
        String where = " WHERE obstime='"
                + TimeUtil.formatToSqlTimestamp(endTime) + "' ";

        List<Pseudogageval> pseudoList = getPseudoGageVal(where);
        int pseudoCount = pseudoList.size();

        int ngages = proc_count + pseudoCount;

        badGages = readBadGageList();

        boolean bad_gages_exist = badGages.size() > 0;

        // TODO: implement this function
        // get_snow_polygons (&PolyList, date_st3.cdate);

        /* read through gage data sets to store data in the gage structure */
        List<MPEGageData> gageData = new ArrayList<>(ngages);

        int[] pHourlyPPIdx = new int[] { 0 };
        int[] pHourlyPCIdx = new int[] { 0 };

        // process the regular gage values first
        while ((pHourlyPPIdx[0] < hourlyPPList.size())
                || (pHourlyPCIdx[0] < hourlyPCList.size())) {
            PrecipTotal totalPrecip = pu.getTotalHourlyPrecip(hourlyPCList,
                    pHourlyPCIdx, hourlyPPList, pHourlyPPIdx, endTime, 1, 0f,
                    CommonHydroConstants.PRECIP_TS_RANK
                            | CommonHydroConstants.PRECIP_PP, true,
                    pc_record_cnt, pp_record_cnt);

            /* Retrieve the Latitude/Longitude of the station. */
            Coordinate latLon = getMpeLocLatLon(totalPrecip.getLid());

            if (latLon == null) {
                continue;
            }
            if ((latLon.x == 0.) || (latLon.y == 0.)) {
                continue;
            }

            /* calculate HRAP coordinates from lat,lon */
            Coordinate hrap_point = new Coordinate(0, 0);
            try {
                hrap_point = HRAP.getInstance().latLonToGridCoordinate(latLon,
                        PixelOrientation.LOWER_LEFT);
            } catch (Exception e) {
                statusHandler.handle(Priority.PROBLEM,
                        "error computing hrap coordinate ", e);
            }

            /* make sure the gage is within the area. if so load the info */
            MPEGageData gage = new MPEGageData();
            gage.id = totalPrecip.getLid();
            gage.pe = totalPrecip.getPe();
            gage.ts = totalPrecip.getTs();
            gage.gval = totalPrecip.getValue();

            /* Test if the gage is in the bad gage list */
            if (bad_gages_exist) {
                int pBadTest = Collections.binarySearch(badGages,
                        totalPrecip.getLid());

                if (pBadTest >= 0) {
                    continue;
                }

            }

            /*
             * Initialize the qc value of this gage to indicate that it passed
             * both the spatial consistency and multisensor qc checks.
             */
            gage.qc = 3;

            gage.latLon = latLon;

            gage.hrap = hrap_point;
            gage.hrapLoc = new Coordinate(hrap_point.x
                    - getHRAPSubGrid().getExtent().x, hrap_point.y
                    - getHRAPSubGrid().getExtent().y);

            gage.manedit = totalPrecip.getQc() == 'M';

            gage.td = totalPrecip.getQc() == 'D';

            if (totalPrecip.getQc() == 'L') {
                gage.qc = 2;
            }

            if (totalPrecip.getQc() == 'C') {
                gage.qc = 1;
            }

            gage.reported_missing = totalPrecip.isReportedMissing();

            gage.setBad(false);

            gageData.add(gage);
        }

        hourlyPPList.clear();
        hourlyPPList = null;

        hourlyPCList.clear();
        hourlyPCList = null;

        /* process the pseudo gage values */

        if (pseudoList.size() > 0) {
            int pseudoIdx = 0;
            while (pseudoIdx < pseudoList.size()) {
                Pseudogageval pseudoGage = pseudoList.get(pseudoIdx);
                /* calculate the national HRAP coordinates from lat,lon */

                Coordinate latLon = new Coordinate((pseudoGage.getLon() * -1),
                        pseudoGage.getLat());

                Coordinate hrap_point = new Coordinate(0, 0);
                try {
                    hrap_point = HRAP.getInstance().latLonToGridCoordinate(
                            latLon, PixelOrientation.LOWER_LEFT);
                } catch (Exception e) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Error computing HRAP ", e);
                }

                /* make sure the gage is within the area. if so load the info */

                /*
                 * Pseudo gages are not subject to the SCC and MSC QC checks.
                 */
                MPEGageData gage = new MPEGageData();
                gage.qc = 3;

                gage.latLon = latLon;

                gage.id = pseudoGage.getId().getPseudoGageId();
                gage.pe = "PP";

                gage.gval = pseudoGage.getGageValue();

                if (gage.gval == -999.) {
                    gage.reported_missing = true;
                } else {
                    gage.gval = gage.gval;
                }

                gage.hrap = hrap_point;
                gage.hrapLoc = new Coordinate(hrap_point.x
                        - getHRAPSubGrid().getExtent().x, hrap_point.y
                        - getHRAPSubGrid().getExtent().y);

                gage.hrapLoc = hrap_point;

                gage.manedit = "T".equals(pseudoGage.getManEdited());

                gage.td = false;

                gageData.add(gage);

                pseudoIdx++;
            }

            if (pseudoList != null) {
                pseudoList.clear();
                pseudoList = null;
            }
        }

        /*
         * fill in remaining fields of gage structure and convert the units of
         * the value
         */
        for (MPEGageData gage : gageData) {
            if (gage.gval == CommonHydroConstants.MISSING_PRECIP) {
                /*
                 * If the value is missing, set the gage value to the MPE
                 * missing representation.
                 */
                gage.gval = -999f;
            }

            gage.rid = "ZZZ";
            gage.xmrgVal = CommonHydroConstants.MPE_MISSING;
            gage.mval = CommonHydroConstants.MPE_MISSING;
            gage.rval = CommonHydroConstants.MPE_MISSING;
            gage.edit = "";
            gage.bval = CommonHydroConstants.MPE_MISSING;
            gage.locVal = CommonHydroConstants.MPE_MISSING;
            gage.gageOnly = CommonHydroConstants.MPE_MISSING;
            gage.useInP3 = true;
        }

        return gageData;
    }

    /**
     * Gets ingest info.
     * 
     * @return
     */
    public List<Ingestfilter> getIngestInfoList() {

        /*
         * Construct the where clause used to retrieve rows from the
         * IngestFilter table.
         */
        String whereClause = "WHERE ingest = 'T' ORDER by ts ASC";

        List<Ingestfilter> pIngestHead = null;
        pIngestHead = IHFSDbGenerated.GetIngestFilter(whereClause);

        return pIngestHead;
    }

    /**
     * Loads PChourly
     * 
     * @param queryBeginTime
     * @param queryEndTime
     * @param lid
     * @param ts
     * @return
     */
    public List<Hourlypc> loadPCHourly(Date queryBeginTime, Date queryEndTime,
            String lid, List<String> ts) {

        final String fullQuery = pu.buildHourlyHQL(queryBeginTime,
                queryEndTime, lid, ts, Hourlypc.class.getName(), null);
        List<Object[]> results = null;
        try {
            results = DirectDbQuery.executeQuery(fullQuery, "ihfs",
                    QueryLanguage.HQL);
        } catch (VizException e) {
            statusHandler.error("Failed to retrieve the Hourly PC data. ", e);
        }

        if (results == null || results.isEmpty()) {
            return new ArrayList<>(0);
        }

        List<Hourlypc> hourlyPcRecords = new ArrayList<>(results.size());
        for (Object object : results) {
            Object[] dataValues = (Object[]) object;

            /*
             * First few fields are needed to build an {@link HourlypcId}.
             */
            HourlypcId id = new HourlypcId((String) dataValues[0],
                    (String) dataValues[1], (Date) dataValues[2]);
            Hourlypc record = new Hourlypc(id, (String) dataValues[3],
                    (String) dataValues[4], (Short) dataValues[5],
                    (Short) dataValues[6], (Short) dataValues[7],
                    (Short) dataValues[8], (Short) dataValues[9],
                    (Short) dataValues[10], (Short) dataValues[11],
                    (Short) dataValues[12], (Short) dataValues[13],
                    (Short) dataValues[14], (Short) dataValues[15],
                    (Short) dataValues[16], (Short) dataValues[17],
                    (Short) dataValues[18], (Short) dataValues[19],
                    (Short) dataValues[20], (Short) dataValues[21],
                    (Short) dataValues[22], (Short) dataValues[23],
                    (Short) dataValues[24], (Short) dataValues[25],
                    (Short) dataValues[26], (Short) dataValues[27],
                    (Short) dataValues[28]);
            hourlyPcRecords.add(record);
        }

        return hourlyPcRecords;
    }

    /**
     * Loads PPhourly
     * 
     * @param queryBeginTime
     * @param queryEndTime
     * @param lid
     * @param ts
     * @return
     */
    public List<Hourlypp> loadPPHourly(Date queryBeginTime, Date queryEndTime,
            String lid, List<String> ts) {

        final String selectAdditional = ", b.sixhr06, b.sixhr12, b.sixhr18, b.sixhr24, b.sixhrqc, b.sixhroffset ";
        final String fullQuery = pu.buildHourlyHQL(queryBeginTime,
                queryEndTime, lid, ts, Hourlypp.class.getName(),
                selectAdditional);
        List<Object[]> results = null;
        try {
            results = DirectDbQuery.executeQuery(fullQuery, "ihfs",
                    QueryLanguage.HQL);
        } catch (VizException e) {
            statusHandler.error("Failed to retrieve the Hourly PP data. ", e);
        }

        if (results == null || results.isEmpty()) {
            return new ArrayList<>(0);
        }

        List<Hourlypp> hourlyPpRecords = new ArrayList<>(results.size());
        for (Object object : results) {
            Object[] dataValues = (Object[]) object;

            /*
             * First few fields are needed to build an {@link HourlypcId}.
             */
            HourlyppId id = new HourlyppId((String) dataValues[0],
                    (String) dataValues[1], (Date) dataValues[2]);
            Hourlypp record = new Hourlypp(id, (String) dataValues[3],
                    (String) dataValues[4], (Short) dataValues[5],
                    (Short) dataValues[6], (Short) dataValues[7],
                    (Short) dataValues[8], (Short) dataValues[9],
                    (Short) dataValues[10], (Short) dataValues[11],
                    (Short) dataValues[12], (Short) dataValues[13],
                    (Short) dataValues[14], (Short) dataValues[15],
                    (Short) dataValues[16], (Short) dataValues[17],
                    (Short) dataValues[18], (Short) dataValues[19],
                    (Short) dataValues[20], (Short) dataValues[21],
                    (Short) dataValues[22], (Short) dataValues[23],
                    (Short) dataValues[24], (Short) dataValues[25],
                    (Short) dataValues[26], (Short) dataValues[27],
                    (Short) dataValues[28], (Short) dataValues[29],
                    (Short) dataValues[30], (Short) dataValues[31],
                    (Short) dataValues[32], (String) dataValues[33],
                    (String) dataValues[34]);
            hourlyPpRecords.add(record);
        }

        return hourlyPpRecords;
    }

    /**
     * loadPeRaw
     * 
     * Method to load data from current precipitation table of IHFS database.
     * Combines legacy load_PC_raw and load_PP_raw methods in load_PCPP_data.
     * 
     * @param beginstr
     *            - string representing beginning time
     * @param endstr
     *            -- string representing ending time
     * @param locId
     *            -- location identifier
     * @param typeSource
     *            -- type source
     * @param pe
     *            -- physical element, either PC or PP
     * @return -- list of objects from IHFS database query
     */
    public List<Object[]> loadPeRaw(String beginstr, String endstr,
            String locId, java.util.List<String> typeSource,
            HydroConstants.PhysicalElement pe) {

        List<Object[]> retVal = null;
        String tsClause = "";
        StringBuilder query = new StringBuilder();
        StringBuilder where = new StringBuilder();
        String lid = "";
        String value = "";
        String obstime = "";
        String ts = "";
        String physElt = "";
        String qcwhere = "";
        String dur = "";

        if ((typeSource != null) && !typeSource.isEmpty()) {
            tsClause = pu.buildTsClause(typeSource, "id.ts");
            if (tsClause == null) {
                return null;
            }
        }

        switch (pe) {
        case PC:
            query.append("select pc.lid, pc.pe, pc.dur, pc.ts, pc.extremum, pc.value, pc.shef_qual_code, pc.quality_code, pc.revision, pc.product_id, pc.producttime, pc.postingtime, pc.obstime, location.name from location, curpc pc where location.lid = pc.lid");
            lid = "pc.lid";
            value = "pc.value";
            obstime = "pc.obstime";
            ts = "pc.ts";
            physElt = " pc.";
            qcwhere = "";
            dur = "";
            break;
        case PP:
            query.append("select pp.lid, pp.pe, pp.dur, pp.ts, pp.extremum, pp.value, pp.shef_qual_code, pp.quality_code, pp.revision, pp.product_id, pp.producttime, pp.postingtime, pp.obstime, location.name from location, curpp pp where location.lid = pp.lid");
            lid = "pp.lid";
            value = "pp.value";
            obstime = "pp.obstime";
            ts = "pp.ts";
            physElt = " pp.";
            qcwhere = "pp.quality_code >= "
                    + HydroQC.QUESTIONABLE_BAD_THRESHOLD;
            dur = "pp.dur";
            break;
        }

        if ((locId != null) && !locId.isEmpty() && (typeSource != null)
                && !typeSource.isEmpty()) {
            where.append(" AND ");
            where.append(lid).append(" = '");
            where.append(locId).append("' AND ");
            where.append(tsClause).append(" AND ");
            where.append(value).append(" != '-9999.0' AND ");
            where.append(obstime).append(" >= '");
            where.append(beginstr).append("' AND ");
            where.append(obstime).append(" <= '");
            where.append(endstr);
            switch (pe) {
            case PC:
                where.append("' ORDER BY ");
                where.append(obstime).append(" DESC ");
                break;
            case PP:
                where.append("' AND ");
                where.append(qcwhere).append(" ORDER BY ");
                where.append(dur).append(" DESC, ");
                where.append(obstime).append(" DESC ");
                break;
            }
        } else if ((typeSource != null) && !typeSource.isEmpty()) {
            where.append(" AND ");
            where.append(tsClause).append(" AND ");
            where.append(value).append(" != '-9999.0' AND ");
            where.append(obstime).append(" >= '");
            where.append(beginstr).append("' AND ");
            where.append(obstime + " <= '");
            where.append(endstr);
            switch (pe) {
            case PC:
                where.append("' ORDER BY ");
                where.append(lid).append(" ASC, ");
                where.append(obstime).append(" DESC ");
                break;
            case PP:
                where.append("' AND ");
                where.append(qcwhere).append(" ORDER BY ");
                where.append(lid).append(" ASC, ");
                where.append(dur).append(" DESC, ");
                where.append(obstime).append(" DESC ");
                break;
            }
        } else if ((locId != null) && !locId.isEmpty()) {
            where.append(" AND ");
            where.append(lid).append(" = '");
            where.append(locId).append("' AND ");
            where.append(value).append(" != '-9999.0' AND ");
            where.append(obstime).append(" >= '");
            where.append(beginstr).append("' AND ");
            where.append(obstime).append(" <= '");
            where.append(endstr);
            switch (pe) {
            case PC:
                where.append("' ORDER BY ");
                where.append(ts).append(" ASC, ");
                where.append(obstime).append(" DESC ");
                break;
            case PP:
                where.append("' AND ");
                where.append(qcwhere).append(" ORDER BY ");
                where.append(ts).append(" ASC, ");
                where.append(dur).append(" DESC, ");
                where.append(obstime).append(" DESC ");
                break;
            }
        } else {
            where.append(" AND ");
            where.append(value).append(" != '-9999.0' AND ");
            where.append(obstime).append(" >= '");
            where.append(beginstr).append("' AND ");
            where.append(obstime + " <= '");
            where.append(endstr);
            switch (pe) {
            case PC:
                where.append("' ORDER BY ");
                where.append(lid).append(" ASC, ");
                where.append(ts).append(" ASC, ");
                where.append(obstime).append(" DESC ");
                break;
            case PP:
                where.append("' AND ");
                where.append(qcwhere).append(" ORDER BY ");
                where.append(lid).append(" ASC, ");
                where.append(ts).append(" ASC, ");
                where.append(dur).append(" DESC, ");
                where.append(obstime).append(" DESC ");
                break;
            }
        }

        if ((typeSource != null) && !typeSource.isEmpty()) {
            where.replace(where.indexOf(" id."), where.indexOf(" id.") + 4,
                    physElt);

        }

        query.append(where.toString());

        try {
            retVal = (ArrayList<Object[]>) DirectDbQuery.executeQuery(
                    query.toString(), HydroConstants.IHFS, QueryLanguage.SQL);
        } catch (VizException e) {
            statusHandler.error("Failed to retrieve the PE raw data. ", e);
        }

        return retVal;
    }

    /**
     * Reads Gage Data
     * 
     * @param endTime
     * @return
     */
    public List<MPEGageData> readGageData(Date endTime) {
        Calendar cal = TimeUtil.newGmtCalendar(endTime);
        cal.add(Calendar.HOUR_OF_DAY, -1);
        Date startTime = cal.getTime();
        return readGageData(startTime, endTime);
    }

    public void addEditedGage(MPEGageData gage) {
        editGages.put(gage.getId(), gage);
    }

    public int getDQCDays() {
        return dqcdays;
    }

    public void setDQCDays(int days) {
        dqcdays = days;
    }

    public Map<String, MPEGageData> readEditGages() {
        return editGages;
    }

    public MPEGageData getEditedGage(String gageId) {
        return editGages.get(gageId);
    }

    public MPEGageData getEditedGage(MPEGageData gage) {
        MPEGageData gageData = null;
        if (gage != null) {
            gageData = editGages.get(gage.getId());
        }

        return gageData;
    }

    public void clearEditGages() {
        editGages.clear();
    }

    public void addBadGage(String gage) {
        if (!badGages.contains(gage)) {
            badGages.add(gage);
        }
    }

    public List<String> readBadGages() {
        return badGages;
    }

    public String getBadGage(String gageId) {
        return badGages.get(badGages.indexOf(gageId));
    }

    public String getBadGage(MPEGageData gage) {
        String gageData = null;
        if (gage != null) {
            gageData = badGages.get(badGages.indexOf(gage));
        }

        return gageData;
    }

    /**
     * Removes Bad Gage.
     * 
     * @param gageId
     */
    public void removeBadGage(String gageId) {
        if (badGages.contains(gageId)) {
            badGages.remove(gageId);
        }
    }

    /**
     * Reads Bad Gage List
     * 
     * @return
     */
    public List<String> readBadGageList() {
        badGages = new ArrayList<>();

        String dirname = appsDefaults.getToken("mpe_bad_gages_dir");
        File file = new File(FileUtil.join(dirname, "mpe_bad_gage_list"));

        if (file.exists()) {

            try (BufferedReader badGageFile = new BufferedReader(
                    new FileReader(file))) {

                while (badGageFile.ready()) {
                    badGages.add(badGageFile.readLine());
                }

            } catch (IOException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Error get Bad Gage list " + file, e);
            }

            Collections.sort(badGages);
        }
        return badGages;
    }

    /**
     * Writes Bad Gage List
     */
    public void writeBadGageList() {
        String dirname = appsDefaults.getToken("mpe_bad_gages_dir");
        File file = new File(FileUtil.join(dirname, "mpe_bad_gage_list"));
        file.setReadable(true, false);
        file.setWritable(true, false);
        Collections.sort(badGages);

        if (file != null) {
            try (BufferedWriter badGageFile = new BufferedWriter(
                    new FileWriter(file))) {

                for (int i = 0; i < badGages.size(); i++) {
                    badGageFile.write(badGages.get(i));
                    badGageFile.newLine();
                }
            } catch (IOException e) {
                statusHandler.handle(Priority.PROBLEM, "Error writing file "
                        + file.toString() + " ", e);
            }
        }
    }

    /**
     * Gets MPE Location coordinates
     * 
     * @param lid
     * @return
     */
    public Coordinate getMpeLocLatLon(String lid) {
        boolean status = false;
        /*
         * the first time, read the station information from the station file
         * into the station data array.
         */
        if (locationMap == null) {
            status = readStationFile();
            if (status == false) {
                return null;
            }
        }

        /* get the info for the matching identifier, if there is a match. */
        Coordinate latLon = locationMap.get(lid);

        return latLon;
    }

    /**
     * Reads Station File
     * 
     * @return
     */
    public boolean readStationFile() {
        boolean status = false;
        String stationDir = appsDefaults.getToken("rfcwide_gageloc_dir");
        String stationPathname = FileUtil
                .join(stationDir, "mpe_gage_locations");
        try (BufferedReader in = new BufferedReader(new FileReader(
                stationPathname))) {

            int stationCount = Integer.parseInt(in.readLine());
            locationMap = new HashMap<>(stationCount);

            for (int i = 0; i < stationCount; i++) {
                String[] tokens = in.readLine().split(" ");
                if (tokens.length == 3) {
                    double lat = Double.parseDouble(tokens[1]);
                    double lon = -Double.parseDouble(tokens[2]);
                    locationMap.put(tokens[0], new Coordinate(lon, lat));
                } else {
                    continue;
                }
            }

        } catch (FileNotFoundException e) {
            statusHandler.handle(Priority.PROBLEM, "MPE Station File "
                    + stationPathname + " not found. ", e);
            return status;
        } catch (IOException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Could not open/read MPE Station file " + stationPathname
                            + " ", e);
            return status;
        } catch (NumberFormatException e) {
            statusHandler.handle(Priority.PROBLEM, "Error parsing string ", e);
            return status;
        }

        return status = true;
    }

    /**
     * Gets RawPP data
     * 
     * @param where
     * @return
     */
    public List<Rawpp> getRawPP(String where) {
        List<Rawpp> retVal = null;

        StringBuilder query = new StringBuilder("FROM ");
        query.append(Rawpp.class.getName());
        query.append(" ");
        query.append(where);

        List<Object[]> results;
        try {
            results = DirectDbQuery.executeQuery(query.toString(),
                    HydroConstants.IHFS, QueryLanguage.HQL);
            retVal = new ArrayList<>(results.size());
            for (Object[] item : results) {
                retVal.add((Rawpp) item[0]);
            }
        } catch (VizException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Could not get records from Rawpp ", e);
        }
        return retVal;
    }

    /**
     * Updates RawPP
     * 
     * @param where
     */
    public void updateRawPP(String where) {
        StringBuilder query = new StringBuilder("UPDATE ");
        query.append(Rawpp.class.getAnnotation(Table.class).name());
        query.append(" ");
        query.append(where);

        try {
            DirectDbQuery.executeStatement(query.toString(),
                    HydroConstants.IHFS, QueryLanguage.SQL);
        } catch (VizException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Could not update records in Rawpp ", e);
        }
    }

    /**
     * Inserts a new record into the Rawpp table.
     * 
     * @param where
     *            String where clause of sql insert statement.
     */
    public void insertRawPP(String where) {
        StringBuilder query = new StringBuilder("INSERT INTO ");
        query.append(Rawpp.class.getAnnotation(Table.class).name());
        query.append(" ");
        query.append(where);

        try {
            DirectDbQuery.executeStatement(query.toString(),
                    HydroConstants.IHFS, QueryLanguage.SQL);
        } catch (VizException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Could not insert records to Rawpp ", e);
        }
    }

    /**
     * Gets Pseudo Gage Value
     * 
     * @param where
     * @return
     */
    public List<Pseudogageval> getPseudoGageVal(String where) {

        List<Pseudogageval> retVal = null;
        StringBuilder query = new StringBuilder("FROM ");
        query.append(Pseudogageval.class.getName());
        query.append(" ");
        query.append(where);
        try {
            List<Object[]> results = DirectDbQuery.executeQuery(
                    query.toString(), HydroConstants.IHFS, QueryLanguage.HQL);
            retVal = new ArrayList<>(results.size());
            for (Object[] item : results) {
                retVal.add((Pseudogageval) item[0]);
            }
        } catch (VizException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Could not get records from Pseudogageval ", e);
        }

        return retVal;
    }

    /**
     * Updates Pseudo Gage Value.
     * 
     * @param where
     * @throws VizException
     */
    public void updatePseudoGageVal(String where) throws VizException {
        StringBuilder query = new StringBuilder("UPDATE ");
        query.append(Pseudogageval.class.getAnnotation(Table.class).name());
        query.append(" ");
        query.append(where);

        DirectDbQuery.executeStatement(query.toString(), HydroConstants.IHFS,
                QueryLanguage.SQL);
    }

    /**
     * Inserts Pseudo Gage Value.
     * 
     * @param where
     */
    public void insertPseudoGageVal(String where) {
        StringBuilder query = new StringBuilder("INSERT INTO ");
        query.append(Pseudogageval.class.getAnnotation(Table.class).name());
        query.append(" ");
        query.append(where);

        try {
            DirectDbQuery.executeStatement(query.toString(),
                    HydroConstants.IHFS, QueryLanguage.SQL);
        } catch (VizException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Could not insert records in Pseudogageval ", e);
        }
    }

    /**
     * @return the rFC
     */
    public String getRFC() {
        return RFC;
    }
}
