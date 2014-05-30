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
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.TimeZone;

import javax.persistence.Table;

import org.apache.commons.lang.builder.ToStringBuilder;
import org.opengis.metadata.spatial.PixelOrientation;

import com.raytheon.uf.common.dataplugin.shef.tables.Hourlypc;
import com.raytheon.uf.common.dataplugin.shef.tables.Hourlypp;
import com.raytheon.uf.common.dataplugin.shef.tables.Pseudogageval;
import com.raytheon.uf.common.dataplugin.shef.tables.Rawpp;
import com.raytheon.uf.common.hydro.spatial.HRAP;
import com.raytheon.uf.common.hydro.spatial.HRAPCoordinates;
import com.raytheon.uf.common.hydro.spatial.HRAPSubGrid;
import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.SimulatedTime;
import com.raytheon.uf.common.util.FileUtil;
import com.raytheon.uf.viz.core.catalog.DirectDbQuery;
import com.raytheon.uf.viz.core.catalog.DirectDbQuery.QueryLanguage;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.hydrocommon.HydroConstants;
import com.raytheon.viz.hydrocommon.whfslib.PrecipUtil;
import com.raytheon.viz.hydrocommon.whfslib.PrecipUtil.total_precip;
import com.raytheon.viz.mpe.core.MPEDataManager.MPERadarData.RadarAvailability;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * TODO Add Description
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 1, 2008            randerso     Initial creation
 * Nov 6, 2008  1649      snaples      Added updatePseudogageval method
 * Nov 6, 2008  1649      snaples      Added new methods for getting and 
 *                                     updating RawPP records
 * Nov 24, 2008 1748      snaples      Added getters to MPEGageData
 * Jun 18, 2013 16053     snaples      Removed methods set and getRadarEditFlag
 * Dec 15 2013  DCS 167   cgobs        DualPol capabilities
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class MPEDataManager {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(MPEDataManager.class);

    private static final int TOP_OF_HOUR_WINDOW = 10;

    public static class MPEDateInfo {
        private final Date lastSaveTime;

        private final Date lastExecTime;

        private final boolean autoSave;

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

    public static class MPERadarLoc {
        private String id;

        private Coordinate latLon;

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

    public static class MPERadarData {
        public static enum RadarAvailability {
            AVAILABLE, MISSING, ZERO
        };

        private Date productDate;

        private short[] rawRadarData;

        private short[] unbiasedRadarData;

        private double rwBiasValUsed;
//        private double daaBiasValUsed;
       
        
        private double memSpanUsed;

        private String editBias;

        private RadarAvailability radAvail;

        private boolean ignoreRadar;

        private int numGages;

        private boolean radarDataAvailable;

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

        /*
        public double getDAABiasValUsed() {
            return daaBiasValUsed;
        }
        */
        
        public void setRwBiasValUsed(double rwBiasValUsed) {
            this.rwBiasValUsed = rwBiasValUsed;
        }

        /*
        public void setDAABiasValUsed(double daaBiasValUsed) {
            this.daaBiasValUsed = daaBiasValUsed;
        }
        */
        
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

    public static class MPEGageData {

        String id;

        String rid;

        String edit;

        float gval;

        float xmrg_val;

        float mval;

        float rval;

        float bval;

        float loc_val;

        float gage_only;

        float sat_val;

        Coordinate latLon;

        Coordinate hrap; /* The national HRAP grid */

        Coordinate hrap_loc; /* The local HRAP grid */

        boolean td;

        boolean manedit;

        int qc; /* quality control */

        String ts;

        String pe;

        boolean reported_missing;

        boolean use_in_p3;

        boolean is_bad;

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
            // float newVal = gval;
            // if (getId().contains("PSEUDO")) {
            // System.out.println("PSEUDO");
            // UnitConverter conv = SI.MILLIMETER.getConverterTo(NonSI.INCH);
            // newVal = (float) conv.convert(getGval());
            // }
            // System.out.println("getGval - " + newVal);
            // return newVal;
            return gval;
        }

        /**
         * @return the xmrg_val
         */
        public float getXmrg_val() {
            return xmrg_val;
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
         * @return the loc_val
         */
        public float getLoc_val() {
            return loc_val;
        }

        /**
         * @return the gage_only
         */
        public float getGage_only() {
            return gage_only;
        }

        /**
         * @return the sat_val
         */
        public float getSat_val() {
            return sat_val;
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
        public Coordinate getHrap_loc() {
            return hrap_loc;
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
         * @return the use_in_p3
         */
        public boolean isUse_in_p3() {
            return use_in_p3;
        }

        /**
         * @return the is_bad
         */
        public boolean isIs_bad() {
            return is_bad;
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
         * @param xmrg_val
         *            the xmrg_val to set
         */
        public void setXmrg_val(float xmrg_val) {
            this.xmrg_val = xmrg_val;
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
         * @param loc_val
         *            the loc_val to set
         */
        public void setLoc_val(float loc_val) {
            this.loc_val = loc_val;
        }

        /**
         * @param gage_only
         *            the gage_only to set
         */
        public void setGage_only(float gage_only) {
            this.gage_only = gage_only;
        }

        /**
         * @param sat_val
         *            the sat_val to set
         */
        public void setSat_val(float sat_val) {
            this.sat_val = sat_val;
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
         * @param hrap_loc
         *            the hrap_loc to set
         */
        public void setHrap_loc(Coordinate hrap_loc) {
            this.hrap_loc = hrap_loc;
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
         * @param use_in_p3
         *            the use_in_p3 to set
         */
        public void setUse_in_p3(boolean use_in_p3) {
            this.use_in_p3 = use_in_p3;
        }

        /**
         * @param is_bad
         *            the is_bad to set
         */
        public void setIs_bad(boolean is_bad) {
            this.is_bad = is_bad;
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

    }

    private static final int MAX_GAGEQC_DAYS = 10;

    private static final SimpleDateFormat sdf;

    private static MPEDataManager instance;

    private final AppsDefaults appsDefaults = AppsDefaults.getInstance();;

    private final String RFC;

    private List<MPERadarLoc> radarList;

    private HashMap<String, MPEGageData> editGages = new HashMap<String, MPEGageData>();

    private ArrayList<String> badGages = new ArrayList<String>();

    private Map<Date, MPEDateInfo> dateMap;

    private Date latestAvailableDate = null;

    private Map<String, Coordinate> locationMap;

    private Rectangle HRAPExtent;

    private int dqcdays = Integer.parseInt(appsDefaults
            .getToken("mpe_dqc_num_days"));

    private HRAPSubGrid subGrid;

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

    static {
        sdf = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
        sdf.setTimeZone(TimeZone.getTimeZone("GMT"));
    }

    /**
     * private constructor for singleton
     */
    private MPEDataManager() {
        RFC = appsDefaults.getToken("st3_rfc");
    }

    private void getDates(boolean update) {
        String starttime = "";
        if (update && latestAvailableDate != null) {
            starttime = sdf.format(latestAvailableDate);
        } else {
            dateMap = new HashMap<Date, MPEDateInfo>();
            starttime = sdf.format(getEarliestDate());
        }
        String sqlQuery = "select obstime,last_save_time,last_exec_time,auto_save from rwresult where rfc='"
                + RFC
                + "' and obstime between '"
                + starttime
                + "' and '"
                + sdf.format(getLatestDate()) + "'";

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
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
    }

    public Map<Date, MPEDateInfo> getDateMap(boolean update) {
            
    	getDates(update);
        return dateMap;
    }

    public void readRadarLoc() {
        String sqlQuery = "select radid, lat, lon from radarloc where use_radar='T' order by radid asc";

        radarList = new ArrayList<MPERadarLoc>();
        try {
            List<Object[]> results = DirectDbQuery.executeQuery(sqlQuery,
                    HydroConstants.IHFS, QueryLanguage.SQL);
            for (Object[] item : results) {
                // note db stores west longitude as positive so must negate
                MPERadarLoc radarLoc = new MPERadarLoc((String) item[0],
                        (Double) item[1], -(Double) item[2]);
                radarList.add(radarLoc);
            }
        } catch (VizException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
    }

    public Map<String, MPERadarData> OrigReadRadarData(Date date) {
        getRadars();
        StringBuffer sqlQuery = new StringBuffer();
        sqlQuery.append("select radid,num_gages,rad_avail, rw_bias_val_used,mem_span_used, edit_bias, ignore_radar from rwradarresult where obstime='"
                + sdf.format(date) + "' and radid in(");
        for (int i = 0; i < radarList.size(); i++) {
            sqlQuery.append("'");
            sqlQuery.append(radarList.get(i).getId());
            sqlQuery.append("'");
            if (i != radarList.size() - 1) {
                sqlQuery.append(",");
            }
        }
        sqlQuery.append(") order by radid asc");

        Map<String, MPERadarData> radarResultList = new HashMap<String, MPERadarData>(
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
                    radarData.setNumGages((Integer) item[1]);

                    RadarAvailability radAvail = RadarAvailability.MISSING;
                    if ("y".equals(item[2])) {
                        radAvail = RadarAvailability.AVAILABLE;
                    } else if ("z".equals(item[2])) {
                        radAvail = RadarAvailability.ZERO;
                    }
                    radarData.setRadAvail(radAvail);

                    radarData.setRwBiasValUsed((Double) item[3]);
                    radarData.setMemSpanUsed((Double) item[4]);
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
                                    + sdf.format(date) + "Z");
                    // VizApp.logAndAlert(IStatus.WARNING, null, "Warning",
                    // "Record not found in RWRadarResult table for "
                    // + radarLoc.getId() + " for time "
                    // + sdf.format(date) + "Z", Activator
                    // .getDefault(), Activator.PLUGIN_ID);
                }

                radarResultList.put(radarLoc.getId(), radarData);
            }
        } catch (VizException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }

        return radarResultList;
    }

    public Map<String, MPERadarData> readSPRadarData(Date date)
    {
    	//reads DPA radar data
    	return readRadarData(date, "rwradarresult");
    	
    }
    
    public Map<String, MPERadarData> readDPRadarData(Date date)
    {
    	//reads DAA radar data
      	return readRadarData(date, "daaradarresult");
    }
    
    
    public Map<String, MPERadarData> readRadarData(Date date, String tableName) {
        getRadars();
        StringBuffer sqlQuery = new StringBuffer();
        sqlQuery.append("select radid,num_gages, rad_avail, rw_bias_val_used, mem_span_used, edit_bias, ignore_radar from " + 
        				 tableName + " where obstime='" + sdf.format(date) + "' and radid in(");
        for (int i = 0; i < radarList.size(); i++) {
            sqlQuery.append("'");
            sqlQuery.append(radarList.get(i).getId());
            sqlQuery.append("'");
            if (i != radarList.size() - 1) {
                sqlQuery.append(",");
            }
        }
        sqlQuery.append(") order by radid asc");

        Map<String, MPERadarData> radarResultList = new HashMap<String, MPERadarData>(
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
                    radarData.setNumGages((Integer) item[1]);

                    RadarAvailability radAvail = RadarAvailability.MISSING;
                    if ("y".equals(item[2])) {
                        radAvail = RadarAvailability.AVAILABLE;
                    } else if ("z".equals(item[2])) {
                        radAvail = RadarAvailability.ZERO;
                    }
                    radarData.setRadAvail(radAvail);

                    radarData.setRwBiasValUsed((Double) item[3]);
                    radarData.setMemSpanUsed((Double) item[4]);
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
                                    + sdf.format(date) + "Z");
                    // VizApp.logAndAlert(IStatus.WARNING, null, "Warning",
                    // "Record not found in RWRadarResult table for "
                    // + radarLoc.getId() + " for time "
                    // + sdf.format(date) + "Z", Activator
                    // .getDefault(), Activator.PLUGIN_ID);
                }

                radarResultList.put(radarLoc.getId(), radarData);
            }
        } catch (VizException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }

        return radarResultList;
    }

    public Date readProductDateTime(String radarId, Date date) {
        Date productDate = null;

        StringBuilder query = new StringBuilder("select obstime");
        // query.append(",minoff,radid,abs(minoff) as abs");
        query.append(" from dparadar where radid = '");
        query.append(radarId);
        query.append("' and supplmess in (0,4) and obstime between '");

        Calendar cal = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
        cal.setTime(date);
        cal.add(Calendar.MINUTE, -TOP_OF_HOUR_WINDOW);
        Date start = cal.getTime();
        query.append(sdf.format(start));
        query.append("' and '");

        cal.setTime(date);
        cal.add(Calendar.MINUTE, TOP_OF_HOUR_WINDOW);
        Date end = cal.getTime();
        query.append(sdf.format(end));

        query.append("' order by abs(minoff) asc limit 1;");
        try {
            List<Object[]> results = DirectDbQuery.executeQuery(
                    query.toString(), HydroConstants.IHFS, QueryLanguage.SQL);

            if (results.size() > 0) {
                productDate = (Date) results.get(0)[0];
            }

        } catch (VizException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
        return productDate;
    }

    public List<MPERadarLoc> getRadars() {
        if (radarList == null) {
            readRadarLoc();
        }

        return radarList;
    }

    public Date getLatestDate() {
        Date latestDate = SimulatedTime.getSystemTime().getTime();
        Calendar cal = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
        cal.setTime(latestDate);
        cal.set(Calendar.MINUTE, 0);
        cal.set(Calendar.SECOND, 0);
        cal.set(Calendar.MILLISECOND, 0);
        latestDate = cal.getTime();
        return latestDate;
    }

    public Date getEarliestDate() {
        Calendar cal = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
        cal.setTime(getLatestDate());
        cal.add(Calendar.DAY_OF_MONTH, -MAX_GAGEQC_DAYS);
        Date earliestDate = cal.getTime();

        return earliestDate;
    }

    public HRAPSubGrid getHRAPSubGrid() {
        if (subGrid == null) {
            try {
                subGrid = HRAP.getInstance().getHRAPSubGrid(getHRAPExtent());
            } catch (Exception e) {
                statusHandler.handle(Priority.PROBLEM, "Error reading ", e);
            }
        }
        return subGrid;
    }

    public Rectangle getHRAPExtent() {
        if (HRAPExtent == null) {
            try {
                HRAPExtent = HRAPCoordinates.getHRAPCoordinates();
            } catch (Exception e) {
                // TODO Auto-generated catch block
                e.printStackTrace();
            }
        }
        return HRAPExtent;
    }

    public List<MPEGageData> readGageData(Date startTime, Date endTime) {
        PrecipUtil pu = PrecipUtil.getInstance();

        /* read process PC token from .Apps_defaults */
        String processpc = appsDefaults.getToken("mpe_process_PC");

        boolean process_PC;
        if (processpc.equalsIgnoreCase("OFF")) {
            process_PC = false;
            System.out.println("Process PC Data = OFF ");
        } else {
            process_PC = true;
            System.out.println("Process PC Data = ON ");
        }

        // Read all records from the HourlyPC and HourlyPP tables for this
        // datetime.
        int[] pc_record_cnt = new int[] { 0 };
        ArrayList<Hourlypc> hourlyPCList = new ArrayList<Hourlypc>();
        if (process_PC == true) {
            hourlyPCList = pu.load_PC_hourly(startTime, endTime, null, null);
        } else {
            System.out.println("Processing of PC data turned off ");
        }
        pc_record_cnt[0] = hourlyPCList.size();

        int[] pp_record_cnt = new int[] { 0 };
        ArrayList<Hourlypp> hourlyPPList = pu.load_PP_hourly(startTime,
                endTime, null, null);
        pp_record_cnt[0] = hourlyPPList.size();

        // This is a PP/PC record count. It is not an actual count of stations
        // to process gage data for.
        int proc_count = pc_record_cnt[0] + pp_record_cnt[0];

        // read all records from PseudoGageVal table for this datetime
        String where = " WHERE obstime='" + sdf.format(endTime) + "' ";

        List<Pseudogageval> pseudoList = getPseudoGageVal(where);
        int pseudoCount = pseudoList.size();

        int ngages = proc_count + pseudoCount;

        badGages = readBadGageList();

        boolean bad_gages_exist = badGages.size() > 0;

        // TODO: implement this function
        // get_snow_polygons (&PolyList, date_st3.cdate);

        /* read through gage data sets to store data in the gage structure */
        ArrayList<MPEGageData> gageData = new ArrayList<MPEGageData>(ngages);

        int[] pHourlyPPIdx = new int[] { 0 };
        int[] pHourlyPCIdx = new int[] { 0 };

        // process the regular gage values first
        while ((pHourlyPPIdx[0] < hourlyPPList.size())
                || (pHourlyPCIdx[0] < hourlyPCList.size())) {
            total_precip total_precip = pu.get_total_hourly_precip(
                    hourlyPCList, pHourlyPCIdx, hourlyPPList, pHourlyPPIdx,
                    endTime, 1, 0f, PrecipUtil.PRECIP_TS_RANK
                            | PrecipUtil.PRECIP_PP, true, pc_record_cnt,
                    pp_record_cnt);

            /* Retrieve the Latitude/Longitude of the station. */
            Coordinate latLon = get_mpe_loc_latlon(total_precip.lid);

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
                        "error computing hrap coordinate", e);
            }

            /* make sure the gage is within the area. if so load the info */
            MPEGageData gage = new MPEGageData();
            gage.id = total_precip.lid;
            gage.pe = total_precip.PE;
            gage.ts = total_precip.TS;
            gage.gval = total_precip.value;

            /* Test if the gage is in the bad gage list */
            if (bad_gages_exist) {
                int pBadTest = Collections.binarySearch(badGages,
                        total_precip.lid);

                if (pBadTest >= 0) {
                    // logMessage ( "Gage %s is in bad gage file. Skipped
                    // ...\n",
                    // total_precip.lid );
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
            gage.hrap_loc = new Coordinate(hrap_point.x
                    - getHRAPSubGrid().getExtent().x, hrap_point.y
                    - getHRAPSubGrid().getExtent().y);

            gage.manedit = total_precip.qc == 'M';

            gage.td = total_precip.qc == 'D';

            if (total_precip.qc == 'L') {
                gage.qc = 2;
            }

            if (total_precip.qc == 'C') {
                gage.qc = 1;
            }

            gage.reported_missing = total_precip.reported_missing;

            gage.is_bad = false;

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
                            "error computing hrap coordinate", e);
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
                gage.hrap_loc = new Coordinate(hrap_point.x
                        - getHRAPSubGrid().getExtent().x, hrap_point.y
                        - getHRAPSubGrid().getExtent().y);

                gage.hrap_loc = hrap_point;

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
            if (gage.gval == PrecipUtil.MISSING_PRECIP) {
                /*
                 * If the value is missing, set the gage value to the MPE
                 * missing representation.
                 */
                gage.gval = -999f;
            }

            gage.rid = "ZZZ";
            gage.xmrg_val = -999f;
            gage.mval = -999f;
            gage.rval = -999f;
            gage.edit = "";
            gage.bval = -999f;
            gage.loc_val = -999f;
            gage.gage_only = -999f;
            gage.use_in_p3 = true;
        }

        return gageData;
    }

    public List<MPEGageData> readGageData(Date endTime) {
        Calendar cal = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
        cal.setTime(endTime);
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

    public ArrayList<String> readBadGages() {
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

    public void removeBadGage(String gageId) {
        if (badGages.contains(gageId)) {
            badGages.remove(gageId);
        }
    }

    /**
     * @return
     */
    public ArrayList<String> readBadGageList() {
        badGages = new ArrayList<String>();

        String dirname = appsDefaults.getToken("mpe_bad_gages_dir");
        File file = new File(FileUtil.join(dirname, "mpe_bad_gage_list"));

        if (file.exists()) {
            BufferedReader badGageFile = null;
            try {
                badGageFile = new BufferedReader(new FileReader(file));

                while (badGageFile.ready()) {
                    badGages.add(badGageFile.readLine());
                }

            } catch (IOException e) {
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                        e);
            } finally {
                try {
                    if (badGageFile != null) {
                        badGageFile.close();
                    }
                } catch (IOException e) {
                    // TODO Auto-generated catch block
                    e.printStackTrace();
                }
            }
            Collections.sort(badGages);
        }
        return badGages;
    }

    public void writeBadGageList() {
        String dirname = appsDefaults.getToken("mpe_bad_gages_dir");
        File file = new File(FileUtil.join(dirname, "mpe_bad_gage_list"));
        file.setReadable(true, false);
        file.setWritable(true, false);
        Collections.sort(badGages);

        if (file != null) {
            BufferedWriter badGageFile = null;
            try {
                badGageFile = new BufferedWriter(new FileWriter(file));

                for (int i = 0; i < badGages.size(); i++) {
                    badGageFile.write(badGages.get(i));
                    badGageFile.newLine();
                }
                badGageFile.flush();
                badGageFile.close();

            } catch (IOException e) {
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                        e);
            } finally {
                try {
                    if (badGageFile != null) {
                        badGageFile.close();
                    }
                } catch (IOException e) {
                    // TODO Auto-generated catch block
                    e.printStackTrace();
                }
            }
        }
    }

    public Coordinate get_mpe_loc_latlon(String lid) {
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

    public boolean readStationFile() {
        boolean status = false;
        String stationDir = appsDefaults.getToken("rfcwide_gageloc_dir");
        String stationPathname = FileUtil
                .join(stationDir, "mpe_gage_locations");

        BufferedReader in = null;
        try {
            in = new BufferedReader(new FileReader(stationPathname));

            int stationCount = Integer.parseInt(in.readLine());

            locationMap = new HashMap<String, Coordinate>(stationCount);

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
            statusHandler.handle(Priority.PROBLEM,
                    "MPE Station File not found. ", e);
            return status;
        } catch (IOException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Could not open/read MPE Station file. ", e);
            return status;
        } catch (NumberFormatException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
            return status;
        } finally {
            try {
                if (in != null) {
                    in.close();
                }
            } catch (IOException e) {
                // TODO Auto-generated catch block
                e.printStackTrace();
            }
        }
        return status = true;
    }

    public ArrayList<Rawpp> getRawPP(String where) {
        StringBuilder query = new StringBuilder("FROM ");
        query.append(Rawpp.class.getName());
        query.append(" ");
        query.append(where);

        ArrayList<Rawpp> retVal = new ArrayList<Rawpp>();
        try {
            List<Object[]> results = DirectDbQuery.executeQuery(
                    query.toString(), HydroConstants.IHFS, QueryLanguage.HQL);

            retVal.ensureCapacity(results.size());
            for (Object[] item : results) {
                retVal.add((Rawpp) item[0]);
            }
        } catch (VizException e) {
            e.printStackTrace();
        }

        return retVal;
    }

    public void updateRawPP(String where) {
        StringBuilder query = new StringBuilder("UPDATE ");
        query.append(Rawpp.class.getAnnotation(Table.class).name());
        query.append(" ");
        query.append(where);

        try {
            DirectDbQuery.executeStatement(query.toString(),
                    HydroConstants.IHFS, QueryLanguage.SQL);
        } catch (VizException e) {
            e.printStackTrace();
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
            e.printStackTrace();
        }
    }

    public ArrayList<Pseudogageval> getPseudoGageVal(String where) {
        StringBuilder query = new StringBuilder("FROM ");
        query.append(Pseudogageval.class.getName());
        query.append(" ");
        query.append(where);

        ArrayList<Pseudogageval> retVal = new ArrayList<Pseudogageval>();
        try {
            List<Object[]> results = DirectDbQuery.executeQuery(
                    query.toString(), HydroConstants.IHFS, QueryLanguage.HQL);

            retVal.ensureCapacity(results.size());
            for (Object[] item : results) {
                retVal.add((Pseudogageval) item[0]);
            }
        } catch (VizException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }

        return retVal;
    }

    public void updatePseudoGageVal(String where) throws VizException {
        StringBuilder query = new StringBuilder("UPDATE ");
        query.append(Pseudogageval.class.getAnnotation(Table.class).name());
        query.append(" ");
        query.append(where);

        DirectDbQuery.executeStatement(query.toString(), HydroConstants.IHFS,
                QueryLanguage.SQL);
    }

    public void insertPseudoGageVal(String where) {
        StringBuilder query = new StringBuilder("INSERT INTO ");
        query.append(Pseudogageval.class.getAnnotation(Table.class).name());
        query.append(" ");
        query.append(where);

        try {
            DirectDbQuery.executeStatement(query.toString(),
                    HydroConstants.IHFS, QueryLanguage.SQL);
        } catch (VizException e) {
            e.printStackTrace();
        }
    }

    /**
     * @return the rFC
     */
    public String getRFC() {
        return RFC;
    }
}
