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
package com.raytheon.viz.hydro.stationreporting;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.TimeZone;

import com.raytheon.uf.common.time.SimulatedTime;
import com.raytheon.uf.viz.core.catalog.DirectDbQuery;
import com.raytheon.uf.viz.core.catalog.DirectDbQuery.QueryLanguage;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.hydro.stationreporting.StationReportingConstants.Duration;
import com.raytheon.viz.hydro.stationreporting.StationReportingConstants.ListType;
import com.raytheon.viz.hydro.stationreporting.StationReportingConstants.SortOrder;
import com.raytheon.viz.hydrocommon.HydroConstants;
import com.raytheon.viz.hydrocommon.HydroDataCache;

/**
 * StationReportingDataManager.java Aug 18, 2008 Used to query the various
 * datatables related to Station Reporting.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 18, 2008 		   Eric Babin  Initial Creation
 * 04Sept2008   1509       dhladky     abstraction.
 * 13Oct2008    1580       askripsky   Refactored
 * 21 Feb 2010  2915       mpduff      Fixed Time Zone problem.
 * Jul 21, 2015 4500       rjpeter     Use Number in blind cast.
 * </pre>
 * 
 * @author ebabin
 * @version 1.0
 */

public class StationReportingDataManager {

    private static final String TIMING_DATA_QUERY = "select dcp.lid,dcp.rptfreq,dcp.rptime,telem.rptfreq from dcp,telem where telem.lid=dcp.lid";

    private static StationReportingDataManager manager = null;

    private ArrayList<StationReportingData> stationData;

    private HashMap<String, StationReportingTimingData> stationTime;

    private final String LATEST_OBS_FOR_LID_QUERY = "select lid,pe,dur,ts,extremum,obstime,value,shef_Qual_Code,quality_Code,revision,product_Id,producttime,postingtime from latestobsvalue where lid = ':lid' order by obstime desc, pe desc,ts desc,dur desc,extremum desc";

    private String previousWhere = " ";

    private ArrayList<StationReportingData> rval = null;

    /**
     * Private constructor.
     */
    private StationReportingDataManager() {
        if (stationData == null) {
            stationData = new ArrayList<StationReportingData>();
        }
        previousWhere = "";
    }

    /**
     * Singleton pattern of data manager.
     * 
     * @return manager
     */
    public static synchronized StationReportingDataManager getInstance() {
        if (manager == null) {
            manager = new StationReportingDataManager();
        }
        return manager;
    }

    /**
     * Get the station list data sorted by lid.
     * 
     * @param resultCount
     * @param hours
     * @param duration
     * @param sortOrder
     * @param listType
     * @throws VizException
     */
    public ArrayList<StationReportingData> getStationReportingData(
            ListType listType, SortOrder sortOrder, Duration duration, int hours)
            throws VizException {
        ArrayList<StationReportingData> retVal = new ArrayList<StationReportingData>();

        retVal = loadStationData(listType, sortOrder, duration, hours);

        return retVal;
    }

    /**
     * Method to query the ihfs database for latest obs by LID. (Used when user
     * click on Top table, go see all obs for a particular LID)
     * 
     * @param lid
     *            The location id.
     * @return ArrayList<Latestobsvalue>
     */
    public ArrayList<StationReportingData> getLatestObsForLid(String lid) {
        ArrayList<StationReportingData> retVal = new ArrayList<StationReportingData>();

        List<Object[]> data;
        try {
            data = DirectDbQuery.executeQuery(
                    LATEST_OBS_FOR_LID_QUERY.replace(":lid", lid),
                    HydroConstants.IHFS, QueryLanguage.SQL);
            for (Object[] rowData : data) {
                retVal.add(convertObsToDataRecord(rowData));
            }
        } catch (VizException e) {
            e.printStackTrace();
        }

        return retVal;
    }

    public StationReportingTimingData getTimingData(String lid) {
        if (stationTime == null) {
            loadTimingData();

            if (stationTime != null) {
                return stationTime.get(lid);
            }
        }

        return null;
    }

    /**
     * Retrieves the data from the database
     * 
     * @throws VizException
     */
    private ArrayList<StationReportingData> loadStationData(
            StationReportingConstants.ListType listType,
            StationReportingConstants.SortOrder sortOrder,
            StationReportingConstants.Duration duration, int durationHours)
            throws VizException {
        HydroDataCache hydroCache = HydroDataCache.getInstance();
        SimpleDateFormat currentTimeFormat = new SimpleDateFormat(
                "yyyy-MM-dd HH:mm:ss");
        currentTimeFormat.setTimeZone(TimeZone.getTimeZone("GMT"));
        previousWhere = "";

        StringBuilder where = new StringBuilder();

        if (listType == ListType.ALL) {
            where.append(" ");
        } else if (listType == ListType.DURATION) {
            Calendar cal = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
            Date date = SimulatedTime.getSystemTime().getTime();
            cal.setTime(date);
            cal.add(Calendar.HOUR, (-1 * durationHours));
            String obsTimeString = StationReportingData.formatDBTimestamp(cal
                    .getTime());

            where.append(" WHERE lid NOT IN ( SELECT DISTINCT lid "
                    + " FROM latestobsvalue WHERE obstime > '" + obsTimeString
                    + "')");
        } else if (listType == ListType.NEVER) {
            where.append(" WHERE lid = 'X-N/A-X' "
                    + "UNION "
                    + "SELECT lid, "
                    + "  '', 0, '', '', "
                    + "  '1-1-1 00:00:00', "
                    + " 0.0, 0, '', 0,'', "
                    + "  '1-1-1 00:00:00', "
                    + "  '1-1-1 00:00:00' "
                    + "  FROM location "
                    + " WHERE lid NOT IN (SELECT DISTINCT lid FROM latestobsvalue) ");
        }

        /* define the sort order */
        if (sortOrder.equals(StationReportingConstants.SortOrder.LOCATION)) {
            where.append(" ORDER BY lid ASC, obstime DESC");
        } else {
            where.append(" ORDER BY obstime DESC");
        }

        if (!previousWhere.equals(where.toString())) {
            previousWhere = where.toString();
            rval = new ArrayList<StationReportingData>();

            // Verify data is empty
            if (stationData == null) {
                stationData = new ArrayList<StationReportingData>();
            } else {
                stationData.clear();
            }

            // lid character varying(8) NOT NULL,
            // pe character varying(2) NOT NULL,
            // dur smallint NOT NULL,
            // ts character varying(2) NOT NULL,
            // extremum character varying(1) NOT NULL,
            // obstime timestamp without time zone,
            // "value" double precision,
            // revision smallint,
            // shef_qual_code character varying(1),
            // quality_code integer,
            // product_id character varying(10),
            // producttime timestamp without time zone,
            // postingtime timestamp without time zone,
            String query = "SELECT lid, pe, dur, ts, extremum, obstime, value, "
                    + "revision, shef_qual_code, quality_code, product_id, producttime, "
                    + "postingtime from latestobsvalue " + where;
            List<Object[]> rs = DirectDbQuery.executeQuery(query,
                    HydroConstants.IHFS, QueryLanguage.SQL);

            List<String> lidList = new ArrayList<String>();

            if (rs != null) {
                for (Object[] oa : rs) {
                    /* See if this is a new lid */
                    String lid = (String) oa[0];
                    if (!lidList.contains(lid)) {
                        try {
                            StationReportingData srd = new StationReportingData(
                                    lid, hydroCache.getLocationMap().get(lid));
                            srd.setPe((String) oa[1]);
                            srd.setDur(((Number) oa[2]).shortValue());
                            srd.setTs((String) oa[3]);
                            srd.setExtremum((String) oa[4]);
                            srd.setObstime(currentTimeFormat
                                    .format((Date) oa[5]));
                            srd.setValue((Double) oa[6]);
                            srd.setRevision(((Number) oa[7]).shortValue());
                            srd.setShefQualCode((String) oa[8]);
                            srd.setQualityCode(((Number) oa[9]).intValue());
                            srd.setProductId((String) oa[10]);
                            srd.setProducttime(currentTimeFormat
                                    .format((Date) oa[11]));
                            srd.setPostingtime(currentTimeFormat
                                    .format((Date) oa[12]));

                            rval.add(srd);
                            lidList.add(lid);
                        } catch (java.text.ParseException e) {
                            // Just continue onto the next record
                            e.printStackTrace();
                        }
                    }
                }
            } else {
                return null;
            }
        }

        return rval;
    }

    /**
     * Converts the Object[] from a query to a StationReportingData object.
     * 
     * @param dataRow
     *            The database data to convert
     * @return The corresponding StationReportingData
     */
    private StationReportingData convertObsToDataRecord(Object[] dataRow) {
        StationReportingData retVal = new StationReportingData();

        retVal.setLid(dataRow[0].toString());

        retVal.setPe(dataRow[1].toString());

        if (dataRow[2] != null) {
            retVal.setDur(((Number) dataRow[2]).shortValue());
        }

        retVal.setTs(dataRow[3].toString());
        retVal.setExtremum(dataRow[4].toString());

        if (dataRow[6] != null) {
            retVal.setValue(Double.valueOf(dataRow[6].toString()));
        }

        retVal.setShefQualCode(dataRow[7].toString());

        if (dataRow[8] != null) {
            retVal.setQualityCode(((Number) dataRow[8]).intValue());
        }

        if (dataRow[9] != null) {
            retVal.setRevision(((Number) dataRow[9]).shortValue());
        }

        retVal.setProductId(dataRow[10].toString());
        try {
            retVal.setPostingtime(dataRow[12].toString());
            retVal.setProducttime(dataRow[11].toString());
            retVal.setObstime(dataRow[5].toString());
        } catch (Exception e) {

        }

        return retVal;
    }

    /**
     * Converts the Object[] from a query to a StationReportingData object.
     * 
     * @param dataRow
     *            The database data to convert
     * @return The corresponding StationReportingData
     */
    // private StationReportingData convertToDataRecord(Object[] dataRow) {
    // StationReportingData rval = new StationReportingData();
    //
    // rval.setLid(dataRow[StationReportingConstants.LID_INDEX].toString());
    //
    // if (dataRow[StationReportingConstants.NAME_INDEX] != null) {
    // rval.setName(dataRow[StationReportingConstants.NAME_INDEX]
    // .toString());
    // } else {
    // rval.setName("");
    // }
    //
    // rval.setPe(dataRow[StationReportingConstants.PE_INDEX].toString());
    // rval.setDur((Short) dataRow[StationReportingConstants.DUR_INDEX]);
    // rval.setTs(dataRow[StationReportingConstants.TS_INDEX].toString());
    // rval.setExtremum(dataRow[StationReportingConstants.EXTREMUM_INDEX]
    // .toString());
    // rval.setValue(Double
    // .valueOf(dataRow[StationReportingConstants.VALUE_INDEX]
    // .toString()));
    // rval
    // .setShefQualCode(dataRow[StationReportingConstants.SHEF_QUAL_CODE_INDEX]
    // .toString());
    // rval
    // .setQualityCode((Integer)
    // dataRow[StationReportingConstants.QUALITY_INDEX]);
    // rval
    // .setRevision((Short) dataRow[StationReportingConstants.REVISION_INDEX]);
    // rval.setProductId(dataRow[StationReportingConstants.PRODUCT_ID_INDEX]
    // .toString());
    // try {
    // rval
    // .setPostingtime(dataRow[StationReportingConstants.POSTING_TIME_INDEX]
    // .toString());
    // rval
    // .setProducttime(dataRow[StationReportingConstants.PRODUCT_TIME_INDEX]
    // .toString());
    // rval.setObstime(dataRow[StationReportingConstants.OBSTIME_INDEX]
    // .toString());
    // } catch (Exception e) {
    //
    // }
    //
    // return rval;
    // }
    /**
     * Retrieves the DCP and Telem timing data from the database
     */
    private void loadTimingData() {
        // Exec query
        try {
            ArrayList<Object[]> data = (ArrayList<Object[]>) DirectDbQuery
                    .executeQuery(TIMING_DATA_QUERY, HydroConstants.IHFS,
                            QueryLanguage.SQL);

            for (Object[] currData : data) {
                addTimeData(currData);
            }

        } catch (VizException e) {

        }
    }

    /**
     * Converts and stores DCP and Telem timing data to the station timing data.
     * 
     * @param data
     *            The timing data to convert and store
     */
    private void addTimeData(Object[] data) {
        // Pull data out
        String lid = getData(data[StationReportingConstants.TIME_LID_INDEX]);
        String dcpFreq = getData(data[StationReportingConstants.TIME_DCP_FREQUENCY_INDEX]);
        String dcpTime = getData(data[StationReportingConstants.TIME_DCP_TIME_INDEX]);
        String TelemFreq = getData(data[StationReportingConstants.TIME_TELEM_FREQUENCY_INDEX]);

        if (stationTime == null) {
            stationTime = new HashMap<String, StationReportingTimingData>();
        }

        if (!stationTime.containsKey(lid)) {
            stationTime.put(lid, new StationReportingTimingData(lid, dcpFreq,
                    dcpTime, TelemFreq));
        }

    }

    private String getData(Object data) {
        String rval = null;

        if (data != null) {
            rval = data.toString();
        }

        return rval;
    }
}
