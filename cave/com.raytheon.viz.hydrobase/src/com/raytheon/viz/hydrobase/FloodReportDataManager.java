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
package com.raytheon.viz.hydrobase;

import java.util.ArrayList;
import java.util.Date;
import java.util.Map;
import java.util.TreeMap;

import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.hydrocommon.HydroConstants;
import com.raytheon.viz.hydrocommon.HydroDataCache;
import com.raytheon.viz.hydrocommon.datamanager.HydroDataManager;

/**
 * Flood Report Dialog data manager.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 2, 2009  2259       mpduff      Initial creation
 * May 14, 2012 14965      wkwock      Fix crash in query for data
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class FloodReportDataManager extends HydroDataManager {
    private static FloodReportDataManager instance = null;

    private static final int INDEX_PT_A = 0;

    private static final int INDEX_PT_B = 1;

    private static final int INDEX_PT_C = 2;

    private static final int INDEX_PT_D = 3;

    private static final int INDEX_ANCHOR1 = 4;

    private static final int INDEX_ANCHOR2 = 5;

    private Map<String, FloodReportData> dataMap = new TreeMap<String, FloodReportData>();

    private Object[] lastCrest = null;

    /**
     * The selected location id.
     */
    private String selectedLid = null;
    
    /**
     * The selected key for the dataMap.
     */
    private String selectedKey = null;

    /**
     * The data start date
     */
    private Date startDate;

    /**
     * The data end date
     */
    private Date endDate;

    private boolean drawGraph = false;

    /**
     * Private constructor for Singleton.
     */
    private FloodReportDataManager() {

    }

    /**
     * Get an instance of the data manager class.
     * 
     * @return An instance of FloodReportDataManager
     */
    public static synchronized FloodReportDataManager getInstance() {
        if (instance == null) {
            instance = new FloodReportDataManager();
        }

        return instance;
    }

    /**
     * Get a list of HSA's out of the HSA table.
     * 
     * @return ArrayList of HSA's, or null if no data
     */
    public ArrayList<String> getHsaList() {
        ArrayList<String> hsaList = new ArrayList<String>();
        String query = "select hsa from hsa order by hsa";

        ArrayList<Object[]> rs = runQuery(query);

        if (rs.size() > 0) {
            for (Object[] oa : rs) {
                hsaList.add((String) oa[0]);
            }
        }

        return hsaList;
    }

    /**
     * Get a list of lids from the floodts table.
     * 
     * @param where
     *            The where clause to constrain the query
     * @return ArrayList<String> of location ids
     */
    public ArrayList<String> getLidList(String where) {
        ArrayList<String> lidList = new ArrayList<String>();
        String query = "select distinct lid from floodts ";

        ArrayList<Object[]> rs = runQuery(query + where);

        if (rs.size() > 0) {
            for (Object[] oa : rs) {
                lidList.add((String) oa[0]);
            }
        }

        return lidList;
    }

    /**
     * Get Flood Report data.
     * 
     * @param lid
     *            The location id
     * @return FloodReportData object populated with data, null if problem or no
     *         data
     */
    public ArrayList<FloodReportData> getFloodRptData(String lid, String start, String end) {
        ArrayList<FloodReportData> dataList = new ArrayList<FloodReportData>();
        String lname = HydroDataCache.getInstance().getLocationMap().get(lid);
        double fs = getFloodStage(lid);
        int[] fldEventIds = getFloodEventIds(lid, start, end);

        for (int i = 0; i < fldEventIds.length; i++) {
            Object[] crestData = getCrest(lid, fldEventIds[i]);
            FloodReportData data = new FloodReportData();
            if ((crestData == null) || (crestData.length == 0)) {
                return null;
            }

            data.setCrest((Double) crestData[0]);
            data.setCrestDate(((Date) crestData[1]));
            data.setFloodEventId(fldEventIds[i]);
            data.setFloodStage(fs);
            data.setLid(lid);
            data.setLongName(lname);
            if (lastCrest != null) {
                data.setLastCrest((Double) lastCrest[0]);
                data.setLastCrestDate((Date) lastCrest[1]);
            }
            
            dataList.add(data);
        }
        
        return dataList;
    }

    /**
     * Get the flood stage for the site.
     * 
     * @param lid
     *            The site
     * @return The flood stage for the site
     */
    public double getFloodStage(String lid) {
        double fs = -999;

        ArrayList<Object[]> rs = runQuery("select fs from riverstat where lid = '"
                + lid + "'");
        if ((rs != null) && (rs.size() > 0) && rs.get(0)[0]!=null) {
            fs = (Double) rs.get(0)[0];
        }

        return fs;
    }

    /**
     * Get the river basin.
     * 
     * @param lid
     *            The location id
     * @return The name of the river basin the gage is in
     */
    public String getRiverBasin(String lid) {
        String basin = null;

        ArrayList<Object[]> rs = runQuery("select rb from location where lid = '"
                + lid + "'");
        if (rs != null) {
            basin = (String) rs.get(0)[0];
        }

        return basin;
    }

    /**
     * Get the state.
     * 
     * @param lid
     *            The location id
     * @return The state the gage is in
     */
    public String getState(String lid) {
        String state = null;

        ArrayList<Object[]> rs = runQuery("select state from location where lid = '"
                + lid + "'");
        if (rs != null) {
            state = (String) rs.get(0)[0];
        }

        return state;
    }

    /**
     * Get the river name.
     * 
     * @param lid
     *            The location id
     * @return The name of the river the gage is on
     */
    public String getRiver(String lid) {
        String river = null;

        ArrayList<Object[]> rs = runQuery("select stream from riverstat where lid = '"
                + lid + "'");
        if (rs != null) {
            river = (String) rs.get(0)[0];
        }

        return river;
    }

    /**
     * Get the flood event id.
     * 
     * @param lid
     *            The lid to search on
     * @return The ids for the lid
     */
    public int[] getFloodEventIds(String lid, String start, String end) {
        int[] id = null;

        ArrayList<Object[]> rs = runQuery("select distinct flood_event_id from floodts where lid = '"
                + lid + "' and obstime >= '" + start + "' and obstime <= '" + end + "'");
        
        if ((rs != null) && (rs.size() > 0)) {
            id = new int[rs.size()];
            for (int i = 0; i < rs.size(); i++) {
                Object[] oa = rs.get(i);
                id[i] = (Integer) oa[0];
            }
        }

        return id;
    }

    /**
     * Get the crest and date/time of the crest.
     * 
     * @param lid
     *            The location id
     * @param id
     *            The flood event id
     * @return Object[] where element 0 = crest value (double) and element 1 =
     *         date/time (Date)
     */
    public Object[] getCrest(String lid, int id) {
        String query = "Select value, obstime from floodts where lid = '" + lid
                + "' and flood_event_id = " + id + " order by obstime";

        ArrayList<Object[]> rs = runQuery(query);
        lastCrest = new Object[rs.get(0).length];
        Object[] retVal = new Object[2];
        if ((rs != null) && (rs.size() > 0)) {
            System.arraycopy(rs.get(0), 0, lastCrest, 0, rs.get(0).length);
            int numberSinceCrest = 0;

            // The crest value
            double crestVal = HydroConstants.FLOOD_REPORT_MSG;

            for (Object[] oa : rs) {
                if (oa != null) {
                    /*
                     * if a crest is already defined and the value immediately
                     * after the crest is identical, consider it a sustained
                     * crest
                     */
                    double crest = (Double) oa[0];

                    if ((crestVal != HydroConstants.FLOOD_REPORT_MSG)
                            && (crest == crestVal) && (numberSinceCrest == 0)) {
                        System.arraycopy(oa, 0, lastCrest, 0, oa.length);
                    } else if ((crest > crestVal)
                            || (crestVal == HydroConstants.FLOOD_REPORT_MSG)) {
                        /*
                         * check for a higher crest. if new crest found, then
                         * clear out the sustained crest
                         */
                        crestVal = (Double) oa[0];
                        lastCrest[0] = HydroConstants.FLOOD_REPORT_MSG;
                        numberSinceCrest = 0;
                        System.arraycopy(oa, 0, retVal, 0, oa.length);
                    } else {
                        numberSinceCrest++;
                    }
                }
            }
        }

        return retVal;
    }

    /**
     * Get the value and obstime from the floodts table.
     * 
     * @param lid
     *            The lid to query on
     * @param eventId
     *            The event id to query on
     * @return ArrayList<Object[]> of {value, obstime}
     */
    public ArrayList<Object[]> getFloodEventData(String lid, int eventId) {
        String query = "Select value, obstime from floodts where lid = '" + lid
                + "' and flood_event_id = " + eventId + " order by obstime";

        ArrayList<Object[]> rs = runQuery(query);

        return rs;
    }

    /**
     * Get the times the floodstage was crossed.
     * 
     * @param data
     *            The FloodReportData object
     * @param fs
     *            The flood stage
     * @return String[] { "time above flood stage", "time below flood stage" }
     */
    public Date[] getPassthruTimes(FloodReportData data) {
        if (data == null) {
            return null;
        }

        double floodStage = data.getFloodStage();
        Date[] times = new Date[2];

        int index = 0;
        boolean[] ptsMissing = { true, true, true, true, true, true };

        FloodReportData ptA = new FloodReportData();
        FloodReportData ptB = new FloodReportData();
        FloodReportData ptC = new FloodReportData();
        FloodReportData ptD = new FloodReportData();
        FloodReportData anchor1 = new FloodReportData();
        FloodReportData anchor2 = new FloodReportData();
        boolean findFirstAnchor = true;
        boolean findLastAnchor = true;

        /* Get the data to search */
        ArrayList<Object[]> rs = FloodReportDataManager.getInstance()
                .getFloodEventData(data.getLid(), data.getFloodEventId());

        /* get first Anchor point */
        for (int i = 0; i < rs.size(); i++) {
            index = i;
            if (findFirstAnchor == false) {
                // Break out of for loop
                break;
            }
            Object[] oa = rs.get(i);
            if ((Double) oa[0] < floodStage) {
                ptA.setCrest((Double) oa[0]);
                ptA.setCrestDate((Date) oa[1]);
                ptsMissing[INDEX_PT_A] = false;

                if (i + 1 < rs.size()) {
                    Object[] next = rs.get(i + 1);
                    if ((Double) next[0] >= floodStage) {
                        ptB.setCrest((Double) next[0]);
                        ptB.setCrestDate((Date) next[1]);
                        ptsMissing[INDEX_PT_B] = false;
                        findFirstAnchor = false;
                    }
                }
            } else { /* missing ptA */
                ptB.setCrest((Double) oa[0]);
                ptB.setCrestDate((Date) oa[1]);
                ptsMissing[INDEX_PT_B] = false;
                findFirstAnchor = false;
            }
        }

        /* interpolate to find time of progression above flood stage */

        if ((ptsMissing[INDEX_PT_A] == false)
                && (ptsMissing[INDEX_PT_B] == false)) {

            anchor1 = FloodReportUtils
                    .floodreptInterp(ptA, ptB, floodStage);
            ptsMissing[INDEX_ANCHOR1] = false;
        } else {
            if (ptA.getCrest() == floodStage) {
                anchor1 = FloodReportUtils.copyFloodReportData(ptA);
                ptsMissing[INDEX_ANCHOR1] = false;
            } else if (ptB.getCrest() == floodStage) {
                anchor1 = FloodReportUtils.copyFloodReportData(ptB);
                ptsMissing[INDEX_ANCHOR1] = false;
            }
        }

        /*
         * get last Anchor point, use index to continue loop where we left
         * off
         */
        for (int i = index; i < rs.size(); i++) {
            if (findLastAnchor == false) {
                // Break out of for loop
                break;
            }
            Object[] oa = rs.get(i);
            if ((Double) oa[0] >= floodStage) {
                Object[] next;
                if (i == rs.size() - 1) {
                    next = null;
                } else {
                    next = rs.get(i + 1);
                }

                if (next != null) {
                    if ((Double) next[0] < floodStage) {
                        ptC.setCrest((Double) oa[0]);
                        ptC.setCrestDate((Date) oa[1]);
                        ptsMissing[INDEX_PT_C] = false;

                        ptD.setCrest((Double) next[0]);
                        ptD.setCrestDate((Date) next[1]);
                        ptsMissing[INDEX_PT_D] = false;

                        findLastAnchor = false;
                    }
                } else {
                    ptC.setCrest((Double) oa[0]);
                    ptC.setCrestDate((Date) oa[1]);
                    ptsMissing[INDEX_PT_C] = false;

                    findLastAnchor = false;
                }
            } else { /* point C was skipped or MSG. */
                /* ptB is MSG or above fs */
                ptC = FloodReportUtils.copyFloodReportData(ptB);
                ptsMissing[INDEX_PT_C] = false;

                ptD.setCrest((Double) oa[0]);
                ptD.setCrestDate((Date) oa[1]);
                ptsMissing[INDEX_PT_D] = false;

                findLastAnchor = false;
            }
        }

        /* interpolate to find time of recession below flood stage */
        if ((ptsMissing[INDEX_PT_C] == false)
                && (ptsMissing[INDEX_PT_D] == false)) {
            anchor2 = FloodReportUtils
                    .floodreptInterp(ptC, ptD, floodStage);
            ptsMissing[INDEX_ANCHOR2] = false;
        } else {
            if (ptC.getCrest() == floodStage) {
                anchor2 = FloodReportUtils.copyFloodReportData(ptC);
                ptsMissing[INDEX_ANCHOR2] = false;
            } else if (ptD.getCrest() == floodStage) {
                anchor2 = FloodReportUtils.copyFloodReportData(ptD);
                ptsMissing[INDEX_ANCHOR2] = false;
            }
        }

        if (anchor1.getCrest() != HydroConstants.FLOOD_REPORT_MSG) {
            times[0] = anchor1.getCrestDate();
        }

        if (anchor2.getCrest() != HydroConstants.FLOOD_REPORT_MSG) {
            times[1] = anchor2.getCrestDate();
        }

        return times;
    }

    /**
     * Delete a flood event.
     * 
     * @param where
     *            Where clause
     * @return 1 if successful, -1 if unsuccessful
     */
    public int deleteFloodEvent(String where) {
        int status = 1;

        String query = "delete from floodts";
        try {
            runStatement(query + where);
        } catch (VizException e) {
            e.printStackTrace();
            status = -1;
        }

        return status;
    }

    /**
     * Insert a new Crest record.
     * 
     * @param query
     *            The query
     * @return 1 if successful, -1 if unsuccessful, -2 if record already exists
     */
    public int insertCrest(String query) {
        int status = 1;

        try {
            runStatement(query);
        } catch (VizException e) {
            if (e.getCause().toString().indexOf("violates unique constraint") > 0) {
                status = -2;
            } else {
                status = -1;
            }
        }

        return status;
    }

    /**
     * Set the dataMap.
     * 
     * @param dataMap
     *            The dataMap to set
     */
    public void setReportData(Map<String, FloodReportData> dataMap) {
        this.dataMap = dataMap;
    }

    /**
     * Return the dataMap holding the crest data.
     * 
     * @return The dataMap
     */
    public Map<String, FloodReportData> getReportData() {
        return dataMap;
    }

    /**
     * @return the selectedLid
     */
    public String getSelectedLid() {
        return selectedLid;
    }

    /**
     * @param selectedLid
     *            the selectedLid to set
     */
    public void setSelectedLid(String selectedLid) {
        this.selectedLid = selectedLid;
    }

    /**
     * @return the startDate
     */
    public Date getStartDate() {
        return startDate;
    }

    /**
     * @param startDate
     *            the startDate to set
     */
    public void setStartDate(Date startDate) {
        this.startDate = startDate;
    }

    /**
     * @return the endDate
     */
    public Date getEndDate() {
        return endDate;
    }

    /**
     * @param endDate
     *            the endDate to set
     */
    public void setEndDate(Date endDate) {
        this.endDate = endDate;
    }

    /**
     * @return the drawGraph
     */
    public boolean isDrawGraph() {
        return drawGraph;
    }

    /**
     * @param drawGraph
     *            the drawGraph to set
     */
    public void setDrawGraph(boolean drawGraph) {
        this.drawGraph = drawGraph;
    }

    /**
     * @return the selectedKey
     */
    public String getSelectedKey() {
        return selectedKey;
    }

    /**
     * @param selectedKey the selectedKey to set
     */
    public void setSelectedKey(String selectedKey) {
        this.selectedKey = selectedKey;
    }
}
