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

import java.sql.Timestamp;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.TimeZone;

import com.raytheon.uf.common.dataplugin.shef.tables.Riverstatus;
import com.raytheon.uf.common.dataplugin.shef.tables.Statprof;
import com.raytheon.uf.common.dataplugin.shef.tables.StatprofId;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.SimulatedTime;
import com.raytheon.uf.viz.core.catalog.DirectDbQuery;
import com.raytheon.uf.viz.core.catalog.DirectDbQuery.QueryLanguage;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.hydrocommon.HydroConstants;
import com.raytheon.viz.hydrocommon.cresthistory.CrestData;
import com.raytheon.viz.hydrocommon.cresthistory.CrestHistoryData;
import com.raytheon.viz.hydrocommon.data.RiverDataPoint;

/**
 * Class for managing database query calls. RiverDataManager.java
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 03Sept2008   #1509      dhladky     Initial Creation
 * 03 Aug 2010  #4383      lbousaidi   get the stage crest date 
 * 									   for staff gage Window
 * 04 Nov 2010  #5518	   lbousaidi    added flag for All/Above/Bellow 
 * 									   Action Stage
 * 14 feb 2011  #4383      lbousaidi   changed getRiverDataPoint: added crestQuery, 
 * 									   set riverName using locationquery instead of 
 * 									   riverInfoQuery, added rb field in locationQuery
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */

public class RiverDataManager {

    /** Singleton instance of this class */
    private static RiverDataManager riverManager = null;

    /* Private Constructor */
    private RiverDataManager() {
    }

    /**
     * Get an instance of this singleton.
     * 
     * @return Instance of this class
     */
    public static synchronized RiverDataManager getInstance() {
        if (riverManager == null) {
            riverManager = new RiverDataManager();
        }
        return riverManager;
    }

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(RiverDataManager.class);

    private LinkedHashMap<String, CrestHistoryData> crestData = null;

    private LinkedHashMap<String, LinkedHashMap<String, RiverDataPoint>> riverData = null;

    private static String locationQuery = "SELECT name, county, state, elev, hsa, lat, lon, rb from location WHERE lid =";

    private static String riverStatQuery = "SELECT stream, mile, zd, tide, bf, wstg, fs, fq, action_flow, primary_pe FROM riverstat WHERE lid =";

    private static String descriptionQuery = "SELECT proximity, reach FROM descrip WHERE lid =";

    private static String crestQuery = "select stage, q, datcrst, timcrst, cremark, hw, jam, olddatum, suppress, prelim from crest where lid = '";

    private static String floodCatQuery = "SELECT minor_stage, moderate_stage, major_stage, minor_flow, moderate_flow, major_flow from floodcat WHERE lid = ";

    private static String riverInfoQuery = "SELECT rivermonlocation.group_id, rivermongroup.group_name FROM rivermongroup, rivermonlocation WHERE rivermongroup.group_id = rivermonlocation.group_id AND rivermonlocation.lid = ";

    private static String riverPointQuery = "SELECT l.lid, l.name, l.county, l.state, l.elev, l.hsa, l.lat, l.lon, "
            + "rml.group_id, "
            + "rmg.group_name, "
            + "r.stream, r.mile, r.zd AS zero, r.tide, r.bf AS bankfull, r.wstg AS action_stage, r.fs AS flood_stage, r.fq AS flood_flow, r.action_flow, r.primary_pe, "
            + "d.proximity, d.reach, "
            + "f.minor_stage AS minor, f.moderate_stage AS moderate, f.major_stage AS major, f.minor_flow AS minor, f.moderate_flow AS moderate, f.major_flow AS major "
            + "FROM location l "
            + "LEFT JOIN floodcat f ON l.lid::text = f.lid::text "
            + "LEFT JOIN descrip d ON l.lid::text = d.lid::text, riverstat r, rivermonlocation rml "
            + "LEFT JOIN rivermongroup rmg ON rml.group_id::text = rmg.group_id::text "
            + "WHERE l.lid::text = r.lid::text and r.lid::text = rml.lid::text "
            + "ORDER BY rml.group_id, r.mile desc";

    private static String riverObsvalueQueryFront = "SELECT distinct(foo.lid), foo.value, foo.maxtime from "
            + "(SELECT distinct height.lid, height.value, max(height.obstime) as maxtime "
            + "FROM height "
            + "WHERE height.lid in "
            + "(select distinct(lid) "
            + "from rivermonlocation "
            + "where rivermonlocation.group_id = ";

    private static String riverObsvalueQueryBack = ") GROUP BY height.lid, height.value) "
            + "AS foo "
            + "GROUP BY foo.lid, foo.value, foo.maxtime order by foo.lid, foo.maxtime desc";

    private static String riverFcstvalueQueryFront = "SELECT distinct(foo.lid), foo.value, foo.maxtime from "
            + "(SELECT distinct fcstheight.lid, fcstheight.value, max(fcstheight.validtime) as maxtime "
            + "FROM fcstheight "
            + "WHERE fcstheight.lid in "
            + "(select distinct(lid) "
            + "from rivermonlocation "
            + "where rivermonlocation.group_id = ";

    private static String riverFcstvalueQueryBack = ") GROUP BY fcstheight.lid, fcstheight.value) "
            + "AS foo "
            + "GROUP BY foo.lid, foo.value, foo.maxtime order by foo.lid, foo.maxtime desc";

    private static String riverIDQuery = "SELECT group_id FROM rivermonlocation where lid =";

    /**
     * River Query Crest
     * 
     * @param lid
     * @return CrestHistoryData
     */
    public CrestHistoryData getRiverCrests(RiverDataPoint rdp, int allFlag) {
        CrestHistoryData crests = null;

        if (crestData == null) {
            crestData = new LinkedHashMap<String, CrestHistoryData>();
        }

        crests = new CrestHistoryData();
        String query = null;

        /* get crest data depending on action stage */
        if (rdp != null) {
            String sql = "select stage, q, datcrst, timcrst, cremark, hw, jam, olddatum, suppress, prelim "
                    + "from crest where lid = '" + rdp.getLid();

            if (allFlag == 1) {
                query = sql + "' and stage >'" + rdp.getActionStage()
                        + "' ORDER BY stage, q";
            } else if (allFlag == 2) {
                query = sql + "' and stage <'" + rdp.getActionStage()
                        + "' ORDER BY stage, q";
            } else {
                query = sql + "' ORDER BY stage, q";
            }

            ArrayList<Object[]> objects = null;

            try {
                objects = (ArrayList<Object[]>) DirectDbQuery.executeQuery(
                        query, HydroConstants.IHFS, QueryLanguage.SQL);
                if (objects != null) {
                    for (Object[] crestob : objects) {
                        crests.addCrestData(new CrestData(crestob));
                    }
                }
            } catch (Exception e) {
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                        e);
            }
        }

        if (crests.getCrestDataArray().size() > 0) {
            // max stage level, default sort mode
            // There is a problem when the stage is not set in the db.
            // Added this work around here to manually find the max stage value
            double maxStag = 0;
            for (int i = 0; i < crests.getCrestDataArray().size(); i++) {
                if (crests.getCrestDataArray().get(i).getStage() > maxStag) {
                    maxStag = crests.getCrestDataArray().get(i).getStage();
                }
            }
            crests.setMaxStageLevel(maxStag);

            // set the values to gage by
            crests.setActionLevel(rdp.getActionStage());
            crests.setMajorLevel(rdp.getMajorStage());
            crests.setMinorLevel(rdp.getMinorStage());
            crests.setModerateLevel(rdp.getModerateStage());

            // we assume a top of zero at all times, find oldest record
            crests.sortByDate();
            CrestData cd5 = crests.getCrestDataArray().get(
                    crests.getCrestDataArray().size() - 1);

            Calendar cal = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
            Date now = SimulatedTime.getSystemTime().getTime();
            cal.setTime(now);

            if (cd5.getCrestDate() != null) {
                crests.setStartingYear(cd5.getCrestDate().get(Calendar.YEAR));
            } else {
                crests.setStartingYear(HydroConstants.DEFAULT_YEAR);
            }
            crests.setEndingYear(cal.get(Calendar.YEAR));

            crests.sortByStage();

            crestData.put(rdp.getLid(), crests);
        }

        return crests;
    }

    /**
     * Gets the crest data for just the record flood, flow
     * 
     * @param rdp
     * @return
     */
    public RiverDataPoint getRiverCrest(RiverDataPoint rdp) {
        Date date = SimulatedTime.getSystemTime().getTime();
        int allFlag = 0;
        CrestHistoryData crests = getRiverCrests(rdp, allFlag);
        ArrayList<CrestData> temp = crests.getCrestDataArray();
        if (temp.size() > 0) {
            // maximum stage value
            crests.sortByStage();
            CrestData cd = temp.get(0);
            rdp.setCrestValue(cd.getStage());

            // maximum flow (q) value for max stage value
            rdp.setCrestFlow(cd.getFlow());

            // Stage Crest date
            rdp.setCrestTime(cd.getCrestDate());

        } else {
            // maximum stage value
            rdp.setCrestValue(HydroConstants.MISSING_VALUE);

            // maximum flow value
            rdp.setCrestFlow(HydroConstants.MISSING_VALUE);

            // sort by date
            Calendar cal = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
            cal.setTime(date);
            rdp.setCrestTime(cal);
        }

        return rdp;
    }

    /**
     * data structure to house the river water shed data
     * 
     * @return HashMap<String, LinkedHashMap<String, RiverDataPoint>>
     */
    public LinkedHashMap<String, LinkedHashMap<String, RiverDataPoint>> getRiverSummaryData() {
        ArrayList<Object[]> data = null;

        try {
            data = (ArrayList<Object[]>) DirectDbQuery.executeQuery(
                    riverPointQuery, HydroConstants.IHFS, QueryLanguage.SQL);
            if (data != null) {
                riverData = new LinkedHashMap<String, LinkedHashMap<String, RiverDataPoint>>();
                String riverID = null;
                LinkedHashMap<String, RiverDataPoint> riverPoints = null;
                for (Object[] point : data) {
                    RiverDataPoint rdp = new RiverDataPoint(point);
                    // start
                    if (riverID == null) {
                        riverID = rdp.getRiverID();
                        riverPoints = new LinkedHashMap<String, RiverDataPoint>();
                        riverPoints.put(rdp.getLid(), rdp);
                    }
                    // new river switch
                    else if (!rdp.getRiverID().equals(riverID)) {
                        // sock away the old one
                        riverData.put(riverID, riverPoints);
                        // rename Name
                        riverID = rdp.getRiverID();
                        // create new
                        riverPoints = new LinkedHashMap<String, RiverDataPoint>();
                        riverPoints.put(rdp.getLid(), rdp);
                    }
                    // in river run
                    else {
                        riverPoints.put(rdp.getLid(), rdp);
                    }
                }
                // take care of last river
                riverData.put(riverID, riverPoints);
            }
        } catch (VizException e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                    e);
        }

        return riverData;
    }

    /**
     * Populate the Data for time and point obs and fcst, really not pretty
     * avert your eyes.
     * 
     * @param riverPoints
     * @param riverID
     * @return HashMap<String, RiverDataPoint>
     */
    public LinkedHashMap<String, RiverDataPoint> populateRiverData(
            String riverID, LinkedHashMap<String, RiverDataPoint> riverPoints) {

        ArrayList<Object[]> obsdata = null;
        ArrayList<Object[]> fcstdata = null;

        try {
            String obsquery = riverObsvalueQueryFront + "'" + riverID + "'"
                    + riverObsvalueQueryBack;
            String fcstquery = riverFcstvalueQueryFront + "'" + riverID + "'"
                    + riverFcstvalueQueryBack;
            obsdata = (ArrayList<Object[]>) DirectDbQuery.executeQuery(
                    obsquery, HydroConstants.IHFS, QueryLanguage.SQL);
            fcstdata = (ArrayList<Object[]>) DirectDbQuery.executeQuery(
                    fcstquery, HydroConstants.IHFS, QueryLanguage.SQL);
            if (obsdata != null) {
                String lid = null;
                Calendar obstime = null;

                for (Object[] obspoint : obsdata) {
                    if (obspoint[0] != null) {
                        String newlid = (String) obspoint[0];
                        // into
                        if (!newlid.equals(lid) || (lid == null)) {
                            lid = newlid;
                            // we care about this data
                            if (obspoint[1] != null) {
                                riverPoints.get(lid).setObsValue(
                                        (Double) obspoint[1]);
                            }
                            if (obspoint[2] != null) {
                                obstime = Calendar.getInstance(TimeZone
                                        .getTimeZone("GMT"));
                                obstime.setTimeInMillis(((Timestamp) obspoint[2])
                                        .getTime());
                                riverPoints.get(lid).setObsTime(obstime);
                            }
                        }
                    }
                }
            }
            if (fcstdata != null) {
                String lid = null;
                Calendar fcsttime = null;

                for (Object[] fcstpoint : fcstdata) {
                    if (fcstpoint[0] != null) {
                        String newlid = (String) fcstpoint[0];
                        // into
                        if (!newlid.equals(lid) || (lid == null)) {
                            lid = newlid;
                            // we care about this data
                            if (fcstpoint[1] != null) {
                                riverPoints.get(lid).setFcstValue(
                                        (Double) fcstpoint[1]);
                            }
                            if (fcstpoint[2] != null) {
                                fcsttime = Calendar.getInstance(TimeZone
                                        .getTimeZone("GMT"));
                                fcsttime.setTimeInMillis(((Timestamp) fcstpoint[2])
                                        .getTime());
                                riverPoints.get(lid).setFcstTime(fcsttime);
                            }
                        }
                    }
                }
            }
        } catch (VizException e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                    e);
        }

        return riverPoints;
    }

    /**
     * Gets the RiverID for the GageData
     * 
     * @param lid
     * @return
     */
    public String getRiverID(String lid) {
        String riverID = null;

        try {
            ArrayList<Object[]> riverObject = (ArrayList<Object[]>) DirectDbQuery
                    .executeQuery(riverIDQuery + "'" + lid + "'",
                            HydroConstants.IHFS, QueryLanguage.SQL);

            if (riverObject.size() > 0) {
                riverID = (String) (riverObject.get(0))[0];
            }
        } catch (VizException e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                    e);
        }

        return riverID;
    }

    /**
     * Get an individual River Data Point
     * 
     * @param lid
     * @return
     */
    public RiverDataPoint getRiverDataPoint(String lid) {

        // Get the location information first
        RiverDataPoint rdp = new RiverDataPoint(lid);
        try {
            ArrayList<Object[]> locationObject = (ArrayList<Object[]>) DirectDbQuery
                    .executeQuery(locationQuery + "'" + lid + "'",
                            HydroConstants.IHFS, QueryLanguage.SQL);
            if (locationObject.size() == 1) {
                Object[] location = locationObject.get(0);
                if ((String) location[0] != null) {
                    rdp.setLocName((String) location[0]);
                }
                if ((String) location[1] != null) {
                    rdp.setCounty((String) location[1]);
                }
                if ((String) location[2] != null) {
                    rdp.setState((String) location[2]);
                }
                if ((Double) location[3] != null) {
                    rdp.setElevation((Double) location[3]);
                }
                if ((String) location[4] != null) {
                    rdp.setHsa((String) location[4]);
                }
                if ((Double) location[5] != null) {
                    rdp.setLat((Double) location[5]);
                }
                if ((Double) location[6] != null) {
                    rdp.setLon((Double) location[6]);
                }
                if ((String) location[7] != null) {
                    rdp.setRiverName((String) location[7]);
                }
            }
        } catch (VizException e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                    e);
        }
        // next find the river stat values
        try {
            ArrayList<Object[]> statObject = (ArrayList<Object[]>) DirectDbQuery
                    .executeQuery(riverStatQuery + "'" + lid + "'",
                            HydroConstants.IHFS, QueryLanguage.SQL);
            if (statObject.size() == 1) {
                Object[] stat = statObject.get(0);
                if ((String) stat[0] != null) {
                    rdp.setStreamName((String) stat[0]);
                }
                if ((Double) stat[1] != null) {
                    rdp.setMile((Double) stat[1]);
                }
                if ((Double) stat[2] != null) {
                    rdp.setZeroDatum((Double) stat[2]);
                }
                if ((String) stat[3] != null) {
                    rdp.setTide((String) stat[3]);
                }
                if ((Double) stat[4] != null) {
                    rdp.setBankFull((Double) stat[4]);
                }
                if ((Double) stat[5] != null) {
                    rdp.setActionStage((Double) stat[5]);
                }
                if ((Double) stat[6] != null) {
                    rdp.setFloodStage((Double) stat[6]);
                }
                if ((Double) stat[7] != null) {
                    rdp.setFloodFlow((Double) stat[7]);
                }
                if ((Double) stat[8] != null) {
                    rdp.setActionFlow((Double) stat[8]);
                }
                if ((String) stat[9] != null) {
                    rdp.setPrimaryPE((String) stat[9]);
                }
            }
        } catch (VizException e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                    e);
        }
        // next find the description values
        try {
            ArrayList<Object[]> descripObject = (ArrayList<Object[]>) DirectDbQuery
                    .executeQuery(descriptionQuery + "'" + lid + "'",
                            HydroConstants.IHFS, QueryLanguage.SQL);
            if (descripObject.size() == 1) {
                Object[] descrip = descripObject.get(0);
                if ((String) descrip[0] != null) {
                    rdp.setProximity((String) descrip[0]);
                }
                if ((String) descrip[1] != null) {
                    rdp.setReach((String) descrip[1]);
                }
            }
        } catch (VizException e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                    e);
        }
        // get data for the flood categories
        try {
            ArrayList<Object[]> floodObject = (ArrayList<Object[]>) DirectDbQuery
                    .executeQuery(floodCatQuery + "'" + lid + "'",
                            HydroConstants.IHFS, QueryLanguage.SQL);
            if (floodObject.size() == 1) {
                Object[] flood = floodObject.get(0);
                if ((Double) flood[0] != null) {
                    rdp.setMinorStage((Double) flood[0]);
                }
                if ((Double) flood[1] != null) {
                    rdp.setModerateStage((Double) flood[1]);
                }
                if ((Double) flood[2] != null) {
                    rdp.setMajorStage((Double) flood[2]);
                }
                if ((Double) flood[3] != null) {
                    rdp.setMinorFlow((Double) flood[3]);
                }
                if ((Double) flood[4] != null) {
                    rdp.setModerateFlow((Double) flood[4]);
                }
                if ((Double) flood[5] != null) {
                    rdp.setMajorFlow((Double) flood[5]);
                }
            }
        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                    e);
        }
        // Get data for Crest information
        try {
            CrestHistoryData crests = new CrestHistoryData();
            ArrayList<CrestData> crestList = crests.getCrestDataArray();

            ArrayList<Object[]> crestObject = (ArrayList<Object[]>) DirectDbQuery
                    .executeQuery(crestQuery + lid + "' ORDER BY stage, q",
                            HydroConstants.IHFS, QueryLanguage.SQL);

            if (crestObject != null) {
                for (Object[] crestob : crestObject) {
                    crests.addCrestData(new CrestData(crestob));
                }
            }
            if (crestList.size() > 0) {
                crests.sortByStage();
                CrestData cd = crestList.get(0);

                rdp.setCrestValue(cd.getStage());
                rdp.setCrestFlow(cd.getFlow());
                rdp.setCrestTime(cd.getCrestDate());
            }

        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                    e);
        }
        // get data for the river information
        try {
            ArrayList<Object[]> riverInfoObject = (ArrayList<Object[]>) DirectDbQuery
                    .executeQuery(riverInfoQuery + "'" + lid + "'",
                            HydroConstants.IHFS, QueryLanguage.SQL);
            if (riverInfoObject.size() == 1) {
                Object[] riverInfo = riverInfoObject.get(0);
                if ((String) riverInfo[0] != null) {
                    rdp.setRiverID((String) riverInfo[0]);
                }
            }
        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                    e);
        }

        return rdp;
    }

    public String getStreamName(String lid) {
        List<Object[]> results = null;
        String stream = null;
        try {
            results = DirectDbQuery.executeQuery(
                    "select stream from riverstat where lid = '" + lid + "'",
                    HydroConstants.IHFS, QueryLanguage.SQL);
            if ((results != null) && (results.size() > 0)) {
                stream = (String) results.get(0)[0];
            }
        } catch (VizException e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                    e);
        }

        return stream;
    }

    /**
     * Get the Statprof data for the specified stream.
     * 
     * @param stream
     *            The stream
     * @return The statprof data for the stream.
     */
    public List<Statprof> getStatProf(String stream) {
//        String query = "from "
//                + com.raytheon.edex.plugin.shef.objects.Statprof.class
//                        .getName();
//        query += " where stream = '" + stream + "' order by mile desc";
        String query = "select lid, name, primary_pe, stream, fs, wstg, fq, " +
                "action_flow, zd, mile, reach, proximity from statprof " + 
                "where stream = '" + stream + "' order by mile desc";

        List<Statprof> dataList = new ArrayList<Statprof>();
        try {
            List<Object[]> results = DirectDbQuery.executeQuery(query,
                    HydroConstants.IHFS, QueryLanguage.SQL);
            if ((results != null) && (results.size() > 0)) {
                for (Object[] oa : results) {
                    if (oa[0] != null) {
//                        dataList.add((Statprof) oa[0]);
                        Statprof sp = new Statprof();
                        StatprofId spid = new StatprofId();
                        spid.setLid((String) oa[0]);
                        spid.setName((String) oa[1]);
                        spid.setPrimaryPe((String) oa[2]);
                        spid.setStream((String) oa[3]);
                        spid.setFs((Double) oa[4]);
                        spid.setWstg((Double) oa[5]);
                        spid.setFq((Double) oa[6]);
                        spid.setActionFlow((Double) oa[7]);
                        spid.setZd((Double) oa[8]);
                        spid.setMile((Double) oa[9]);
                        spid.setReach((String) oa[10]);
                        spid.setProximity((String) oa[11]);
                        sp.setId(spid);
                        
                        // Check for missing data values
                        if (spid.getPrimaryPe() == null) {
                        	continue;
                        } else if (spid.getPrimaryPe().startsWith("H")) {
                            if ((spid.getFs() == null) || (spid.getWstg() == null)) {
                                continue;
                            }
                        } else {
                            if ((spid.getFq() == null) || (spid.getActionFlow() == null)) {
                                continue;
                            }
                        }
                        dataList.add(sp);
                    }
                }
            }
        } catch (VizException e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                    e);
        }

        return dataList;
    }

    public ArrayList<Riverstatus> getRiverStatus(String lid, String pe,
            long validTime, long basisTime) {

        SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
        Date validTimeDate = new Date();
        validTimeDate.setTime(validTime);
        String validTimeStr = sdf.format(validTimeDate);
        Date basisTimeDate = new Date();
        basisTimeDate.setTime(basisTime);
        String basisTimeStr = sdf.format(basisTimeDate);

        String where = " where lid = '" + lid + "' " + "and pe = '" + pe
                + "' and (validTime > '" + validTimeStr
                + "' or ts like 'F%%') and "
                + "basisTime is null or basistime >= '" + basisTimeStr + "')";

        String query = "from "
                + com.raytheon.uf.common.dataplugin.shef.tables.Riverstatus.class
                        .getName();
        query += where;

        ArrayList<Riverstatus> dataList = null;
        try {
            List<Object[]> results = DirectDbQuery.executeQuery(query,
                    HydroConstants.IHFS, QueryLanguage.HQL);
            if ((results != null) && (results.size() > 0)) {
                dataList = new ArrayList<Riverstatus>();
                for (Object[] oa : results) {
                    dataList.add((Riverstatus) oa[0]);
                }
            }
        } catch (VizException e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                    e);
        }

        return dataList;
    }

    public List<Object[]> getTsList() {
        List<Object[]> rs = null;

        String query = "select distinct(ts), ts_rank from ingestfilter";

        try {
            rs = DirectDbQuery.executeQuery(query, HydroConstants.IHFS,
                    QueryLanguage.SQL);
        } catch (VizException e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                        e);
        }

        return rs;
    }
}
