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
package com.raytheon.viz.hydro.pointprecipitation;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.List;
import java.util.TimeZone;

import com.raytheon.uf.common.dataplugin.shef.tables.Curpc;
import com.raytheon.uf.common.dataplugin.shef.tables.Rawpc;
import com.raytheon.uf.common.dataplugin.shef.tables.RawpcId;
import com.raytheon.uf.common.dataplugin.shef.tables.Rawpp;
import com.raytheon.uf.common.dataplugin.shef.tables.RawppId;
import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.viz.hydro.pointdatacontrol.data.IngestFilter;
import com.raytheon.viz.hydro.pointprecipitation.PointPrecipConstants.RawPrecipTable;
import com.raytheon.viz.hydrocommon.HydroConstants;
import com.raytheon.viz.hydrocommon.datamanager.HydroDataManager;


/**
 * Class for managing database query calls. PointPrecipDataManager.java
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 03Sept2008   #1509      dhladky     Initial Creation.
 * 31Aug2009    #2257      mpduff      Adding data access methods.
 * sep292010    #4384      lbousaidi   Fixed quality_code and change name of table
 * 									   that retrieves PC data from rawpp to rawpc    
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */

public class PointPrecipDataManager extends HydroDataManager {
    private static final String QUERY = "select lid,obstime,lid from latestobsvalue";
    
    private static int adjustedPCStartTime = HydroConstants.MISSING_VALUE;
    
    private static PointPrecipDataManager instance = null;
    
    /** Private constructor */
    private PointPrecipDataManager() {
        
    }

    public static synchronized PointPrecipDataManager getInstance() {
        if (instance == null) {
            instance = new PointPrecipDataManager();
        }
        
        return instance;
    }
    
    /**
     * Get tabularDisplayData from the DB
     * 
     * @return String[]
     */
    public Object[] getPointPrecipData() {

        ArrayList<Object[]> data;

        data = runQuery(QUERY);
        
        if (data != null) {
            return data.get(0);
        }
        
        return null;
    }

    // ------------------------------------------
    // Query 'locclass' view in ihfs database for
    // selected 'hsa' field using SQL.
    // ------------------------------------------
    public String[] queryHsa() {

        StringBuilder myQuery = new StringBuilder(
                "select distinct locclass.hsa from locclass order by locclass.hsa asc");
        ArrayList<Object[]> hsaData;
        List<String> items = new ArrayList<String>();
        
        // ---------------------------------------------------------
        // Query the HSA in LOCCLASS in the IHFS database using SQL.
        // ---------------------------------------------------------
        hsaData = runQuery(myQuery.toString());
        for (Object[] rowData : hsaData) {
            items.add((String) rowData[0]);
        }

        return items.toArray(new String[items.size()]);
    }


    // ---------------------------------------------------------------
    // Query 'ingestfilter' table in ihfs database for selected fields
    // using SQL.
    // ---------------------------------------------------------------
    public String[] queryIngestfilterPc() {

        StringBuilder myQuery = new StringBuilder(
                "select distinct ing.ts from ingestfilter ing where pe = 'PC' and ing.ts like 'R%' order by 1");
        ArrayList<Object[]> PCData;
        List<String> items = new ArrayList<String>();
        
        // ------------------------------------------------------------
        // Query the IngestFilter Table in the IHFS database using SQL.
        // ------------------------------------------------------------
        PCData = runQuery(myQuery.toString());
        for (Object[] rowData : PCData) {
            items.add((String) rowData[0]);
        }
        
        return items.toArray(new String[items.size()]);
    }
    
    // ---------------------------------------------------------------
    // Query 'ingestfilter' table in ihfs database for selected fields
    // using SQL.
    // ---------------------------------------------------------------
    public String[] queryIngestfilterPp() {

        StringBuilder myQuery = new StringBuilder(
                "select distinct ing.ts from ingestfilter ing where pe = 'PP' and ing.ts like 'R%' order by 1");
        ArrayList<Object[]> PPData;
        List<String> items = new ArrayList<String>();
        // ------------------------------------------------------------
        // Query the IngestFilter Table in the IHFS database using SQL.
        // ------------------------------------------------------------
        PPData = runQuery(myQuery.toString());
        for (Object[] rowData : PPData) {
            items.add((String) rowData[0]);
        }
        
        return items.toArray(new String[items.size()]);
    }

    // ---------------------------------------------------------------
    // Query 'curpc' table in ihfs database for selected fields
    // using SQL.
    // ---------------------------------------------------------------
    public ArrayList<Curpc> queryCurPc(Date queryBeginTime,
            Date queryEndTime, String lid, List<String> pTs) {
        ArrayList<Object[]> pcData;
        String tsClause = null;
        StringBuilder where = new StringBuilder();
        ArrayList<Curpc> data = new ArrayList<Curpc>();        
 
        /*
         * In order to consider cases that there is no PC data during dry
         * periods, retrieve more wider data for gages, use token 
         * adjust_PC_startingtime to specify the PC data retrieval starting point
         */
        if (adjustedPCStartTime == HydroConstants.MISSING_VALUE) {
           AppsDefaults ad = AppsDefaults.getInstance();
           String tokenString = ad.getToken("adjust_PC_startingtime");
           if ((tokenString != null) && (tokenString.length() > 0)) {
               adjustedPCStartTime = Integer.parseInt(tokenString);
               if (adjustedPCStartTime < 0) {
                   adjustedPCStartTime = PointPrecipConstants.DEFAULT_ADJUSTED_STARTTIME_HRS;
               }
           }
        } else {
           adjustedPCStartTime = PointPrecipConstants.DEFAULT_ADJUSTED_STARTTIME_HRS;
        }
        
        // Adjust the start hours
        Calendar adjustedBeginTime = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
        adjustedBeginTime.setTimeInMillis(queryBeginTime.getTime());
        adjustedBeginTime.add(Calendar.HOUR, adjustedPCStartTime * -1);

        if (pTs.size() > 0) {
            tsClause = build_ts_clause(pTs);
            if (tsClause == null) {
                return null;
            }
        }
        
        if ((lid.length() > 0) && (pTs.size() > 0)) {
            where.append(String.format(" where lid = '%s' and %s ", lid, tsClause));
            where.append(" AND value != '-9999.0' AND ");
            where.append(String.format(" obstime >= '%s' AND ", 
                    HydroConstants.DATE_FORMAT.format(adjustedBeginTime.getTime())));
            where.append(String.format(" obstime <= '%s' ", 
                    HydroConstants.DATE_FORMAT.format(queryEndTime)));
            where.append("AND value IS NOT NULL AND ");
            where.append(" shef_qual_code NOT LIKE 'B%%' AND shef_qual_code ");
            where.append(" NOT LIKE 'R%%' ORDER BY obstime DESC ");            
        } else if (pTs.size() > 0) {
            where.append(String.format(" WHERE %s ", tsClause));
            where.append(" AND value != '-9999.0' AND ");
            where.append(String.format("obstime >= '%s' AND ", 
                    HydroConstants.DATE_FORMAT.format(adjustedBeginTime.getTime())));
            where.append(String.format(" obstime <= '%s' ", 
                    HydroConstants.DATE_FORMAT.format(queryEndTime)));
            where.append("AND value IS NOT NULL AND ");
            where.append(" shef_qual_code NOT LIKE 'B%%' AND shef_qual_code ");
            where.append(" NOT LIKE 'R%%' ORDER BY lid ASC, obstime DESC");
        } else if (lid.length() > 0) {
            where.append(String.format(" WHERE lid = '%s' ", lid));
            where.append(" AND value != '-9999.0' AND ");
            where.append(String.format("obstime >= '%s' AND ",
                    HydroConstants.DATE_FORMAT.format(adjustedBeginTime.getTime())));
            where.append(String.format(" obstime <= '%s' ",
                    HydroConstants.DATE_FORMAT.format(queryEndTime)));
            where.append("AND value IS NOT NULL AND ");
            where.append(" shef_qual_code NOT LIKE 'B%%' AND shef_qual_code ");
            where.append(" NOT LIKE 'R%%' ORDER BY ts ASC, obstime DESC");
        } else {
            where.append(" WHERE value != '-9999.0' AND ");
            where.append(String.format("obstime >= '%s' AND ",
                    HydroConstants.DATE_FORMAT.format(adjustedBeginTime.getTime())));
            where.append(String.format(" obstime <= '%s' ",
                    HydroConstants.DATE_FORMAT.format(queryEndTime)));
            where.append("AND value IS NOT NULL AND ");
            where.append(" shef_qual_code NOT LIKE 'B%%' AND shef_qual_code ");
            where.append(" NOT LIKE 'R%%' ORDER BY lid ASC, ts ASC, obstime DESC");
        }
        
        String query = "select lid, pe, dur, ts, extremum, obstime, value, shef_qual_code, revision, product_id, producttime, postingtime from curpc";
        pcData = runQuery(query + where.toString());
        
        int i = 0;
        for (Object[] rowData : pcData) {
            Curpc curpc = new Curpc();
            curpc.getId().setLid((String) rowData[i]);
            curpc.setPe((String) rowData[++i]);
            curpc.setDur((Short) rowData[++i]);
            curpc.getId().setTs((String) rowData[++i]);
            curpc.getId().setExtremum((String) rowData[++i]);
            curpc.getId().setObstime((Date) rowData[++i]);
            curpc.setValue((Double) rowData[++i]);
            curpc.setShefQualCode((String) rowData[++i]);
            curpc.setRevision((Short) rowData[++i]);
            curpc.setProductId((String) rowData[++i]);
            curpc.setProducttime((Date) rowData[++i]);
            curpc.setPostingtime((Date) rowData[++i]);
            
            i = 0;
            data.add(curpc);
        }
        
        return data;
    }
    
    /**
     * Get the lat, lon, and name from location.
     * 
     * @param lid
     *      The lid to query on
     * @return
     *      Object[] - 0 = lat, 1 = lon, 2 = name
     */
    public Object[] getLocInfo(String lid) {
        String query = "select lat, lon, name from location where lid = '" + lid + "'";
        
        Object[] result = null;
               
        ArrayList<Object[]> rs = runQuery(query);
 
        if ((rs!=null) && (rs.size() > 0)) { 
           	return rs.get(0);
        } else {
        	return result;
    	}                     
        
    }
    
    public ArrayList<Rawpc> loadPcRaw(Date beginTime, Date endTime, String lid, List<String> tsList, RawPrecipTable table) {
        String tsClause = null;
        StringBuilder where = new StringBuilder();
        ArrayList<Object[]> pcData;
        ArrayList<Rawpc> pRawPc = new ArrayList<Rawpc>();
        if (lid == null) {
            lid = "";
        }

        /*
         * In order to consider cases that there is no PC data during dry
         * periods, retrieve more wider data for gages, use token 
         * adjust_PC_startingtime to specify the PC data retrieval starting point
         */
       if (adjustedPCStartTime == HydroConstants.MISSING_VALUE) {
          AppsDefaults ad = AppsDefaults.getInstance();
          String tokenString = ad.getToken("adjust_PC_startingtime");
          if ((tokenString != null) && (tokenString.length() > 0)) {
              adjustedPCStartTime = Integer.parseInt(tokenString);
              if (adjustedPCStartTime <= 0) {
                  adjustedPCStartTime = PointPrecipConstants.DEFAULT_ADJUSTED_STARTTIME_HRS;
              }
          }
       } else {
          adjustedPCStartTime = PointPrecipConstants.DEFAULT_ADJUSTED_STARTTIME_HRS;
       }
       
       
    
       // Adjust the start hours
       Calendar adjustedBeginTime = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
       adjustedBeginTime.setTimeInMillis(beginTime.getTime());
       /*if (adjustedPCStartTime != -9999) { 
           adjustedBeginTime.add(Calendar.HOUR, adjustedPCStartTime * -1);
       }*/ 
      
       adjustedBeginTime.add(Calendar.HOUR_OF_DAY, adjustedPCStartTime * -1);         
       

       if (tsList.size() > 0) {
           tsClause = build_ts_clause(tsList);
           if (tsClause == null) {
               return null;
           }
       }
       
       if ((lid.length() > 0) && (tsList.size() > 0)) {
           where.append(String.format(" where lid = '%s' and %s ", lid, tsClause));
           where.append(" AND value != '-9999.0' AND ");
           where.append(String.format(" obstime >= '%s' AND ", 
                   HydroConstants.DATE_FORMAT.format(adjustedBeginTime.getTime())));
           where.append(String.format(" obstime <= '%s' ", 
                   HydroConstants.DATE_FORMAT.format(endTime)));
           where.append("AND value IS NOT NULL AND ");
           where.append(" shef_qual_code NOT LIKE 'B%%' AND shef_qual_code ");
           where.append(" NOT LIKE 'R%%' ORDER BY obstime DESC ");            
       } else if (tsList.size() > 0) {
           where.append(String.format(" WHERE %s ", tsClause));
           where.append(" AND value != '-9999.0' AND ");
           where.append(String.format("obstime >= '%s' AND ", 
                   HydroConstants.DATE_FORMAT.format(adjustedBeginTime.getTime())));
           where.append(String.format(" obstime <= '%s' ", 
                   HydroConstants.DATE_FORMAT.format(endTime)));
           where.append("AND value IS NOT NULL AND ");
           where.append(" shef_qual_code NOT LIKE 'B%%' AND shef_qual_code ");
           where.append(" NOT LIKE 'R%%' ORDER BY lid ASC, obstime DESC");
       } else if (lid.length() > 0) {
           where.append(String.format(" WHERE lid = '%s' ", lid));
           where.append(" AND value != '-9999.0' AND ");
           where.append(String.format("obstime >= '%s' AND ",
                   HydroConstants.DATE_FORMAT.format(adjustedBeginTime.getTime())));
           where.append(String.format(" obstime <= '%s' ",
                   HydroConstants.DATE_FORMAT.format(endTime)));
           where.append("AND value IS NOT NULL AND ");
           where.append(" shef_qual_code NOT LIKE 'B%%' AND shef_qual_code ");
           where.append(" NOT LIKE 'R%%' ORDER BY ts ASC, obstime DESC");
           
       } else {
           where.append(" WHERE value != '-9999.0' AND ");
           where.append(String.format("obstime >= '%s' AND ",
                   HydroConstants.DATE_FORMAT.format(adjustedBeginTime.getTime())));
           where.append(String.format(" obstime <= '%s' ",
                   HydroConstants.DATE_FORMAT.format(endTime)));
           where.append("AND value IS NOT NULL AND ");
           where.append(" shef_qual_code NOT LIKE 'B%%' AND shef_qual_code ");
           where.append(" NOT LIKE 'R%%' ORDER BY lid ASC, ts ASC, obstime DESC");
           
       }
       
       /* get the data */
       if (table == RawPrecipTable.CurRawPrecip) {
           String query = "select lid, pe, dur, ts, extremum, obstime, value, shef_qual_code, quality_code, revision, product_id, producttime, postingtime from curpc";
           pcData = runQuery(query + where.toString());      
        	   
           if ((pcData != null) && (pcData.size() > 0)) {
               int i = 0;
               for (Object[] oa: pcData) {
                   Rawpc rawpc = new Rawpc();
                   RawpcId id = new RawpcId();
                   rawpc.setId(id);
                   rawpc.getId().setLid((String) oa[i]);
                   rawpc.setPe((String) oa[++i]);
                   rawpc.setDur(((Integer) oa[++i]).shortValue());
                   rawpc.getId().setTs((String) oa[++i]);
                   rawpc.getId().setExtremum((String) oa[++i]);
                   rawpc.getId().setObstime((Date) oa[++i]);
                   rawpc.setValue((Double) oa[++i]);
                   rawpc.setShefQualCode((String) oa[++i]);
                   rawpc.setQualityCode((Integer) oa[++i]);
                   rawpc.setRevision(((Integer) oa[++i]).shortValue());
                   rawpc.setProductId((String) oa[++i]);
                   rawpc.setProducttime((Date) oa[++i]);
                   rawpc.setPostingtime((Date) oa[++i]);
                   i = 0;
                   pRawPc.add(rawpc);
               }
           }
       } else {
           // Get rawpc 
           String query = "select lid, pe, dur, ts, extremum, obstime, value, shef_qual_code, quality_code, revision, product_id, producttime, postingtime from rawpc";           
           pcData = runQuery(query + where.toString());
           
           if ((pcData != null) && (pcData.size() > 0)) {
               int i = 0;
               for (Object[] oa: pcData) {
                   Rawpc rawpc = new Rawpc();
                   rawpc.getId().setLid((String) oa[i]);
                   rawpc.setPe((String) oa[++i]);
                   rawpc.setDur((Short) oa[++i]);
                   rawpc.getId().setTs((String) oa[++i]);
                   rawpc.getId().setExtremum((String) oa[++i]);
                   rawpc.getId().setObstime((Date) oa[++i]);
                   rawpc.setValue((Double) oa[++i]);
                   rawpc.setShefQualCode((String) oa[++i]);
                   rawpc.setQualityCode((Integer) oa[++i]);
                   rawpc.setRevision((Short) oa[++i]);
                   rawpc.setProductId((String) oa[++i]);
                   rawpc.setProducttime((Date) oa[++i]);
                   rawpc.setPostingtime((Date) oa[++i]);
                   i = 0;
                   pRawPc.add(rawpc);
               }
           }
       }
        
       return pRawPc;
    }
    
    public ArrayList<Rawpp> loadPpRaw(Date beginTime, Date endTime, String lid, List<String> tsList, RawPrecipTable table) {
        StringBuilder where = new StringBuilder();
        String tsClause = null;
        ArrayList<Object[]> pcData;
        ArrayList<Rawpp> pRawPp = new ArrayList<Rawpp>();
        if (lid == null) {
            lid = "";
        }

        /* Only retrieve valid PP data. */
        String qcWhere = buildQCWhere(PointPrecipConstants.QC_NOT_FAILED);
        
        if (tsList.size() > 0) {
            tsClause = build_ts_clause(tsList);
            if (tsClause == null) {
                return null;
            }
        }
        
        if ((lid.length() > 0) && (tsList.size() > 0)) {
            where.append(String.format(" WHERE lid = '%s' AND %s ", lid, tsClause));
            where.append(String.format(" AND value != '-9999.0' AND obstime >= '%s' ", 
                    HydroConstants.DATE_FORMAT.format(beginTime)));
            where.append(String.format(" AND obstime <= '%s' AND", 
                    HydroConstants.DATE_FORMAT.format(endTime)));
            where.append(qcWhere + " order by dur desc, obstime desc");
        } else if (tsList.size() > 0) {
            where.append(" where " + tsClause + " and value != '-9999.0' ");
            where.append(String.format("AND obstime >= '%s' ", 
                    HydroConstants.DATE_FORMAT.format(beginTime)));
            where.append(String.format(" AND obstime <= '%s' ",
                    HydroConstants.DATE_FORMAT.format(endTime)));
            where.append(" and " + qcWhere + " order by lid asc,");
            where.append(" dur DESC, obstime DESC");
        } else if (lid.length() > 0) {
            where.append(" where lid = '" + lid + "' and value != '-9999.0'");
            where.append(String.format(" and obstime >= '%s' and ",
                    HydroConstants.DATE_FORMAT.format(beginTime)));
            where.append(String.format(" obstime <= '%s' AND %s ",
                    HydroConstants.DATE_FORMAT.format(endTime), qcWhere));
            where.append(" ORDER BY ts ASC, DUR DESC, obstime DESC");
           
            
        } else {
            where.append(" WHERE value != '-9999.0' AND ");
            where.append(String.format("obstime >= '%s' AND ",
                    HydroConstants.DATE_FORMAT.format(beginTime)));
            where.append(String.format("obstime <= '%s' AND %s ",
                    HydroConstants.DATE_FORMAT.format(endTime), qcWhere));
            where.append(" ORDER BY lid ASC, ts ASC, dur DESC, obstime DESC");
            
        }
        
        /* get the data */
        if (table == RawPrecipTable.CurRawPrecip) {
            String query = "select lid, pe, dur, ts, extremum, obstime, value, shef_qual_code, quality_code, revision, product_id, producttime, postingtime from curpp";
            pcData = runQuery(query + where.toString());
            
            if ((pcData != null) && (pcData.size() > 0)) {
                int i = 0;
                for (Object[] oa: pcData) {
                    Rawpp rawpp = new Rawpp();
                    RawppId id = new RawppId();
                    rawpp.setId(id);
                    rawpp.getId().setLid((String) oa[i]);
                    rawpp.setPe((String) oa[++i]);
                    rawpp.getId().setDur(((Integer) oa[++i]).shortValue());
                    rawpp.getId().setTs((String) oa[++i]);
                    rawpp.getId().setExtremum((String) oa[++i]);
                    rawpp.getId().setObstime((Date) oa[++i]);
                    rawpp.setValue((Double) oa[++i]);
                    rawpp.setShefQualCode((String) oa[++i]);
                    rawpp.setQualityCode((Integer) oa[++i]);
                    rawpp.setRevision(((Integer) oa[++i]).shortValue());
                    rawpp.setProductId((String) oa[++i]);
                    rawpp.setProducttime((Date) oa[++i]);
                    rawpp.setPostingtime((Date) oa[++i]);
                    i = 0;
                    pRawPp.add(rawpp);
                }
            }
        } else {
            // get rawpp
            String query = "select lid, pe, dur, ts, extremum, obstime, value, shef_qual_code, quality_code, revision, product_id, producttime, postingtime from rawpp";
            pcData = runQuery(query + where.toString());
                       
            int i = 0;
            for (Object[] oa: pcData) {
                Rawpp rawpp = new Rawpp();
                rawpp.getId().setLid((String) oa[i]);
                rawpp.setPe((String) oa[++i]);
                rawpp.getId().setDur((Short) oa[++i]);
                rawpp.getId().setTs((String) oa[++i]);
                rawpp.getId().setExtremum((String) oa[++i]);
                rawpp.getId().setObstime((Date) oa[++i]);
                rawpp.setValue((Double) oa[++i]);
                rawpp.setShefQualCode((String) oa[++i]);
                rawpp.setRevision((Short) oa[++i]);
                rawpp.setProductId((String) oa[++i]);
                rawpp.setProducttime((Date) oa[++i]);
                rawpp.setPostingtime((Date) oa[++i]);
                i = 0;
                pRawPp.add(rawpp);
            }
        }
        
        return pRawPp;
    }
    
    private String buildQCWhere(int request) {
        String where = null;
        
        /* The data value is valid as per all checks. */
        if (request == PointPrecipConstants.QC_PASSED) {
            where = "quality_code >= " + PointPrecipConstants.GOOD_QUESTIONABLE_THRESHOLD;
        } else if (request == PointPrecipConstants.QC_NOT_PASSED) {
            /* The gross range certainty check did not pass. The data is not usable. */
            where = "quality_code < " + PointPrecipConstants.GOOD_QUESTIONABLE_THRESHOLD;
        } else if (request == PointPrecipConstants.QC_FAILED) {
            /* The data value is invalid because it failed at least one
               certainty check */
            where = "quality_code < " + PointPrecipConstants.QUESTIONABLE_BAD_THRESHOLD;
        } else if (request == PointPrecipConstants.QC_QUESTIONABLE) {
            /* The data value is questionable.  It passed all certainty checks  
               but failed at least one quality check. */
            where = "quality_code BETWEEN " + PointPrecipConstants.QUESTIONABLE_BAD_THRESHOLD +
                " and " + (PointPrecipConstants.GOOD_QUESTIONABLE_THRESHOLD - 1);
        } else if (request == PointPrecipConstants.QC_NOT_FAILED) {
            /* The data is deemed full valid or is questionable, but it is 
               not deemed bad with certainty. */
            where = "quality_code >= " + PointPrecipConstants.QUESTIONABLE_BAD_THRESHOLD;
        }
        
        return where;
    }
    
    /**
     * Check if the given lid is an ALERT station.
     * 
     * @param lid
     *      The lid to check
     * @return
     *      true if alert station, false if not
     */
    public boolean isAlertStation(String lid) {
        boolean alert = false;
        
        String query = "select type from telem where lid = '" + lid + "'";
        
        ArrayList<Object[]> rs = runQuery(query);
        if ((rs != null) && (rs.size() > 0)) {
            Object[] oa = rs.get(0);
            if (oa[0] != null && ((String) oa[0]).equalsIgnoreCase("ALERT")) {
                alert = true;
            }
        }
        
        return alert;
    }
    
    public ArrayList<IngestFilter> getIngestTsInfo(String lid) {
        ArrayList<Object[]> rs = null;
        ArrayList<IngestFilter> filterList = null;
        String query = "select lid, pe, duration, ts, extremum, ts_rank from ingestFilter";
        String where = " where lid = '" + lid + "' and ingest = 'T' order by ts asc";
        
        rs = runQuery(query + where);
        if ((rs != null) && (rs.size() > 0)) {
            filterList = new ArrayList<IngestFilter>();
            for (Object[] oa: rs) {
                IngestFilter filter = new IngestFilter(oa);
                filterList.add(filter);
            }
        }
        
        return filterList;
    }
    
    public String build_ts_clause(List<String> ts) {
        if ((ts == null) || ts.isEmpty()) {
            return "";
        }
        StringBuilder tsClause = new StringBuilder("ts ");

        if (ts.get(0).startsWith("!")) {
            tsClause.append("not in ('");
            tsClause.append(ts.get(0).substring(1));
        } else {
            tsClause.append("in ('");
            tsClause.append(ts.get(0));
        }

        for (int i = 1; i < ts.size(); i++) {
            tsClause.append("', '");
            tsClause.append(ts.get(i));
        }
        tsClause.append("')");

        return tsClause.toString();
    }
    
    public String getDur(short dur) {
        String query = "select name from shefdur where dur = " + dur + " ";
        ArrayList<Object[]> rs = runQuery(query);
        String durStr = null;
        
        if ((rs != null) && (rs.size() > 0)) {
            Object[] oa = rs.get(0);
            durStr = (String) oa[0];
        }
        
        return durStr;
    }
}
