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
package com.raytheon.viz.hydro.util;

import java.text.ParseException;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.List;
import java.util.TimeZone;

import com.raytheon.uf.common.dataplugin.shef.tables.Fcstheight;
import com.raytheon.uf.common.dataplugin.shef.tables.Ingestfilter;
import com.raytheon.uf.common.dataplugin.shef.tables.Riverstatus;
import com.raytheon.uf.common.dataplugin.shef.tables.RiverstatusId;
import com.raytheon.uf.common.dataplugin.shef.tables.Rpfparams;
import com.raytheon.uf.common.time.SimulatedTime;
import com.raytheon.uf.viz.core.catalog.DirectDbQuery;
import com.raytheon.uf.viz.core.catalog.DirectDbQuery.QueryLanguage;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.hydrocommon.HydroConstants;
import com.raytheon.viz.hydrocommon.util.DbUtils;
import com.raytheon.viz.hydrocommon.util.QualityCodeUtil;
import com.raytheon.viz.hydrocommon.whfslib.IHFSDbGenerated;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 31, 2009            mpduff     Initial creation.
 * Nov 06, 2009 2639       mpduff     Added getRiverStat method.
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class HydroData {
    /**
     * Get Rpfparams data.
     * 
     * @param where
     *      The where clause to use
     * @return list of Rpfparams data
     * @throws VizException
     */
    public static List<Rpfparams> getRpfParams(String where) throws VizException {
        ArrayList<Rpfparams> returnList = new ArrayList<Rpfparams>();
        String query = null;
        if ((where == null) || (where.length() == 0)) {
            query = "from "
                    + com.raytheon.uf.common.dataplugin.shef.tables.Rpfparams.class
                            .getName();
        } else {
            query = "from "
                    + com.raytheon.uf.common.dataplugin.shef.tables.Rpfparams.class
                            .getName() + " " + where;
        }
        
        List<Object[]> results = DirectDbQuery.executeQuery(query, HydroConstants.IHFS, QueryLanguage.HQL);

        for (Object[] item : results) {
            returnList.add((Rpfparams) item[0]);
        }
        
        return returnList;
    }
    
    public static ArrayList<Forecast> bldTsFcstRiv(String lid, String pe, String tsFilter,
            boolean useLatest, Date basisTime) {
        String useTs = null;;
        ArrayList<Forecast> fcstList = null;
        String where = null;
        String qcwhere = null;
        String tablename = null;
        int ordinal = 0;
        boolean[] doKeep;
        int keepCnt = 0;

        try {
                
            /* define which typesource code to use, whether it is passed in
               whether it is determined by the ingestfilter ranks */
            
            if ((tsFilter == null) || (tsFilter.length() == 0))
            {
               ordinal = 0;
               useTs = getBestTs(lid, pe, "F%", ordinal);
            } else {
               useTs = tsFilter;
            }
            
            /* define the qc filter to apply */
            qcwhere = QualityCodeUtil.buildQcWhere(QualityCodeUtil.QC_NOT_FAILED);
            
             
            /* set the tablename to use */
            tablename = DbUtils.getTableName(pe, useTs);
                    
            /* convert the times for the where clause */
            String basisTimeStr = HydroConstants.DATE_FORMAT.format(basisTime);
            
            /* get current system time */
            Date now = SimulatedTime.getSystemTime().getTime();
            
            /* convert the local current time to ansi time in GMT format */
            String currentTime = HydroConstants.DATE_FORMAT.format(now);
    
            /* retrieve a list of unique basis times; use descending sort. 
               only consider forecast data before some ending time,
               and with some limited basis time ago */
            where = "WHERE lid = '" + lid + "' and pe ='" + pe + "' " +
                    "and ts = '" + useTs + "' and probability < 0.0 and " +
                    "validtime >= '" + currentTime + "' and " +
                    "basistime >= '" + basisTimeStr + "' and " +
                    "value != " + HydroConstants.MISSING_VALUE + 
                    " and " + qcwhere + " ORDER BY basistime DESC ";
           
            ArrayList<String[]> uniqueResults = loadUnique("basistime", tablename, where);
            
            if (uniqueResults.size() <= 0) {
                return null;
            }
        
            /* retrieve the data; the ordering by validtime is important.
            as before, limit the forecast time valid time window
            and as needed, the age of the forecast (basistime). */ 
            if (useLatest || (uniqueResults.size() == 1)) {
                where = String.format(" WHERE lid = '%s' AND pe = '%s' AND " + 
                        " ts = '%s' AND " +
                        " probability < 0.0 AND " + 
                        " validtime >= '%s' AND basistime = '%s' AND " +
                        " value != %d AND %s " +
                        " ORDER BY validtime ASC",
                        lid, pe, useTs, currentTime, uniqueResults.get(0)[0], 
                        HydroConstants.MISSING_VALUE, qcwhere);  
                
            } else {
                where = String.format(" WHERE lid = '%s' AND pe = '%s' AND " +
                        " ts = '%s' AND " +
                        " probability < 0.0 AND " +
                        " validtime >= '%s' AND basistime >= '%s' AND " +
                        " value != %d AND %s" +
                        " ORDER BY validtime ASC",
                        lid, pe, useTs, currentTime, basisTimeStr, 
                        HydroConstants.MISSING_VALUE, qcwhere);        
            }
            
            fcstList = getForecast(where, tablename);
            
            /* 
             * if only getting the latest basis time's data
             * or only one basis time was found, then consider all;
             * otherwise, need to adjoin/butt the time series together
             * for the multiple basis times. 
             */
            doKeep = new boolean[fcstList.size()];
            if (useLatest || (uniqueResults.size() <= 1)) {
                for (int i = 0; i < fcstList.size(); i++) {
                    doKeep[i] = true;
                }
            } else {
                setFcstKeep(uniqueResults, fcstList, doKeep);
            }
            
            /* now load the values and info to return, knowing which items
               to keep since all the values have been tagged.  first get
               the count of the number of values to keep and allocate the data */
            for (int i = 0; i < fcstList.size(); i++) {
                if (doKeep[i]) {
                    keepCnt++;
                }
            }

            for (int i = 0; i < fcstList.size(); i++) {
                if (!doKeep[i]) {
                    fcstList.remove(i);
                }
            }
        
        } catch (VizException ve) {
            ve.printStackTrace();
        }
           
        return fcstList;    
    }
    
    /**
    
    For a given location and pe code and type-source prefix, this function
    returns the type-source code with the lowest rank in IngestFilter.
    Alternatively, if a specific ordinal number is passed, then the 
    Nth ranking ts is returned.  If no (<= 0) ordinal number (i.e. 1st, 2nd)
    is requested, then the highest rank (1st) is returned.
    The type-source prefix is normally given as a one-character string,
    R for observed data and F for forecast data.
    
    The function argument returns a status variable indicating 
    whether the request was satisfied.
    
    **************************************************************/
    /**
     * 
     */
    
    
    /**
     * For a given location and pe code and type-source prefix, this function
     * returns the type-source code with the lowest rank in IngestFilter.
     * Alternatively, if a specific ordinal number is passed, then the 
     * Nth ranking ts is returned.  If no (<= 0) ordinal number (i.e. 1st, 2nd)
     * is requested, then the highest rank (1st) is returned.
     * The type-source prefix is normally given as a one-character string,
     * R for observed data and F for forecast data.
     * 
     * @param lid
     *      The location id
     * @param pe
     *      The physical element
     * @param tsPrefix
     *      One character string, R for observed and F for forecast
     * @param ordinal
     *      the specific ranking to return, if <= 0 return highest ranking
     * @return The ts with the best ranking
     */
    public static String getBestTs(String lid, String pe, String tsPrefix,
            int ordinal) {
        String tsFound = null;
        String where = null;
        List<Ingestfilter> ingestFilterList = null;
        Ingestfilter ingestPtr;
    
        /* get the ingest filter entries for this location. note that the
           retrieval is ordered so that if multiple best ranks exist, there
           is some predicatibility for the identified best one. also note that
           this approach ignores the duration, extremum, and probabilty code. */
    
       where = String.format(" WHERE lid = '%s' AND pe = '%s' AND " + 
            " ts LIKE '%s%%' AND ingest = 'T' ORDER BY ts_rank, ts",
            lid, pe, tsPrefix);
       
       ingestFilterList = IHFSDbGenerated.GetIngestFilter(where);
    
       if ((ingestFilterList != null) && (ingestFilterList.size() > 0)) {
           /* if no specific ordinal number was requested, return with
              the highest rank. */
       
           if (ordinal <= 0) {
               tsFound = ingestFilterList.get(0).getId().getTs();
           }
       
       } else {  /* if a specific ordinal number was requested. */
           /* if a specific ordinal number was requested. */
           
           /* get a count of the number of matching ts entries.
              if the requested ordinal number is greater than 
              the number available then return with a not found status. */
      
           if (ordinal <= ingestFilterList.size()) {
               ingestPtr = ingestFilterList.get(ordinal);
               tsFound = ingestPtr.getId().getTs();
           }
       }     
    
       return tsFound;
    }
    
    public static ArrayList<String[]> loadUnique(String field, String table, String where) throws VizException {
        ArrayList<String[]> returnList = new ArrayList<String[]>();
        StringBuilder sql = new StringBuilder("SELECT DISTINCT ");
        
        if (field.contains("||")) {
            String[] fields = field.split("\\|\\|");
            sql.append(fields[0]);
            for (int i = 1; i < fields.length; i++) {
                sql.append(", ");
                sql.append(fields[i]);
            }
        } else {
            sql.append(field);
        }
        
        sql.append(" from " + table + " " + where);
        
        List<Object[]> results = DirectDbQuery.executeQuery(sql.toString(), HydroConstants.IHFS, QueryLanguage.SQL);
        
        for (int i = 0; i < results.size(); i++) {
            Object[] oa = results.get(i);
            ArrayList<String> al = new ArrayList<String>();
            for (Object o: oa) {
                if (o instanceof java.sql.Timestamp) {
                    al.add(HydroConstants.DATE_FORMAT.format((java.sql.Timestamp) o));
                } else if (o instanceof Integer){
                    al.add(((Integer) o).toString());
                } else {
                    al.add((String) o);
                }
            }
            returnList.add(al.toArray(new String[al.size()]));
        }
        
        return returnList;
    }
    
    public static ArrayList<Forecast> getForecast(String where, String tablename) throws VizException {
        List<Forecast> fcstList = new ArrayList<Forecast>();
        List<Object[]> result = null;
        
        if (tablename.equalsIgnoreCase("fcstheight")) {
            String query = "from "
                + com.raytheon.uf.common.dataplugin.shef.tables.Fcstheight.class
                .getName() + " " + where;
            
            result = DirectDbQuery.executeQuery(query, HydroConstants.IHFS, QueryLanguage.HQL);
            
            /* Convert to Forecast object */
            for (Object[] item : result) {
                Fcstheight fh = (Fcstheight) item[0];
                Forecast f = new Forecast();
                f.setBasistime(fh.getId().getBasistime());
                f.setDur(fh.getId().getDur());
                f.setExtremum(fh.getId().getExtremum());
                f.setLid(fh.getId().getLid());
                f.setPe(fh.getId().getPe());
                f.setPostingtime(fh.getPostingtime());
                f.setProbability(fh.getId().getProbability());
                f.setProductId(fh.getProductId());
                f.setProducttime(fh.getProducttime());
                f.setQualityCode(fh.getQualityCode());
                f.setRevision(fh.getRevision());
                f.setShefQualCode(fh.getShefQualCode());
                f.setTs(fh.getId().getTs());
                f.setValidtime(fh.getId().getValidtime());
                f.setValue(fh.getValue());
                fcstList.add(f);
            }
        }
        
        
        return (ArrayList<Forecast>) fcstList;
    }
    
    /**
     * Determine which items in the forecast time series to keep,
     * as there may be overlap due to multiple time_series.  
     * 
     * @param uniqueResults
     *      List of results from the unique query
     * @param fcstList
     *      List of Forecast data
     * @param doKeep
     *      Array of boolean flags on which to keep
     * @return array of flags determining which time series to keep
     */
    private static boolean[] setFcstKeep(ArrayList<String[]> uniqueResults, ArrayList<Forecast> fcstList, boolean[] doKeep) {
        /* get counts of linked lists, one for the forecast values themselves
        and one for the number of unique basis times */
        boolean[] tsFirstChk = new boolean[uniqueResults.size()];
        int[] basisIndex = new int[fcstList.size()];
        Date[] tsStartTime = new Date[fcstList.size()];
        Date[] tsEndTime = new Date[fcstList.size()];
        Date[] tsBasisTime = new Date[fcstList.size()];
        String basisTimeStr = null;
        
        for (int i = 0; i < uniqueResults.size(); i++) {
            tsFirstChk[i] = false;
        }
        
        Date now = Calendar.getInstance(TimeZone.getTimeZone("GMT")).getTime();
        /* allocate arrays for each basis time */
        for (int i = 0; i < fcstList.size(); i++) {
            tsStartTime[i] = now;
        }
        
        for (int i = 0; i < fcstList.size(); i++) {
            tsEndTime[i] = now;
        }
        
        for (int i = 0; i < fcstList.size(); i++) {
            tsBasisTime[i] = now;
        }
        
        
        /* now loop thru the retrieved time series data values and get the
        start and end times for each of the basis times found. */
        for (int i = 0; i < fcstList.size(); i++) {
            /* find out which basis time's time series this value belongs to */
            basisIndex[i] = HydroConstants.MISSING_VALUE;
            basisTimeStr = HydroConstants.DATE_FORMAT.format(fcstList.get(i).getBasistime());
            
            for (int j = 0; ((j < uniqueResults.size()) && (basisIndex[i] == HydroConstants.MISSING_VALUE)); j++) {
                if (uniqueResults.get(j)[0].equalsIgnoreCase(basisTimeStr)) {
                    basisIndex[i] = j;
                }
            }
            
            if (basisIndex[i] == HydroConstants.MISSING_VALUE) {
                // TODO error log this message:
                // fprintf(stderr, "Unexpected error assigning basis_index for %d\n", i);
            }
            
            /* check if the values constitute the start or end times
            for the time series and record these times if they do */
            Date validTime = fcstList.get(i).getValidtime();
            if (tsFirstChk[basisIndex[i]]) {
                if (validTime.before(tsStartTime[basisIndex[i]])) {
                    tsStartTime[basisIndex[i]] = validTime;
                } else if (validTime.after(tsEndTime[basisIndex[i]])) {
                    tsEndTime[basisIndex[i]] = validTime;
                }
            } else {
                tsStartTime[basisIndex[i]] = validTime;
                tsEndTime[basisIndex[i]] = validTime;
                tsFirstChk[basisIndex[i]] = true;
            }
        }
        
        /* for each of the unique basis times, assign the basis time
           in a convenient array for use in the adjust_startend 
           function. */
        for (int j = 0; j < uniqueResults.size(); j++) {
            String bTimeStr = uniqueResults.get(j)[0];
            Date bTime;
            try {
                bTime = HydroConstants.DATE_FORMAT.parse(bTimeStr);
                tsBasisTime[j] = bTime;
            } catch (ParseException e) {
                // TODO Auto-generated catch block
                e.printStackTrace();
            }            
        }
        
        /* knowing the actual start and end times for the multiple
          time series, loop thru the time series and adjust the start
          and end times so that they reflect the time span to use;
          i.e. there is no overlap.  THIS IS THE KEY STEP IN THE 
          PROCESS OF DEFINING AN AGGREGATE VIRTUAL TIME SERIES!!! */
        adjustStartEnd(uniqueResults.size(), tsBasisTime, tsStartTime, tsEndTime);
        
        
        return doKeep;
    }
    
    /**
     * This method uses the time series with the latest basis time first,
     * and uses it in its entirety.  Then the time series with the next
     * latest basis time is used.  If it overlaps portions of the already
     * saved time series, then only that portion which doesn't 
     * overlap is used.  This process continues until all time series
     * have been considered.  In essences, this method adjoins adjacent
     * time series.
     * 
     * @param basisTime
     *      Date[] of basis times
     * @param startValidTime
     *      Date[] of starting valid times
     * @param endValidTime
     *      Date[] of ending valid times
     */
    private static void adjustStartEnd(int ulCount, Date[] basisTime, Date[] startValidTime, Date[] endValidTime) {
        int[] basisOrder = new int[ulCount];
        Date tmpTime = Calendar.getInstance(TimeZone.getTimeZone("GMT")).getTime();
        int curIndex = 0;
        Date fullStartValidTime;
        Date fullEndValidTime;
        
        /* initialize array to keep track of order
           of the basis time series' */
        for (int i = 0; i < ulCount; i++) {
            basisOrder[i] = -1;
        }
        
        /* 
         * find the order of the time series by their latest basis time.
         * if two time series have the same basis time, use the one that
         * has the earlier starting time. note that the order is such
         * that the latest basis time is last in the resulting order array. 
         */
        for (int i = 0; i < ulCount; i++) {
            curIndex = 0;
            boolean found = false;
            
            for (int j = 0; j < ulCount; j++) {
                /* only consider the time series if it hasn't been accounted
                   for in the order array */
                found = false;
                for (int k = 0; k < i; k++) {
                    if (j == basisOrder[k]) {
                        found = true;
                        break;
                    }
                }
                if (!found) {
                    if (basisTime[j].after(tmpTime)) {
                        curIndex = j;
                        tmpTime = basisTime[j];
                    } else if (basisTime[j] == tmpTime) {
                        if (startValidTime[j].before(startValidTime[curIndex])) {
                            curIndex = j;
                            tmpTime = basisTime[j];
                        }
                    }
                }
            }
            basisOrder[i] = curIndex;
        }
        
        /* do NOT adjust the start and end time of the time series
           with the latest ending time.  loop through all the other
           time series and adjust their start and end times as necessary
           so that they do not overlap the time limits of the 
           being-built aggregate time series. */
        curIndex = basisOrder[0];
        
        fullStartValidTime = startValidTime[curIndex];
        fullEndValidTime = endValidTime[curIndex];
        
        for (int i = 1; i < ulCount; i++) {
            curIndex = basisOrder[i];
            /* each additional time series being considered is checked to 
               see if it falls outside the time window already encompassed
               by the assembled time series. there are four cases that can 
               occur; each is handled below. */
             
            /* if the basis time series being considered is fully within the
               time of the already existing time series, then 
               ignore it completely, and reset its times. */
            
            if (startValidTime[curIndex].after(fullStartValidTime) && endValidTime[curIndex].before(fullEndValidTime)) {
                startValidTime[curIndex].setTime(0);
                endValidTime[curIndex].setTime(0);
            } 
            
            
            else if (startValidTime[curIndex].before(fullStartValidTime) && endValidTime[curIndex].after(fullEndValidTime)) {
                /* if the basis time series being considered covers time both before
                   and after the existing time series, use the portion of it 
                   that is before the time series.  it is not desirable to use
                   both the before and after portion (this results in a 
                   non-contiguous time-series that is weird), and given a choice
                   it is better to use the forecast data early on than the 
                   later forecast data, so use the before portion */
                fullStartValidTime.setTime(fullStartValidTime.getTime() - 1000);
                endValidTime[curIndex] = fullStartValidTime;
                fullStartValidTime = startValidTime[curIndex];
            } else if (startValidTime[curIndex].before(fullStartValidTime) && endValidTime[curIndex].before(fullEndValidTime)) {
                /* if the basis time series being considered straddles the beginning
                   or is completely before the existing time series, then use the
                   portion of it that is before the time series. */
                fullStartValidTime.setTime(fullStartValidTime.getTime() - 1000);
                endValidTime[curIndex] = fullStartValidTime;
                fullStartValidTime = startValidTime[curIndex];
            } else if (startValidTime[curIndex].after(fullStartValidTime) && endValidTime[curIndex].after(fullEndValidTime)) {
                /* if the basis time series being considered straddles the end
                   or is completely after the existing time series, then use the
                   portion of it that is after the time series. */
                fullEndValidTime.setTime(fullEndValidTime.getTime() - 1000);
                startValidTime[curIndex] = fullEndValidTime;
                fullEndValidTime = endValidTime[curIndex];
            }
        }
    }
    
    /**
     * Loads the max fcst info into the RiverStatus table for the
     * current location and pe.
     * 
     * @param record
     *      Forecast record to load
     */
    public static void loadRiverStatus(Forecast record) throws VizException {
        Riverstatus rstatus = new Riverstatus();
        RiverstatusId rsid = new RiverstatusId();
        
        /* update the value in the RiverStatus table.  if no record
           currently in the table, then insert/put a new record.
           first build the record to update/insert */
        rsid.setLid(record.getLid());
        rsid.setPe(record.getPe());
        rsid.setTs(record.getTs());
        rstatus.setId(rsid);
        rstatus.setBasistime(record.getBasistime());
        rstatus.setDur((short)record.getDur());
        rstatus.setExtremum(record.getExtremum());
        rstatus.setProbability((float)record.getProbability());
        rstatus.setValidtime(record.getValidtime());
        rstatus.setValue(record.getValue());

        DirectDbQuery.saveOrUpdate(rstatus, HydroConstants.IHFS);
    }
    
    public static void deleteRiverStatus(String lid, String pe, String ts) throws VizException {
        String where = String.format(" WHERE lid='%s' AND pe='%s' AND ts='%s' ",
                lid, pe, ts);
        String query = "delete from RiverStatus " + where;
        
        DirectDbQuery.executeStatement(query, HydroConstants.IHFS, QueryLanguage.SQL);
    }
}
