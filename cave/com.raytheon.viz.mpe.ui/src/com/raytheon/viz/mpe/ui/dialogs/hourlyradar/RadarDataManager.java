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
package com.raytheon.viz.mpe.ui.dialogs.hourlyradar;

import java.util.Calendar;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.List;

import com.raytheon.uf.common.ohd.AppsDefaults;
import com.raytheon.uf.viz.core.catalog.DirectDbQuery;
import com.raytheon.uf.viz.core.catalog.DirectDbQuery.QueryLanguage;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.hydrocommon.HydroConstants;

/**
 * Review Hourly Radar Data Access.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 28, 2009 2675       mpduff      Initial creation
 * Aug 13, 2009 2675       mpduff      TIM changes added
 * Aug 11, 2015 4500       rjpeter     Fix type casts.
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class RadarDataManager {
    private static RadarDataManager instance = null;

    /**
     * ngrd = number of HRAP bins in 230 km maximum value allowed = 65 SJU (San
     * Juan) would have value = 69 without check on max value
     */
    private int ngrd = -999;

    /** Private constructor */
    private RadarDataManager() {

    }

    /**
     * Get an instance of this class.
     * 
     * @return The instance of the class
     */
    public static synchronized RadarDataManager getInstance() {
        if (instance == null) {
            instance = new RadarDataManager();
        }

        return instance;
    }

    /**
     * Get the DPA filename.
     * 
     * @param radId
     *            The radar id
     * @param dtg
     *            The obstime for the record and the associated file name
     * @return The filename
     * @throws VizException
     */
    public String getDPAFileName(String radId, Date dtg) throws VizException {
        String filename = null;
 
        String where = " where radId = '" + radId + "' and obstime = '"
                + HydroConstants.DATE_FORMAT.format(dtg) + "'";
        String query = "select grid_filename from dpaRadar " + where;
        System.out.println(query);
        List<Object[]> rs = DirectDbQuery.executeQuery(query,
                HydroConstants.IHFS, QueryLanguage.SQL);

        if (rs.size() > 0) {
            filename = (String) rs.get(0)[0];
            System.out.println("DPA filename: " + filename);
        }

        return filename;
    }

    
    /**
     * Get the DAA filename.
     * 
     * @param radId
     *            The radar id
     * @param dtg
     *            The obstime for the record and the associated file name
     * @return The filename
     * @throws VizException
     */
    public String getDAAFileName(String radId, Date dtg) throws VizException {
        String filename = null;
 
        String where = " where radId = '" + radId + "' and obstime = '"
                + HydroConstants.DATE_FORMAT.format(dtg) + "'";
        String query = "select grid_filename from daaRadar " + where;
        System.out.println(query);
        List<Object[]> rs = DirectDbQuery.executeQuery(query,
                HydroConstants.IHFS, QueryLanguage.SQL);

        if (rs.size() > 0) {
            filename = (String) rs.get(0)[0];
            System.out.println("DAA filename: " + filename);
        }

        return filename;
    }
    
    /**
     * Get the closest obstime to the TOH (top of hour) from either the DpaRadar table or the DaaRadar table,
     * within the time window.
     * @param topOfHourDateTime
     *            The current time looking for
     * @return The best obstime for topOfHourDateTime
     * @throws VizException
     */

    public Date getClosestObstimeToTOH(String radId, Date topOfHourDateTime, String tableName)
        
    {
    	AppsDefaults ad = AppsDefaults.getInstance();
    	
    	int timeWindow = 0;
    	
    	if (tableName.equalsIgnoreCase("DpaRadar"))
    	{
    		timeWindow = ad.getInt("dpa_wind", 10);
    	}
    	else if (tableName.equalsIgnoreCase("DaaRadar"))
    	{
    		timeWindow = ad.getInt("daa_wind", 10);
    	}
    	else
    	{
    		throw(new Error("You must pass in either DaaRadar or DpaRadar"));
    	}
    	
    	
        Date beforeTime = getLatestObstimeBeforeTOH(radId, topOfHourDateTime, tableName);
        Date atOrAfterTime = getEarliestObstimeOnOrAfterTOH(radId, topOfHourDateTime, tableName);
            
        Date bestObstime = getBestObsTime(topOfHourDateTime, beforeTime, atOrAfterTime, timeWindow);

        return bestObstime;
    }
 
    public Date getObstimeAtTOH(String radId, Date topOfHourDateTime, String tableName)
    {
    	   //TableName can be either "dparadar" or  "daaradar"
    	
    	  Date bestObstime = null;

          // Get the next hour
          Calendar cal = new GregorianCalendar();
          cal.setTime(topOfHourDateTime);
      //    cal.add(Calendar.HOUR, 1);

          // Query for the latest obstime less than the next hour
          String where = " where obstime = '"
                  + HydroConstants.DATE_FORMAT.format(cal.getTime())
                  + "' and radId = '" + radId + "'";
          String query = "select obstime from " + tableName + " " + where;

          try
          {

        	  List<Object[]> rs = DirectDbQuery.executeQuery(query,
        			  HydroConstants.IHFS, QueryLanguage.SQL);

        	  if (rs.size() > 0)
        	  {
        		  bestObstime = topOfHourDateTime;
        	  }
        	  else
        	  {
        		  bestObstime = null;
        	  }

          }
          
          catch(VizException e)
          {
        	  e.printStackTrace();
        	  bestObstime = null;
          }

          return bestObstime;
      
    }
    
    public Date getLatestObstimeBeforeTOH(String radId, Date topOfHourDateTime, String tableName)
    {
        Date latestObsTime = null;

        // Get the next hour
        Calendar topOfHourCalendar = new GregorianCalendar();
        topOfHourCalendar.setTime(topOfHourDateTime);
        
        Calendar previousTopOfHourCalendar = new GregorianCalendar();
        previousTopOfHourCalendar.setTime(topOfHourDateTime);
        previousTopOfHourCalendar.add(Calendar.HOUR, -1);

        // Query for the latest obstime less than the next hour
        String where = " where obstime < '"
                + HydroConstants.DATE_FORMAT.format(topOfHourCalendar.getTime()) 
                + "' AND  obstime >= '"
                + HydroConstants.DATE_FORMAT.format(previousTopOfHourCalendar.getTime())
                + "' AND radId = '" + radId + "'";
        
        String query = "select max(obstime) from " + tableName + " " + where;

        
        try
        {

        	List<Object[]> rs = DirectDbQuery.executeQuery(query,
        			HydroConstants.IHFS, QueryLanguage.SQL);

        	if (rs.size() > 0) {
        		latestObsTime = (Date) rs.get(0)[0];
        	}      	
        	else
        	{
        		latestObsTime = null;
        	}
        		
        }
        catch (VizException e)
        {
        	e.printStackTrace();
        	latestObsTime = null;
        }

        return latestObsTime;
    }
    
    public Date getEarliestObstimeOnOrAfterTOH(String radId, Date topOfHourDateTime, String tableName)
    {
        Date earliestObsTime = null;

        // Get the next hour
        Calendar topOfHourCalendar = new GregorianCalendar();
        topOfHourCalendar.setTime(topOfHourDateTime);
        
        Calendar nextTopOfHourCalendar = new GregorianCalendar();
        nextTopOfHourCalendar.setTime(topOfHourDateTime);
        nextTopOfHourCalendar.add(Calendar.HOUR, 1);

        // Query for the latest obstime less than the next hour
        String where = " where obstime >= '"
                + HydroConstants.DATE_FORMAT.format(topOfHourCalendar.getTime()) 
                + "' AND  obstime < '"
                + HydroConstants.DATE_FORMAT.format(nextTopOfHourCalendar.getTime())
                + "' AND radId = '" + radId + "'";
        
        String query = "select min(obstime) from " + tableName + " " + where;

        
        try
        {

        	List<Object[]> rs = DirectDbQuery.executeQuery(query,
        			HydroConstants.IHFS, QueryLanguage.SQL);

        	if (rs.size() > 0) {
        		earliestObsTime = (Date) rs.get(0)[0];
        	}      	
        	else
        	{
        		earliestObsTime = null;
        	}
        		
        }
        catch (VizException e)
        {
        	e.printStackTrace();
        	earliestObsTime = null;
        }

        return earliestObsTime;
    }
    
    
    private Date getBestObsTime(Date topOfHourDateTime, 
    							Date beforeTOHDateTime,
    							Date atOrAfterTOHDateTime,
    							int timeWindowInMinutes)
    {
    	Date bestDateTime = null;
    	
    	final long MILLIS_PER_MINUTE = 1000 * 60;
    	
    	long startTime = topOfHourDateTime.getTime() - timeWindowInMinutes * MILLIS_PER_MINUTE;
    	long endTime = topOfHourDateTime.getTime() + timeWindowInMinutes * MILLIS_PER_MINUTE;
    	
    	long topOfHourTime = topOfHourDateTime.getTime();
    	long atOrAfterTime = -1;
    	long beforeTime = -1;
    	
    	long millisAfter = -1;
    	long millisBefore = -1;
    	
 
    	//check the obstime that is at or after the TOH
    	if (atOrAfterTOHDateTime != null )
    	{
    		atOrAfterTime = atOrAfterTOHDateTime.getTime();
    		
    		//Check the time window
    		if ( (atOrAfterTime >= topOfHourTime)  && (atOrAfterTime <= endTime) )
    		{
    			millisAfter = atOrAfterTime - topOfHourTime;
    			
    			//Check for exactly TOH
        		if (millisAfter == 0)
        		{
        			return topOfHourDateTime; //found record exactly at TOH
        		}
    		}
    	}
    	
    	
    	if ( beforeTOHDateTime != null)
    	{
    		beforeTime = beforeTOHDateTime.getTime();
	
    		if ( (beforeTime >= startTime)  && (beforeTime < topOfHourTime) )
    		{
    			millisBefore = topOfHourTime - beforeTime;
    		}
    	}
    	
    	
    	
    	//both valid 
    	if ( (millisAfter >= 0)  && (millisBefore > 0 ) )
    	{
    		if (millisAfter <= millisBefore)
    		{
    			//after time wins
    			bestDateTime = atOrAfterTOHDateTime;
    		}
    		else
    		{
    			//before time wins
    			bestDateTime = beforeTOHDateTime;
    		}
    		
    	}
    	
    	//before is null
    	else if (millisAfter >= 0)
    	{
    		bestDateTime = atOrAfterTOHDateTime;
    	}
    	else if (millisBefore > 0)
    	{
    		bestDateTime  = beforeTOHDateTime;
    	}
    	
    	else //There are no valid records within the time window
    	{
    		
    		bestDateTime = null;
    	}
    	
    	
    	return bestDateTime;
    }
    
    


    /**
     * Get the latest obstime from the DpaRadar table.
     * 
     * @param date
     *            The current time looking for
     * @return The max obstime for that date
     * @throws VizException
     */
    
    /*
     * this function was previously used (erroneously) to find the date/time for a search
     * in the RWRadarResult table for the 4 panel window
     * it is not used anywhere else
     * PST - 1/10/14
     */
    public Date getLatestObstimeDpaRadar(String radId, Date date)
            throws VizException {
        Date latest = null;

        // Get the next hour
        Calendar cal = new GregorianCalendar();
        cal.setTime(date);
        cal.add(Calendar.HOUR, 1);

        // Query for the latest obstime less than the next hour
        String where = " where obstime < '"
                + HydroConstants.DATE_FORMAT.format(cal.getTime())
                + "' and radId = '" + radId + "'";
        String query = "select max(obstime) from dparadar" + where;

        List<Object[]> rs = DirectDbQuery.executeQuery(query,
                HydroConstants.IHFS, QueryLanguage.SQL);

        if (rs.size() > 0) {
            latest = (Date) rs.get(0)[0];
        } else {
            latest = cal.getTime();
        }
        if (latest == null) {
            latest = cal.getTime();
        }

        return latest;
    }

    /**
     * Get the number of grid cells on each side of the radar site
     * 
     * @param radId
     *            The radar id
     * @return The number of grid cells above, below and to the sides of the
     *         radar site
     * @throws VizException
     */
    public int getNgrd(String radId) throws VizException {
        if ((radId != null) && (radId.length() > 0)) {
            if (ngrd == -999) {
                String query = "select lat, lon from radarloc "
                        + "where use_radar = 'T' and radid = '" + radId + "'";

                List<Object[]> rs = DirectDbQuery.executeQuery(query,
                        HydroConstants.IHFS, QueryLanguage.SQL);

                Object[] oa = rs.get(0);
                double lat = (Double) oa[0];

                double z = 4.7625 / ((1 + Math.sin((Math.PI * 60 / 180))) / (1 + Math
                        .sin((Math.PI * lat / 180.))));
                ngrd = (int) (230 / z + 1);
                if (ngrd > 65) {
                    ngrd = 65;
                }
            }
        }

        return ngrd;
    }

    /**
     * Get the latitude/longitude for the given site.
     * 
     * @param radId
     *            The radar id
     * @return the lat/lon in a double array
     */
    public double[] getLatLon(String radId) {
        double[] latlon = new double[2];

        String query = "select lat, lon from radarloc where "
                + "use_radar='T' and radid = '" + radId + "'";

        try {
            List<Object[]> rs = DirectDbQuery.executeQuery(query.toString(),
                    HydroConstants.IHFS, QueryLanguage.SQL);

            Object[] arr = rs.get(0);
            latlon[0] = (Double) arr[1];
            latlon[0] *= -1;
            latlon[1] = (Double) arr[0];

        } catch (VizException e) {
            e.printStackTrace();
            latlon[0] = -999;
            latlon[1] = -999;
        }

        return latlon;
    }

    /**
     * Get the ignore_radar value in IHFS.
     * 
     * @param radId
     *            The Radar ID
     * @param dpaDate
     *            The data date
     * @return true if ignore_radar set to 'y'
     * @throws VizException
     */
    public boolean getIgnoreRadarSP(String radId, Date dpaDate)
            throws VizException {
        boolean ignore = false;

        // need date to be to the current hour
        Calendar cal = new GregorianCalendar();
        cal.setTime(dpaDate);
        cal.set(Calendar.MINUTE, 0);
        cal.set(Calendar.SECOND, 0);
        dpaDate = cal.getTime();

        final String where = "WHERE radid='" + radId + "' AND obstime='"
                + HydroConstants.DATE_FORMAT.format(dpaDate) + "'";
        final String query = "select ignore_radar from rwRadarResult ";

        //System.out.printf("getIgnoreRadarSP: query = %s %s\n" ,query, where);
        
        List<Object[]> rs = DirectDbQuery.executeQuery(query + where,
                HydroConstants.IHFS, QueryLanguage.SQL);

        if (rs.size() > 0) {
            String value = (String) rs.get(0)[0];
            if (value.equalsIgnoreCase("Y")) {
                ignore = true;
            }
        }

        return ignore;
    }

    /**
     * Get the ignore_radar value in IHFS.
     * 
     * @param radId
     *            The Radar ID
     * @param dpaDate
     *            The data date
     * @return true if ignore_radar set to 'y'
     * @throws VizException
     */
    public boolean getIgnoreRadarDP(String radId, Date daaDate)
            throws VizException {
        boolean ignore = false;

        // need date to be to the current hour
        Calendar cal = new GregorianCalendar();
        cal.setTime(daaDate);
        cal.set(Calendar.MINUTE, 0);
        cal.set(Calendar.SECOND, 0);
        daaDate = cal.getTime();

        final String where = "WHERE radid='" + radId + "' AND obstime='"
                + HydroConstants.DATE_FORMAT.format(daaDate) + "'";
        final String query = "select ignore_radar from DAARadarResult ";
      
        //System.out.printf("getIgnoreRadarDP: query = %s %s\n" ,query, where);
        
        List<Object[]> rs = DirectDbQuery.executeQuery(query + " " + where,
                HydroConstants.IHFS, QueryLanguage.SQL);

        if (rs.size() > 0) {
            String value = (String) rs.get(0)[0];
            if (value.equalsIgnoreCase("Y")) {
                ignore = true;
            }
        }

        return ignore;
    }

    /**
     * Update the ignore_radar field in rwRadarResult table in IHFS.
     * 
     * @param radId
     *            The radar id field
     * @param ignoreRadar
     *            true to ignore radar
     * @return number of rows modified
     * @throws VizException
     */
    public int updateIgnoreRadarSP(String radId, Date dpaDate, boolean ignoreRadar)
            throws VizException {
        int status = 1;

        // need date to be to the current hour
        Calendar cal = new GregorianCalendar();
        cal.setTime(dpaDate);
        cal.set(Calendar.MINUTE, 0);
        cal.set(Calendar.SECOND, 0);
        dpaDate = cal.getTime();

        final String where = "WHERE radid='" + radId + "' AND obstime='"
                + HydroConstants.DATE_FORMAT.format(dpaDate) + "'";

        String ignore = null;
        if (ignoreRadar) {
            ignore = "y";
        } else {
            ignore = "n";
        }

        final String sql = "update rwRadarResult set ignore_radar = '" + ignore
                + "' ";

        //System.out.printf("updateIgnoreRadarSP: query = %s %s\n" ,sql, where);
        
        status = DirectDbQuery.executeStatement(sql + where,
                HydroConstants.IHFS, QueryLanguage.SQL);

        return status;
    }

    /**
     * Update the ignore_radar field in DAARadarResult table in IHFS.
     * 
     * @param radId
     *            The radar id field
     * @param ignoreRadar
     *            true to ignore radar
     * @return number of rows modified
     * @throws VizException
     */
    public int updateIgnoreRadarDP(String radId, Date daaDate, boolean ignoreRadar)
            throws VizException {
        int status = 1;

        // need date to be to the current hour
        Calendar cal = new GregorianCalendar();
        cal.setTime(daaDate);
        cal.set(Calendar.MINUTE, 0);
        cal.set(Calendar.SECOND, 0);
        daaDate = cal.getTime();

        final String where = "WHERE radid='" + radId + "' AND obstime='"
                + HydroConstants.DATE_FORMAT.format(daaDate) + "'";

        String ignore = null;
        if (ignoreRadar) {
            ignore = "y";
        } else {
            ignore = "n";
        }

        final String sql = "update DAARadarResult set ignore_radar = '" + ignore
                + "' ";

        //System.out.printf("updateIgnoreRadarDP: query = %s %s\n" ,sql, where);

        status = DirectDbQuery.executeStatement(sql + where,
                HydroConstants.IHFS, QueryLanguage.SQL);

        return status;
    }

    /**
     * Get the Adaptable Parameters from the dpaAdapt table in IHFS.
     * 
     * @param radId
     *            The Radar ID
     * @param dtg
     *            The DPA date/time group
     * @return Object[] of data, null if no data or problem
     * @throws VizException
     */
    public DPAAdaptableParam getAdaptableParameters(String radId, Date dtg)
            throws VizException {
        DPAAdaptableParam data = null;
        String query = "select min_reflth, max_reflth, ref_tltest, rng_tltin, rng_tltout, max_birng,"
                + "min_birng, min_echoar, min_awrefl, max_pctred, mlt_zrcoef, pwr_zrcoef, min_zrefl,"
                + "max_zrefl, max_stmspd, max_timdif, min_artcon, tim_p1cont, tim_p2cont, max_ecarch,"
                + "rng_cutoff, rng_e1coef, rng_e2coef, rng_e3coef, min_prate, max_prate, tim_restrt,"
                + "max_timint, min_timprd, thr_hlyout, end_timgag, max_prdval, max_hlyval, tim_biest,"
                + "thr_nosets, res_bias, longest_lag, bias_applied from dpaAdapt ";
        String where = " WHERE radid='" + radId + "' and obstime='"
                + HydroConstants.DATE_FORMAT.format(dtg) + "'";

        List<Object[]> rs = DirectDbQuery.executeQuery(query + where,
                HydroConstants.IHFS, QueryLanguage.SQL);

        if (rs.size() > 0) {
            Object[] retArr = rs.get(0);
            data = new DPAAdaptableParam();
            data.setMin_reflth(((Number) retArr[0]).floatValue());
            data.setMax_reflth(((Number) retArr[1]).floatValue());
            data.setRef_tltest(((Number) retArr[2]).floatValue());
            data.setRng_tltin(((Number) retArr[3]).floatValue());
            data.setRng_tltout(((Number) retArr[4]).floatValue());
            data.setMax_birng(((Number) retArr[5]).floatValue());
            data.setMin_birng(((Number) retArr[6]).floatValue());
            data.setMin_echoar(((Number) retArr[7]).floatValue());
            data.setMin_awrefl(((Number) retArr[8]).floatValue());
            data.setMax_pctred(((Number) retArr[9]).floatValue());
            data.setMlt_zrcoef(((Number) retArr[10]).floatValue());
            data.setPwr_zrcoef(((Number) retArr[11]).floatValue());
            data.setMin_zrefl(((Number) retArr[12]).floatValue());
            data.setMax_zrefl(((Number) retArr[13]).floatValue());
            data.setMax_stmspd(((Number) retArr[14]).floatValue());
            data.setMax_timdif(((Number) retArr[15]).floatValue());
            data.setMin_artcon(((Number) retArr[16]).floatValue());
            data.setTim_p1cont(((Number) retArr[17]).floatValue());
            data.setTim_p2cont(((Number) retArr[18]).floatValue());
            data.setMax_ecarch(((Number) retArr[19]).floatValue());
            data.setRng_cutoff(((Number) retArr[20]).floatValue());
            data.setRng_e1coef(((Number) retArr[21]).floatValue());
            data.setRng_e2coef(((Number) retArr[22]).floatValue());
            data.setRng_e3coef(((Number) retArr[23]).floatValue());
            data.setMin_prate(((Number) retArr[24]).floatValue());
            data.setMax_prate(((Number) retArr[25]).floatValue());
            data.setTim_restrt(((Number) retArr[26]).floatValue());
            data.setMax_timint(((Number) retArr[27]).floatValue());
            data.setMin_timprd(((Number) retArr[28]).floatValue());
            data.setThr_hlyout(((Number) retArr[29]).floatValue());
            data.setEnd_timgag(((Number) retArr[30]).floatValue());
            data.setMax_prdval(((Number) retArr[31]).floatValue());
            data.setMax_hlyval(((Number) retArr[32]).floatValue());
            data.setTim_biest(((Number) retArr[33]).floatValue());
            data.setThr_nosets(((Number) retArr[34]).floatValue());
            data.setRes_bias(((Number) retArr[35]).floatValue());
            data.setLongest_lag(((Number) retArr[36]).floatValue());
            data.setBias_applied(String.valueOf(retArr[37]));
        }
        return data;
    }

    /**
     * Get the supplemental data from the dpaRadar table in IHFS.
     * 
     * @param radId
     *            The Radar ID
     * @param dtg
     *            The DPA date/time group
     * @return Object[] of data, null if no data or problem
     * @throws VizException
     */
    public DPASupplementalData getSupplementalData(String radId, Date dtg)
            throws VizException {
        DPASupplementalData data = null;
        String query = "select nisolbin, noutint, noutrep, nbadscan, nhourout, volcovpat, "
                + "opermode, areared, biscanr, supplmess, minoff, maxvald, maxvalh, s1_bias_value, "
                + "producttime from dpaRadar where radid = '"
                + radId
                + "' "
                + " AND obstime = '"
                + HydroConstants.DATE_FORMAT.format(dtg)
                + "'";

        List<Object[]> rs = DirectDbQuery.executeQuery(query,
                HydroConstants.IHFS, QueryLanguage.SQL);
        if (rs.size() > 0) {
            Object[] retArr = rs.get(0);
            data = new DPASupplementalData();
            data.setNisolbin(((Number) retArr[0]).intValue());
            data.setNoutint(((Number) retArr[1]).intValue());
            data.setNoutrep(((Number) retArr[2]).intValue());
            data.setNbadscan(((Number) retArr[3]).intValue());
            data.setNhourout(((Number) retArr[4]).intValue());
            data.setVolcovpat(((Number) retArr[5]).intValue());
            data.setOpermode(((Number) retArr[6]).intValue());
            data.setAreared(((Number) retArr[7]).floatValue());
            data.setBiscanr(((Number) retArr[8]).floatValue());
            data.setSupplmess(((Number) retArr[9]).intValue());
            data.setMinoff(((Number) retArr[10]).intValue());
            data.setMaxvald(((Number) retArr[11]).floatValue());
            data.setMaxvalh(((Number) retArr[12]).floatValue());
            data.setS1_bias_value(((Number) retArr[13]).floatValue());
            data.setProducttime((Date) retArr[14]);
        }
        return data;
    }

    /**
     * Get the rad_avail value from RWRadarResult table
     * 
     * @param radId
     *            The Radar ID
     * @param dpaDate
     *            The data date
	 *  @return int  0 = available with > 0 precip 
	 *				 1 = not available
	 *			     2 = available with all precip == 0
     */
    public int getAvailableRadarSP(String radId, Date dpaDate)
            throws VizException {

        // need date to be to the current hour
        Calendar cal = new GregorianCalendar();
        cal.setTime(dpaDate);
        cal.set(Calendar.MINUTE, 0);
        cal.set(Calendar.SECOND, 0);
        dpaDate = cal.getTime();

        final String where = "WHERE radid='" + radId + "' AND obstime='"
                + HydroConstants.DATE_FORMAT.format(dpaDate) + "'";
        final String query = "select rad_avail from rwRadarResult ";

        System.out.printf("getAvailableRadarSP: query = %s %s\n" ,query, where);
        List<Object[]> rs = DirectDbQuery.executeQuery(query + where,
                HydroConstants.IHFS, QueryLanguage.SQL);

        // if record not found, then rs.size() = 0
        int avail = 1;
        if (rs.size() > 0)
        {
            String value = (String) rs.get(0)[0];
            
            if ("y".equals(value)) {
                avail = 0;
            } else if ("z".equals(value)) {
                avail = 2;
            }
        }

        return avail;
    }
//---------------------------------------------
    /**
     * Get the rad_avail value from the DAARadarResult table
     * 
     * @param radId
     *            The Radar ID
     * @param dpaDate
     *            The data date
 	 * @return int  0 = available with > 0 precip 
	 *			    1 = not available
	 *			    2 = available with all precip == 0
     */
    public int getAvailableRadarDP(String radId, Date daaDate)
            throws VizException {

        // need date to be to the current hour
        Calendar cal = new GregorianCalendar();
        cal.setTime(daaDate);
        cal.set(Calendar.MINUTE, 0);
        cal.set(Calendar.SECOND, 0);
        daaDate = cal.getTime();

        final String where = "WHERE radid='" + radId + "' AND obstime='"
                + HydroConstants.DATE_FORMAT.format(daaDate) + "'";
        final String query = "select rad_avail from DAARadarResult ";

        System.out.printf("getAvailableRadarDP: query = %s %s\n" ,query, where);
        List<Object[]> rs = DirectDbQuery.executeQuery(query + where,
                HydroConstants.IHFS, QueryLanguage.SQL);

        // if record not found, then rs.size() = 0
        int avail = 1;
        if (rs.size() > 0)
        {
            String value = (String) rs.get(0)[0];
            
            if ("y".equals(value)) {
                avail = 0;
            } else if ("z".equals(value)) {
                avail = 2;
            }
        }

        return avail;
    }

}
