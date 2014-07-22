/**
 * 
 * NcScdParser
 * 
 * This java class parses each field in SCD (Supplementary Climatological Data).
 *
 * <pre>
 * SOFTWARE HISTORY
 *
 * Date    		Ticket#		Engineer	Description
 * -------		-------  	--------	-----------
 * 12/2008		41			T. Lee		Created
 * 04/2009		41			T. Lee		Migrated to TO10
 * 07/2009		41			T. Lee		Migrated to TO11
 * 11/2009		41			T. Lee		Migrated to TO11D6
 * 06/2011		41			F. J. Yen	Renamed from ScdParser and converted to HDF5.
 * 										Changed type of suspectTimeFlag from Boolean to
 * 										String since undefined in PointDataDescription.
 * 09/2011      457         S. Gurung   Renamed H5 to Nc and h5 to nc
 * Jul 23, 2014 3410       bclement    location changed to floats
 * </pre>
 *
 * @author T. Lee
 * @version 1
 * 
 */
package gov.noaa.nws.ncep.edex.plugin.ncscd.util;

import gov.noaa.nws.ncep.common.dataplugin.ncscd.NcScdRecord;
import gov.noaa.nws.ncep.common.tools.IDecoderConstantsN;
import gov.noaa.nws.ncep.edex.util.UtilN;

import java.util.Calendar;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.raytheon.uf.common.pointdata.spatial.ObStation;
import com.raytheon.uf.common.pointdata.spatial.SurfaceObsLocation;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.edex.pointdata.spatial.ObStationDao;

public class NcScdParser {
 
	/**
     * No argument constructor.
     */
    public NcScdParser() {
    }
    
    private static ObStation getStationInfo(String icao) {
        ObStation station = null;

        ObStationDao dao = new ObStationDao();
        if (dao != null) {
        	try {
        		station = dao.queryByIcao(icao);
        	} catch (Exception e) {
        		//System.out.println("Error from dao.queryByIcao for "+icao);
        		return null;
        	}
        }

        return station;
    }

    /**
     * Decode the SCD report.
     * 
     * @param message
     * 			The raw SCD report.
     * @param record
     * 			NCSCD record.
     * 			
     */
    public static void processNcScd(String message, NcScdRecord record ) throws Exception {
    	/*
    	 * Regular expression for SCD report, allowing two lines of report.
    	 */
    	final Pattern scdPattern = Pattern.compile(IDecoderConstantsN.SCD_REPORT);
    	Matcher m = scdPattern.matcher(message);    	
    	String ncscdreport = null;
//    	Boolean suspectTimeFlag = false;
    	String suspectTimeFlag = "false";
    	while (m.find()) {
    		ncscdreport = m.group().replaceAll("\r\r\n", " ").trim();
    		ncscdreport = UtilN.removeExtraWhiteSpaces(ncscdreport);

    		/*
    		 * Set raw report and station ID.
    		 */
    		record.setReport (ncscdreport);   	    		
    		String[] xxx = ncscdreport.split(" ", 20);
    		
            String icao = xxx[0];
            ObStation station = getStationInfo(icao);
            
            if (station != null) {
                SurfaceObsLocation obsLoc = new SurfaceObsLocation(icao);
                float lat = (float) station.getGeometry().getY();
                float lon = (float) station.getGeometry().getX();
                obsLoc.assignLocation(lat, lon);
                obsLoc.setElevation(station.getElevation());

                record.setLocation(obsLoc);
                
            } else {
                // Couldn't find icao in spatial table
                throw new Exception( "Station id not found : "+icao );
            }

    		/*
    		 * Set correction flag.
    		 */	 		
    		try {
    			if (!Character.isDigit(xxx[2].charAt(0))) {
    				record.setCorIndicator(xxx[2]);
    			}
    		} catch (Exception e) {
				/* do nothing */
    		}

    		/*
    		 * Set date (dd) to observation time (hhmm) with issuance time (ddhhmm) as the
    		 * base time:  If the obs time is greater than the issuance time and the time
    		 * difference is greater than than or equal to 23h, e.g., obs time = 2315 and
    		 * issuance time = 250015, subtract one calendar day from the issuance time, i.e.,
    		 * obs time = 242315. If the time difference is between 1h to 23h, i.e.,
    		 * obs time = 0445 and issuance time = 240245, subtract a day from the issue
    		 * time too, i.e., obs time = 230445 but set the suspect time flag to TRUE.
    		 *
    		 * If the obs time is less than the issuance time but the time difference is
    		 * greater than or equal to 23h, e.g., obs time = 0015 and issuance time = 252345,
    		 * add one calendar day to the issuance time, i.e., obs time = 260015.  If the
    		 * time difference is great than 1h but less than 23h, e.g., obs time = 0245
    		 * and issuance time = 240445, use the same date but set the time flag to TRUE,
    		 * i.e., obs time = 240245.
    		 */
    		Calendar issue = record.getIssueTime();
            Calendar obs = TimeUtil.newCalendar(issue);
    		int indx;
    		if ( !Character.isDigit(xxx[2].charAt(0)) ) {
    			indx = 3;
    		} else {
    			indx = 2;
    		}
    		int obs_hour = Integer.parseInt(xxx[indx].substring(0,2));
    		int obs_min = Integer.parseInt(xxx[indx].substring(2,4));
    		obs.set(Calendar.HOUR_OF_DAY, obs_hour);
    		obs.set(Calendar.MINUTE, obs_min);
    		long tdif = Math.abs(obs.getTime().getTime() - issue.getTime().getTime());

    		final long MIN_HOUR = 60 * 60 * 1000;	    	
    		final long MAX_HOUR = 23 * MIN_HOUR;

    		if ( obs.compareTo(issue) > 0 ) {    			 
    			if ( tdif <= MIN_HOUR ) {
    				/* do nothing */
    			} else if ( tdif >= MAX_HOUR ) {
    				obs.add(Calendar.DATE, -1);
    			} else if ( tdif > MIN_HOUR && tdif < MAX_HOUR ) {
    				obs.add(Calendar.DATE, -1);
//   				  suspectTimeFlag = true;
    				suspectTimeFlag = "true";
    			}
    		} else {
    			if ( tdif <= MIN_HOUR ) {
    				/* do nothing */
    			} else if ( tdif >= MAX_HOUR ) {
    				obs.add(Calendar.DATE, 1);
    			} else if ( tdif > MIN_HOUR && tdif < MAX_HOUR ) {
//    				suspectTimeFlag = true;
    				suspectTimeFlag = "true";
    			}                 
    		}
    		if ( obs != null ) {
    			record.setSuspectTimeFlag(suspectTimeFlag);
    			record.setObsTime(obs);	
    			DataTime dt = new DataTime (obs);
    			record.setDataTime(dt);
    		}       	

    		/*
    		 * Parse and set weather.  The algorithm is able to handle multiple weather reports,
    		 * e.g., R+ SNOW IP- ...
    		 */
    		String wthr="";
    		try {
    			if ( !Character.isDigit(xxx[2].charAt(0)) ) {
    				indx = 4;
    			} else {
    				indx = 3;
    			}
    			int start = indx;
    			int counter=0;
    			try {
    				while (!(Character.isDigit(xxx[indx].charAt(0))) ) {
    					counter++;
    					indx++;
    				}
    			} catch (ArrayIndexOutOfBoundsException e) {
    				/* do nothing */
    			}
    			while ( counter > 0 ) {
    				wthr = wthr + xxx[start]+ " ";
    				counter--;
    				start++;
    			}
    			/*
    			 * Check for rogue report.
    			 */
    			if (wthr.contains("COR") || wthr.contains("RTD") || wthr.contains("AMD")) { 
    				record.setCorIndicator(wthr);
    			} else {
    				record.setWTHR (wthr.trim());
    			}
    		}
    		catch (ArrayIndexOutOfBoundsException e2 ) {
    			/* do nothing */
    		}

    		/*
    		 * Parse cloud group, 8NNhClhCmCh.
    		 */
    		indx = ncscdreport.indexOf (" 8");
    		if ( indx >= 0 ) {
    			record.setCFRT(Integer.parseInt(ncscdreport.substring(indx+2,indx+3)));

    			record.setCFRL(Integer.parseInt(ncscdreport.substring(indx+3,indx+4)));


    			record.setCTYL(Integer.parseInt(ncscdreport.substring(indx+4,indx+5)));

    			/*
    			 * If we have only high clouds, set cloud base height code from 0 to 9.
    			 */
    			if ( ncscdreport.substring(indx+3,indx+7).equals("0000")) {
    				int cbas = 9;
    				record.setCBAS(cbas);
    			} else {
    				record.setCBAS(Integer.parseInt(ncscdreport.substring(indx+5,indx+6)));
    			}

    			record.setCTYM(Integer.parseInt(ncscdreport.substring(indx+6,indx+7)));
    			
    			record.setCTYH(Integer.parseInt(ncscdreport.substring(indx+7,indx+8)));    			
    		}

    		/*
    		 * Set depth of new snow.
    		 */
    		indx = ncscdreport.indexOf (" 931");
    		if ( indx >= 0 ) {
    			try {
    				int snew=Integer.parseInt(ncscdreport.substring(indx+4,indx+7));
    				record.setSNEW((float)(snew/10.F));
    			} catch (Exception e) {
    				/* do nothing */
    			}
    		}

    		/*
    		 * Set water equivalent of snow on the ground.
    		 */
    		indx = ncscdreport.indexOf (" 933");
    		if ( indx >= 0 ) {
    			try {
    				int weqs =Integer.parseInt(ncscdreport.substring(indx+4,indx+7));
    				record.setWEQS((float)(weqs/10.F));
    			} catch (Exception e) {
    				/* do nothing */
    			}	
    		}

    		/*
    		 * Depth of snow on the ground.
    		 */
    		indx = ncscdreport.indexOf (" 4/");
    		if ( indx >= 0 ) {
    			try { 
    				int snow=Integer.parseInt(ncscdreport.substring(indx+3,indx+6));
    				record.setSNOW((float)(snow/10.F));

    			} catch (Exception e) {
    				/* do nothing */
    			}
    		}			

    		/*
    		 * 24-h maximum and minimum temperatures.
    		 */
    		indx = ncscdreport.indexOf (" 40");
    		int indx1 = ncscdreport.indexOf (" 41");
    		if ( indx > 0 || indx1 > 0) {
    			try { 
    				if ( indx1 > 0 ) {
    					indx = indx1;
    				}
    				int sign=Integer.parseInt(ncscdreport.substring(indx+2,indx+3));
    				int tmax=Integer.parseInt(ncscdreport.substring(indx+3,indx+6));
    				if ( sign == 1 ) tmax = - tmax;
    				record.setTDXC((float)(tmax/10.F));
    				sign=Integer.parseInt(ncscdreport.substring(indx+6,indx+7));
    				int tmin=Integer.parseInt(ncscdreport.substring(indx+7,indx+10));
    				if ( sign == 1 ) tmin = - tmin;
    				record.setTDNC((float)(tmin/10.F));
    			} catch (Exception e) {
    				/* do nothing */
    			}
    		}

    		/*
    		 * 6-h precipitation amount in inches.
    		 */
    		indx = ncscdreport.indexOf (" 6");
    		if ( indx >= 0 ) {
    			try {
    				int p06i=Integer.parseInt(ncscdreport.substring(indx+2,indx+6));
    				record.setP06I((float)(p06i/100.F));
    			} catch (Exception e) {
    				/* do nothing */
    			}
    		}

    		/*
    		 * Duration of sunshine in minutes.
    		 */
    		indx = ncscdreport.indexOf (" 98");
    		if ( indx >= 0 ) {
    			try {
    				int msun=Integer.parseInt(ncscdreport.substring(indx+3,indx+6));
    				record.setMSUN(msun);
    			} catch (Exception e) {
    				/* do nothing */
    			}
    		}

    		/*
    		 * Calendar day total snowfall.
    		 */
    		indx = ncscdreport.indexOf (" 24/931");
    		if ( indx >= 0 ) {
    			try {
    				int s24i=Integer.parseInt(ncscdreport.substring(indx+7,indx+10));
    				record.setS24I((float)(s24i/10.F));
    			} catch (Exception e) {
    				/* do nothing */
    			}
    		}

    		/*
    		 * Calendar day total precipitation in inches.
    		 */
    		indx = ncscdreport.indexOf (" 7");
    		if ( indx >= 0 ) {
    			try {
    				int p24i=Integer.parseInt(ncscdreport.substring(indx+2,indx+6));
    				record.setP24I((float)(p24i/100.F));
    			} catch (Exception e) {
    				/* do nothing */
    			}
    		}
    	}
    }
}