/**
 * 
 * ScdParser
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
 * </pre>
 *
 * @author T. Lee
 * @version 1
 * 
 */
package gov.noaa.nws.ncep.edex.plugin.scd.util;

import java.util.Calendar;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.edex.decodertools.time.TimeTools;
import gov.noaa.nws.ncep.common.dataplugin.scd.ScdRecord;
import gov.noaa.nws.ncep.common.tools.IDecoderConstantsN;
import gov.noaa.nws.ncep.edex.util.UtilN;

public class ScdParser {
 
	/**
     * No argument constructor.
     */
    public ScdParser() {
    }
    
    /**
     * Decode the SCD report.
     * 
     * @param message
     * 			The raw SCD report.
     * @param record
     * 			SCD record.
     * 			
     */
    public static void processScd(String message, ScdRecord record) {
    	/*
    	 * Regular expression for SCD report, allowing two lines of report.
    	 */
    	final Pattern scdPattern = Pattern.compile(IDecoderConstantsN.SCD_REPORT);
    	Matcher m = scdPattern.matcher(message);    	
    	String scdreport = null;
    	Boolean suspectTimeFlag = false;
    	while (m.find()) {
    		scdreport = m.group().replaceAll("\r\r\n", " ").trim();
    		scdreport = UtilN.removeExtraWhiteSpaces(scdreport);

    		/*
    		 * Set raw report and station ID.
    		 */
    		record.setReport (scdreport);   	    		
    		String[] xxx = scdreport.split(" ", 20);
    		record.setStationID(xxx[0]);

    		/*
    		 * Set correction flag.
    		 */	 		
    		try {
    			if (!Character.isDigit(xxx[2].charAt(0))) {
    				record.setCorr(xxx[2]);
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
    		Calendar obs = TimeTools.copy(issue);
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
    				suspectTimeFlag = true;
    			}
    		} else {
    			if ( tdif <= MIN_HOUR ) {
    				/* do nothing */
    			} else if ( tdif >= MAX_HOUR ) {
    				obs.add(Calendar.DATE, 1);
    			} else if ( tdif > MIN_HOUR && tdif < MAX_HOUR ) {
    				suspectTimeFlag = true;
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
    				record.setCorr(wthr);
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
    		indx = scdreport.indexOf (" 8");
    		if ( indx >= 0 ) {
    			try {
    				record.setCFRT(Integer.parseInt(scdreport.substring(indx+2,indx+3)));
    			} catch (Exception e ) {
    				/* do nothing */
    			}
    			try {
    				record.setCFRL(Integer.parseInt(scdreport.substring(indx+3,indx+4)));
    			} catch (Exception e ) {
    				/* do nothing */
    			}
    			try {
    				record.setCTYL(Integer.parseInt(scdreport.substring(indx+4,indx+5)));
    			} catch (Exception e ) {
    				/* do nothing */
    			}

    			/*
    			 * If we have only high clouds, set cloud base height code from 0 to 9.
    			 */
    			try {
    				if ( scdreport.substring(indx+3,indx+7).equals("0000")) {
    					int cbas = 9;
    					record.setCBAS(cbas);
    				} else {
    					record.setCBAS(Integer.parseInt(scdreport.substring(indx+5,indx+6)));
    				}
    			} catch (Exception e ) {
    				/* do nothing */
    			}
    			try {
    				record.setCTYM(Integer.parseInt(scdreport.substring(indx+6,indx+7)));
    			} catch (Exception e ) {
    				/* do nothing */
    			}
    			try {
    				record.setCTYH(Integer.parseInt(scdreport.substring(indx+7,indx+8)));
    			} catch (Exception e) {
    				/* do nothing */
    			}
    		}

    		/*
    		 * Set depth of new snow.
    		 */
    		indx = scdreport.indexOf (" 931");
    		if ( indx >= 0 ) {
    			try {
    				int snew=Integer.parseInt(scdreport.substring(indx+4,indx+7));
    				record.setSNEW((float)(snew/10.F));
    			} catch (Exception e) {
    				/* do nothing */
    			}
    		}

    		/*
    		 * Set water equivalent of snow on the ground.
    		 */
    		indx = scdreport.indexOf (" 933");
    		if ( indx >= 0 ) {
    			try {
    				int weqs =Integer.parseInt(scdreport.substring(indx+4,indx+7));
    				record.setWEQS((float)(weqs/10.F));
    			} catch (Exception e) {
    				/* do nothing */
    			}	
    		}

    		/*
    		 * Depth of snow on the ground.
    		 */
    		indx = scdreport.indexOf (" 4/");
    		if ( indx >= 0 ) {
    			try { 
    				int snow=Integer.parseInt(scdreport.substring(indx+3,indx+6));
    				record.setSNOW((float)(snow/10.F));

    			} catch (Exception e) {
    				/* do nothing */
    			}
    		}			

    		/*
    		 * 24-h maximum and minimum temperatures.
    		 */
    		indx = scdreport.indexOf (" 40");
    		int indx1 = scdreport.indexOf (" 41");
    		if ( indx > 0 || indx1 > 0) {
    			try { 
    				if ( indx1 > 0 ) {
    					indx = indx1;
    				}
    				int sign=Integer.parseInt(scdreport.substring(indx+2,indx+3));
    				int tmax=Integer.parseInt(scdreport.substring(indx+3,indx+6));
    				if ( sign == 1 ) tmax = - tmax;
    				record.setTDXC((float)(tmax/10.F));
    				sign=Integer.parseInt(scdreport.substring(indx+6,indx+7));
    				int tmin=Integer.parseInt(scdreport.substring(indx+7,indx+10));
    				if ( sign == 1 ) tmin = - tmin;
    				record.setTDNC((float)(tmin/10.F));
    			} catch (Exception e) {
    				/* do nothing */
    			}
    		}

    		/*
    		 * 6-h precipitation amount in inches.
    		 */
    		indx = scdreport.indexOf (" 6");
    		if ( indx >= 0 ) {
    			try {
    				int p06i=Integer.parseInt(scdreport.substring(indx+2,indx+6));
    				record.setP06I((float)(p06i/100.F));
    			} catch (Exception e) {
    				/* do nothing */
    			}
    		}

    		/*
    		 * Duration of sunshine in minutes.
    		 */
    		indx = scdreport.indexOf (" 98");
    		if ( indx >= 0 ) {
    			try {
    				int msun=Integer.parseInt(scdreport.substring(indx+3,indx+6));
    				record.setMSUN(msun);
    			} catch (Exception e) {
    				/* do nothing */
    			}
    		}

    		/*
    		 * Calendar day total snowfall.
    		 */
    		indx = scdreport.indexOf (" 24/931");
    		if ( indx >= 0 ) {
    			try {
    				int s24i=Integer.parseInt(scdreport.substring(indx+7,indx+10));
    				record.setS24I((float)(s24i/10.F));
    			} catch (Exception e) {
    				/* do nothing */
    			}
    		}

    		/*
    		 * Calendar day total precipitation in inches.
    		 */
    		indx = scdreport.indexOf (" 7");
    		if ( indx >= 0 ) {
    			try {
    				int p24i=Integer.parseInt(scdreport.substring(indx+2,indx+6));
    				record.setP24I((float)(p24i/100.F));
    			} catch (Exception e) {
    				/* do nothing */
    			}
    		}
    	}
    }
}