/*
 * 
 * FfgParser
 * 
 * This java class parses FFG (Flash Flood Guidance) data.
 *  
 * <pre>
 * SOFTWARE HISTORY
 *
 * Date         Ticket#         Engineer    Description
 * ------------ ----------      ----------- --------------------------
 * 03/2009		14				T. Lee		Created and re-factored for TO10
 * 07/2009		14				T. Lee		Migration to TO11
 * 11/2009		14				T. Lee		Migration to TO11D6
 * </pre>
 *
 * @author T.Lee
 * @version 1.0
 */

package gov.noaa.nws.ncep.edex.plugin.ffg.util;

import gov.noaa.nws.ncep.common.dataplugin.ffg.FfgPrecip;
import gov.noaa.nws.ncep.common.dataplugin.ffg.FfgRecord;
import gov.noaa.nws.ncep.common.tools.IDecoderConstantsN;
import gov.noaa.nws.ncep.edex.util.UtilN;

import java.util.Calendar;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.wmo.WMOHeader;

public class FfgParser {
	
    /**
     * No arg constructor
     */    
    public FfgParser() {
    }

    /**
     * Extracts the AWIPS identifier from the FFG bulletin
     * 
     * @param message The raw message
     * @return the awips_id
     */
    public static String processAwipsID(String message) {
    	
    	/*
    	 * Regular expression for AWIPS identifier
    	 */
    	final String AWIPS_ID = "FFG[A-Z1-3]{2}.\\x0d\\x0d\\x0a";
    	
    	/*
    	 * Pattern used for extracting data from the WMO header 
    	 */
    	final Pattern awipsIdPattern = Pattern.compile(AWIPS_ID);
    	Matcher theMatcher = awipsIdPattern.matcher(message);
    	String awips_id = null;
    	if (theMatcher.find()) {
    		awips_id = theMatcher.group(0).trim();
    	} 
    	return awips_id;
    }		
	
    /**
     * Decode FFG report
     * 
     * @param bull The bulletin message
     * @param record
     */
    public static void processPrecip (String bull, FfgRecord record) {
    	
    	/*
    	 * Regular expression for FFG precipitation report
    	 */
    	final Pattern precipPattern = Pattern.compile(IDecoderConstantsN.FFG_REPORT);   
    	if ( record == null ) return;
	    Matcher m = precipPattern.matcher(bull);
	
	    while ( m.find() ) {
	    	FfgPrecip currentPrecip = new FfgPrecip();
	    	String precipReport = m.group().trim();

	    	/*
	    	 * Add precipitation report to the database
	    	 */
	    	currentPrecip.setReport (precipReport);   	 
	    	String[] xxx = precipReport.split(" ", 2);
	    	currentPrecip.setZoneID(xxx[0]);    
	    	boolean has_colon = xxx[1].contains (":");
	    	String[] ppp;
	    	if ( has_colon ) {
	    		String[] yyy  = xxx[1].split(":", 2 );
	    		ppp = yyy[0].split("/");
	    	} else {
	    		ppp = xxx[1].split("/");
	    	}

	    	/*
	    	 * Set precipitation guidance based on the number of slashes.  For instance
	    	 * if the number equals 5 (or 4 slashes), report can be in the form of 
	    	 * F01/F03/F06/F12/ or F01/F03/F06/F12/F24. Set Ffg24, Ffg12 and so on.
	    	 */

	    	switch ( ppp.length ) {
	    	case 6:
	    	case 5:
	    		try {
	    			currentPrecip.setFf24(Float.parseFloat(ppp[4]));
	    		} catch (Exception e) {
	    			// empty block
	    		}

	    	case 4:
	    		try {
	    			currentPrecip.setFf12(Float.parseFloat(ppp[3]));
	    		} catch (Exception e) {
	    			// empty block
	    		}

	    	case 3:
	    		try {
	    			currentPrecip.setFf06(Float.parseFloat(ppp[2]));
	    		} catch (Exception e) {
	    			// empty block
	    		}

	    	case 2:
	    		try {
	    			currentPrecip.setFf03(Float.parseFloat(ppp[1]));
	    		} catch (Exception e) {
	    			// empty block
	    		}

	    	case 1:
	    	case 0:
	    		try {
	    			currentPrecip.setFf01(Float.parseFloat(ppp[0])); 
	    		} catch (Exception e) {
	    			// empty block
	    		}
	    	}
	    	record.addPrecip(currentPrecip);
	    }
    }   	 

    /**
     * Process and set WMO header
     * 
     * @param wmohd The WMO header
     * @param record The FfgRecord
     * @param mndTime The MND time
     */
    public static void processWMO(byte[] wmohd, FfgRecord record, Calendar mndTime) {
    	WMOHeader hd = new WMOHeader (wmohd);
    	Calendar issueTime = UtilN.findDataTime(hd.getYYGGgg(),mndTime);
    	DataTime dt = new DataTime (issueTime);
    	if ( wmohd != null ) {
    		record.setWmoHeader( hd.getWmoHeader() );
    		record.setIssueOffice( hd.getCccc() );
    		record.setIssueTime( issueTime );
    		record.setDataTime( dt );


    		record.setDesignatorBBB( hd.getBBBIndicator() );			
    	} 
    }
}