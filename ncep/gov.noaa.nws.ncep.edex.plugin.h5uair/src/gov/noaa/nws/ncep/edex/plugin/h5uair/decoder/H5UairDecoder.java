/**
 * UairDecoder
 * 
 * This java class provides the Decoder class for upper air 
 * sounding data.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    	Engineer    Description
 * -------		------- 	-------- 	-----------
 * 03/2010		210			L. Lin		Initial coding
 * 05/2010      210         L. Lin      Migration to TO11DR11
 * 4/2011		210			T. Lee		Persisted to HDF5
 * </pre>
 * 
 * This code has been developed by the SIB for use in the AWIPS2 system.
 * @author L. Lin
 * @version 1.0
 */

package gov.noaa.nws.ncep.edex.plugin.h5uair.decoder;

import java.util.Calendar;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import com.raytheon.uf.edex.wmo.message.WMOHeader;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.edex.exception.DecoderException;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.edex.plugin.AbstractDecoder;
import com.raytheon.edex.esb.Headers;
import gov.noaa.nws.ncep.common.dataplugin.h5uair.H5UairRecord;
import gov.noaa.nws.ncep.edex.plugin.h5uair.util.ShipMobile;
import gov.noaa.nws.ncep.edex.plugin.h5uair.util.TimeGroup;
import gov.noaa.nws.ncep.edex.plugin.h5uair.util.H5UairParser;
import gov.noaa.nws.ncep.edex.util.UtilN;
import gov.noaa.nws.ncep.edex.tools.decoder.SnsTnsLocTbl;

public class H5UairDecoder extends AbstractDecoder {
    private static String pluginName;
	private H5UairRecord record;    
	
    /**
     * Constructor
     * 
     * @throws DecoderException
     */
    public H5UairDecoder(String name) throws DecoderException {
    	pluginName = name;
    }
    
    public PluginDataObject[] decode(byte[] data, Headers headers) throws DecoderException {
        byte[] messageData = null;
    	String traceId = null;
        WMOHeader hd;
        Boolean nil=false;
        Boolean ship= false;
        Boolean drop = false;
        
        if (headers != null) {
        	/*
        	 * traceId equals to the file name
        	 */
            traceId = (String) headers.get("traceId");
        }
        
        /*
    	 * Check if there are more bulletins.
    	 */ 
        H5UairSeparator sep = H5UairSeparator.separate(data, headers);
    	if ( sep.hasNext() ) {
    		messageData = sep.next();
    	} else {
    		throw new DecoderException("uair -Out of data");
    	}
    	String theMessage = new String(messageData);
    	record = new H5UairRecord ();
	
    	/*
    	 * Set issue time and bull message.
    	 */
    	hd = new WMOHeader(messageData);   
    	Calendar cal = null;
    	Calendar issueTime = UtilN.findDataTime(hd.getYYGGgg(),cal);
    	record.setIssueTime(issueTime);
    	// Replace special characters to a blank so that it may be readable.
    	record.setBullMessage(UtilN.removeLeadingWhiteSpaces(theMessage
				.replace('\r', ' ').replace('\003', ' ')
				.replace('\000', ' ').replace('\001', ' ')));
		// Get and set dataType
		String dataType = H5UairParser.getDataType(theMessage);
		
    	record.setDataType(dataType);
    	record.setReportType("UAIR");
    	record.setWmoHeader(hd.getWmoHeader());
    	record.setCorr(H5UairParser.findCorIndicator(theMessage));

    	String stationNumber=null;
    	if ( dataType != null ) {
    		String p1 = dataType.substring(0,2);
    		if ( p1.equals("UU") ) {
    			ship = true;
    		} else if ( p1.equals("XX") ) {
    			drop = true;
    		} else {
    	    	stationNumber = H5UairParser.getStationNumber(theMessage);
    		}
    	}
    	record.setStnum (stationNumber);
    	
    	/* Regular expression for "nil" */
    	final String NIL  = "(NIL)";
    	Pattern nilPattern = Pattern.compile(NIL);
    	Matcher nilMatcher = nilPattern.matcher(theMessage);

    	if ( nilMatcher.find()) {
    		nil=true;
    		record.setNil(true);
    	}
    	
    	Calendar obsTime = null;
    	
    	//set station id, lat, lon
    	if ( stationNumber != null ) {
    		SnsTnsLocTbl.getSnsTnsLocation(stationNumber, "snstns.xml");
    		record.setStid(SnsTnsLocTbl.getStnId());
    		record.setSlat((float)SnsTnsLocTbl.getStnLat());
    		record.setSlon((float)SnsTnsLocTbl.getStnLon());
    		record.setSelv((float)SnsTnsLocTbl.getStnElev());

    		// Get time group
    		TimeGroup.TimeField(theMessage);
    		obsTime = TimeGroup.getObsTime();
    		record.setObsTime(obsTime);
    		record.setSynopticTime(TimeGroup.getSynopticTime());
    		record.setUTC(TimeGroup.getIutc());
    	}
    	    	
    	if ( ! nil ) {
    		 /* Regular expression for UAIR report */
    		 final String UAIR_REPORT  = "(TT|PP)(AA|BB|CC|DD) ( )?(\\d{5}|\\d{4}/) (\\d{5})(.*)=";
    		 Pattern reportPattern = Pattern.compile(UAIR_REPORT, Pattern.DOTALL);
    		 Matcher reportMatcher = reportPattern.matcher(theMessage);
    		 
    		 final String UAIR_BULL = "(TT|PP)(AA|BB|CC|DD) ( )?(\\d{5}|\\d{4}/) (\\d{5})(.*)\\x03";
    		 Pattern bullPattern = Pattern.compile(UAIR_BULL, Pattern.DOTALL);
    		 Matcher bullMatcher = bullPattern.matcher(theMessage);
    		 
    		 String codeMessage=null;
    		 if ( reportMatcher.find()) {
    			 codeMessage = reportMatcher.group(6);
    			 if ( codeMessage.length() > 4 ) {
        			 codeMessage = UtilN.removeExtraWhiteSpaces(codeMessage.replace('\r', ' ').replace('\n', ' '));
    			 }
    		 } else if ( bullMatcher.find()) {
    			 codeMessage = bullMatcher.group(6);
    			 if ( codeMessage.length() > 4 ) {
    				 codeMessage = UtilN.removeExtraWhiteSpaces(codeMessage.replace('\r', ' ').replace('\n', ' ').replace('\003', ' '));
    			 }
    		 } else {
    			 // Handle ship or dropsonde data
    			 final String SHIP  = "(UU|XX)(AA|BB|CC|DD) (.*)(=|\\x03)";
    			 Pattern shipPattern = Pattern.compile(SHIP, Pattern.DOTALL);
    			 Matcher shipMatcher = shipPattern.matcher(theMessage);
    			 if ( shipMatcher.find()) {
    				 codeMessage = shipMatcher.group(3);
    				 codeMessage = UtilN.removeExtraWhiteSpaces(codeMessage.replace('\r', ' ').replace('\n', ' ').replace('\003', ' '));

    				 ShipMobile.ShipMobileField(codeMessage,dataType);
    				 codeMessage = ShipMobile.getCodeMessage();
    				 record.setStid(ShipMobile.getStnId());
    				 record.setSlat(ShipMobile.getXlat());
    				 record.setSlon(ShipMobile.getXlon());
    				 record.setStnum(ShipMobile.getStationNumber());
    				 record.setSelv((float)ShipMobile.getElevation());
    				 obsTime = ShipMobile.getObsTime();
    				 record.setObsTime(obsTime);
    				 record.setSynopticTime(ShipMobile.getSynopticTime());
    				 record.setUTC(ShipMobile.getIutc());
    			 }
    		 }
    		 
    		//Parse UAIR report.
    		 if ( codeMessage != null ) {
    			 H5UairParser.getLevels(codeMessage, record);
    		 }
    		
    	}
    	
    	// set dataTime
		 if ( obsTime != null ) {
			 DataTime dataTime=new DataTime(obsTime);
			 record.setDataTime(dataTime);
		 } else {
			 DataTime dataTime=new DataTime(issueTime);
			 record.setDataTime(dataTime);
		 }

       	/*
    	 * Return the UairRecord record object.
    	 */
    	if ( record !=null ){
    		try {
    			if (headers != null) {
    				traceId = (String) headers.get("traceId");
    			}
    			record.setTraceId(traceId);
    			record.setPluginName(pluginName);
    			record.constructDataURI();
    		} catch (PluginException e) {
    			throw new DecoderException("Unable to construct dataURI", e);
    		}
    	}
	
    	/*
    	 * Return UAIR record object.
    	 */ 
    	if (record == null) {
    		return new PluginDataObject[0];
    	}
    	return new PluginDataObject[] {record};
    }
}
