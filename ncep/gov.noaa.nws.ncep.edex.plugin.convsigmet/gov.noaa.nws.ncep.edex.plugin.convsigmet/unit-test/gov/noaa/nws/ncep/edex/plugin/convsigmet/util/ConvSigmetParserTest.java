/**
 * This Java class is the JUnit test for the convsigmet parser.
 *
 * <pre>
 *
 * L. Lin       04/09   Creation
 * </pre>
 *
 */
package gov.noaa.nws.ncep.edex.plugin.convsigmet.util;

import static org.junit.Assert.*;
import gov.noaa.nws.ncep.common.dataplugin.convsigmet.ConvSigmetRecord;
import gov.noaa.nws.ncep.common.dataplugin.convsigmet.ConvSigmetSection;
import gov.noaa.nws.ncep.edex.plugin.convsigmet.util.ConvSigmetParser;
import gov.noaa.nws.ncep.edex.util.UtilN;

import com.raytheon.edex.plugin.AbstractDecoder;
import com.raytheon.edex.util.Util;
import java.util.zip.DataFormatException;
import java.util.Calendar;
import org.junit.Test;


public class ConvSigmetParserTest extends AbstractDecoder {

        @Test
        public void testProcessWMO() {
          
            final String wmoHeader = "WSUS33";
            final String testBull = "WSUS33 KKCI 011455\n\n\r" +
            "SIGW  \n\n\r";
            
            ConvSigmetRecord record = null;

            record = ConvSigmetParser.processWMO(testBull, null);
            String wmo=record.getWmoHeader();
            assertEquals(wmo, wmoHeader);
            
            final String issueString = "011455";
            //Calendar mndTime = Calendar.getInstance();
            //Calendar timeGroup=ConvsigmetParser.convertDdhhmmToStandardCal(issueString, mndTime);
            Calendar timeGroup = null;
            try {
				timeGroup = Util.findCurrentTime(issueString); 
			 } catch (DataFormatException e) {
				 System.out.println("Unable to get issue time");
	            }
            Calendar issueTime=record.getIssueTime();
            System.out.println("======= This is the issue date:");
			System.out.println("issue date:year= " + timeGroup.get(Calendar.YEAR) );
			System.out.println("issue date:month= " + timeGroup.get(Calendar.MONTH) );
			System.out.println("issue date:day= " + timeGroup.get(Calendar.DAY_OF_MONTH) );
			System.out.println("issue date:hour= " + timeGroup.get(Calendar.HOUR) );
			System.out.println("issue date:minute= " + timeGroup.get(Calendar.MINUTE) );
		
            assertEquals(timeGroup, issueTime);
        }

        @Test
        public void testProcessFcstRegion() {
          
            
            final String fcstRegion = "W";
            final String testBull = "WSUS33 KKCI 241455\n\n\r" +
                                    "SIGW  \n\n\r";
            
        	String regionRet = ConvSigmetParser.processFcstRegion(testBull);
            assertEquals(fcstRegion, regionRet);
           
        }
        
        @Test
        public void testProcessStartTime() {
        
        	final String timeString ="261755";
    	    final String testBull = "WSUS31 KKCI 261755\n\n\r" +
                                    "SIGE  \n\n\r"  +
                                    "\036MKCE WST 261755\n\n\r" +
                                    "CONVECTIVE SIGMET...NONE\n\n\r" +
                                    "\n\n\r" +
                                    "OUTLOOK VALID 261955-262355\n\n\r" +
                                    "FROM 50E GRB-ROD-TTH-50E GRB\n\n\r" +
                                    "WST ISSUANCES POSS. REFER TO MOST RECENT ACUS01 KWNS FROM STORM\n\n\r" +
                                    "PREDICTION CENTER FOR SYNOPSIS AND METEOROLOGICAL DETAILS.\n\n\r";

         
            //Calendar mndTime = Calendar.getInstance();
            //Calendar startTime=ConvsigmetParser.convertDdhhmmToStandardCal(timeString, mndTime);
            Calendar startTime = null;
            try {
				startTime = Util.findCurrentTime(timeString); 
			 } catch (DataFormatException e) {
				 System.out.println("Unable to get start time");
	            }
            Calendar timeRet=ConvSigmetParser.processStartTime(testBull, null);
            	
            assertEquals(startTime,timeRet);	
            
        }
        
        @Test
        public void testProcessEndTime() {
        
        	final String timeString ="261855";
        	final String testBull = "WSUS31 KKCI 261755\n\n\r" +
            "SIGE  \n\n\r"  +
            "\036MKCE WST 261755\n\n\r" +
            "CONVECTIVE SIGMET 19C\n\n\r" +
            "VALID UNTIL 1855Z\n\n\r" +
            "MN IA NE\n\n\r" +
            "FROM 50SE RWF-30W MCW-20SSE OVR\n\n\r" +
            "LINE EMBD SEV TS 30 NM WIDE MOV FROM 26035KT. TOPS TO FL350.\n\n\r" +
            "HAIL TO 1 IN...WIND GUSTS TO 50KT POSS.\n\n\r" +
            "\n\n\r" +
            "OUTLOOK VALID 261955-262355\n\n\r" +
            "FROM 50E GRB-ROD-TTH-50E GRB\n\n\r" +
            "WST ISSUANCES POSS. REFER TO MOST RECENT ACUS01 KWNS FROM STORM\n\n\r" +
            "PREDICTION CENTER FOR SYNOPSIS AND METEOROLOGICAL DETAILS.\n\n\r";
    	    
         
            //Calendar mndTime = Calendar.getInstance();
            //Calendar endTime=ConvsigmetParser.convertDdhhmmToStandardCal(timeString, mndTime);
            Calendar endTime = null;
            try {
				endTime = Util.findCurrentTime(timeString); 
			 } catch (DataFormatException e) {
				 System.out.println("Unable to get end time");
	            }
            ConvSigmetSection curSection = new ConvSigmetSection();
            curSection.setStartTime(endTime);
            Calendar timeRet=ConvSigmetParser.processEndTime(testBull, curSection, null);
            	
            assertEquals(endTime,timeRet);	
            
        }
        
        @Test
        public void testProcessCorrectionFlag() {
        
    	    final String testBull1 = "WSUS31 KKCI 261755\n\n\r" +
                                    "SIGE  \n\n\r"  +
                                    "\036MKCE WST 261755 COR\n\n\r" +
                                    "CONVECTIVE SIGMET...NONE\n\n\r" +
                                    "\n\n\r" +
                                    "OUTLOOK VALID 261955-262355\n\n\r" +
                                    "FROM 50E GRB-ROD-TTH-50E GRB\n\n\r" +
                                    "WST ISSUANCES POSS. REFER TO MOST RECENT ACUS01 KWNS FROM STORM\n\n\r" +
                                    "PREDICTION CENTER FOR SYNOPSIS AND METEOROLOGICAL DETAILS.\n\n\r";

    	    final String testBull2 = "WSUS31 KKCI 261755\n\n\r" +
    	    						"SIGE  \n\n\r"  +
    	    						"\036MKCE WST 261755\n\n\r" +
    	    						"CONVECTIVE SIGMET...NONE\n\n\r" +
    	    						"\n\n\r" +
    	    						"OUTLOOK VALID 261955-262355\n\n\r" +
    	    						"FROM 50E GRB-ROD-TTH-50E GRB\n\n\r" +
    	    						"WST ISSUANCES POSS. REFER TO MOST RECENT ACUS01 KWNS FROM STORM\n\n\r" +
    	    						"PREDICTION CENTER FOR SYNOPSIS AND METEOROLOGICAL DETAILS.\n\n\r";

    	    final boolean YES=true;
    	    final boolean NO=false;
         
            boolean corRet = ConvSigmetParser.processCorrectionFlag(testBull1);	
            assertEquals(YES,corRet);
            
            corRet = ConvSigmetParser.processCorrectionFlag(testBull2);	
            assertEquals(NO,corRet);
            
        }
        
        @Test
        public void testProcessSequenceID() {
        
        	final String sequenceID = "19C";
    	    final String testBull = "WSUS31 KKCI 261755\n\n\r" +
                                    "SIGE  \n\n\r"  +
                                    "\036MKCE WST 261755\n\n\r" +
                                    "CONVECTIVE SIGMET 19C\n\n\r" +
                                    "VALID UNTIL 1855Z\n\n\r" +
                                    "MN IA NE\n\n\r" +
                                    "FROM 50SE RWF-30W MCW-20SSE OVR\n\n\r" +
                                    "LINE EMBD SEV TS 30 NM WIDE MOV FROM 26035KT. TOPS TO FL350.\n\n\r" +
                                    "HAIL TO 1 IN...WIND GUSTS TO 50KT POSS.\n\n\r" +
                                    "\n\n\r" +
                                    "OUTLOOK VALID 261955-262355\n\n\r" +
                                    "FROM 50E GRB-ROD-TTH-50E GRB\n\n\r" +
                                    "WST ISSUANCES POSS. REFER TO MOST RECENT ACUS01 KWNS FROM STORM\n\n\r" +
                                    "PREDICTION CENTER FOR SYNOPSIS AND METEOROLOGICAL DETAILS.\n\n\r";
    	    
           
            String idRet = ConvSigmetParser.processSequenceID(testBull);
            assertEquals(sequenceID, idRet);	
            
        }
        
        @Test
        public void testProcessValidTime() {
        
        	final String startString = "261955";
        	final String endString = "262355";
        	final String testBull = "WSUS31 KKCI 261755\n\n\r" +
            						"SIGE  \n\n\r"  +
            						"\036MKCE WST 261755\n\n\r" +
            						"CONVECTIVE SIGMET 19C\n\n\r" +
            						"VALID UNTIL 1855Z\n\n\r" +
            						"MN IA NE\n\n\r" +
            						"FROM 50SE RWF-30W MCW-20SSE OVR\n\n\r" +
            						"LINE EMBD SEV TS 30 NM WIDE MOV FROM 26035KT. TOPS TO FL350.\n\n\r" +
            						"HAIL TO 1 IN...WIND GUSTS TO 50KT POSS.\n\n\r" +
            						"\n\n\r" +
            						"OUTLOOK VALID 261955-262355\n\n\r" +
            						"FROM 50E GRB-ROD-TTH-50E GRB\n\n\r" +
            						"WST ISSUANCES POSS. REFER TO MOST RECENT ACUS01 KWNS FROM STORM\n\n\r" +
            						"PREDICTION CENTER FOR SYNOPSIS AND METEOROLOGICAL DETAILS.\n\n\r";
    	    
        	Calendar startTime = null;
            Calendar mndTime = null;
			startTime = UtilN.findDataTime(startString, mndTime); 
		 
		    Calendar endTime = null;
			endTime = UtilN.findDataTime(endString, mndTime); 
			  
            ConvSigmetSection curSection = new ConvSigmetSection();
            
            ConvSigmetParser.processValidTime(testBull, curSection, null);
            Calendar startRet = curSection.getStartTime();
            Calendar endRet = curSection.getEndTime();
            	
            assertEquals(endTime, endRet);
            assertEquals(startTime, startRet);
            
        }
        
        @Test
        public void testProcessFlightLevel() {
        
        	final String classType = "LINE";
        	final int direction = 260;
        	final int speed = 35;
        	final int flightLevel = 350;
        	
    	    final String testBull = "WSUS31 KKCI 261755\n\n\r" +
                                    "SIGE  \n\n\r"  +
                                    "\036MKCE WST 261755\n\n\r" +
                                    "CONVECTIVE SIGMET 19C\n\n\r" +
                                    "VALID UNTIL 1855Z\n\n\r" +
                                    "MN IA NE\n\n\r" +
                                    "FROM 50SE RWF-30W MCW-20SSE OVR\n\n\r" +
                                    "LINE EMBD SEV TS 30 NM WIDE MOV FROM 26035KT. TOPS TO FL350.\n\n\r" +
                                    "HAIL TO 1 IN...WIND GUSTS TO 50KT POSS.\n\n\r" +
                                    "\n\n\r" +
                                    "OUTLOOK VALID 261955-262355\n\n\r" +
                                    "FROM 50E GRB-ROD-TTH-50E GRB\n\n\r" +
                                    "WST ISSUANCES POSS. REFER TO MOST RECENT ACUS01 KWNS FROM STORM\n\n\r" +
                                    "PREDICTION CENTER FOR SYNOPSIS AND METEOROLOGICAL DETAILS.\n\n\r";
    	    
           
            ConvSigmetSection retSection = ConvSigmetParser.processPhenomena(testBull);
            
            assertEquals(classType, retSection.getClassType());	
            assertEquals(direction, retSection.getDirection());
            assertEquals(speed, retSection.getSpeed());
            assertEquals(flightLevel, retSection.getFlightLevel());
            
        }

}