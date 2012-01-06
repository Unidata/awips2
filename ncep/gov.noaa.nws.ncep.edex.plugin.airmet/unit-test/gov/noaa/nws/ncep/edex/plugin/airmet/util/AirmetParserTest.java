/**
 * This Java class is the JUnit test for the airmet parser.
 *
 * <pre>
 *
 * L. Lin       05/09   Creation
 * </pre>
 *
 */
package gov.noaa.nws.ncep.edex.plugin.airmet.util;

import static org.junit.Assert.*;
import gov.noaa.nws.ncep.common.dataplugin.airmet.AirmetLocation;
import gov.noaa.nws.ncep.common.dataplugin.airmet.AirmetRecord;
import gov.noaa.nws.ncep.common.dataplugin.airmet.AirmetReport;
import gov.noaa.nws.ncep.edex.plugin.airmet.util.AirmetParser;
import gov.noaa.nws.ncep.edex.util.UtilN;
import java.util.ArrayList;
import java.util.Iterator;

import com.raytheon.edex.plugin.AbstractDecoder;
import com.raytheon.edex.util.Util;
import java.util.zip.DataFormatException;
import java.util.Calendar;
import org.junit.Test;


public class AirmetParserTest extends AbstractDecoder {

        @Test
        public void testProcessWMO() {
          
            final String wmoHeader = "WAUS45";
            final String testBull = "WAUS45 KKCI 121112 AAA\n\n\r" +
            "WA5S  \n\n\r" +
            "\036SLCS WA 121112 AMD\n\n\r" +
            "AIRMET SIERRA UPDT 2 FOR IFR AND MTN OBSCN VALID UNTIL 121500\n\n\r";
            
            AirmetRecord record = null;

            record = AirmetParser.processWMO(testBull, null);
            String wmo=record.getWmoHeader();
            assertEquals(wmo, wmoHeader);
            
            final String issueString = "121112";
            
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
        public void testGetReportType() {
          
            final String reportType = "SIERRA";
            final String testBull = "WAUS45 KKCI 121112 AAA\n\n\r" +
            "WA5S  \n\n\r" +
            "\036SLCS WA 121112 AMD\n\n\r" +
            "AIRMET SIERRA UPDT 2 FOR IFR AND MTN OBSCN VALID UNTIL 121500\n\n\r";
            
            String retType = AirmetParser.getReportName(testBull);
            
            assertEquals(reportType, retType);
        }
       
        @Test
        public void testGetUpdateNumber() {
          
            final Integer updateNumber = 2;
            final String testBull = "WAUS45 KKCI 121112 AAA\n\n\r" +
            "WA5S  \n\n\r" +
            "\036SLCS WA 121112 AMD\n\n\r" +
            "AIRMET SIERRA UPDT 2 FOR IFR AND MTN OBSCN VALID UNTIL 121500\n\n\r";
            
            Integer retUpdate = AirmetParser.getUpdateNumber(testBull);
            
            assertEquals(updateNumber, retUpdate);
        }
        
        @Test
        public void testGetCorrectionFlag() {
          
            final Integer correctionFlag = 2;
            final String testBull = "WAUS45 KKCI 121112 AAA\n\n\r" +
            "WA5S  \n\n\r" +
            "\036SLCS WA 121112 AMD\n\n\r" +
            "AIRMET SIERRA UPDT 2 FOR IFR AND MTN OBSCN VALID UNTIL 121500\n\n\r";
            
            Integer retCorrection = AirmetParser.getCorrectionFlag(testBull);
            
            assertEquals(correctionFlag, retCorrection);
        }
        
        @Test
        public void testGetCancelFlag() {
          
            final Integer cancelFlag = 1;
            final String testReport = "AIRMET MTN OBSCN...CO NM...UPDT\n\n\r" +
            "FROM TBE TO CME TO 60W INK TO 50E ELP TO 50W CME TO 50ESE ABQ TO\n\n\r" +
            "30SSW ALS TO TBE\n\n\r" +
            "CANCEL AIRMET. CONDS HV ENDED.\n\n\r";
            
            Integer retCancel = AirmetParser.getCancelFlag(testReport);
            
            assertEquals(cancelFlag, retCancel);
        }
        
        @Test
        public void testGetClassType() {
          
            final String hazardType = "INSTRUMENT FLIGHT RULES";
            final String testReport = "AIRMET IFR...CO NM\n\n\r" +
            "FROM TBE TO CME TO 60W INK TO 50E ELP TO 50W CME TO 50ESE ABQ TO\n\n\r" +
            "30SSW ALS TO TBE\n\n\r" +
            "CIG BLW 010/VIS BLW 3SM BR. CONDS CONTG BYD 15Z ENDG 15-18Z.\n\n\r";
            
            String retType = AirmetParser.getHazardType(testReport);
            
            assertEquals(hazardType, retType);
        }
        
        @Test
        public void testGetRegion() {
          
        	final String testBull = "WAUS45 KKCI 121112 AAA\n\n\r" +
            "WA5S  \n\n\r" +
            "\036SLCS WA 121112 AMD\n\n\r" +
            "AIRMET SIERRA UPDT 2 FOR IFR AND MTN OBSCN VALID UNTIL 121500\n\n\r";
            AirmetRecord record = null;

            record = AirmetParser.processWMO(testBull, null);
            
            String retRegion = AirmetParser.getRegion("SIERRA");
            
            assertEquals("S", retRegion);
        }
        
        @Test
        public void testGetStartTime() {
          
        	final String timeString ="121112";
        	final String testBull = "WAUS45 KKCI 121112 AAA\n\n\r" +
            "WA5S  \n\n\r" +
            "\036SLCS WA 121112 AMD\n\n\r" +
            "AIRMET SIERRA UPDT 2 FOR IFR AND MTN OBSCN VALID UNTIL 121500\n\n\r";
            Calendar startTime = null;

            try {
				startTime = Util.findCurrentTime(timeString); 
			 } catch (DataFormatException e) {
				 System.out.println("Unable to get start time");
	            }
            Calendar retStart = AirmetParser.getStartTime(testBull, null);
            assertEquals(startTime, retStart);
        }
        
        @Test
        public void testGetEndTime() {
          
        	final String timeString ="121500";
        	final String testBull = "WAUS45 KKCI 121112 AAA\n\n\r" +
            "WA5S  \n\n\r" +
            "\036SLCS WA 121112 AMD\n\n\r" +
            "AIRMET SIERRA UPDT 2 FOR IFR AND MTN OBSCN VALID UNTIL 121500\n\n\r";
            Calendar endTime = null;

            try {
				endTime = Util.findCurrentTime(timeString); 
			 } catch (DataFormatException e) {
				 System.out.println("Unable to get end time");
	            }
            Calendar retEnd = AirmetParser.getEndTime(testBull, null);
            assertEquals(endTime, retEnd);
        }
        
        @Test
        public void testGetValidDay() {
          
        	final String testBull = "WAUS45 KKCI 121112 AAA\n\n\r" +
            "WA5S  \n\n\r" +
            "\036SLCS WA 121112 AMD\n\n\r" +
            "AIRMET SIERRA UPDT 2 FOR IFR AND MTN OBSCN VALID UNTIL 121500\n\n\r";

        	String validDay = "12";
        	String retDay = AirmetParser.getValidDay(testBull);
            
            assertEquals(validDay, retDay);
        }
        
        @Test
        public void testProcessSequenceID() {
        
        	final String testBull = "WAUS45 KKCI 121112 AAA\n\n\r" +
            "WA5S  \n\n\r" +
            "\036SLCS WA 121112 AMD\n\n\r" +
            "AIRMET SIERRA UPDT 2 FOR IFR AND MTN OBSCN VALID UNTIL 121500\n\n\r";

        	Integer series = 1;
        	final String sequenceID = "SLC21";
    	    
            String idRet = AirmetParser.getSequenceID(testBull, series);
            assertEquals(sequenceID, idRet);	
            
        }
        
        @Test
        public void testProcessValidTime() {
        
        	final String testBull = "WAUS45 KKCI 121112 AAA\n\n\r" +
            "WA5S  \n\n\r" +
            "\036SLCS WA 121112 AMD\n\n\r" +
            "AIRMET SIERRA UPDT 2 FOR IFR AND MTN OBSCN VALID UNTIL 121500\n\n\r" +
            "OTLK VALID 2100-0300Z\n\n\r" +
            "AREA 1...TURB ND SD NE KS MN IA MO WI LS MI\n\n\r" +
            "BOUNDED BY 30N INL-YQT-70N SAW-20E IOW-MCI-SLN-20WSW GLD-40E SNY-\n\n\r" +
            "50SSW BFF-50NNW ISN-30N INL\n\n\r" +
            "MOD TURB BTN FL240 AND FL410. CONDS CONTG THRU 03Z.\n\n\r";
        	
        	final String startString = "122100";
        	final String endString = "130300";
        	final String validDay = "12";
        	
        	Calendar startTime = null;
            Calendar mndTime = null;
			startTime = UtilN.findDataTime(startString, mndTime); 
		 
		    Calendar endTime = null;
			endTime = UtilN.findDataTime(endString, mndTime); 
			  
            AirmetReport curSection = new AirmetReport();
            
            AirmetParser.processValidTime(testBull, curSection, validDay, null);
            Calendar startRet = curSection.getStartTime();
            Calendar endRet = curSection.getEndTime();
            	
            assertEquals(endTime, endRet);
            assertEquals(startTime, startRet);
            
        }
        
        @Test
        public void testProcessReport() {
        	
        	AirmetReport section = new AirmetReport();
        	
        	final String level1 = "240";
            final String level2 = "410";

            final String testReport = "WAUS45 KKCI 121112 AAA\n\n\r" +
            "WA5S  \n\n\r" +
            "\036SLCS WA 121112 AMD\n\n\r" +
            "AIRMET TURB...ND SD NE KS MN IA MO WI LS MI\n\n\r" +
            "FROM 30N INL TO YQT TO 60ESE YQT TO 20SE ODI TO DSM TO PWE\n\n\r" +
            "MOD TURB BTN FL240 AND FL410. CONDS CONTG BYD 21Z THRU 03Z.\n\n\r";
            
        	ArrayList<String> locationList = new ArrayList<String>();

            locationList.add("30N INL");
            locationList.add("YQT");
            locationList.add("60ESE YQT");
            locationList.add("20SE ODI");
            locationList.add("DSM");
            locationList.add("PWE");
            
        	//section = AirmetParser.processReport(testReport);
        	
        	//assertEquals(level1, section.getFlightLevel1());
        	//assertEquals(level2, section.getFlightLevel2());
        	
               
            if (section.getAirmetLocation() != null && section.getAirmetLocation().size() > 0) {
            	for (Iterator<AirmetLocation> iter = section.getAirmetLocation().iterator(); iter.hasNext();) {
            		AirmetLocation loc = iter.next();
            		String location = loc.getLocation();
            		System.out.println("location=" + location);
            		//assertTrue(locationList.contains(location));
            	}
            } 
        }
        
        @Test
        public void testProcessOutLook() {
        	
        	AirmetReport section = new AirmetReport();
        	
            final String sequenceID = "1Z";
        	final String forecastRegion = "Z";
        	
        	final String testReport = "OTLK VALID 2100-0300Z\n\n\r" +
            "AREA 1...TURB ND SD NE KS MN IA MO WI LS MI\n\n\r" +
            "BOUNDED BY 30N INL-YQT-70N SAW-20E IOW-MCI-SLN\n\n\r" +
            "MOD TURB BTN FL240 AND FL410. CONDS CONTG THRU 03Z.\n\n\r";

        	
        	ArrayList<String> locationList = new ArrayList<String>();

            locationList.add("30N INL");
            locationList.add("YQT");
            locationList.add("70N SAW");
            locationList.add("20E IOW");
            locationList.add("YQT");
            locationList.add("MCI");
            locationList.add("SLN");

            
          
        	//section = AirmetParser.processOutLook(testReport, forecastRegion);
        	
        	
        	//assertEquals(sequenceID, section.getSequenceID());
        	
               
            if (section.getAirmetLocation() != null && section.getAirmetLocation().size() > 0) {
            	for (Iterator<AirmetLocation> iter = section.getAirmetLocation().iterator(); iter.hasNext();) {
            		AirmetLocation loc = iter.next();
            		String location = loc.getLocation();
            		System.out.println("location=" + location);
            		//assertTrue(locationList.contains(location));
            	}
            } 
        }
        

}