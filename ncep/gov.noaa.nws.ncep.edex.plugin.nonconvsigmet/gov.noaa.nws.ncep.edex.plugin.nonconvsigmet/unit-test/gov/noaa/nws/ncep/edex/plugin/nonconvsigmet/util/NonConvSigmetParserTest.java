/**
 * This Java class is the JUnit test for the non-convsigmet parser.
 *
 * <pre>
 *
 * Uma Josyula       04/09   Creation
 * </pre>
 *
 */

package gov.noaa.nws.ncep.edex.plugin.nonconvsigmet.util;

import static org.junit.Assert.*;
import gov.noaa.nws.ncep.common.dataplugin.nonconvsigmet.NonConvSigmetRecord;
import gov.noaa.nws.ncep.edex.plugin.nonconvsigmet.util.NonConvSigmetParser;
//import gov.noaa.nws.ncep.edex.util.UtilN;

import com.raytheon.edex.plugin.AbstractDecoder;
import com.raytheon.edex.util.Util;
import java.util.zip.DataFormatException;
import java.util.Calendar;
import org.junit.Test;

public class NonConvSigmetParserTest extends AbstractDecoder {
    @Test
    public void testProcessWMO() {
      
        final String wmoHeader = "WSUS01";
        final String testBull = "WSUS01 KKCI 071510\n\n\r";

        NonConvSigmetRecord record = null;

        record = NonConvSigmetParser.processWMO(testBull, null);
        String wmo=record.getWmoHeader();
        assertEquals(wmo, wmoHeader);
        
        final String issueString = "071510";

        Calendar timeGroup = null;
        try {
			timeGroup = Util.findCurrentTime(issueString); 
		 } catch (DataFormatException e) {
			 System.out.println("Unable to get issue time");
            }
        Calendar issueTime=record.getIssueTime();
        System.out.println("******* This is the issue date:");
		System.out.println("issue date:year= " + timeGroup.get(Calendar.YEAR) );
		System.out.println("issue date:month= " + timeGroup.get(Calendar.MONTH) );
		System.out.println("issue date:day= " + timeGroup.get(Calendar.DAY_OF_MONTH) );
		System.out.println("issue date:hour= " + timeGroup.get(Calendar.HOUR) );
		System.out.println("issue date:minute= " + timeGroup.get(Calendar.MINUTE) );
	
        assertEquals(timeGroup, issueTime);
    }
    
  @Test
  public void testProcessStartEndTime() {
    
    	final String stTimeString ="071510";
    	final String endTimeString ="071910";
  		final String fcstRegion = "BOSN";

    	NonConvSigmetRecord record = new NonConvSigmetRecord();
	    final String testBull = "WSUS01 KKCI 071510\n\n\r" +
                                "WS1N \n\n\r"+
                                "BOSN WS 071510 \n\n\r" +
                                "SIGMET NOVEMBER 3 VALID UNTIL 071910Z\n\n\r" +
                                "NY LO PA OH LE WV VA MD NC SC GA \n\n\r" +
                                "FROM MSS TO FLO TO LGC TO GQO TO HMV TO CLE TO 30NNW BUF TO MSS \n\n\r" +
                                "OCNL SEV TURB BTN 080 AND FL250. RPRTD BY ACFT. REPLACES SIGMET \n\n\r" +
                                "NOVEMBER 2. CONDS CONTG BYD 1910Z.";
                                
        Calendar startTime = null;
        Calendar endTime = null;
        try {
			startTime = Util.findCurrentTime(stTimeString);
			endTime = Util.findCurrentTime(endTimeString);  
		 } catch (DataFormatException e) {
			 System.out.println("Unable to get start  and end time");
            }
		record = NonConvSigmetParser.processStartEndTime(testBull, record, null);
        Calendar sttimeRet=record.getStartTime();
        Calendar endtimeRet=record.getEndTime();
        String regionRet= record.getForecastRegion();

        assertEquals(startTime,sttimeRet);
        assertEquals(endTime,endtimeRet);
 		assertEquals(fcstRegion, regionRet);


    }

  @Test
  public void testProcessPhenomena() {
    
  		final String corRemarks = " ";
  		final int flightLevel1 = 80;
  		final int flightLevel2 = 250;
  		final String hazardType ="TURB";
  		final String hazardIntensity="OCNL SEV";
  		final String hazardCause="RPRTD BY ACFT";
  		final String hazardCondition="CONDS CONTG BYD 1910Z";
  		final String stateList ="NY LO PA OH LE WV VA MD NC SC GA";
  		final String awipsId = "WS1N";
  		NonConvSigmetRecord record = new NonConvSigmetRecord();
  		final String testBull =   "WSUS01 KKCI 071510\n\n\r" +
	                              "WS1N\n\n\r"+
	                              "BOSN WS 071510\n\n\r" +
	                              "SIGMET NOVEMBER 3 VALID UNTIL 071910\n\n\r" +
	                              "NY LO PA OH LE WV VA MD NC SC GA\n\n\r" +
	                              "FROM MSS TO FLO TO LGC TO GQO TO HMV TO CLE TO 30NNW BUF TO MSS\n\n\r" +
	                              "OCNL SEV TURB BTN 080 AND FL250. RPRTD BY ACFT. REPLACES SIGMET\n\n\r" +
	                              "NOVEMBER 2. CONDS CONTG BYD 1910Z.";
      
	  	record = NonConvSigmetParser.processPhenomena(testBull, record);
 		String corrRet = record.getCorrectionRemarks();
 		int flLevelRet1 =record.getFlightLevel1();
 		int flLevelRet2 =record.getFlightLevel2();
 		String hazardTypeRet=record.getHazardType();
 		String hazardIntensityRet=record.getHazardIntensity();
 		String hazardCauseRet=record.getHazardCause();
 		String hazardConditionRet=record.getHazardCondition();
 		String stListRet=record.getStateList();
 		String awipsIdRet=record.getAwipsId();
 		
 		

 		assertEquals(flightLevel1, flLevelRet1);
 		assertEquals(flightLevel2, flLevelRet2);
	  	assertEquals(hazardType, hazardTypeRet);
	  	assertEquals(hazardIntensity, hazardIntensityRet);
	  	assertEquals(hazardCause, hazardCauseRet);
	  	assertEquals(hazardCondition, hazardConditionRet);
	  	assertEquals(stateList, stListRet);
 		assertEquals(awipsId, awipsIdRet);
 		assertEquals(corRemarks, corrRet);

  }

  @Test
  public void testProcessSigmetId() {
	    
		final String sigmetId = "NOVEMBER 3";
		
		NonConvSigmetRecord record = new NonConvSigmetRecord();
		final String testBull =   "WSUS01 KKCI 071510\n\n\r" +
	                              "WS1N \n\n\r"+
	                              "BOSN WS 071510 \n\n\r" +
	                              "SIGMET NOVEMBER 3 VALID UNTIL 071910\n\n\r" +
	                              "NY LO PA OH LE WV VA MD NC SC GA \n\n\r" +
	                              "FROM MSS TO FLO TO LGC TO GQO TO HMV TO CLE TO 30NNW BUF TO MSS \n\n\r" +
	                              "OCNL SEV TURB BTN 080 AND FL250. RPRTD BY ACFT. REPLACES SIGMET \n\n\r" +
	                              "NOVEMBER 2. CONDS CONTG BYD 1910Z.";
    
	  	record = NonConvSigmetParser.processSigmetId(testBull, record);
	  	String sigmetIdRet = record.getSigmetId();
		assertEquals(sigmetId, sigmetIdRet);	

  }
  
}
