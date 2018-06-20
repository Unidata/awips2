/**
 * WcpParserTest.java
 * 
 * Junit test for WcpParser
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 17Apr2009		37		F. J. Yen	Initial creation
 * 
 * </pre>
 * 
 * @author Fee Jing Yen, SIB
 * @version 1
 * 
 */
package gov.noaa.nws.ncep.edex.plugin.wcp.util;

import static org.junit.Assert.*;

import java.util.Calendar;

import org.junit.Test;

import gov.noaa.nws.ncep.common.dataplugin.wcp.WcpRecord;

public class WcpParserTest {
	WcpRecord record = new WcpRecord();

	@Test
	public void testProcessWMO() {
		String theBulletin = "941 \n\n\r" + "WWUS60 KWNS 132255 CCA\n\n\r" +
    	"SEVSPC" + "FILE CREATED 13-APR-09 AT 22:55:00 UTC\n\n\r" +
    	"SEVR 090413 1635 WT0150 2300\n\n\r" +
    	"03145.08335 03201.08041 03016.08041 03001.08335;\n\n\r" +
    	"\n\n\r" + "SEVR 090413 1800 WT0151 0100^\n\n\r" +
		"03417.08826 03823.08717 03823.08441 03417.08559;\n\n\r";
	    /*
	     * Replace white spaces with blank
	     */
	    record.setBullMessage((theBulletin.substring(5)).replace('\r', ' ')
					.replace('\003', ' ').replace('\000', ' ').replace(
							'\001', ' '));
	
	    Calendar createdTime = null;	
	    createdTime = WcpParser.processFileCreatedDate(theBulletin, record);
	
	    try {
		    WcpParser.processWMO(theBulletin, createdTime, record);		
		    String dsBBB = record.getDesignatorBBB();
		    assertEquals ( "CCA", dsBBB );
		    Calendar issTim = record.getIssueTime();
		    assertEquals (2009, issTim.get(Calendar.YEAR) );
		    assertEquals (3, issTim.get(Calendar.MONTH));
		    assertEquals (13, issTim.get(Calendar.DAY_OF_MONTH));
		    assertEquals (22, issTim.get(Calendar.HOUR_OF_DAY));
		    assertEquals (55, issTim.get(Calendar.MINUTE));

	    } catch (Exception e) {
		    // empty block
	    }
	}
	
	@Test
	public void testProcessFileCreatedDate() {
		//fail("Not yet implemented");
	    Calendar createdTime = null; 
	    record = new WcpRecord();
	    final String theBulletin = "941 \n\n\r" + "WWUS60 KWNS 132255\n\n\r" +
	    	"SEVSPC" + "FILE CREATED 13-APR-09 AT 22:55:00 UTC\n\n\r" +
	    	"SEVR 090413 1635 WT0150 2300\n\n\r" +
	    	"03145.08335 03201.08041 03016.08041 03001.08335;\n\n\r" +
	    	"\n\n\r" + "SEVR 090413 1800 WT0151 0100\n\n\r" +
			"03417.08826 03823.08717 03823.08441 03417.08559;\n\n\r";
				
		createdTime = WcpParser.processFileCreatedDate(theBulletin, record);		
		assertEquals (2009, createdTime.get(Calendar.YEAR) );
		assertEquals (3, createdTime.get(Calendar.MONTH));
		assertEquals (13, createdTime.get(Calendar.DAY_OF_MONTH));
	}
}
