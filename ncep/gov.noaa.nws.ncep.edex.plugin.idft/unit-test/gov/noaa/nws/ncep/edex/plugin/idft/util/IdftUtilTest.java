/**
 * IdftUtilTest.java
 * 
 * Junit test for IdftUtil
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 02Jun2009		100		F. J. Yen	Initial creation
 * 27May2010		100		F. J. Yen	Migrated from to11dr3 to to11dr11
 * 
 * </pre>
 * 
 * @author Fee Jing Yen, SIB
 * @version 1
 * 
 */
package gov.noaa.nws.ncep.edex.plugin.idft.util;

import static org.junit.Assert.*;
import java.util.Calendar;
import org.junit.Test;

import gov.noaa.nws.ncep.common.dataplugin.idft.IdftRecord;

public class IdftUtilTest {
	IdftRecord record = new IdftRecord();

	@Test
	public void testProcessValidTime(){
		Calendar cal = Calendar.getInstance();
		String valTimeStr = "312HR FORECAST VT 06/14/09 0000 UTC\r\r\n";
		cal.set(Calendar.YEAR,2009);
		cal.set(Calendar.MONTH,5);
		cal.set(Calendar.DAY_OF_MONTH,14);
		cal.set(Calendar.HOUR_OF_DAY, 0);
		cal.set(Calendar.MINUTE,0);
		cal.set(Calendar.SECOND,0);
		cal.set(Calendar.MILLISECOND,0);
		Calendar processValTime = IdftUtil.processValidTime(valTimeStr, record);	
		assertEquals(cal, processValTime);
		
		valTimeStr = "24HR FORECAST VT 02/28/09 0000 UTC\r\r\n";
		cal.set(Calendar.YEAR,2009);
		cal.set(Calendar.MONTH,1);
		cal.set(Calendar.DAY_OF_MONTH,28);
		cal.set(Calendar.HOUR_OF_DAY, 0);
		cal.set(Calendar.MINUTE,0);
		cal.set(Calendar.SECOND,0);
		cal.set(Calendar.MILLISECOND,0);
		processValTime = IdftUtil.processValidTime(valTimeStr, record);	
		assertEquals(cal, processValTime);
	}
}