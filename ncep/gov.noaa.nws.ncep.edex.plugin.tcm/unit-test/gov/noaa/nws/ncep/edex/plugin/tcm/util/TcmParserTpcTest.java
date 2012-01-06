package gov.noaa.nws.ncep.edex.plugin.tcm.util;

import static org.junit.Assert.assertEquals;
import gov.noaa.nws.ncep.common.dataplugin.tcm.TcmPositionWinds;
import gov.noaa.nws.ncep.common.dataplugin.tcm.TcmRecord;
import gov.noaa.nws.ncep.edex.plugin.tcm.decoder.TcmSeparator;
import gov.noaa.nws.ncep.common.tools.IDecoderConstantsN;


import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.Calendar;
import java.util.Iterator;
import java.util.Set;

import org.apache.log4j.Logger;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

public class TcmParserTpcTest {
	TcmSeparator sep;
	TcmRecord record = new TcmRecord();
	Set<TcmPositionWinds> tpw;
	StringBuffer contents_tpc = new StringBuffer();
	String tpc_data = null;

	@Before 
	public void initialize () {	
		final Logger log = Logger.getLogger(getClass().getName());
		sep = new TcmSeparator();
		File file_tpc = new File ("unit-test/gov/noaa/nws/ncep/edex/plugin/tcm/decoder/2009061820.mar");

		BufferedReader reader = null;
		
		// create data file
	    try {
            reader = new BufferedReader(new FileReader(file_tpc));
            String text = null;
 
            /*
             * Repeat until all lines is read. Add control characters.
             */ 
            while ((text = reader.readLine()) != null) {
                if ( text.length() != 0 ) {                
                	contents_tpc.append(text).append("\r\r\n");
                }
            }
        } 
	    catch (FileNotFoundException e) {
            if (log.isInfoEnabled()) {
            	log.info("File is not found");
            }
        } 
	    catch (IOException e) {
	    	if (log.isInfoEnabled()) {
	    		log.info("I/O Exception");
	    	}
	    } 
	    finally {
            try {
                if (reader != null) {
                    reader.close();
                }
            } 
    	    catch (IOException e) {
    	    	if (log.isInfoEnabled()) {
    	    		log.info("I/O Exception");
    	    	}
    	    } 
        }    
		sep = new TcmSeparator();
		tpc_data = contents_tpc.toString();
	}

	@After
	public void tearDown() throws Exception {
		record = null;
	}

	@Test
	public void testTpcProcessTcm () {
		TcmParser tp = new TcmParser();
		tp.processTcm(tpc_data, record);
		assertEquals ("EP", record.getBasin());
		assertEquals ("ONE-E",record.getStormName());
		assertEquals ("01", record.getStormNumber());
		assertEquals ("2", record.getAdvisoryNumber());
		assertEquals ("TROPICAL DEPRESSION", record.getStormType());
		assertEquals (true,record.getCorr());
		assertEquals (null, record.getEyeSize());
		assertEquals (30L,record.getPositionAccuracy().longValue());
		assertEquals (null, record.getMndTime());
		assertEquals (1005L, record.getCentralPressure().longValue());
		assertEquals ("0",record.getNe12ft());
		assertEquals ("60",record.getSe12ft());
		assertEquals ("0",record.getSw12ft());
		assertEquals ("0",record.getNw12ft());

	}
	
	@Test
	public void testTpcProcessPositionWinds () {
		TcmParser tp = new TcmParser();
		byte[] data = tpc_data.getBytes();
		Calendar cal = null;
		tp.processWMO(data, record, cal);
		tp.processTcm(tpc_data, record);
		tpw = record.getTcmPosWinds();
		Iterator<TcmPositionWinds> it = tpw.iterator();
		
		// Check observation time
		cal = record.getObsTime();
		assertEquals (18,cal.get(Calendar.DAY_OF_MONTH));
		assertEquals (21,cal.get(Calendar.HOUR_OF_DAY));
		assertEquals (0,cal.get(Calendar.MINUTE));;
		
		// F00
		TcmPositionWinds pw = it.next();
		System.out.println (pw.getClat());
		assertEquals (17.9, pw.getClat().doubleValue());
		assertEquals (-108.2, pw.getClon().doubleValue());
		assertEquals (-9999L, pw.getWindMax().longValue());
		assertEquals (-9999L, pw.getGust().longValue());
		assertEquals (null, pw.getNe34k());
		assertEquals (null, pw.getSe34k());
		assertEquals (null, pw.getSw34k());
		assertEquals (null, pw.getNw34k());
		cal = pw.getValidTime();
		assertEquals (18,cal.get(Calendar.DAY_OF_MONTH));
		assertEquals (18,cal.get(Calendar.HOUR_OF_DAY));
		assertEquals (0,cal.get(Calendar.MINUTE));
		assertEquals ("F00", pw.getFcstHour());

		// F48
		pw = it.next();
		assertEquals (24.1, pw.getClat().doubleValue());
		assertEquals (-106.9, pw.getClon().doubleValue());
		assertEquals (25L, pw.getWindMax().longValue());
		assertEquals (35L, pw.getGust().longValue());
		assertEquals (-9999L, pw.getStormDrct().longValue());
		assertEquals (-9999L, pw.getStormSped().longValue());
		cal = pw.getValidTime();
		assertEquals (20,cal.get(Calendar.DAY_OF_MONTH));
		assertEquals (18,cal.get(Calendar.HOUR_OF_DAY));
		assertEquals (0,cal.get(Calendar.MINUTE));;
		assertEquals ("F48", pw.getFcstHour());
	}
}