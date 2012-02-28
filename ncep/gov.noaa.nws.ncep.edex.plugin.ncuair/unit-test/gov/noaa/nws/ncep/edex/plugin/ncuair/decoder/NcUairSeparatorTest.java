/**
 * This Java class is the JUnit test for the decoder separator.
 * 
 * <pre>
 * 
 * T. Lee	11/08	Creation
 * T. Lee	 3/09	Migrate to TO10
 * S. Gurung 09/11  Renamed H5 to Nc and h5 to nc
 * </pre>
 * 
 */
package gov.noaa.nws.ncep.edex.plugin.ncuair.decoder;

import static org.junit.Assert.*;
import org.apache.log4j.Logger;
import org.junit.Before;
import org.junit.Test;

import gov.noaa.nws.ncep.edex.plugin.ncuair.decoder.NcUairSeparator;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.File;
import java.io.IOException;
import java.io.FileNotFoundException;

public class NcUairSeparatorTest {
	NcUairSeparator sep;
	char[] cbuf;
	int ntime = 0;
	StringBuffer contents = new StringBuffer();
	byte[] actual = null;

	@Before 
	public void initialize () {	
		final Logger log = Logger.getLogger(getClass().getName());
		sep = new NcUairSeparator();
		File file = new File ("unit-test/gov/noaa/nws/ncep/edex/plugin/ncuair/decoder/20100327.uair");
		BufferedReader reader = null;
	
	    try {
            reader = new BufferedReader(new FileReader(file));
            String text = null;
 
            /*
             * Repeat until all lines is read. Add control characters.
             */ 
            while ((text = reader.readLine()) != null) {
                if ( text.length() != 0 ) {                
                	contents.append(text).append("\r\r\n");
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
		sep = new NcUairSeparator();
		actual = contents.toString().getBytes();
		sep.setData(actual, null);
	}

	@Test
	public void testHasNext() {
		assertTrue("Find Uair separator! ", sep.hasNext());		
	}

	@Test
	public void testGetRecord() {
		byte[] expected = sep.next();		
		String a = new String (actual);
		String e = new String (expected);
		//assertEquals(e.trim(),a.trim());
	}
}
