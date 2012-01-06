/**
 * This Java class is the JUnit test for the decoder separator.
 * 
 * <pre>
 * 
 * F. J. Yen	 1/08	Creation
 * F. J. Yen	 4/09	Migrate to TO10
 * F. J. Yen	 8/09	Migrate to TO11
 * </pre>
 * 
 */
package gov.noaa.nws.ncep.edex.plugin.wcp.decoder;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;

import org.apache.log4j.Logger;
import org.junit.Before;
import org.junit.Test;

public class WcpSeparatorTest {
    WcpSeparator sep;

    char[] cbuf;

    int ntime = 0;

    StringBuffer contents = new StringBuffer();

    byte[] actual = null;

    @Before
    public void initialize() {
        final Logger log = Logger.getLogger(getClass().getName());
        sep = new WcpSeparator();
        File file = new File(
                "unit-test/gov/noaa/nws/ncep/edex/plugin/wcp/decoder/2008102206.sevmkc");
        BufferedReader reader = null;

        try {
            reader = new BufferedReader(new FileReader(file));
            String text = null;

            /*
             * Repeat until all lines are read. Add control characters.
             */
            while ((text = reader.readLine()) != null) {
                if (text.length() != 0) {
                    contents.append(text).append("\r\r\n");
                }
            }
        } catch (FileNotFoundException e) {
            if (log.isInfoEnabled()) {
                log.info("File is not found");
            }
        } catch (IOException e) {
            if (log.isInfoEnabled()) {
                log.info("I/O Exception");
            }
        } finally {
            try {
                if (reader != null) {
                    reader.close();
                }
            } catch (IOException e) {
                if (log.isInfoEnabled()) {
                    log.info("I/O Exception");
                }
            }
        }
        sep = new WcpSeparator();
        actual = contents.toString().getBytes();
        sep.setData(actual, null);
    }

    @Test
    public void testHasNext() {
        assertTrue("Find WCP separator! ", sep.hasNext());
    }

    @Test
    public void testGetRecord() {
        byte[] expected = sep.next();
        String a = new String(actual);
        String e = new String(expected);
        String e4 = e.substring(4);
        System.out.println("expected e4=\n" + e4);
        String a3 = a.substring(3);
        System.out.println("actual a3=\n" + a3);
        assertEquals(e4.trim(), a3.trim());
    }
}
