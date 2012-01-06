/**
 * This Java class is the JUnit test for the decoder separator.
 * 
 * <pre>
 * 
 * F. J. Yen	 6/02/2009	Creation
 * </pre>
 * 
 */
package gov.noaa.nws.ncep.edex.plugin.idft.decoder;

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

public class IdftSeparatorTest {
    IdftSeparator sep;

    char[] cbuf;

    int ntime = 0;

    StringBuffer contents = new StringBuffer();

    byte[] messageData = null;

    @Before
    public void initialize() {
        final Logger log = Logger.getLogger(getClass().getName());
        sep = new IdftSeparator();
        File file = new File(
                "unit-test/gov/noaa/nws/ncep/edex/plugin/idft/decoder/2009060100.mar");
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
        sep = new IdftSeparator();
        messageData = contents.toString().getBytes();
        sep.setData(messageData, null);
    }

    @Test
    public void testHasNext() {
        assertTrue("Find IDFT separator! ", sep.hasNext());
    }

    /*
     * Test getting the first record in the bulletin
     */
    @Test
    public void testHasNext1() {
        byte[] expected = sep.next();
        String actual = "FZXX41 KWNO 010000\r\r\n" + "IDMPLR\r\r\n"
                + "PART PAA GLOBAL SEA ICE DRIFT VECTORS\r\r\n"
                + "AMOSPHERIC DRIVING ONLY\r\r\n"
                + "24HR FORECAST VT 06/02/09 0000 UTC\n"
                + " 1  197   5.9 \r\r\n";
        String e = new String(expected);
        assertEquals(e.trim(), actual.trim());
    }

    /*
     * Test getting the second record in the bulletin
     */
    @Test
    public void testHasNext2() {
        byte[] expected = sep.next();
        expected = sep.next();
        String actual = "FZXX41 KWNO 010000\r\r\n" + "IDMPLR\r\r\n"
                + "PART PAA GLOBAL SEA ICE DRIFT VECTORS\r\r\n"
                + "AMOSPHERIC DRIVING ONLY\r\r\n"
                + "24HR FORECAST VT 06/02/09 0000 UTC\n"
                + " 2  241   1.7 \r\r\n";
        String e = new String(expected);
        assertEquals(e.trim(), actual.trim());
    }
}
