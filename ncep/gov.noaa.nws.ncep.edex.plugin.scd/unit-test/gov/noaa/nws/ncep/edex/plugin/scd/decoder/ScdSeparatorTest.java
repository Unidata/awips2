/**
 * This Java class is the JUnit test for the decoder separator.
 * 
 *<pre>
 * SOFTWARE HISTORY
 *
 * Date                 Ticket#         Engineer                Description
 * ------------         ----------      -----------             --------------------------
 * 11/2008				#41				T. Lee					Initial Creation
 * 03/2009				#41				T. Lee					Migrated to TO10
 * 04/2009				#41				T. Lee					Added test cases for multiple reports
 * </pre>
 * 
 */
package gov.noaa.nws.ncep.edex.plugin.scd.decoder;

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

public class ScdSeparatorTest {
    ScdSeparator sep;

    char[] cbuf;

    int ntime = 0;

    StringBuffer contents = new StringBuffer();

    byte[] messageData = null;

    @Before
    public void initialize() {
        final Logger log = Logger.getLogger(getClass().getName());
        sep = new ScdSeparator();
        File file = new File(
                "unit-test/gov/noaa/nws/ncep/edex/plugin/scd/decoder/2009012900.SCD");
        BufferedReader reader = null;

        try {
            reader = new BufferedReader(new FileReader(file));
            String text = null;

            /*
             * Repeat until all lines is read. Add control characters.
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
        sep = new ScdSeparator();
        messageData = contents.toString().getBytes();
        sep.setData(messageData, null);
    }

    @Test
    public void testHasNext() {
        assertTrue("Find SCD separator! ", sep.hasNext());
    }

    /*
     * "Normal" case.
     */
    @Test
    public void testNext1() {
        byte[] expected = sep.next();
        String actual = "134 \r\r\n" + "CXUS63 KIWX 290000\r\r\n"
                + "SCDSBN\r\r\n" + "KSBN SCD 2359 931000 4/009 \r\r\n";
        String e = new String(expected);
        assertEquals(e.trim(), actual.trim());
    }

    /*
     * Normal case but retrieving 2nd bulletin.
     */
    // @Test
    public void testNext2() {
        byte[] expected = sep.next();
        expected = sep.next();
        String actual = "178 \r\r\n" + "CXUS65 KRIW 290000\r\r\n"
                + "SCDRIW\r\r\n" + "KRIW SCD 0000 8411878 4/001 \r\r\n";
        String e = new String(expected);
        assertEquals(e.trim(), actual.trim());
    }

    /*
     * Multiple reports in one bulletin.
     */
    // @Test
    public void testNext3() {
        byte[] expected = sep.next();
        expected = sep.next();
        expected = sep.next();
        String actual = "178 \r\r\n" + "CXUS65 KRIW 290000\r\r\n"
                + "SCDRIW\r\r\n" + "KAPX SCD 0005 8200005 931000 4/016 60000";
        String e = new String(expected);
        assertEquals(e.trim(), actual.trim());
    }
}
