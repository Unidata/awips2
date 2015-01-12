/**
 * This Java class is the JUnit test for the AWW decoder separator.
 *
 * <pre>
 *
 * L. Lin       04/09   Creation
 * </pre>
 *
 */
package gov.noaa.nws.ncep.edex.plugin.aww.decoder;

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

public class AwwSeparatorTest {
    AwwSeparator sep;

    char[] cbuf;

    int ntime = 0;

    StringBuffer contents = new StringBuffer();

    byte[] actual = null;

    @Before
    public void initialize() {
        final Logger log = Logger.getLogger(getClass().getName());
        sep = new AwwSeparator();
        File file = new File(
                "unit/gov/noaa/nws/ncep/edex/plugin/aww/decoder/2008091614.warn");

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
        sep = new AwwSeparator();
        actual = contents.toString().getBytes();
        sep.setData(actual, null);
    }

    @Test
    public void testHasNext() {
        assertTrue("Find AWW separator! ", sep.hasNext());
    }

    @Test
    public void testGetRecord() {
        byte[] expected = sep.next();
        String a = new String(actual);
        String e = new String(expected);
        String b = a.substring(8);
        assertEquals(e.trim(), b.trim());
    }
}
