/**
 * This Java class is the JUnit test for the StormTrack decoder separator.
 * 
 * <pre>
 * 
 * F. J. Yen	 6/10	Creation
 * </pre>
 * 
 */
package gov.noaa.nws.ncep.edex.plugin.stormtrack.decoder;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;

import org.apache.log4j.Logger;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

public class StormTrackSeparatorTest {
    StormTrackSeparator sep;

    char[] cbuf;

    int ntime = 0;

    StringBuffer contents = new StringBuffer();

    byte[] actual = null;

    @BeforeClass
    public static void setUpBeforeClass() throws Exception {
    }

    @Before
    public void initialize() {
        final Logger log = Logger.getLogger(getClass().getName());
        sep = new StormTrackSeparator();
        File file = new File(
                "unit/gov/noaa/nws/ncep/edex/plugin/stormtrack/decoder/aep202009.dat");
        BufferedReader reader = null;

        try {
            System.out.println(" here ");

            reader = new BufferedReader(new FileReader(file));
            String text = null;

            /*
             * Repeat until all lines are read. Add control characters.
             */

            while ((text = reader.readLine()) != null) {
                System.out.println("line of text : " + text);
                if (text.length() != 0) {
                    contents.append(text).append("\n");
                } else {

                    System.out.println(" null text ");
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
                System.out.println(" here 2 ");

                if (reader != null) {
                    System.out.println(" here 2.1");

                    reader.close();
                }
            } catch (IOException e) {
                System.out.println(" here 3 ");

                if (log.isInfoEnabled()) {
                    log.info("I/O Exception");
                }
            }
        }
        sep = new StormTrackSeparator();

        actual = contents.toString().getBytes();
        if (new String(actual).contains("\n")) {
            System.out.print(" YES YES");
        }
        sep.setData(actual, null);
    }

    @Test
    public void testHasNext() {
        assertTrue("Find StormTrack separator! ", sep.hasNext());
    }

    @Test
    public void testGetRecord() {
        byte[] expected = sep.next();
        String a = new String(actual);
        String e = new String(expected);
        String e0 = e.substring(0);
        System.out.println("expected e0=\n" + e0 + "<<");
        // Following a1 is for using new format
        String a1 = a.substring(0, 129);
        // Following a1 is for using old format
        // String a1 = a.substring(0,44);
        System.out.println("actual a1=\n" + a1 + "<<");
        assertEquals(e0.trim(), a1.trim());
    }
}
