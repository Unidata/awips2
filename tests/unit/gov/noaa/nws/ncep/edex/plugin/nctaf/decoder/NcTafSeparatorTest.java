/**
 * This Java class is the JUnit test for the nctaf decoder separator.
 * 
 *<pre>
 * SOFTWARE HISTORY
 *
 * Date                 Ticket#         Engineer                Description
 * ------------         ----------      -----------             --------------------------
 * 09/2011				#458			S. Gurung				Initial Creation
 * </pre>
 * 
 */
package gov.noaa.nws.ncep.edex.plugin.nctaf.decoder;

import static org.junit.Assert.assertTrue;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;

import org.apache.log4j.Logger;
import org.junit.Before;
import org.junit.Test;

public class NcTafSeparatorTest {
    NcTafSeparator sep;

    char[] cbuf;

    int ntime = 0;

    StringBuffer contents = new StringBuffer();

    byte[] messageData = null;

    @Before
    public void initialize() {
        final Logger log = Logger.getLogger(getClass().getName());
        sep = new NcTafSeparator();
        File file = new File(
                "unit/gov/noaa/nws/ncep/edex/plugin/nctaf/decoder/2011091217.TAF");
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
        sep = new NcTafSeparator();
        messageData = contents.toString().getBytes();
        sep.setData(messageData, null);
    }

    @Test
    public void testHasNext() {
        assertTrue("Find NCTAF separator! ", sep.hasNext());
    }

    /*
     * "Normal" case.
     */
    /*
     * @Test public void testNext1() { NcTafDecoder.NcTafDecoderInput input =
     * sep.next(); NcTafParser parser = null; NcTafRecord record = null; String
     * expected = null; try { parser = new NcTafParser(input.tafParts,
     * input.wmoHeader); record = parser.getDecodedRecord(); expected =
     * record.toString(); }catch (Exception e) { } String actual = "302 \r\r\n"
     * + "FTUS45 KPUB 121700 RRA\r\r\n" + "TAFCOS\r\r\n"+ "TAF\r\r\n" +
     * "KCOS 121202Z 1218/1318 16008KT P6SM SCT100\r\r\n" +
     * "FM130200 36010KT P6SM SCT100=\r\r\n"; //String e = new String(expected);
     * assertEquals(expected.trim(), actual.trim()); }
     */

}
