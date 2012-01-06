/**
 * This Java class is the JUnit test for the BUFRSGWHV decoder.
 * 
 ** <pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    	Engineer    Description
 * ------------ ---------- 	----------- --------------------------
 * 11/16/10		209		F.J. Yen	Initial coding
 * </pre>
 *
 * @author fjyen
 * @version 1.0
 */
package gov.noaa.nws.ncep.edex.plugin.bufrsgwhv.decoder;

import static org.junit.Assert.assertTrue;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;

import org.junit.Before;
import org.junit.Test;

public class BufrsgwhvDecoderTest {
    BufrSgwhvSeparator sep;

    @Before
    public void initialize() {
        File file = new File(
                "unit-test/gov/noaa/nws/ncep/edex/plugin/bufrsgwhv/decoder/js2_d20101019_s005205_e023034.bf");
        InputStream stream = null;
        byte[] bufrData = null;
        try {
            stream = new FileInputStream(file);
            bufrData = new byte[(int) file.length()];
            int bytesRead = stream.read(bufrData);
            if (bytesRead != bufrData.length) {
                bufrData = null;
            }
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        } catch (IOException e) {
            e.printStackTrace();

        } finally {
            try {
                if (stream != null) {
                    stream.close();
                }
            } catch (IOException e) {
                e.printStackTrace();
            }
        }
        sep = new BufrSgwhvSeparator();
        if (bufrData != null) {
            sep.setData(bufrData, null);
        }
    }

    @Test
    public void testHasNext() {
        assertTrue("Find BUFR Data! ", sep.hasNext());
    }
}
