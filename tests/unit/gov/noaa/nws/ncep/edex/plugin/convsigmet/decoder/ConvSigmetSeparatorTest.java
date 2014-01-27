/**
 * This Java class is the JUnit test for the convsigmet decoder separator.
 *
 * <pre>
 *
 * L. Lin       04/09   Creation
 * </pre>
 *
 */

package gov.noaa.nws.ncep.edex.plugin.convsigmet.decoder;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;

import org.junit.Before;
import org.junit.Test;

public class ConvSigmetSeparatorTest {
    ConvSigmetSeparator sep;

    char[] cbuf;

    int ntime = 0;

    StringBuffer contents = new StringBuffer();

    byte[] actual = null;

    @Before
    public void setUp() throws Exception {
        sep = new ConvSigmetSeparator();
        File file = new File(
                "unit/gov/noaa/nws/ncep/edex/plugin/convsigmet/decoder/2009022414.conv");
        System.out.println(file.toString());
        BufferedReader reader = null;

        try {
            reader = new BufferedReader(new FileReader(file));
            String text = null;

            // repeat until all lines is read
            while ((text = reader.readLine()) != null) {
                if (text.length() != 0) {
                    contents.append(text).append("\r\r\n");
                }
            }
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        } catch (IOException e) {
            e.printStackTrace();
        } finally {
            try {
                if (reader != null) {
                    reader.close();
                }
            } catch (IOException e) {
                e.printStackTrace();
            }
        }
        sep = new ConvSigmetSeparator();
        actual = contents.toString().getBytes();
        sep.setData(actual, null);

    }

    @Test
    public void testHasNext() {
        assertTrue("Find Convsigmet separator! ", sep.hasNext());
    }

    @Test
    public void testGetRecord() {
        byte[] expected = sep.next();
        String a = new String(actual);
        String e = new String(expected);
        System.out.println("expected=\n" + e);
        String b = a.substring(3);
        System.out.println("actual b=\n" + b);
        assertEquals(e.trim(), b.trim());
    }

}