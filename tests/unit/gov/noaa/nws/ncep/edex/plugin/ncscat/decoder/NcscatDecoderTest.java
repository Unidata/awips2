/**

 **/
package gov.noaa.nws.ncep.edex.plugin.ncscat.decoder;

import static org.junit.Assert.assertEquals;
import gov.noaa.nws.ncep.common.dataplugin.ncscat.NcscatMode;

import java.nio.ByteBuffer;

import javax.xml.bind.DatatypeConverter;

import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

import com.raytheon.uf.common.time.DataTime;

/**
 * Unit Tests for NtransDecoder Class
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 20, 2014            bhebbard     Initial creation
 * 
 * </pre>
 * 
 * @author bhebbard
 * @version 1.0
 */

public class NcscatDecoderTest {

    static ByteBuffer simulatedInput;

    DataTime actual, expected;

    /**
     * @throws java.lang.Exception
     */
    @BeforeClass
    public static void setUpBeforeClass() throws Exception {
        byte[] b = DatatypeConverter.parseHexBinary(validAscatDump);
        simulatedInput = ByteBuffer.wrap(b);
    }

    /**
     * @throws java.lang.Exception
     */
    @AfterClass
    public static void tearDownAfterClass() throws Exception {
        simulatedInput.clear();
    }

    /**
     * @throws java.lang.Exception
     */
    @Before
    public void setUp() throws Exception {
    }

    /**
     * @throws java.lang.Exception
     */
    @After
    public void tearDown() throws Exception {
    }

    // @formatter:off
    /**
     * Now, the individual test cases!
     * 
     *
     */
    // @formatter:on

    /**
     * VALID CASES
     */

    /**
     * A "typical" scenario:
     * 
     * Test method for
     * {@link gov.noaa.nws.ncep.edex.plugin.ncscat.decoder.NcscatDecoder#consistentWith(java.nio.ByteBuffer)}
     * .
     */
    @Test
    public final void testConsistentWith_01() {
        // simple case: valid ASCAT data confirmed consistent with ASCAT
        //
        Boolean actual = NcscatMode.ASCAT.consistentWith(simulatedInput);
        //
        Boolean expected = true;
        //
        assertEquals(expected, actual);
    }

    /**
     * A "typical" scenario:
     * 
     * Test method for
     * {@link gov.noaa.nws.ncep.edex.plugin.ncscat.decoder.NcscatDecoder#consistentWith(java.nio.ByteBuffer)}
     * .
     */
    @Test
    public final void testConsistentWith_02() {
        // case: valid ASCAT data NOT consistent with ASCAT_HI
        //
        Boolean actual = NcscatMode.ASCAT_HI.consistentWith(simulatedInput);
        //
        Boolean expected = false;
        //
        assertEquals(expected, actual);
    }

    /**
     * A "typical" scenario:
     * 
     * Test method for
     * {@link gov.noaa.nws.ncep.edex.plugin.ncscat.decoder.NcscatDecoder#consistentWith(java.nio.ByteBuffer)}
     * .
     */
    @Test
    public final void testConsistentWith_03() {
        // case: valid ASCAT data NOT consistent with QUIKSCAT
        //
        Boolean actual = NcscatMode.QUIKSCAT.consistentWith(simulatedInput);
        //
        Boolean expected = false;
        //
        assertEquals(expected, actual);
    }

    /**
     * A "typical" scenario:
     * 
     * Test method for
     * {@link gov.noaa.nws.ncep.edex.plugin.ncscat.decoder.NcscatDecoder#consistentWith(java.nio.ByteBuffer)}
     * .
     */
    // CURRENTLY TOO BIG FOR HARDCODED DATASET; GOES PAST END OF BUFFER
    // @Test
    // public final void testConsistentWith_04() {
    // case: valid ASCAT data NOT consistent with QUIKSCAT_HI
    //
    // Boolean actual = NcscatMode.QUIKSCAT_HI.consistentWith(simulatedInput);
    //
    // Boolean expected = false;
    //
    // assertEquals(expected, actual);
    // }

    /**
     * A "typical" scenario:
     * 
     * Test method for
     * {@link gov.noaa.nws.ncep.edex.plugin.ncscat.decoder.NcscatDecoder#consistentWith(java.nio.ByteBuffer)}
     * .
     */
    @Test
    public final void testConsistentWith_05() {
        // case: valid ASCAT data NOT consistent with EXASCT
        //
        Boolean actual = NcscatMode.EXASCT.consistentWith(simulatedInput);
        //
        Boolean expected = false;
        //
        assertEquals(expected, actual);
    }

    /**
     * A "typical" scenario:
     * 
     * Test method for
     * {@link gov.noaa.nws.ncep.edex.plugin.ncscat.decoder.NcscatDecoder#consistentWith(java.nio.ByteBuffer)}
     * .
     */
    @Test
    public final void testConsistentWith_06() {
        // case: valid ASCAT data NOT consistent with EXASCT_HI
        //
        Boolean actual = NcscatMode.EXASCT_HI.consistentWith(simulatedInput);
        //
        Boolean expected = false;
        //
        assertEquals(expected, actual);
    }

    /**
     * A "typical" scenario:
     * 
     * Test method for
     * {@link gov.noaa.nws.ncep.edex.plugin.ncscat.decoder.NcscatDecoder#consistentWith(java.nio.ByteBuffer)}
     * .
     */
    @Test
    public final void testConsistentWith_07() {
        // case: valid ASCAT data NOT consistent with OSCAT
        //
        Boolean actual = NcscatMode.OSCAT.consistentWith(simulatedInput);
        //
        Boolean expected = false;
        //
        assertEquals(expected, actual);
    }

    /**
     * A "typical" scenario:
     * 
     * Test method for
     * {@link gov.noaa.nws.ncep.edex.plugin.ncscat.decoder.NcscatDecoder#consistentWith(java.nio.ByteBuffer)}
     * .
     */
    @Test
    public final void testConsistentWith_08() {
        // case: valid ASCAT data NOT consistent with OSCAT_HI
        //
        Boolean actual = NcscatMode.OSCAT_HI.consistentWith(simulatedInput);
        //
        Boolean expected = false;
        //
        assertEquals(expected, actual);
    }

    /**
     * A "typical" scenario:
     * 
     * Test method for
     * {@link gov.noaa.nws.ncep.edex.plugin.ncscat.decoder.NcscatDecoder#consistentWith(java.nio.ByteBuffer)}
     * .
     */
    @Test
    public final void testConsistentWith_09() {
        // case: valid ASCAT data NOT consistent with WSCAT
        //
        Boolean actual = NcscatMode.WSCAT.consistentWith(simulatedInput);
        //
        Boolean expected = false;
        //
        assertEquals(expected, actual);
    }

    /**
     * A "typical" scenario:
     * 
     * Test method for
     * {@link gov.noaa.nws.ncep.edex.plugin.ncscat.decoder.NcscatDecoder#consistentWith(java.nio.ByteBuffer)}
     * .
     */
    @Test
    public final void testConsistentWith_10() {
        // case: valid ASCAT data NOT consistent with WSCAT
        //
        Boolean actual = NcscatMode.WSCAT.consistentWith(simulatedInput);
        //
        Boolean expected = false;
        //
        assertEquals(expected, actual);
    }

    // @formatter:off
    private final static String validAscatDump = (
            /*00000000*/  "01 21 00 10 00 30 00 00  1a 9d 1a ae 1a bf 1a d0" +
            /*00000010*/  "1a e1 1a f1 1b 02 1b 12  1b 23 1b 33 1b 43 1b 53" +
            /*00000020*/  "1b 63 1b 73 1b 83 1b 92  1b a2 1b b1 1b c0 1b cf" +
            /*00000030*/  "1b de 1d 49 1d 51 1d 59  1d 61 1d 69 1d 70 1d 77" +
            /*00000040*/  "1d 7d 1d 83 1d 89 1d 8f  1d 94 1d 99 1d 9e 1d a2" +
            /*00000050*/  "1d a6 1d a9 1d ad 1d b0  1d b2 1d b4 08 8b 08 b2" +
            /*00000060*/  "08 d9 09 01 09 2a 09 54  09 7e 09 a9 09 d4 0a 00" +
            /*00000070*/  "0a 2d 0a 5a 0a 88 0a b7  0a e7 0b 17 0b 48 0b 7a" +
            /*00000080*/  "0b ad 0b e0 0c 14 13 e7  14 38 14 89 14 db 15 2e" +
            /*00000090*/  "15 82 15 d7 16 2c 16 82  16 d9 17 30 17 88 17 e1" +
            /*000000a0*/  "18 3a 18 94 18 ef 19 49  19 a5 1a 00 1a 5c 1a b9" +
            /*000000b0*/  "00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00" +
            /*000000c0*/  "00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00" +
            /*000000d0*/  "00 00 00 00 00 00 00 c0  00 80 00 00 08 00 08 00" +
            /*000000e0*/  "08 80 00 80 00 00 00 00  00 00 00 00 00 00 00 00" +
            /*000000f0*/  "00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00" +
            /*00000100*/  "00 00 00 00 d8 f1 d8 f1  d8 f1 d8 f1 d8 f1 d8 f1" +
            /*00000110*/  "d8 f1 d8 f1 d8 f1 d8 f1  d8 f1 d8 f1 d8 f1 d8 f1" +
            /*00000120*/  "d8 f1 d8 f1 d8 f1 d8 f1  d8 f1 03 75 03 ea 01 3d" +
            /*00000130*/  "01 14 01 05 01 1c 01 4f  d8 f1 d8 f1 d8 f1 d8 f1" +
            /*00000140*/  "d8 f1 d8 f1 d8 f1 d8 f1  d8 f1 d8 f1 d8 f1 d8 f1" +
            /*00000150*/  "d8 f1 d8 f1 d8 f1 d8 f1  00 00 00 00 00 00 00 00" +
            /*00000160*/  "00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00" +
            /*00000170*/  "00 00 00 00 00 00 00 00  00 00 00 00 00 00 12 c0" +
            /*00000180*/  "29 ad 28 3c 26 70 21 79  1d 10 1d 10 00 00 00 00" +
            /*00000190*/  "00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00" +
            /*000001a0*/  "00 00 00 00 00 00 00 00  00 00 00 00 d8 f1 d8 f1" +
            /*000001b0*/  "d8 f1 d8 f1 d8 f1 d8 f1  d8 f1 d8 f1 d8 f1 d8 f1" +
            /*000001c0*/  "d8 f1 d8 f1 d8 f1 d8 f1  d8 f1 d8 f1 d8 f1 d8 f1" +
            /*000001d0*/  "d8 f1 00 02 00 02 00 01  00 01 00 02 00 02 00 02" +
            /*000001e0*/  "d8 f1 d8 f1 d8 f1 d8 f1  d8 f1 d8 f1 d8 f1 d8 f1" +
            /*000001f0*/  "d8 f1 d8 f1 d8 f1 d8 f1  d8 f1 d8 f1 d8 f1 d8 f1" +
            /*00000200*/  "00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00" +
            /*00000210*/  "00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00" +
            /*00000220*/  "00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00" +
            /*00000230*/  "00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00" +
            /*00000240*/  "00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00" +
            /*00000250*/  "00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00" +
            /*00000260*/  "00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00" +
            /*00000270*/  "00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00" +
            /*00000280*/  "00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00" +
            /*00000290*/  "00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00" +
            /*000002a0*/  "00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00" +
            /*000002b0*/  "00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00" +
            /*000002c0*/  "00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00" +
            /*000002d0*/  "00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00" +
            /*000002e0*/  "00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00" +
            /*000002f0*/  "00 00 00 00 00 00 00 00  00 00 00 00 01 21 00 10" +
            /*00000300*/  "00 30 00 00 1a ab 1a bc  1a ce 1a df 1a ef 1b 00" +
            /*00000310*/  "1b 11 1b 22 1b 32 1b 43  1b 53 1b 63 1b 73 1b 83" +
            /*00000320*/  "1b 93 1b a2 1b b2 1b c1  1b d1 1b e0 1b ef 1d 5e" +
            /*00000330*/  "1d 66 1d 6e 1d 76 1d 7e  1d 85 1d 8c 1d 92 1d 99" +
            /*00000340*/  "1d 9f 1d a4 1d a9 1d ae  1d b3 1d b7 1d bb 1d bf" +
            /*00000350*/  "1d c2 1d c5 1d c8 1d ca  08 5c 08 83 08 aa 08 d3" +
            /*00000360*/  "08 fb 09 25 09 4f 09 79  09 a5 09 d1 09 fe 0a 2b" +
            /*00000370*/  "0a 59 0a 88 0a b7 0a e8  0b 19 0b 4b 0b 7d 0b b1" +
            /*00000380*/  "0b e5 13 c7 14 18 14 6a  14 be 15 12 15 66 15 bc" +
            /*00000390*/  "16 13 16 6a 16 c2 17 1a  17 74 17 ce 18 28 18 84" +
            /*000003a0*/  "18 df 19 3b 19 98 19 f5  1a 53 1a b0 00 00 00 00" +
            /*000003b0*/  "00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00" +
            /*000003c0*/  "00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00" +
            /*000003d0*/  "00 00 00 c0 00 00 00 00  08 00 08 00 00 00 00 80" +
            /*000003e0*/  "00 80 00 00 00 00 00 00  00 00 00 00 00 00 00 00" +
            /*000003f0*/  "00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00" +
            /*00000400*/  "d8 f1 d8 f1 d8 f1 d8 f1  d8 f1 d8 f1 d8 f1 d8 f1" +
            /*00000410*/  "d8 f1 d8 f1 d8 f1 d8 f1  d8 f1 d8 f1 d8 f1 d8 f1" +
            /*00000420*/  "d8 f1 d8 f1 d8 f1 03 52  04 3d 01 4e 01 1b 01 0c" +
            /*00000430*/  "01 30 01 5f 01 79 d8 f1  d8 f1 d8 f1 d8 f1 d8 f1" +
            /*00000440*/  "d8 f1 d8 f1 d8 f1 d8 f1  d8 f1 d8 f1 d8 f1 d8 f1" +
            /*00000450*/  "d8 f1 d8 f1 00 00 00 00  00 00 00 00 00 00 00 00" +
            /*00000460*/  "00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00" +
            /*00000470*/  "00 00 00 00 00 00 00 00  00 00 13 7e 2b fb 27 9c" +
            /*00000480*/  "26 0c 21 d3 1e 0a 1e 1d  1f c2 00 00 00 00 00 00" +
            /*00000490*/  "00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00" +
            /*000004a0*/  "00 00 00 00 00 00 00 00  d8 f1 d8 f1 d8 f1 d8 f1" +
            /*000004b0*/  "d8 f1 d8 f1 d8 f1 d8 f1  d8 f1 d8 f1 d8 f1 d8 f1" +
            /*000004c0*/  "d8 f1 d8 f1 d8 f1 d8 f1  d8 f1 d8 f1 d8 f1 00 02" +
            /*000004d0*/  "00 01 00 01 00 01 00 02  00 02 00 02 00 02 d8 f1" +
            /*000004e0*/  "d8 f1 d8 f1 d8 f1 d8 f1  d8 f1 d8 f1 d8 f1 d8 f1" +
            /*000004f0*/  "d8 f1 d8 f1 d8 f1 d8 f1  d8 f1 d8 f1 00 00 00 00" +
            /*00000500*/  "00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00" +
            /*00000510*/  "00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00" +
            /*00000520*/  "00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00" +
            /*00000530*/  "00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00" +
            /*00000540*/  "00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00" +
            /*00000550*/  "00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00" +
            /*00000560*/  "00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00" +
            /*00000570*/  "00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00" +
            /*00000580*/  "00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00" +
            /*00000590*/  "00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00" +
            /*000005a0*/  "00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00" +
            /*000005b0*/  "00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00" +
            /*000005c0*/  "00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00" +
            /*000005d0*/  "00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00" +
            /*000005e0*/  "00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00" +
            /*000005f0*/  "00 00 00 00 00 00 00 00  01 21 00 10 00 30 00 00" +
            /*00000600*/  "1a ba 1a cb 1a dc 1a ed  1a fe 1b 0f 1b 20 1b 31" +
            /*00000610*/  "1b 41 1b 52 1b 62 1b 73  1b 83 1b 93 1b a3 1b b3" +
            /*00000620*/  "1b c2 1b d2 1b e1 1b f0  1c 00 1d 72 1d 7b 1d 83" +
            /*00000630*/  "1d 8b 1d 92 1d 9a 1d a1  1d a7 1d ae 1d b4 1d b9" +
            /*00000640*/  "1d bf 1d c4 1d c9 1d cd  1d d1 1d d5 1d d8 1d db" +
            /*00000650*/  "1d de 1d e0 08 2d 08 54  08 7b 08 a3 08 cc 08 f5" +
            /*00000660*/  "09 1f 09 4a 09 75 09 a1  09 ce 09 fb 0a 29 0a 58" +
            /*00000670*/  "0a 87 0a b8 0a e9 0b 1b  0b 4e 0b 81 0b b5 13 a5" +
            /*00000680*/  "13 f8 14 4b 14 9f 14 f4  15 4a 15 a1 15 f9 16 51" +
            /*00000690*/  "16 aa 17 04 17 5f 17 ba  18 16 18 73 18 d0 19 2d" +
            /*000006a0*/  "19 8b 19 ea 1a 49 1a a8  00 00 00 00 00 00 00 00" +
            /*000006b0*/  "00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00" +
            /*000006c0*/  "00 00 00 00 00 00 00 00  00 00 00 00 00 c0 00 80" +
            /*000006d0*/  "00 00 00 00 00 00 08 00  00 00 00 00 00 80 00 80" +
            /*000006e0*/  "00 80 00 00 00 00 00 00  00 00 00 00 00 00 00 00" +
            /*000006f0*/  "00 00 00 00 00 00 00 00  00 00 00 00 d8 f1 d8 f1" )
            .replaceAll(" ", "") ;
    // @formatter:on

}
