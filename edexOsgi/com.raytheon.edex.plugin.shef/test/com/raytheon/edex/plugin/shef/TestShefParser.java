/**
 * This software was developed and / or modified by Raytheon Company,
 * pursuant to Contract DG133W-05-CQ-1067 with the US Government.
 * 
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * This software product contains export-restricted data whose
 * export/transfer/disclosure is restricted by U.S. law. Dissemination
 * to non-U.S. persons whether in the United States or abroad requires
 * an export license or other authorization.
 * 
 * Contractor Name:        Raytheon Company
 * Contractor Address:     6825 Pine Street, Suite 340
 *                         Mail Stop B8
 *                         Omaha, NE 68106
 *                         402.291.0100
 * 
 * See the AWIPS II Master Rights File ("Master Rights File.pdf") for
 * further licensing information.
 **/
package com.raytheon.edex.plugin.shef;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.util.List;

import org.junit.Before;
import org.junit.Test;

import com.raytheon.edex.esb.Headers;
import com.raytheon.edex.plugin.shef.ShefSeparator.ShefDecoderInput;
import com.raytheon.edex.plugin.shef.data.ShefRecord;
import com.raytheon.edex.plugin.shef.util.ParserToken;
import com.raytheon.edex.plugin.shef.util.ShefParm;
import com.raytheon.edex.plugin.shef.util.TokenType;
import com.raytheon.uf.common.dataplugin.shef.util.ShefConstants;
import com.raytheon.uf.common.ohd.AppsDefaults;

/**
 * JUnit test class for the ShefParser.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 5, 2018   5049      mpduff      Initial creation
 *
 * </pre>
 *
 * @author mpduff
 */

public class TestShefParser {

    private static final String TRACE_ID = "traceId";

    private AppsDefaults appsDefault = mock(AppsDefaults.class);

    private ShefParm shefParm = mock(ShefParm.class);

    @Before
    public void setup() {
        when(appsDefault.getBoolean(ShefConstants.SHEF_EMIT_SKIPPED, false))
                .thenReturn(false);
        when(shefParm.getSendCodeDurationDefaults("")).thenReturn(null);
    }

    @Test
    public void testTokenizeAFormat() {
        Headers headers = new Headers();
        headers.put(TRACE_ID, TRACE_ID);
        ShefSeparator separator = ShefSeparator.separate(getAFileData(),
                headers);

        while (separator.hasNext()) {
            ShefDecoderInput sdi = separator.next();
            SHEFParser parser = new SHEFParser(sdi, appsDefault, shefParm);
            List<ParserToken> tokens = parser.tokenize(sdi.record);

            assertTrue(
                    "Expected TokenType.START, got " + tokens.get(0).getType(),
                    tokens.get(0).getType() == TokenType.START);
            assertTrue(
                    "Expected TokenType.A_REC, got " + tokens.get(1).getType(),
                    tokens.get(1).getType() == TokenType.A_REC);
            assertTrue("Expected DVDN1, got " + tokens.get(2),
                    tokens.get(2).getToken().equalsIgnoreCase("DVDN1"));
            assertTrue("Expected PE of TX, got " + tokens.get(7).getToken(),
                    tokens.get(7).getToken().equals("TX"));
            assertTrue("Expected value of 47, got " + tokens.get(8).getToken(),
                    tokens.get(8).getToken().equals("47"));
            assertTrue("Expected PE of PP, got " + tokens.get(16).getToken(),
                    tokens.get(16).getToken().equals("PP"));
            assertTrue(
                    "Expected value of 0.00, got " + tokens.get(17).getToken(),
                    tokens.get(17).getToken().equals("0.00"));
        }
    }

    @Test
    public void testTokenizeBFormat() {
        Headers headers = new Headers();
        headers.put(TRACE_ID, TRACE_ID);
        ShefSeparator separator = ShefSeparator.separate(getBFileData(),
                headers);

        while (separator.hasNext()) {
            ShefDecoderInput sdi = separator.next();
            SHEFParser parser = new SHEFParser(sdi, appsDefault, shefParm);
            List<ParserToken> tokens = parser.tokenize(sdi.record);

            assertTrue(
                    "Expected TokenType.START, got " + tokens.get(0).getType(),
                    tokens.get(0).getType() == TokenType.START);
            assertTrue(
                    "Expected TokenType.B_REC_R, got "
                            + tokens.get(1).getType(),
                    tokens.get(1).getType() == TokenType.B_REC_R);
            assertTrue("Expected DMX, got " + tokens.get(2).getToken(),
                    tokens.get(2).getToken().equals("DMX"));
            assertTrue("Expected TAIRVX, got " + tokens.get(7).getToken(),
                    tokens.get(7).getToken().equals("TAIRVX"));
            assertTrue("Expected PPDRVZ, got " + tokens.get(13).getToken(),
                    tokens.get(13).getToken().equals("PPDRVZ"));
            assertTrue("Expected DNS, got " + tokens.get(19).getToken(),
                    tokens.get(19).getToken().equals("DNS"));
            assertTrue("Expected T, got " + tokens.get(24).getToken(),
                    tokens.get(24).getToken().equals("T"));
            assertTrue("Expected DUNN1, got " + tokens.get(74).getToken(),
                    tokens.get(74).getToken().equals("DUNN1"));
            assertTrue("Expected M, got " + tokens.get(81).getToken(),
                    tokens.get(81).getToken().equals("M"));
            assertTrue("Expected .END, got " + tokens.get(85).getToken(),
                    tokens.get(85).getToken().equals(".END"));
        }
    }

    @Test
    public void testTokenizeEFormat() {
        Headers headers = new Headers();
        headers.put(TRACE_ID, TRACE_ID);
        ShefSeparator separator = ShefSeparator.separate(getEFileData(),
                headers);

        int idx = 0;
        while (separator.hasNext()) {
            ShefDecoderInput sdi = separator.next();
            SHEFParser parser = new SHEFParser(sdi, appsDefault, shefParm);
            List<ParserToken> tokens = parser.tokenize(sdi.record);
            ShefRecord shefRec = parser.decode();
            if (idx == 0) {
                assertTrue(
                        "Expected TokenType.E_REC, got "
                                + tokens.get(1).getType(),
                        tokens.get(1).getType() == TokenType.E_REC);
                assertTrue("Expected HGIP, got " + tokens.get(9).getToken(),
                        tokens.get(9).getToken().equals("HGIP"));
                assertTrue("Expected 0.2, got " + tokens.get(19).getToken(),
                        tokens.get(19).getToken().equals("0.2"));
                assertEquals("Expected 4", 4, shefRec.getDataValues().size());

            } else {
                assertTrue("Expected HGIC9, got " + tokens.get(9).getToken(),
                        tokens.get(9).getToken().equals("HGIC9"));
                assertTrue("Expected DIH06, got " + tokens.get(11).getToken(),
                        tokens.get(11).getToken().equals("DIH06"));
                assertTrue("Expected 0.1, got " + tokens.get(53).getToken(),
                        tokens.get(53).getToken().equals("0.1"));
                assertEquals("Expected 56", 56, shefRec.getDataValues().size());
            }
            System.out.println(shefRec.getDataValues().size() + " values");
            System.out.println(shefRec.getDataValues());
            idx++;
        }
    }

    @Test
    public void testBFileWithMissingValues() {
        Headers headers = new Headers();
        headers.put(TRACE_ID, TRACE_ID);
        ShefSeparator separator = null;
        separator = ShefSeparator.separate(getShefFileWithMissingValues(),
                headers);
        while (separator.hasNext()) {
            ShefDecoderInput sdi = separator.next();
            SHEFParser parser = new SHEFParser(sdi, appsDefault, shefParm);
            List<ParserToken> tokens = parser.tokenize(sdi.record);
            assertTrue(
                    "Expected TokenType.START, got " + tokens.get(0).getType(),
                    tokens.get(0).getType() == TokenType.START);
            assertTrue(
                    "Expected TokenType.B_REC_R, got "
                            + tokens.get(1).getType(),
                    tokens.get(1).getType() == TokenType.B_REC_R);
            assertTrue("Expected SGF, got " + tokens.get(2).getToken(),
                    tokens.get(2).getToken().equals("SGF"));
            assertTrue("Expected DRH-18, got " + tokens.get(8).getToken(),
                    tokens.get(8).getToken().equals("DRH-18"));
            assertTrue("Expected SGF, got " + tokens.get(24).getToken(),
                    tokens.get(24).getToken().equals("SGF"));
            assertTrue("Expected /, got " + tokens.get(25).getToken(),
                    tokens.get(25).getToken().equals("/"));
            assertTrue(
                    "Expected empty string, got " + tokens.get(26).getToken(),
                    tokens.get(26).getToken().equals(""));
            assertTrue("Expected /, got " + tokens.get(27).getToken(),
                    tokens.get(27).getToken().equals("/"));
            assertTrue(
                    "Expected empty string, got " + tokens.get(28).getToken(),
                    tokens.get(28).getToken().equals(""));
            assertTrue("Expected /, got " + tokens.get(29).getToken(),
                    tokens.get(29).getToken().equals("/"));
            assertTrue("Expected 0.88, got " + tokens.get(30).getToken(),
                    tokens.get(30).getToken().equals("0.88"));

        }
    }

    private byte[] getAFileData() {
        String shef = "949\n";
        shef += "SRUS53 KOAX 091205\n";
        shef += "RR3OAX\n";
        shef += "IVROCS\n";
        shef += "\n";
        shef += ".A DVDN1 181109 C DH0600/TX 47/TN 19/TA 33/\n";
        shef += ".A1 PP 0.00/SD T/\n";
        shef += ".A DVDN1 181109 C DH0600/TX 47/TN 19/TA 33/\n";
        shef += ".A1 PP 0.00/SD T/\n";

        return shef.getBytes();
    }

    private byte[] getBFileData() {
        String shef = "SRUS53 KDMX 011210" + "\n";
        shef += "RR4DMX" + "\n\n";
        shef += ".BR DMX 1109 Z DH06/TAIRVX/DH12/TAIRVP/PPDRVZ/SFDRVZ/SDIRVZ"
                + "\n";
        shef += ": IOWA AWOS RTP FIRST GUESS PROCESSED BY THE IEM\n";
        shef += ":   06Z to 06Z HIGH TEMPERATURE FOR 05 FEB 2016\n";
        shef += ":   00Z TO 12Z TODAY LOW TEMPERATURE\n";
        shef += ":   12Z YESTERDAY TO 12Z TODAY RAINFALL\n";
        shef += ":   ...BASED ON REPORTED OBS...\n";
        shef += "DNS   :DENISON          :  28/  27 /    T  /    M /  M\n";
        shef += "AMW :DENISON : 28 / 27 / T / M / M\n";
        shef += "DMX :DENISON : T / 27 / T / M / M\n";
        shef += "DVN :ATLANTIC : 30 / 23 / 0.00 / M / T\n";
        shef += "ADU :AUDUBON : 30 / 28 / T / M / M\n";
        shef += "DUNN1 :WASHINGTON : 30 / 25 / T / M / M\n";
        shef += ".END\n";

        return shef.getBytes();
    }

    private byte[] getEFileData() {
        String shef = "540\n";
        shef += "FGUS53 KMSR 091801\n";
        shef += "CRFMIS\n\n";

        shef += "RIVER FORECAST\n";
        shef += "NWS NORTH CENTRAL RIVER FORECAST CENTER TWIN CITIES/CHANHASSEN MN\n";
        shef += "1118 AM CST TUE JAN 9 2018\n\n";

        shef += ": THIS PRODUCT HAS PRELIMINARY DATA THAT MAY BE SUBJECT TO REVISION.\n";
        shef += ": REFER TO YOUR LOCAL WFO FOR THE LATEST OFFICIAL RIVER FORECAST.\n";
        shef += ": \n";
        shef += ": NOTE...  This product includes observed precipitation, plus\n";
        shef += ":          it does NOT include any future precipitation.\n";
        shef += "\n";
        shef += ":\n";
        shef += ":\n";
        shef += ": Mississippi R  St Louis MO - EADM7 \n";
        shef += ": HSA:LSX   Flood Stage:30.0 FT   Fcst Issuance Stage:28.0 FT\n";
        shef += ".E EADM7 0108 Z DH18/DC01091718/HGIP/DIH06 :6-Hr Obs Stage (ft) \n";
        shef += ".E1  -0.2/ 0.0/ 0.1/ 0.2\n";
        shef += ".E EADM7 0109 Z DH18/DC01091718/HGIC9/DIH06 :6-Hr Fcst Stage (ft) \n";
        shef += ".E1  0.3/ 0.4/ 0.4/ 0.4/ 0.4/ 0.5/ 0.5/ 0.4 \n";
        shef += ".E1  0.4/ 0.4/ 0.4/ 0.3/ 0.3/ 0.3/ 0.2/ 0.2 \n";
        shef += ".E1  0.2/ 0.1/ 0.1/ 0.1/ 0.1/ 0.0/ 0.0/ 0.0 \n";
        shef += ".E1  0.0/ 0.0/ 0.0/ 0.0/ 0.0/ 0.0/ 0.0/ 0.1 \n";
        shef += ".E1  0.1/ 0.2/ 0.3/ 0.3/ 0.3/ 0.3/ 0.3/ 0.3 \n";
        shef += ".E1  0.2/ 0.2/ 0.2/ 0.1/ 0.1/ 0.0/ 0.0/ -0.1 \n";
        shef += ".E1  -0.2/ -0.2/ -0.3/ -0.5/ -0.6/ -0.7/ -0.9/ -1.0 \n";
        shef += ":\n";
        shef += ":\n";
        shef += ":\n";
        shef += ":END\n";

        return shef.getBytes();
    }

    private byte[] getShefFileWithMissingValues() {
        String shef = "646\n";
        shef += "SRUS53 KSGF 052030\n";
        shef += "RR3SGF\n\n";
        shef += ".BR SGF 180112 DH12/PPDRZ/DRH-18/PPQRZ/DRH-12/PPQRZ/DRH-06/PPQRZ/DRH-00/PPQRZ\n";
        shef += "SGF            /      /      /  0.88/ 0.99\n";
        shef += ".END\n";

        return shef.getBytes();
    }
}
