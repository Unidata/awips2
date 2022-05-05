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

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Optional;

import org.junit.Before;
import org.junit.Test;

import com.raytheon.edex.esb.Headers;
import com.raytheon.edex.plugin.shef.ShefSeparator.ShefDecoderInput;
import com.raytheon.edex.plugin.shef.data.ShefData;
import com.raytheon.edex.plugin.shef.data.ShefRecord;
import com.raytheon.edex.plugin.shef.util.ParserToken;
import com.raytheon.edex.plugin.shef.util.ShefParm;
import com.raytheon.edex.plugin.shef.util.TokenType;
import com.raytheon.uf.common.dataplugin.shef.util.ParameterCode.PhysicalElement;
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
 * Jan 05, 2018  5049      mduff       Initial creation.
 * Mar 09, 2018  6881      mduff       Renamed to follow standard JUnit naming convention.
 * Jul 10, 2018  6990      mduff       Added a DQZ test for each shef type, .A, .B, and .E.
 * Jul 12, 2018  7022      dgilling    Added test for files using metric units
 *                                     (DUS directive) and containing missing
 *                                     values.
 * Jul 18, 2018  6950      dgilling    Added test for files combining missing
 *                                     data with qualifier codes.
 *
 * </pre>
 *
 * @author mpduff
 */

public class ShefParserTest {

    private static final String EMPTY_STRING = "";

    private static final String SLASH = "/";

    private static final String SGF = "SGF";

    private static final String TRACE_ID = "traceId";

    private AppsDefaults appsDefault = mock(AppsDefaults.class);

    private ShefParm shefParm = mock(ShefParm.class);

    @Before
    public void setup() {
        when(appsDefault.getBoolean(ShefConstants.SHEF_EMIT_SKIPPED, false))
                .thenReturn(false);
        when(shefParm.getSendCodeDurationDefaults(EMPTY_STRING))
                .thenReturn(null);
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
                    "DVDN1".equalsIgnoreCase(tokens.get(2).getToken()));
            assertTrue("Expected PE of TX, got " + tokens.get(7).getToken(),
                    "TX".equals(tokens.get(7).getToken()));
            assertTrue("Expected value of 47, got " + tokens.get(8).getToken(),
                    "47".equals(tokens.get(8).getToken()));
            assertTrue("Expected PE of PP, got " + tokens.get(16).getToken(),
                    "PP".equals(tokens.get(16).getToken()));
            assertTrue(
                    "Expected value of 0.00, got " + tokens.get(17).getToken(),
                    "0.00".equals(tokens.get(17).getToken()));
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
                    "DMX".equals(tokens.get(2).getToken()));
            assertTrue("Expected TAIRVX, got " + tokens.get(7).getToken(),
                    "TAIRVX".equals(tokens.get(7).getToken()));
            assertTrue("Expected PPDRVZ, got " + tokens.get(13).getToken(),
                    "PPDRVZ".equals(tokens.get(13).getToken()));
            assertTrue("Expected DNS, got " + tokens.get(19).getToken(),
                    "DNS".equals(tokens.get(19).getToken()));
            assertTrue("Expected T, got " + tokens.get(24).getToken(),
                    "T".equals(tokens.get(24).getToken()));
            assertTrue("Expected DUNN1, got " + tokens.get(74).getToken(),
                    "DUNN1".equals(tokens.get(74).getToken()));
            assertTrue("Expected M, got " + tokens.get(81).getToken(),
                    "M".equals(tokens.get(81).getToken()));
            assertTrue("Expected .END, got " + tokens.get(85).getToken(),
                    ".END".equals(tokens.get(85).getToken()));
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
                        "HGIP".equals(tokens.get(9).getToken()));
                assertTrue("Expected 0.2, got " + tokens.get(19).getToken(),
                        "0.2".equals(tokens.get(19).getToken()));
                assertEquals("Expected 4", 4, shefRec.getDataValues().size());

            } else {
                assertTrue("Expected HGIC9, got " + tokens.get(9).getToken(),
                        "HGIC9".equals(tokens.get(9).getToken()));
                assertTrue("Expected DIH06, got " + tokens.get(11).getToken(),
                        "DIH06".equals(tokens.get(11).getToken()));
                assertTrue("Expected 0.1, got " + tokens.get(53).getToken(),
                        "0.1".equals(tokens.get(53).getToken()));
                assertEquals("Expected 56", 56, shefRec.getDataValues().size());
            }
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
                    tokens.get(2).getToken().equals(SGF));
            assertTrue("Expected DRH-18, got " + tokens.get(8).getToken(),
                    "DRH-18".equals(tokens.get(8).getToken()));
            assertTrue("Expected SGF, got " + tokens.get(24).getToken(),
                    tokens.get(24).getToken().equals(SGF));
            assertTrue("Expected /, got " + tokens.get(25).getToken(),
                    tokens.get(25).getToken().equals(SLASH));
            assertTrue(
                    "Expected empty string, got " + tokens.get(26).getToken(),
                    EMPTY_STRING.equals(tokens.get(26).getToken()));
            assertTrue("Expected /, got " + tokens.get(27).getToken(),
                    tokens.get(27).getToken().equals(SLASH));
            assertTrue(
                    "Expected empty string, got " + tokens.get(28).getToken(),
                    EMPTY_STRING.equals(tokens.get(28).getToken()));
            assertTrue("Expected /, got " + tokens.get(29).getToken(),
                    tokens.get(29).getToken().equals(SLASH));
            assertTrue("Expected 0.88, got " + tokens.get(30).getToken(),
                    "0.88".equals(tokens.get(30).getToken()));
        }
    }

    @Test
    public void testBFileDQZParsing() {
        Headers headers = new Headers();
        headers.put(TRACE_ID, TRACE_ID);
        ShefSeparator separator = null;
        separator = ShefSeparator.separate(getBShefFileDQZ(), headers);
        while (separator.hasNext()) {
            ShefDecoderInput sdi = separator.next();
            SHEFParser parser = new SHEFParser(sdi, appsDefault, shefParm);
            List<ParserToken> tokens = parser.tokenize(sdi.record);
            assertTrue(
                    "Expected qualifier code DQZ, got "
                            + tokens.get(6).getToken(),
                    "DQZ".equals(tokens.get(6).getToken()));
        }
    }

    @Test
    public void testAFileDQZParsing() {
        Headers headers = new Headers();
        headers.put(TRACE_ID, TRACE_ID);
        ShefSeparator separator = null;
        separator = ShefSeparator.separate(getAShefFileDQZ(), headers);
        while (separator.hasNext()) {
            ShefDecoderInput sdi = separator.next();
            SHEFParser parser = new SHEFParser(sdi, appsDefault, shefParm);
            List<ParserToken> tokens = parser.tokenize(sdi.record);
            assertTrue(
                    "Expected qualifier code DQZ, got "
                            + tokens.get(5).getToken(),
                    "DQZ".equals(tokens.get(5).getToken()));
        }
    }

    @Test
    public void testEFileDQZParsing() {
        Headers headers = new Headers();
        headers.put(TRACE_ID, TRACE_ID);
        ShefSeparator separator = null;
        separator = ShefSeparator.separate(getEShefFileDQZ(), headers);

        while (separator.hasNext()) {
            ShefDecoderInput sdi = separator.next();
            SHEFParser parser = new SHEFParser(sdi, appsDefault, shefParm);
            List<ParserToken> tokens = parser.tokenize(sdi.record);
            assertTrue(
                    "Expected qualifier code DQZ, got "
                            + tokens.get(5).getToken(),
                    "DQZ".equals(tokens.get(5).getToken()));
        }
    }

    @Test
    public void testFileMetricUnitsWithMissingData() {
        Headers headers = new Headers();
        headers.put(TRACE_ID, TRACE_ID);
        ShefSeparator separator = null;
        separator = ShefSeparator.separate(getShefFileMetricUnitsMissingData(),
                headers);

        assertTrue("Failed to decode data", separator.hasNext());

        ShefDecoderInput sdi = separator.next();
        SHEFParser parser = new SHEFParser(sdi, appsDefault, shefParm);
        ShefRecord shefRec = parser.decode();
        Optional<ShefData> dataValue = shefRec.getDataValues()
                .stream().filter(
                        s -> s.getPhysicalElement() == PhysicalElement.PRECIPITATION_INCREMENT)
                .findFirst();

        assertTrue("Record with physical element PP not decoded",
                dataValue.isPresent());
        assertTrue(
                "Expected MISSING value, got "
                        + dataValue.get().getStringValue(),
                dataValue.get().getValue().doubleValue() == -9999.0);
    }

    @Test
    public void testAFileMissingDataWithQualifierCodes() {
        Headers headers = new Headers();
        headers.put(TRACE_ID, TRACE_ID);
        ShefSeparator separator = null;
        separator = ShefSeparator
                .separate(getAFileMissingDataWithQualifierCodes(),
                headers);
        assertTrue("Failed to decode data", separator.hasNext());

        Collection<ShefRecord> records = new ArrayList<>();
        while (separator.hasNext()) {
            ShefDecoderInput sdi = separator.next();
            SHEFParser parser = new SHEFParser(sdi, appsDefault, shefParm);
            ShefRecord shefRec = parser.decode();
            records.add(shefRec);
        }
        assertTrue("Failed to decode all records", records.size() == 3);

        Optional<ShefRecord> shefRecord = records.stream()
                .filter(s -> "BRKM2".equals(s.getLocationId())).findFirst();
        assertTrue("Record with data string MB not decoded",
                shefRecord.isPresent());
        assertTrue(
                "Record with data string MB not decoded as missing data value",
                shefRecord.get().getDataValues().get(0)
                        .getValue() == ShefConstants.SHEF_MISSING);
        assertTrue("Record with data string MB not decoded as B data qualifier",
                "B".equals(shefRecord.get().getDataValues().get(0)
                        .getQualifier()));

        shefRecord = records.stream()
                .filter(s -> "BRKM3".equals(s.getLocationId())).findFirst();
        assertTrue("Record with data string MM not decoded",
                shefRecord.isPresent());
        assertTrue(
                "Record with data string MM not decoded as missing data value",
                shefRecord.get().getDataValues().get(0)
                        .getValue() == ShefConstants.SHEF_MISSING);
        assertTrue(
                "Record with data string MM not decoded as null data qualifier",
                "Z".equals(shefRecord.get().getDataValues().get(0)
                        .getQualifier()));

        shefRecord = records.stream()
                .filter(s -> "BRKM4".equals(s.getLocationId())).findFirst();
        assertTrue("Record with data string MMM not decoded",
                shefRecord.isPresent());
        assertTrue(
                "Record with data string MMM not decoded as missing data value",
                shefRecord.get().getDataValues().get(0)
                        .getValue() == ShefConstants.SHEF_MISSING);
        assertTrue(
                "Record with data string MMM not decoded as M data qualifier",
                "M".equals(shefRecord.get().getDataValues().get(0)
                        .getQualifier()));
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

    private byte[] getBShefFileDQZ() {
        String shef = "RUSS55 KSLC 070918\n";
        shef += "HMTSLC\n";
        shef += ".B DEN 151208 DH18/DQZ/PPHRZZZ/XRIRZZZ/USIRZZZ/TAIRZZZ/UDIRZZZ/TDIRZZZ\n";
        shef += ": PSD SURFACE METEOROLOGY\n";
        shef += ": PRECIP / RH /WIND SPD/ TEMP /WIND DIR/ DEW PT\n";
        shef += "ACVCA 0.00 / 97.49 / 1.82 / 59.61 / 158.82 / 58.89 : MCKINLEYVILLE, CA (40.97, -124.11, 184 FT)\n";
        shef += "ARLC1 0.00 / 79.23 / / 51.86 / / 45.63 : ARNOLD, CA (38.23, -120.36, 3885 FT)\n";
        shef += ".END\n";

        return shef.getBytes();
    }

    private byte[] getAShefFileDQZ() {
        String shef = "949\n";
        shef += "SRUS53 KOAX 091205\n";
        shef += "RR3OAX\n";
        shef += "IVROCS\n";
        shef += "\n";
        shef += ".A DVDN1 181109 C DQZ DH0600/TX 47/TN 19/TA 33/\n";
        shef += ".A1 PP 0.00/SD T/\n";
        shef += ".A DVDN1 181109 C DQZ DH0600/TX 47/TN 19/TA 33/\n";
        shef += ".A1 PP 0.00/SD T/\n";

        return shef.getBytes();
    }

    private byte[] getEShefFileDQZ() {
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
        shef += ".E EADM7 0108 Z DQZ DH18/DC01091718/HGIP/DIH06 :6-Hr Obs Stage (ft) \n";
        shef += ".E1  -0.2/ 0.0/ 0.1/ 0.2\n";
        shef += ".E EADM7 0109 Z DQZ DH18/DC01091718/HGIC9/DIH06 :6-Hr Fcst Stage (ft) \n";
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

    private byte[] getShefFileMetricUnitsMissingData() {
        StringBuilder shef = new StringBuilder();
        shef.append("SRUS53 KGRR 291616").append('\n').append("RR2GRR")
                .append('\n').append(":KALAMAZOO - KALAMAZOO NATURE CENTER")
                .append('\n').append(":LAT/LON 42.3538/-85.5855 ELEV 268 M")
                .append('\n')
                .append(".A KZOM4 20160229 Z DH12 /DUS/TA -0.21/US 2.452/UD 262/PPHRP M/XR 81.7")
                .append('\n').append(".A1 DUE/TB 2.036/TB 4.053").append('\n')
                .append('\n')
                .append(": THESE OBSERVATIONS WERE COLLECTED VIA THE MICHIGAN AUTOMATED WEATHER")
                .append('\n')
                .append(": NETWORK (MAWN), WHICH IS OPERATED BY THE AGRICULTURAL WEATHER OFFICE")
                .append('\n').append(": OF MICHIGAN STATE UNIVERSITY.")
                .append('\n')
                .append(": WWW.AGWEATHER.GEO.MSU.EDU");
        return shef.toString().getBytes();
    }

    private byte[] getAFileMissingDataWithQualifierCodes() {
        StringBuilder shef = new StringBuilder();
        shef.append("SRUS53 KOAX 232005").append('\n').append("RR3OAX")
                .append('\n').append('\n')
                .append(".AR BRKM2 20150623 Z DH1517/HGIRGZ MB").append('\n')
                .append('\n').append(".AR BRKM3 20150623 Z DH1517/HGIRGZ MM")
                .append('\n').append('\n')
                .append(".AR BRKM4 20150623 Z DH1517/HGIRGZ MMM").append('\n')
                .append('\n');
        return shef.toString().getBytes();
    }
}
