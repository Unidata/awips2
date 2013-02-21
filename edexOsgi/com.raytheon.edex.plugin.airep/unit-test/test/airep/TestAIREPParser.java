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
package test.airep;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.TimeZone;

import org.junit.Test;

import com.raytheon.edex.plugin.airep.AirepSeparator;
import com.raytheon.edex.plugin.airep.decoder.AirepParser;
import com.raytheon.uf.common.dataplugin.airep.AirepRecord;
import com.raytheon.uf.edex.decodertools.time.TimeTools;

/**
 * Various tests against the AirepParser. Extracted some from "mains" that held
 * test code.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 7, 2012            jkorman     Initial creation
 *
 * </pre>
 *
 * @author jkorman
 * @version 1.0	
 */

public class TestAIREPParser {

    public static final String WMO_CRCRLF = "\r\r\n";
    
    public static final String WMO_LEAD = "\01";
    
    public static final String WMO_TRAIL = WMO_CRCRLF + "\03";
    
    public static final int TURB_BIT = 0x80;
    
    @Test
    public void testAIREPSeparator() {
        
        String data = WMO_LEAD + WMO_CRCRLF + "205" + WMO_CRCRLF + "UAPA01 KWBC 071554" +
        WMO_CRCRLF + "ARP DAL278 3336N 165E 1543 F320 MS40 110/010KT TB 1=" +
        WMO_CRCRLF + "ARP PAL110 12N 130E 1544 F370 MS48 090/025KT=" +
        WMO_CRCRLF + "ARP UAL595 3746N 08107W 1504 F370 MS46 294/058KT TB LGT RM B752 OV" +
        WMO_CRCRLF + "    BKW=" + WMO_TRAIL;
        String report = null;
        AirepSeparator sep = AirepSeparator.separate(data.getBytes(), null);
        assertNotNull(sep);
        assertTrue(sep.hasNext());
        report = sep.next().report;
        assertEquals("ARP DAL278 3336N 165E 1543 F320 MS40 110/010KT TB 1",report);
        report = sep.next().report;
        assertEquals("ARP PAL110 12N 130E 1544 F370 MS48 090/025KT",report);
        report = sep.next().report;
        assertEquals("ARP UAL595 3746N 08107W 1504 F370 MS46 294/058KT TB LGT RM B752 OV\r BKW",report);
    }

    @Test
    public void testLockup() {
        
        String data = WMO_LEAD + WMO_CRCRLF + "494" + WMO_CRCRLF + "UAUS31 KWBC 112254" +
        WMO_CRCRLF + "ARP UAL819 4626N 10618W 2248 F360 TB CONT LGT CHOP RM B752 OV" +
        WMO_CRCRLF + "    MLS270015=" + WMO_TRAIL;
        
        String report = null;
        AirepSeparator sep = AirepSeparator.separate(data.getBytes(), null);
        assertNotNull(sep);
        assertTrue(sep.hasNext());
        AirepParser p = null;
        Calendar c = TimeTools.getSystemCalendar(2012, 9, 10, 16, 10);
        // The following are in degrees minutes
        p = new AirepParser(sep.next().report, c);
        assertNotNull(p);
        
    }
    
    /**
     * Various forms of location identification.
     */
    @Test
    public void testPositionDecode() {
        AirepParser p = null;
        Calendar c = TimeTools.getSystemCalendar(2012, 9, 10, 16, 10);
        // The following are in degrees minutes
        p = new AirepParser("ARP DAL278 3336N 165E 1543 F320 MS40 110/010KT TB LGT=", c);
        assertEquals("DAL278", p.getAircraftId());
        assertEquals(33.6, p.getLatitude(), 0.01);
        assertEquals(165.0, p.getLongitude(), 0.01);
        
        p = new AirepParser("ARP DAL278 3336N 165W 1543 F320 MS40 110/010KT TB LGT=", c);
        assertEquals("DAL278", p.getAircraftId());
        assertEquals(33.6, p.getLatitude(), 0.01);
        assertEquals(-165.0, p.getLongitude(), 0.01);

        p = new AirepParser("ARP DAL278 N3336 E165 1543 F320 MS40 110/010KT TB LGT=", c);
        assertEquals("DAL278", p.getAircraftId());
        assertEquals(33.6, p.getLatitude(), 0.01);
        assertEquals(165.0, p.getLongitude(), 0.01);

        p = new AirepParser("ARP DAL278 N3336 W165 1543 F320 MS40 110/010KT TB LGT=", c);
        assertEquals("DAL278", p.getAircraftId());
        assertEquals(33.6, p.getLatitude(), 0.01);
        assertEquals(-165.0, p.getLongitude(), 0.01);

        // These are in decimal degrees!
        p = new AirepParser("ARP DAL278 N33.36W089.25 1543 F320 MS40 110/010KT TB LGT=", c);
        assertEquals("DAL278", p.getAircraftId());
        assertEquals(33.36, p.getLatitude(), 0.01);
        assertEquals(-89.25, p.getLongitude(), 0.01);

        p = new AirepParser("ARP DAL278 33.36N089.25W 1543 F320 MS40 110/010KT TB LGT=", c);
        assertEquals("DAL278", p.getAircraftId());
        assertEquals(33.36, p.getLatitude(), 0.01);
        assertEquals(-89.25, p.getLongitude(), 0.01);

    }
    
    /**
     * Test various turbulence decoding.
     */
    @Test
    public void testAIREPParser() {
        
        Calendar c = TimeTools.getSystemCalendar(2012, 9, 10, 16, 10);
        
        AirepParser p = new AirepParser("ARP DAL278 3336N 165E 1543 F320 MS40 110/010KT TB LGT=", c);

        AirepParser.Turbulence t = p.getTurbulence();
        assertNotNull(t);
        assertEquals(0x80 | AirepRecord.TURB_LGT, t.getTurbulence());
        
        p = new AirepParser("ARP DAL278 3336N 165E 1543 F320 MS40 110/010KT TB MOD=", c);
        t = p.getTurbulence();
        assertNotNull(t);
        assertEquals(TURB_BIT | AirepRecord.TURB_MOD, t.getTurbulence());

        p = new AirepParser("ARP DAL278 3336N 165E 1543 F320 MS40 110/010KT TB MDT=", c);
        t = p.getTurbulence();
        assertNotNull(t);
        assertEquals(0x40 | TURB_BIT, t.getTurbulence());
        
        p = new AirepParser("ARP DAL278 3336N 165E 1543 F320 MS40 110/010KT TB LGT OCN MDT CAT=", c);
        t = p.getTurbulence();
        assertNotNull(t);
        assertEquals(TURB_BIT | AirepRecord.TURB_MOD | AirepRecord.TURB_TYPE_CAT | AirepRecord.TURB_FREQ_OCN, t.getTurbulence());

        p = new AirepParser("ARP DAL278 3336N 165E 1543 F320 MS40 110/010KT TB LGT CAT OCN MDT=", c);
        t = p.getTurbulence();
        assertNotNull(t);
        assertEquals(TURB_BIT | AirepRecord.TURB_MOD | AirepRecord.TURB_TYPE_CAT | AirepRecord.TURB_FREQ_OCN, t.getTurbulence());
        
        p = new AirepParser("ARP DAL278 3336N 165E 1543 F320 MS40 110/010KT TB SVR=", c);
        t = p.getTurbulence();
        assertNotNull(t);
        assertEquals(TURB_BIT | AirepRecord.TURB_SEV, t.getTurbulence());
        
        p = new AirepParser("ARP DAL278 3336N 165E 1543 F320 MS40 110/010KT TB XTRM=", c);
        t = p.getTurbulence();
        assertNotNull(t);
        assertEquals(TURB_BIT | AirepRecord.TURB_XTRM, t.getTurbulence());
        
        p = new AirepParser("ARP HAL4 2714N 14713W 0957 F350 MS46 270/052KT TB 1=", c);
        t = p.getTurbulence();
        assertNotNull(t);
        assertEquals(TURB_BIT | AirepRecord.TURB_LGT, t.getTurbulence());
        
        // Compound turbulence value
        p = new AirepParser("ARP UAL761 3825N 11042W 1557 F340 MS44 235/030KT TB LGT-MOD RM\r\r\n     A320 OV HVE=", c);
        t = p.getTurbulence();
        assertNotNull(t);
        assertEquals(TURB_BIT | AirepRecord.TURB_LGT_MOD, t.getTurbulence());
        // Compound turbulence value
        p = new AirepParser("ARP UAL761 3825N 11042W 1557 F340 MS44 235/030KT TB LGTMOD RM\r\r\n     A320 OV HVE=", c);
        t = p.getTurbulence();
        assertNotNull(t);
        assertEquals(TURB_BIT | AirepRecord.TURB_LGT_MOD, t.getTurbulence());

        // Two adjacent intensities-assume the strongest!
        p = new AirepParser("ARP UAL761 3825N 11042W 1557 F340 MS44 235/030KT TB LGT MOD RM\r\r\n     A320 OV HVE=", c);
        t = p.getTurbulence();
        assertNotNull(t);
        assertEquals(TURB_BIT | AirepRecord.TURB_MOD, t.getTurbulence());
        // Checks that the intensity and frequency is extracted from non-reported data.
        p = new AirepParser("ARP UAL761 3825N 11042W 1557 F340 TB OCNL MOD TURBC IN CLOUD TOPS\r\r\n     RM A320 OV HVE=", c);
        t = p.getTurbulence();
        assertNotNull(t);
        assertEquals(TURB_BIT | AirepRecord.TURB_MOD | AirepRecord.TURB_FREQ_OCN, t.getTurbulence());
        
    }
    
    @Test
    public void testAIREPTimes() {
        SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
        sdf.setTimeZone(TimeZone.getTimeZone("ZULU"));
        
        Calendar refTime = TimeTools.getBaseCalendar(2011, 12, 14);
        refTime.set(Calendar.HOUR_OF_DAY, 17);
        refTime.set(Calendar.MINUTE, 15);
        refTime.set(Calendar.SECOND, 00);
        refTime.set(Calendar.MILLISECOND, 0);
        
        AirepParser p = new AirepParser("ARP UAL121 4400N 05700W 1640 F390 MS00 000/099KT TB MOD SK CLEAR=",refTime);
        Calendar c = p.getObservationTime();
        assertNotNull(c);
        assertEquals(14, c.get(Calendar.DAY_OF_MONTH));
        assertEquals(16, c.get(Calendar.HOUR_OF_DAY));
        assertEquals(40, c.get(Calendar.MINUTE));
    }
    
    /**
     * Test that an observation time greater than the reference time rolls
     * back to the previous day.
     */
    @Test
    public void testAIREPDateRollback() {
        Calendar refTime = TimeTools.getBaseCalendar(2011, 12, 14);
        refTime.set(Calendar.HOUR_OF_DAY, 17);
        refTime.set(Calendar.MINUTE, 15);
        refTime.set(Calendar.SECOND, 00);
        refTime.set(Calendar.MILLISECOND, 0);
        
        String data = "ARP UAL121 4400N 05700W 1840 F390 MS00 000/099KT TB MOD SK CLEAR=";
        AirepParser p = new AirepParser(data,refTime);
        Calendar c = p.getObservationTime();
        assertNotNull(c);
        assertEquals(13, c.get(Calendar.DAY_OF_MONTH));
        assertEquals(18, c.get(Calendar.HOUR_OF_DAY));
        assertEquals(40, c.get(Calendar.MINUTE));
    }
    
    /**
     * Test various reported winds
     */
    @Test
    public void testAIREPWinds() {
        
        Calendar c = TimeTools.getSystemCalendar(2012, 9, 10, 16, 10);
        AirepParser p = null;

        // Winds with "KT"
        p = new AirepParser("ARP DAL278 3336N 165E 1543 F320 MS40 110/010KT TB LGT=", c);
        assertNotNull(p);
        assertEquals(110, p.getWindDirection().intValue());
        assertEquals(10, p.getWindSpeed().intValue());

        // Winds with "KTS"
        p = new AirepParser("ARP DAL278 3336N 165E 1543 F320 MS40 265/010KTS TB LGT=", c);
        assertNotNull(p);
        assertEquals(265, p.getWindDirection().intValue());
        assertEquals(10, p.getWindSpeed().intValue());

        // Winds with no units - assume knots
        p = new AirepParser("ARP DAL278 3336N 165E 1543 F320 MS40 265/010 TB LGT=", c);
        assertNotNull(p);
        assertEquals(265, p.getWindDirection().intValue());
        assertEquals(10, p.getWindSpeed().intValue());
        
    }
    
}
