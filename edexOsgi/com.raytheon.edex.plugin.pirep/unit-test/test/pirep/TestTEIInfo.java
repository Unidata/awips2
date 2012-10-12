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
package test.pirep;

import java.util.List;

import org.junit.Test;
import static org.junit.Assert.*;

import com.raytheon.edex.plugin.pirep.PirepSeparator;
import com.raytheon.edex.plugin.pirep.decoder.PirepTools;
import com.raytheon.edex.plugin.pirep.decoder.TEI;
import com.raytheon.edex.plugin.pirep.decoder.TEIInfo;

/**
 * Various unit tests for the TEIInfo parser class.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 7, 2012            jkorman     Initial creation
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */

public class TestTEIInfo {

    /**
     * Test that a legal PIREP is parsed correctly.
     */
    @Test
    public void testParseNormal() {
        final String data = "LYH UA /OV LYH/TM 1226/FL210/TP P180/SK OVC100/WX FV99SM/TA 0/WV 27035KT/TB MDT/IC LGT RIME/RM CB W=";

        List<TEIInfo> parts = TEIInfo.findTEIs(data);
        assertNotNull(parts);
        assertEquals(12, parts.size());
        assertEquals(TEI.PIREP, parts.get(0).getTei());
        assertEquals(TEI.OV, parts.get(1).getTei());
        assertEquals(TEI.TM, parts.get(2).getTei());
        assertEquals(TEI.FL, parts.get(3).getTei());
        assertEquals(TEI.TP, parts.get(4).getTei());
        assertEquals(TEI.SK, parts.get(5).getTei());
        assertEquals(TEI.WX, parts.get(6).getTei());
        assertEquals(TEI.TA, parts.get(7).getTei());
        assertEquals(TEI.WV, parts.get(8).getTei());
        assertEquals(TEI.TB, parts.get(9).getTei());
        assertEquals(TEI.IC, parts.get(10).getTei());
        assertEquals(TEI.RM, parts.get(11).getTei());
    }

    /**
     * Test that out of order elements are parsed correctly.
     */
    @Test
    public void testParseBadOrder() {
        final String data = "LYH UA /OV LYH/TM 1226/FL210/SK OVC100/TP P180/WX FV99SM/TA 0/WV 27035KT/IC LGT RIME/TB MDT/RM CB W=";

        List<TEIInfo> parts = TEIInfo.findTEIs(data);
        assertNotNull(parts);
        assertEquals(12, parts.size());
        assertEquals(TEI.PIREP, parts.get(0).getTei());
        assertEquals(TEI.OV, parts.get(1).getTei());
        assertEquals(TEI.TM, parts.get(2).getTei());
        assertEquals(TEI.FL, parts.get(3).getTei());
        assertEquals(TEI.SK, parts.get(4).getTei());
        assertEquals(TEI.TP, parts.get(5).getTei());
        assertEquals(TEI.WX, parts.get(6).getTei());
        assertEquals(TEI.TA, parts.get(7).getTei());
        assertEquals(TEI.WV, parts.get(8).getTei());
        assertEquals(TEI.IC, parts.get(9).getTei());
        assertEquals(TEI.TB, parts.get(10).getTei());
        assertEquals(TEI.RM, parts.get(11).getTei());
    }

    /**
     * Test that "/SKC" does not get confused with the "/SK" TEI.
     */
    @Test
    public void testSky() {
        final String data = "LYH UA /OV LYH/TM 1226/FL210/SK OVC100/SKC/TA 0/WV 27035KT=";
        final String skyData = "OVC100/SKC";

        List<TEIInfo> parts = TEIInfo.findTEIs(data);
        assertNotNull(parts);
        assertEquals(7, parts.size());
        assertEquals(TEI.PIREP, parts.get(0).getTei());
        assertEquals(TEI.OV, parts.get(1).getTei());
        assertEquals(TEI.TM, parts.get(2).getTei());
        assertEquals(TEI.FL, parts.get(3).getTei());
        assertEquals(TEI.SK, parts.get(4).getTei());
        assertEquals(skyData, parts.get(4).getTeiText());
        assertEquals(TEI.TA, parts.get(5).getTei());
        assertEquals(TEI.WV, parts.get(6).getTei());
    }

    /**
     * Test that "/SKC" does not get confused with the "/SK" TEI.
     */
    @Test
    public void testNoTurbDecode() {
        final String data = "UBUS01 KMSC 061800\nCAO UA /OV DHT310017 /TM 1850 /FL125 /TP BE35 /WX FV30SM /TA 12\n/WV 24021KT /TB NEG=";

        PirepSeparator sep = PirepSeparator.separate(data.getBytes(), null);
        while(sep.hasNext()) {
            List<TEIInfo> parts = TEIInfo.findTEIs(sep.next().getReport());
            PirepTools tools = null;
            for(TEIInfo info : parts) {
                System.out.println(info.getTeiText());
                if(TEI.TB.equals(info.getTei())) {
                    tools = new PirepTools(info.getTeiText());
                    System.out.println(tools.decodeTurbulenceData());
                }
            }
        }
    }
    
    
}
