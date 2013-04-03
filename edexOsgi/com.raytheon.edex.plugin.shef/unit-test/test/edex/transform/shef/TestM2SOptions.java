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
package test.edex.transform.shef;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.StringReader;

import org.junit.Test;

import com.raytheon.edex.transform.shef.obs.ObsToSHEFOptions;

/**
 * Tests extracted from ObsToSHEFOptions.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * ======================================
 * AWIPS2 DR Work
 * 20120918           1185 jkorman     Extracted from mains
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */

public class TestM2SOptions {

    @Test
    public void testConstructOptions() {

        String METAR_CFG_DATA = "/tmp/queue/metar/in\n"
                + "/tmp/queue/metar/out\n"
                + "/tmp/queue/metar/err\n"
                + "+SAOOUT\n"
                + "-ERRORFILE\n"
                + "-SHEFPASS\n"
                + "TAIRZZ TD UP SD UD US PL TX TN PPT PPQ PPH PPD PA TAIRZR TAIRZH TAIRZP TAIRZY PTIR\n"
                + ".begin_names\n" + "  KOMA\n" + "  KOFF\n" + "  KLNK\n"
                + "  KFET\n" + "  KOFK\n" + "  KPMV\n" + "  KCBF\n"
                + "  KSUX\n" + ".end_names\n" + ".begin_sm_alias\n"
                + "  72550 KOMA\n" + "  72551 KLNK\n" + "  72552 KGRI\n"
                + "  72556 KOFK\n" + "  72557 KSUX\n" + ".end_sm_alias\n"
                + ".begin_pc_reset\n" + "  KOMA 40\n" + ".end_pc_reset\n";

        String cmdLine = " -a -pedtsep -v -b -strip -pct 4 -howold 405 -p1 -p6 -pall6 -round -w -q1 -g";

        ObsToSHEFOptions options = new ObsToSHEFOptions(cmdLine, false);

        BufferedReader reader = null;
        try {
            reader = new BufferedReader(new StringReader(METAR_CFG_DATA));
            options.readConfig(reader);

        } catch (Exception e) {
            e.printStackTrace();
        } finally {
            if (reader != null) {
                try {
                    reader.close();
                } catch (IOException ioe) {
                    ioe.printStackTrace();
                }
            }
        }

        assertEquals(40, options.getPCReset("KOMA").intValue());
        assertFalse(options.checkName("KSAT"));
        assertTrue(options.isOptZeroAuto1HourPrecip());
        assertFalse(options.isOptTypeSrcV());
        assertTrue(options.isOptVerbose());
        assertTrue(options.isOptZero6HourPrecip());
    }
}
