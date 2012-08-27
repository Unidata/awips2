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

import com.raytheon.edex.plugin.pirep.decoder.PirepTools;
import com.raytheon.uf.common.dataplugin.pirep.PirepLayerData;
import com.raytheon.uf.common.dataplugin.pirep.PirepRecord;
import com.raytheon.uf.edex.decodertools.aircraft.AircraftFlightCondition;

import static org.junit.Assert.*;

/**
 * Test various PIREP decoder components.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 8, 2012            jkorman     Initial creation
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */

public class TestPIREPRecord {

    /**
     * Test that the getter for TBF (turbulence frequency) and TBI (turbulence
     * intensity) get the data for the greatest intensity.
     */
    @Test
    public void testTurbulenceConstruction() {

        PirepRecord rec = new PirepRecord();

        PirepLayerData layer = new PirepLayerData(rec);
        layer.setLayerType(PirepLayerData.LAYER_TYP_TURBC);
        layer.setFrequency("OCN");
        layer.setFirstValue("LGT");
        layer.setSecondValue("MOD");
        layer.setBaseLayerHeight(15000);
        layer.setTopLayerHeight(20000);
        rec.addLayer(layer);

        layer = new PirepLayerData(rec);
        layer.setLayerType(PirepLayerData.LAYER_TYP_TURBC);
        layer.setFrequency("CON");
        layer.setFirstValue("MOD");
        layer.setSecondValue("SEV");
        layer.setBaseLayerHeight(20000);
        layer.setTopLayerHeight(22000);
        rec.addLayer(layer);

        String[] data = rec.getStrings("TBF");
        assertNotNull(data);
        assertTrue(data.length > 0);
        assertEquals("CON", data[0]);
        data = rec.getStrings("TBI");
        assertNotNull(data);
        assertTrue(data.length > 0);
        assertEquals("MODSEV", data[0]);
    }

    /**
     * Turbulence at one level, differing intensities.
     */
    @Test
    public void testPirepTurbc_1() {
        AircraftFlightCondition expected = new AircraftFlightCondition();
        expected.setBaseHeight(10000);
        expected.setTopHeight(null);
        expected.setIntensity1("MOD");
        expected.setIntensity2("SEV");
        expected.setType(null);
        expected.setFrequency(null);

        PirepTools t = new PirepTools("MOD TO SVR 100");
        checkLevel(t, 1);
        checkData(expected, t.decodeTurbulenceData().get(0));
    }

    /**
     * Turbulence between levels, differing intensities with frequency.
     */
    @Test
    public void testPirepTurbc_2() {
        AircraftFlightCondition expected = new AircraftFlightCondition();
        expected.setBaseHeight(20000);
        expected.setTopHeight(30000);
        expected.setIntensity1("MOD");
        expected.setIntensity2("SEV");
        expected.setType(null);
        expected.setFrequency("OCN");

        PirepTools t = new PirepTools("SVR OCNL MOD 200-300");
        checkLevel(t, 1);
        checkData(expected, t.decodeTurbulenceData().get(0));
    }

    /**
     * Turbulence between levels, differing intensities.
     */
    @Test
    public void testPirepTurbc_3() {
        AircraftFlightCondition expected = new AircraftFlightCondition();
        expected.setBaseHeight(12000);
        expected.setTopHeight(15000);
        expected.setIntensity1("LGT");
        expected.setIntensity2("MOD");
        expected.setType(null);
        expected.setFrequency(null);

        PirepTools t = new PirepTools("LGT-MDT 120-150");
        checkLevel(t, 1);
        checkData(expected, t.decodeTurbulenceData().get(0));
    }

    /**
     * Turbulence below a level, single intensity. Embedded carriage control
     * should be ignored.
     */
    @Test
    public void testPirepTurbc_4() {
        AircraftFlightCondition expected = new AircraftFlightCondition();
        expected.setBaseHeight(-9999);
        expected.setTopHeight(10000);
        expected.setIntensity1("MOD");
        expected.setIntensity2(null);
        expected.setType(null);
        expected.setFrequency(null);

        PirepTools t = new PirepTools("MDT BLO\n 100");
        checkLevel(t, 1);
        checkData(expected, t.decodeTurbulenceData().get(0));
    }

    /**
     * Turbulence above a level, differing intensities. Light turbulence (LGT)
     * is misspelled. Includes a turbulence type.
     */
    @Test
    public void testPirepTurbc_5() {
        AircraftFlightCondition expected = new AircraftFlightCondition();
        expected.setBaseHeight(-9999);
        expected.setTopHeight(12000);
        expected.setIntensity1("LGT");
        expected.setIntensity2("MOD");
        expected.setType("CHOP");
        expected.setFrequency(null);

        PirepTools t = new PirepTools("LIT-MOD CHOP ABOVE 120");
        checkLevel(t, 1);
        checkData(expected, t.decodeTurbulenceData().get(0));
    }

    /**
     * Turbulence between two levels, show that levels are reordered. Includes a
     * turbulence frequency, show that it is corrected.
     */
    @Test
    public void testPirepTurbc_6() {
        AircraftFlightCondition expected = new AircraftFlightCondition();
        expected.setBaseHeight(8000);
        expected.setTopHeight(12000);
        expected.setIntensity1("MOD");
        expected.setIntensity2(null);
        expected.setType(null);
        expected.setFrequency("CON");

        PirepTools t = new PirepTools("CONTINOUS MOD 120-080");
        checkLevel(t, 1);
        List<AircraftFlightCondition> list = t.decodeTurbulenceData();
        checkData(expected, list.get(0));
    }

    /**
     * Checks that the PirepTools has decoded data.
     * 
     * @param tools
     *            The tools object that contains decoded data.
     */
    private void checkLevel(PirepTools tools, int elements) {
        assertNotNull(tools);
        List<AircraftFlightCondition> list = tools.decodeTurbulenceData();
        assertNotNull(list);
        assertEquals(elements, list.size());
    }

    /**
     * Checks individual data elements.
     * 
     * @param expected
     *            The expected flight conditions.
     * @param actual
     *            The actual decoded flight conditions.
     */
    private void checkData(AircraftFlightCondition expected,
            AircraftFlightCondition actual) {
        assertEquals(expected.getFrequency(), actual.getFrequency());
        assertEquals(expected.getIntensity1(), actual.getIntensity1());
        assertEquals(expected.getIntensity2(), actual.getIntensity2());
        assertEquals(expected.getType(), actual.getType());
        assertEquals(expected.getBaseHeight(), actual.getBaseHeight());
        assertEquals(expected.getTopHeight(), actual.getTopHeight());

    }

}
