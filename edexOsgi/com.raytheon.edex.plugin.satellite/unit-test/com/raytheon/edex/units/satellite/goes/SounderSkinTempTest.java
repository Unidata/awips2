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

package com.raytheon.edex.units.satellite.goes;

import javax.measure.converter.UnitConverter;
import javax.measure.unit.SI;

import junit.framework.TestCase;

import com.raytheon.edex.units.satellite.SatelliteUnits;

/**
 * Tests the implementation of the SounderSkinTempPixel's converters
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 20, 2007            njensen     Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 */
public class SounderSkinTempTest extends TestCase {

	public void testConversions() {
		UnitConverter pixelToTemp = SatelliteUnits.SOUNDER_SKIN_TEMP_PIXEL
				.getConverterTo(SI.CELSIUS);
		UnitConverter tempToPixel = (SI.CELSIUS)
				.getConverterTo(SatelliteUnits.SOUNDER_SKIN_TEMP_PIXEL);

		double[] pixels = new double[] { 0, 200 };
		double[] temps = new double[] { 63, -37 };

		assertEquals(pixels.length, temps.length);

		for (int i = 0; i < pixels.length; i++) {
			assertEquals(temps[i], pixelToTemp.convert(pixels[i]), 0.0001);
			assertEquals(pixels[i], tempToPixel.convert(temps[i]), 0.0001);
		}
	}

}
