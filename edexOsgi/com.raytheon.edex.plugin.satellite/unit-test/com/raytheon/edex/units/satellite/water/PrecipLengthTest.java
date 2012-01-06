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

package com.raytheon.edex.units.satellite.water;

import javax.measure.converter.UnitConverter;
import javax.measure.unit.SI;

import junit.framework.TestCase;

import com.raytheon.edex.units.satellite.SatelliteUnits;

/**
 * Tests the implementation of the PrecipLengthPixel's converters
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
public class PrecipLengthTest extends TestCase {

	public void testConversions() {
		UnitConverter pixelToPrecip = SatelliteUnits.PRECIP_PIXEL
				.getConverterTo(SI.MILLI(SI.METRE));
		UnitConverter precipToPixel = (SI.MILLI(SI.METRE))
				.getConverterTo(SatelliteUnits.PRECIP_PIXEL);

		double[] pixels = new double[] { 0, 176, 196, 216, 246, 251, 252, 255 };
		double[] precips = new double[] { 0, 0, 20, 40, 70, 75, 0, 0 };
		double[] resultPixels = new double[] { 0, 0, 196, 216, 246, 251, 0, 0 };

		assertEquals(pixels.length, precips.length);

		for (int i = 0; i < pixels.length; i++) {
			assertEquals(precips[i], pixelToPrecip.convert(pixels[i]), 0.0001);
			assertEquals(resultPixels[i], precipToPixel.convert(precips[i]),
					0.0001);
		}
	}

}
