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

import junit.framework.TestCase;

import com.raytheon.edex.units.satellite.SatelliteUnits;

/**
 * Tests the implementation of the RainfallRatePixel's converters
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
public class RainfallRateTest extends TestCase {

	public void testConversions() {
		UnitConverter pixelToRainfall = SatelliteUnits.RAINFALL_RATE_PIXEL
				.toStandardUnit();
		UnitConverter rainfallToPixel = pixelToRainfall.inverse();

		double[] pixels = new double[] { 0, 108, 133, 168, 208, 248 };
		double[] rainfall = new double[] { 0, 0, 6, 20, 40, 60 };
		double[] resultPixels = new double[] { 0, 0, 133, 168, 208, 248 };

		assertEquals(pixels.length, rainfall.length);

		for (int i = 0; i < pixels.length; i++) {
			assertEquals(rainfall[i], pixelToRainfall.convert(pixels[i]),
					0.0001);
			assertEquals(resultPixels[i], rainfallToPixel.convert(rainfall[i]),
					0.0001);
		}
	}

}
