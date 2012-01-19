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
 * Tests the implementation of the SounderLiftedIndexPixel's converters
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
public class SounderLiftedIndexTest extends TestCase {

	public void testConversions() {
		UnitConverter pixelToLifted = SatelliteUnits.SOUNDER_LIFTED_INDEX_PIXEL
				.getConverterTo(SI.CELSIUS);
		UnitConverter liftedToPixel = SI.CELSIUS
				.getConverterTo(SatelliteUnits.SOUNDER_LIFTED_INDEX_PIXEL);

		double[] pixels = new double[] { 0, 200 };
		double[] lifteds = new double[] { 25, -15 };

		assertEquals(pixels.length, lifteds.length);

		for (int i = 0; i < pixels.length; i++) {
			assertEquals(lifteds[i], pixelToLifted.convert(pixels[i]), 0.0001);
			assertEquals(pixels[i], liftedToPixel.convert(lifteds[i]), 0.0001);
		}
	}

}
