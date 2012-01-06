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
import javax.measure.unit.NonSI;
import javax.measure.unit.SI;

import junit.framework.TestCase;

import com.raytheon.edex.units.satellite.SatelliteUnits;

/**
 * Tests the implementation of the SounderCloudTopHeightPixel's converters
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
public class SounderCloudTopHeightTest extends TestCase {

	public void testConversions() {
		UnitConverter pixelToHeight = SatelliteUnits.SOUNDER_CLOUD_HEIGHT_PIXEL
				.getConverterTo(SI.CENTI(NonSI.FOOT));
		UnitConverter heightToPixel = (SI.CENTI(NonSI.FOOT))
				.getConverterTo(SatelliteUnits.SOUNDER_CLOUD_HEIGHT_PIXEL);

		double[] pixels = new double[] { 75, 76, 100, 125, 150, 175, 200, 225,
				255 };
		double[] heights = new double[] { 0, 17.7, 50, 87.5, 129.9, 179.1, 238,
				312.3, 446.7 };

		assertEquals(pixels.length, heights.length);

		for (int i = 0; i < pixels.length; i++) {
			assertEquals(heights[i], pixelToHeight.convert(pixels[i]), 0.0001);
			assertEquals(pixels[i], heightToPixel.convert(heights[i]), 0.0001);
		}
	}

}
