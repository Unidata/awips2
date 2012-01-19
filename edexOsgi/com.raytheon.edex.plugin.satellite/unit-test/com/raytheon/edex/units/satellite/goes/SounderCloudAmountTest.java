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
import javax.measure.unit.Unit;

import junit.framework.TestCase;

import com.raytheon.edex.units.satellite.SatelliteUnits;

/**
 * Tests the implementation of the SounderCloudAmountPixel's converters
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
public class SounderCloudAmountTest extends TestCase {

	public void testConversions() {
		UnitConverter pixelToAmount = SatelliteUnits.SOUNDER_CLOUD_AMOUNT_PIXEL
				.getConverterTo(Unit.ONE);
		UnitConverter amountToPixel = Unit.ONE
				.getConverterTo(SatelliteUnits.SOUNDER_CLOUD_AMOUNT_PIXEL);

		double[] pixels = new double[] { 0, 75, 76, 166, 167, 253, 254, 255 };
		double[] amounts = new double[] { 0, 0, 1, 50, 51, 99, 100, 100 };
		double[] pixelResults = new double[] { 0, 0, 76, 166, 167, 253, 254,
				254 };

		assertEquals(pixels.length, amounts.length);

		for (int i = 0; i < pixels.length; i++) {
			assertEquals(amounts[i], pixelToAmount.convert(pixels[i]), 0.0001);
			assertEquals(pixelResults[i], amountToPixel.convert(amounts[i]),
					0.0001);
		}
	}

}
