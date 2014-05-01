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

package com.raytheon.uf.common.dataplugin.satellite.units.goes.convert;

import javax.measure.converter.ConversionException;
import javax.measure.converter.UnitConverter;
import javax.measure.unit.NonSI;
import javax.measure.unit.SI;

import org.apache.commons.lang.builder.HashCodeBuilder;

/**
 * Converts a pixel value representing cloud height to a height above MSL in
 * meters
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 30, 2007                njensen  Initial creation
 * Mar 24, 2009       2086     jsanchez Used footToMeter to convert height.
 * 
 * </pre>
 * 
 * @author njensen
 */
public class CloudPixelToHeightConverter extends UnitConverter {

	private static final long serialVersionUID = 1L;
	
	private static UnitConverter footToMeter = (NonSI.FOOT)
    .getConverterTo(SI.METRE);

	/*
	 * (non-Javadoc)
	 * 
	 * @see javax.measure.converter.UnitConverter#convert(double)
	 */
	@Override
	public double convert(double aPixel) throws ConversionException {
		double result = 0.0;

		if (aPixel <= 75) {
			result = 0.0;
		} else if (aPixel <= 76) {
			result = 17.7 * (aPixel - 75.0);
		} else if (aPixel <= 100) {
			result = (32.3 / 24.0 * (aPixel - 76.0)) + 17.7;
		} else if (aPixel <= 125) {
			result = (37.5 / 25.0 * (aPixel - 100.0)) + 50.0;
		} else if (aPixel <= 150) {
			result = (42.4 / 25.0 * (aPixel - 125.0)) + 87.5;
		} else if (aPixel <= 175) {
			result = (49.2 / 25.0 * (aPixel - 150.0)) + 129.9;
		} else if (aPixel <= 200) {
			result = (58.9 / 25.0 * (aPixel - 175.0)) + 179.1;
		} else if (aPixel <= 225) {
			result = (74.3 / 25.0 * (aPixel - 200.0)) + 238.0;
		} else {
			result = (134.4 / 30.0 * (aPixel - 225.0)) + 312.3;
		}

		// above converts pixel to ft/100 MSL, but we need to change it to
		// meters
		result = footToMeter.convert(result);

		return result;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see javax.measure.converter.UnitConverter#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(Object aConverter) {
		return (aConverter instanceof CloudPixelToHeightConverter);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see javax.measure.converter.UnitConverter#hashCode()
	 */
	@Override
	public int hashCode() {
		return HashCodeBuilder.reflectionHashCode(this);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see javax.measure.converter.UnitConverter#inverse()
	 */
	@Override
	public UnitConverter inverse() {
		return new CloudHeightToPixelConverter();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see javax.measure.converter.UnitConverter#isLinear()
	 */
	@Override
	public boolean isLinear() {
		return false;
	}

}
