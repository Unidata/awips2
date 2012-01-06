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
 * Converts a cloud height in meters to a pixel value (calculation is done in
 * ft/100 MSL)
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 30, 2007                njensen Initial creation
 * Mar 24, 2009     2086      jsanchez Used meterToFoot to convert height.
 * 
 * </pre>
 * 
 * @author njensen
 */
public class CloudHeightToPixelConverter extends UnitConverter {

	private static final long serialVersionUID = 1L;

	private static UnitConverter meterToFoot = SI.METRE.getConverterTo(NonSI.FOOT);

	/*
	 * (non-Javadoc)
	 * 
	 * @see javax.measure.converter.UnitConverter#convert( double)
	 */
	@Override
	public double convert(double aHeight) throws ConversionException {
		double result = 0.0;

		// value is in meters, but below calculates pixel based on value being
		// ft/100 MSL
		aHeight = meterToFoot.convert(aHeight);
		
		if (aHeight < 1) {
		    result = 0;
		} else if (aHeight <= 17.7) {
			result = (aHeight / 17.7) + 75.0;
		} else if (aHeight <= 50) {
			result = (24.0 / 32.3 * (aHeight - 17.7)) + 76.0;
		} else if (aHeight <= 87.5) {
			result = (25.0 / 37.5 * (aHeight - 50.0)) + 100.0;
		} else if (aHeight <= 129.9) {
			result = (25.0 / 42.4 * (aHeight - 87.5)) + 125.0;
		} else if (aHeight <= 179.1) {
			result = (25.0 / 49.2 * (aHeight - 129.9)) + 150.0;
		} else if (aHeight <= 238) {
			result = (25.0 / 58.9 * (aHeight - 179.1)) + 175.0;
		} else if (aHeight <= 312.3) {
			result = (25.0 / 74.3 * (aHeight - 238.0)) + 200.0;
		} else {
			result = (30.0 / 134.4 * (aHeight - 312.3)) + 225.0;
		}

		if (result < 0) {
			result = 0;
		} else if (result > 255) {
			result = 255;
		}

		return result;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see javax.measure.converter.UnitConverter#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(Object aConverter) {
		return (aConverter instanceof CloudHeightToPixelConverter);
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
		return new CloudPixelToHeightConverter();
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
