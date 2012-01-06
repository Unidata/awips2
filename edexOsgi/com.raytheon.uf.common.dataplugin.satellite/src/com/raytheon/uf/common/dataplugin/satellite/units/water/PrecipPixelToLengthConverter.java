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

package com.raytheon.uf.common.dataplugin.satellite.units.water;

import javax.measure.converter.ConversionException;
import javax.measure.converter.UnitConverter;
import javax.measure.unit.SI;

import org.apache.commons.lang.builder.HashCodeBuilder;

/**
 * TODO class description
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 29, 2007                 njensen     Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 */
public class PrecipPixelToLengthConverter extends UnitConverter {

	private static final long serialVersionUID = 1L;

	private static UnitConverter millimeterToMeter = (SI.MILLI(SI.METRE))
			.getConverterTo(SI.METRE);

	/*
	 * (non-Javadoc)
	 * 
	 * @see javax.measure.converter.UnitConverter#convert(double)
	 */
	@Override
	public double convert(double aPixel) throws ConversionException {
		double result = 0.0;
		if (aPixel > 176 && aPixel <= 251) {
			result = aPixel - 176.0;
		}

		// above converts pixel to millimeters, but we need to change it to
		// meters
		result = millimeterToMeter.convert(result);

		return result;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see javax.measure.converter.UnitConverter#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(Object aConverter) {
		return (aConverter instanceof PrecipPixelToLengthConverter);
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
		return new PrecipLengthToPixelConverter();
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
