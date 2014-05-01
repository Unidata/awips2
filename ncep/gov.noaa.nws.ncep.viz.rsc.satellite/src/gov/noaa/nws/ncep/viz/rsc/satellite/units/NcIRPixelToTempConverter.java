package gov.noaa.nws.ncep.viz.rsc.satellite.units;

import javax.measure.converter.ConversionException;
import javax.measure.converter.UnitConverter;

import org.apache.commons.lang.builder.HashCodeBuilder;

/**
 * Converts a pixel value from 0-255 into a temperature in Kelvin
 * using NMAP's equation.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 05/25/10                ghull        Initial creation
 * 
 * </pre>
 * 
 * @author gull
 */
public class NcIRPixelToTempConverter extends UnitConverter {

	private static final long serialVersionUID = 1L;

	/*
	 * (non-Javadoc)
	 * 
	 * @see javax.measure.converter.UnitConverter#convert(double)
	 */
	@Override
	public double convert(double aPixel) throws ConversionException {
		double result = 0.0;

		if (aPixel >= 176) {
			result = 418 - aPixel;
		} else {
			result = 330 - (aPixel / 2.0);
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
		return (aConverter instanceof NcIRPixelToTempConverter);
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
		return null; // new NcIRTempToPixelConverter();
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
