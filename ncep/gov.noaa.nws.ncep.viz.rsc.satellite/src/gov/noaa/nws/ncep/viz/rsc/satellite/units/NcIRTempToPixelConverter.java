package gov.noaa.nws.ncep.viz.rsc.satellite.units;

import javax.measure.converter.ConversionException;
import javax.measure.converter.UnitConverter;

import org.apache.commons.lang.builder.HashCodeBuilder;

/**
 * Converts a temperature value in Kelvin to a pixel value from 0 to 255
 * using NMAP's equation
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 05/25                    ghull     Initial creation
 * 06/07         #          archana   Updated the convert() method to 
 *                                    match legacy imttob.f   
 * </pre>
 * 
 * @author ghull
 */
public class NcIRTempToPixelConverter extends UnitConverter {

	private static final long serialVersionUID = 1L;

	@Override
	public double convert(double aTemperature) throws ConversionException {
		double result = Double.NaN;

//		if (aTemperature < 238.15) {
//			result = 418.15 - aTemperature;
//		} else {
//			result = 656.3 - (2.0 * aTemperature);
//		}
//
//		if (result < 0) {
//			result = 0.0;
//		} else if (result > 255) {
//			result = 255.0;
//		}

		if ( aTemperature < 163 || aTemperature > 330)
			return result;
		else if ( aTemperature <= 242 ){
			result = 418 - aTemperature; 
		}else
		     result = 2 * ( 330 - aTemperature );
		return result;
	}

	@Override
	public boolean equals(Object aConverter) {
		return (aConverter instanceof NcIRTempToPixelConverter);
	}

	@Override
	public int hashCode() {
		return HashCodeBuilder.reflectionHashCode(this);
	}

	@Override
	public UnitConverter inverse() {
		return new NcIRPixelToTempConverter();
	}

	@Override
	public boolean isLinear() {
		return false;
	}

}
