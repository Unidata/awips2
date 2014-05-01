package gov.noaa.nws.ncep.viz.rsc.satellite.units;

import javax.measure.converter.UnitConverter;
import javax.measure.quantity.Temperature;
import javax.measure.unit.DerivedUnit;
import javax.measure.unit.SI;
import javax.measure.unit.Unit;

import org.apache.commons.lang.builder.HashCodeBuilder;

/**
 * Represents a pixel value on a satellite IR image. 
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 05/25/10                 ghull      Initial creation
 * 
 * </pre>
 * 
 * @author ghull
 */
public class NcIRPixel extends DerivedUnit<Temperature> {

	private static final long serialVersionUID = 1L;

	@Override
	public boolean equals(Object anObject) {
		return (anObject instanceof NcIRPixel);
	}

	@Override
	public Unit<Temperature> getStandardUnit() {
		return SI.KELVIN;
	}

	@Override
	public int hashCode() {
		return HashCodeBuilder.reflectionHashCode(this);
	}

	@Override
	public UnitConverter toStandardUnit() {
		return new NcIRPixelToTempConverter();
	}

}
