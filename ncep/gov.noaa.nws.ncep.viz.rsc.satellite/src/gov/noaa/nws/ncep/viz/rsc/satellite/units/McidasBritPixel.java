package gov.noaa.nws.ncep.viz.rsc.satellite.units;


import javax.measure.converter.UnitConverter;
import javax.measure.quantity.Dimensionless;
import javax.measure.unit.Unit;

import org.apache.commons.lang.builder.HashCodeBuilder;

import com.raytheon.uf.common.dataplugin.satellite.units.generic.GenericFromPixelConverter;

/**
 * Used to represent the McIdas BRIT Calibration type.  Its the same unit as 
 * com.raytheon.uf.common.dataplugin.satellite.units.generic.GenericPixel.  Even though this Unit
 * is exactly the same as GenericPixel, it was created so that valid pixel value range could be 
 * recognized as 0 - 255.  GenericPixel values are treated as signed values that range from
 * -128 to 127 by the com.raytheon.viz.satellite.rsc.SatFileBasedTileSet.
 * 
 *
 * @author sgilbert
 * @version 1.0	
 */

public class McidasBritPixel extends Unit<Dimensionless> {
	private static final long serialVersionUID = 1L;

	@Override
	public boolean equals(Object anObject) {
		return (anObject instanceof McidasBritPixel);
	}

	@Override
	public Unit<Dimensionless> getStandardUnit() {
		return Unit.ONE;
	}

	@Override
	public int hashCode() {
		return HashCodeBuilder.reflectionHashCode(this);
	}

	@Override
	public UnitConverter toStandardUnit() {
		return new GenericFromPixelConverter();
	}
}
