package gov.noaa.nws.ncep.edex.common.metparameters.quantity;

import gov.noaa.nws.ncep.edex.common.metparameters.parameterconversion.NcUnits;

import javax.measure.unit.Unit;

public interface HeatFlux extends javax.measure.quantity.Quantity {
	public static final Unit<HeatFlux> UNIT = NcUnits.WATT_PER_SECOND_SQUARE;
	}
