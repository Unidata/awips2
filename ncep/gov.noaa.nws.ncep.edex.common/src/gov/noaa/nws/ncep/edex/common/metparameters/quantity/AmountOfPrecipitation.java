package gov.noaa.nws.ncep.edex.common.metparameters.quantity;

import gov.noaa.nws.ncep.edex.common.metparameters.parameterconversion.NcUnits;

import javax.measure.unit.Unit;

public interface AmountOfPrecipitation extends javax.measure.quantity.Quantity {
	public static final Unit<AmountOfPrecipitation> UNIT = NcUnits.KG_PER_METER_SQ;
	}
