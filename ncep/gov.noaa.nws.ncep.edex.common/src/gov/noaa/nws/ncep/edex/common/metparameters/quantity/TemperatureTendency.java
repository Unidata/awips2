package gov.noaa.nws.ncep.edex.common.metparameters.quantity;

import gov.noaa.nws.ncep.edex.common.metparameters.parameterconversion.NcUnits;

import javax.measure.unit.Unit;


public interface TemperatureTendency extends javax.measure.quantity.Quantity {
public static final Unit<TemperatureTendency> UNIT = NcUnits.KELVIN_PER_DAY;
}
