package gov.noaa.nws.ncep.edex.common.metparameters.quantity;

import gov.noaa.nws.ncep.edex.common.metparameters.parameterconversion.NcUnits;

import javax.measure.unit.Unit;

 public interface RateOfChangeInTemperatureWithPressure extends javax.measure.quantity.Quantity {
           public final static Unit<RateOfChangeInTemperatureWithPressure> UNIT = NcUnits.KELVIN_PER_MILLIBAR;
}