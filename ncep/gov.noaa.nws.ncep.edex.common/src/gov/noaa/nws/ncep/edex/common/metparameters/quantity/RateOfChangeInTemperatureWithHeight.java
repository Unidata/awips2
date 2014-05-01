package gov.noaa.nws.ncep.edex.common.metparameters.quantity;

import gov.noaa.nws.ncep.edex.common.metparameters.parameterconversion.NcUnits;

import javax.measure.unit.Unit;

 public interface RateOfChangeInTemperatureWithHeight extends javax.measure.quantity.Quantity {
           public final static Unit<RateOfChangeInTemperatureWithHeight> UNIT = NcUnits.CELSIUS_PER_KILOMETER;
}
