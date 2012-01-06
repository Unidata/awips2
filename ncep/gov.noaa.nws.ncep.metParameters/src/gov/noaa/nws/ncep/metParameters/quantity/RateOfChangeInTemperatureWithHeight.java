package gov.noaa.nws.ncep.metParameters.quantity;

import gov.noaa.nws.ncep.metParameters.parameterConversion.NcUnits;

import javax.measure.quantity.Quantity;
import javax.measure.unit.ProductUnit;
import javax.measure.unit.SI;
import javax.measure.unit.Unit;

 public interface RateOfChangeInTemperatureWithHeight extends javax.measure.quantity.Quantity {
           public final static Unit<RateOfChangeInTemperatureWithHeight> UNIT = NcUnits.CELSIUS_PER_KILOMETER;
 }
