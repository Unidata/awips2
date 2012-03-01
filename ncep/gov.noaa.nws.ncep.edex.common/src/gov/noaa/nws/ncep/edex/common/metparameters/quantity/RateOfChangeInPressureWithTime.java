package gov.noaa.nws.ncep.edex.common.metparameters.quantity;
import gov.noaa.nws.ncep.edex.common.metparameters.parameterconversion.NcUnits;

import javax.measure.unit.Unit;

 public interface RateOfChangeInPressureWithTime extends javax.measure.quantity.Quantity {
           public final static Unit<RateOfChangeInPressureWithTime> UNIT = NcUnits.PASCALS_PER_SEC;
}
