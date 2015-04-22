package gov.noaa.nws.ncep.edex.common.metparameters.quantity;

import gov.noaa.nws.ncep.edex.common.metparameters.parameterconversion.NcUnits;
import javax.measure.unit.Unit;

public interface PotentialForCyclonicUpdraftRotation extends javax.measure.quantity.Quantity {
	public static final Unit<PotentialForCyclonicUpdraftRotation> UNIT = NcUnits.METER_SQUARE_PER_SECOND_SQUARE;
	}