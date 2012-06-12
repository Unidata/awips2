package gov.noaa.nws.ncep.metparameters;

import javax.measure.quantity.Dimensionless;

public class GenericDimensionlessParameter extends AbstractMetParameter implements
	Dimensionless {

	 public GenericDimensionlessParameter() {
		  super( UNIT );
	}
	 
}