package gov.noaa.nws.ncep.metparameters;

import gov.noaa.nws.ncep.metparameters.MetParameterFactory.DeriveMethod;

import javax.measure.quantity.Dimensionless;
import javax.measure.quantity.Length;
import javax.measure.unit.Unit;

// The cloud coverage at a given height. Values are strings representing 
// metar observations FEW, OVC,
//
// A list of CloudCovers is used to determine the skyCoverage.
//
  public class CloudCover extends AbstractMetParameter implements Dimensionless {

	public CloudCover() {
		  super( UNIT );
	}
	
	@Override
	public Boolean hasStringValue() {
		return true;
	};	
  }