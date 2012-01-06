package gov.noaa.nws.ncep.metparameters;

import javax.measure.unit.Unit;

import com.raytheon.uf.common.dataplugin.IDecoderGettable.Amount;

public class MaxSustSurfWindSpeedFcst extends AbstractMetParameter implements
							javax.measure.quantity.Velocity {

	/*
	 * This maps to legacy GFSXMOS parameter SK12, which is observed and not derived.
	 * On NMAP, SK12 is displayed as specific wind-speeds at specific stations. The categorical range 
	 * values defined for this parameter, in sfparm.hl2 are not used in the GUI...
	 * TODO :  Clarify whether or not these ranges need to be implemented at all... 
	 */
	
	public MaxSustSurfWindSpeedFcst(){
		 super( UNIT );
	}	
}