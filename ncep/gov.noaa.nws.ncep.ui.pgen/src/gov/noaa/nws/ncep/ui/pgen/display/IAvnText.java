/*
 * IAvnText
 * 
 * Date created: 29 JULY 2009
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.ui.pgen.display;

/**
 * @author sgilbert
 *
 */
public interface IAvnText extends IText {

	public static enum AviationTextType {
		LOW_PRESSURE_BOX, HIGH_PRESSURE_BOX, FLIGHT_LEVEL,
		LOW_LEVEL_TURBULENCE, HIGH_LEVEL_TURBULENCE,
		CLOUD_LEVEL, FREEZING_LEVEL, MID_LEVEL_ICING
	};
	
	public AviationTextType getAvnTextType();
	
	public String getSymbolPatternName();
	
	public boolean hasSymbolPattern();
	
	public String getTopValue();
	
	public String getBottomValue();
	
	public boolean hasBottomValue();
	
}
