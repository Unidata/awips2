/*
 * IMidCloudText
 * 
 * Date created: 14 JANUARY 2010
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.ui.pgen.display;

/**
 * @author sgilbert
 *
 */
public interface IMidCloudText extends IText {

	public String getCloudTypes();
	
	public String getCloudAmounts();
	
	public boolean hasTurbulence();
	
	public String getTurbulencePattern();
	
	public String getTurbulenceLevels();
	
	public boolean hasIcing();
	
	public String getIcingPattern();
	
	public String getIcingLevels();
	
	public boolean hasTstorm();
	
	public String getTstormTypes();
	
	public String getTstormLevels();
	
	public boolean isTwoColumns();
	
}
