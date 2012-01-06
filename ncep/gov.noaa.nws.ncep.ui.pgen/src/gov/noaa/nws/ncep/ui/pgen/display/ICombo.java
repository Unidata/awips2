/*
 * ICombo
 * 
 * Date created: 02 JUNE 2009
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */
package gov.noaa.nws.ncep.ui.pgen.display;

/**
 * Interface used to get specific attributes of a PGEN Geographic combo symbol object.
 * @author sgilbert
 *
 */
public interface ICombo extends ISinglePoint{

	/**
	 * Gets the names of the symbol patterns to use for this element
	 * @return array of symbol pattern names
	 */
	public String[] getPatternNames();

}
