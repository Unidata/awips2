/**
 * 
 */
package gov.noaa.nws.ncep.standalone.miscsetConverter;

/**
 * @author bhebbard
 *
 */

public interface TypeCommand {
	public abstract String retrieve(TypeRecord tr, float scaleFactor);
}
