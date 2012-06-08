/**
 * Date created (07 June 2010)
 *
 * This code has been developed by the SIB for use in the AWIPS2 system.
 */
package gov.noaa.nws.ncep.gempak.parameters.core.util;

/**
 * Provides utility functions additional to Java's String class.
 * <pre>
* Date          Ticket#     Engineer     Description
* ------------ ---------- ----------- --------------------------
* 07-June-2010    211        Archana.S    Initial Creation
* 10-June-2010    211        Archana.S    Updated the method to
*                                         use String class's
*                                         replaceAll() and trim()
*                                         methods.
*                                         Updated the Javadoc.                                      
* </pre>
* @author Archana.S
* @version 1
*/
public class StringUtil {
	/**
	 * Given an input string with blanks, the same string is returned without any blank
	 * characters 
	 * <p>
	 * The method <tt>removeBlanksWithinString</tt> invokes the String class's 
	 * replaceAll() method  with the regular expression for white-spaces
	 * to replace all whitespace characters with an empty string
	 * and then calls the trim() method of the String class to remove 
	 * any leading and trailing white-spaces.
	 * @param inputString - the string with blanks
	 * @return the input string without any blanks
	 */
	public static String removeBlanksWithinString(String dlinesString) {
        return dlinesString.replaceAll("\\s+","").trim();

	}

}
