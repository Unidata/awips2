/*
 * FillPatternException
 * 
 * Date created: 03 DECEMBER 2008
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */
package gov.noaa.nws.ncep.ui.pgen.display;

import gov.noaa.nws.ncep.ui.pgen.PGenException;

/**
 * This Exception is thrown if requested LinePattern is not found
 * @author sgilbert
 *
 */
public class LinePatternException extends PGenException {

	/**
	 * For Serializable interface
	 */
	private static final long serialVersionUID = 1L;

	/**
	 * Constructor simply calls constructor of superclass
	 * @param message Reason for Exception
	 */
	public LinePatternException(String message) {
		super(message);
	}

	/**
	 * Constructor simply calls constructor of superclass
	 * @param message Reason for Exception
	 * @param cause If caused by another Exception
	 */
	public LinePatternException(String message, Throwable cause) {
		super(message, cause);
	}

}
