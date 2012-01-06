/*
 * ContourException
 * 
 * Date created: 14 June 2010
 *
 * This code has been developed by the SIB for use in the AWIPS2 system.
 */
package gov.noaa.nws.ncep.viz.tools.contour;

/**
 * class for Exceptions thrown by contour Fill generation.
 * @author sgilbert
 *
 */
public class ContourException extends Exception {

	/**
	 * for Serializable interface
	 */
	private static final long serialVersionUID = 1L;

	/**
	 * Constructor simply calls constructor of superclass
	 * @param message Exception message
	 */
	public ContourException(String message) {
		super(message);
	}

	/**
	 * Constructor simply calls constructor of superclass
	 * @param message Exception message
	 * @param cause If another exception is to blame
	 */
	public ContourException(String message, Throwable cause) {
		super(message, cause);
	}

}
