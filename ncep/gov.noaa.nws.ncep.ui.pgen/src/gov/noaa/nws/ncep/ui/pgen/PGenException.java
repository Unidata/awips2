/*
 * PGenException
 * 
 * Date created: 03 DECEMBER 2008
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */
package gov.noaa.nws.ncep.ui.pgen;

/**
 * Superclass for all PGen Exceptions
 * @author sgilbert
 *
 */
public class PGenException extends Exception {

	/**
	 * for Serializable interface
	 */
	private static final long serialVersionUID = 1L;

	/**
	 * Constructor simply calls constructor of superclass
	 * @param message Exception message
	 */
	public PGenException(String message) {
		super(message);
	}

	/**
	 * Constructor simply calls constructor of superclass
	 * @param message Exception message
	 * @param cause If another exception is to blame
	 */
	public PGenException(String message, Throwable cause) {
		super(message, cause);
	}

}
