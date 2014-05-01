package gov.noaa.nws.ncep.ui.pgen;

/**
 * Superclass for all PGen Exceptions in the scenario classes are 
 * not forced to catch the thrown Exception
 * @author mgao
 *
 */
public class PGenRuntimeException extends RuntimeException {

	/**
	 * 
	 */
	private static final long serialVersionUID = 2066263644154503458L;

	/**
	 * Constructor simply calls constructor of superclass
	 * @param message Exception message
	 */
	public PGenRuntimeException(String message) {
		super(message);
	}

	/**
	 * Constructor simply calls constructor of superclass
	 * @param message Exception message
	 * @param cause If another exception is to blame
	 */
	public PGenRuntimeException(String message, Throwable cause) {
		super(message, cause);
	}
}
