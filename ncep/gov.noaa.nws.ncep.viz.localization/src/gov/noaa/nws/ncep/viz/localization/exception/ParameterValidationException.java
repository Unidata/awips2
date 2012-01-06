package gov.noaa.nws.ncep.viz.localization.exception;

public class ParameterValidationException extends LocalizationException {

	/**
	 * 
	 */
	private static final long serialVersionUID = 4576285573960963194L;

	public ParameterValidationException() {
		super(); 
		System.out.println("Invalid parameter found"); 
	}

	public ParameterValidationException(String msg) {
		super(msg); 
	}
}
