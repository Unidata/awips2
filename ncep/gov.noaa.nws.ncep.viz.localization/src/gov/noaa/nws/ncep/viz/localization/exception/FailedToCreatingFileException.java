package gov.noaa.nws.ncep.viz.localization.exception;

public class FailedToCreatingFileException extends LocalizationException {

	/**
	 * 
	 */
	private static final long serialVersionUID = -6468033061521433274L;

	public FailedToCreatingFileException() {
		super(); 
		System.out.println("Failed to create a File!"); 
	}

	public FailedToCreatingFileException(String msg) {
		super(msg); 
	}

}
