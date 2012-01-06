package gov.noaa.nws.ncep.viz.localization.exception;

public class LocalizationException extends RuntimeException {

	/**
	 * 
	 */
	private static final long serialVersionUID = -1834538444564203631L;

	
	public LocalizationException() {
		// Constructor without a message
		super();
	}
	
	public LocalizationException(String msg){
		// Constructor with a message
		super(msg);
	}
	
}
