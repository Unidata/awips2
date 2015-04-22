package gov.noaa.nws.ncep.common.log.exception;

public class NcepLogException extends RuntimeException {

	/**
	 * generated 
	 */
	private static final long serialVersionUID = 161354014114145229L;

	public NcepLogException() {
		// Constructor without a message
		super();
	}
	
	public NcepLogException(String msg){
		// Constructor with a message
		super(msg);
	}
	
	
}
