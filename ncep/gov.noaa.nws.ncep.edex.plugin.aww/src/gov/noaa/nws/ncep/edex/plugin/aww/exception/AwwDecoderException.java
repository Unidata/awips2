package gov.noaa.nws.ncep.edex.plugin.aww.exception;

public class AwwDecoderException extends RuntimeException{

	/**
	 * generated serial version ID
	 */
	private static final long serialVersionUID = -6587319430713127596L;

	public AwwDecoderException() {
		// Constructor without a message
		super();
	}
	
	public AwwDecoderException(String msg){
		// Constructor with a message
		super(msg);
	}

}
