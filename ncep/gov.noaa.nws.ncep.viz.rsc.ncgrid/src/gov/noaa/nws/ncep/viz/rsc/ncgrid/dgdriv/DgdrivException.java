package gov.noaa.nws.ncep.viz.rsc.ncgrid.dgdriv;

public class DgdrivException extends Exception {
    /**
     * For Serializable functionality.
     */
    private static final long serialVersionUID = 1L;

    /**
     * 
     */
    public DgdrivException() {
        super();
    }

    /**
     * @param message
     */
    public DgdrivException(String message) {
        super(message);
    }
    
    /**
     * @param message
     * @param cause
     */
    public DgdrivException(String message, Throwable cause) {
        super(message, cause);
    }
    
    /**
     * @param cause
     */
    public DgdrivException(Throwable cause) {
        super(cause);
    }

}
