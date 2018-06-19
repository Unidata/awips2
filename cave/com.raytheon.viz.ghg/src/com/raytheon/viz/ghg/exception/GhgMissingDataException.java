package com.raytheon.viz.ghg.exception;

/**
 * Exception to throw if GHG Monitor can not load data it needs to initialize
 * the application.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 16, 2015  #5184     dgilling     Initial creation
 * 
 * </pre>
 * 
 * @author dgilling
 * @version 1.0
 */
public final class GhgMissingDataException extends Exception {

    private static final long serialVersionUID = 9066782113765709566L;

    public GhgMissingDataException() {
        super();
    }

    public GhgMissingDataException(String message, Throwable cause,
            boolean enableSuppression, boolean writableStackTrace) {
        super(message, cause, enableSuppression, writableStackTrace);
    }

    public GhgMissingDataException(String message, Throwable cause) {
        super(message, cause);
    }

    public GhgMissingDataException(String message) {
        super(message);
    }

    public GhgMissingDataException(Throwable cause) {
        super(cause);
    }
}
