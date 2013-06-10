package com.raytheon.uf.featureexplorer;

/**
 * Base exception for Feature Explorer
 * 
 * <pre>
 *   
 *    SOFTWARE HISTORY
 *   
 *    Date          Ticket#     Engineer    Description
 *    ------------  ----------  ----------- --------------------------
 *    9/7/2008      SP#15       bclement    Initial Creation.
 *    
 * </pre>
 * 
 * @author bclement
 * 
 */
public class FeatureException extends Exception {

    private static final long serialVersionUID = 3812733413272988454L;

    /**
     * Default Constructor
     * 
     */
    public FeatureException() {
    }

    /**
     * @param message
     */
    public FeatureException(String message) {
        super(message);
    }

    /**
     * @param message
     * @param cause
     */
    public FeatureException(Throwable cause) {
        super(cause);
    }

    /**
     * @param cause
     */
    public FeatureException(String message, Throwable cause) {
        super(message, cause);
    }

}
