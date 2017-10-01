package gov.noaa.nws.mdl.viz.boundaryTool.common.boundary;

import com.raytheon.uf.viz.core.exception.VizException;

/**
 * 
 * @author Mamoudou Ba
 * @version 1.0
 * 
 *          Entirely reused of A2 "ImpossibleTrackException" class
 */

public class ImpossibleTrackException extends VizException {

    private static final long serialVersionUID = -7171916545937661879L;

    /**
	 *
	 */
    public ImpossibleTrackException() {
        super();
    }

    /**
     * @param message
     * @param cause
     */
    public ImpossibleTrackException(String message, Throwable cause) {
        super(message, cause);
    }

    /**
     * @param message
     */
    public ImpossibleTrackException(String message) {
        super(message);
    }

    /**
     * @param cause
     */
    public ImpossibleTrackException(Throwable cause) {
        super(cause);
    }
}
