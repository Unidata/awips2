package gov.noaa.nws.ncep.common.dataplugin.geomag.exception;

import com.raytheon.uf.common.dataplugin.PluginException;

/**
 * Exception class for wrapping errors that occur with GeoMagDecoder
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 04/2013      975         S. Gurung   Initial Creation
 * 
 * </pre>
 * 
 * @author sgurung
 * @version 1
 */
public class GeoMagException extends PluginException {

    /**
     * Default serial version id
     */
    private static final long serialVersionUID = 1L;

    /**
     * Parser exception set with a cause.
     * 
     * @param aCause
     *            The cause of the exception
     */
    public GeoMagException(String aCause) {
        super(aCause);
    }

    /**
     * Parser exception set with a cause and an existing exception. Used for
     * exception chaining to preserve state.
     * 
     * @param aCause
     *            The cause of the exception
     * @param anException
     *            The exception object
     */
    public GeoMagException(String aCause, Exception anException) {
        super(aCause, anException);
    }
}
