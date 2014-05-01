 /**
 * This software was modified from Raytheon's taf plugin by
 * NOAA/NWS/NCEP/NCO in order to output point data in HDF5.
 **/
package gov.noaa.nws.ncep.edex.plugin.nctaf.decoder;

/**TODO Add Description
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 09/09/2011   458		   sgurung	   Initial Creation from Raytheon's taf plugin
 * 
 * </pre>
 * 
 * @author sgurung
 * @version 1.0
 */

public class NcTafParts {

    private String tafHeader = null;
    
    private String tafBody = null;

    /**
     * Empty constructor.
     */
    public NcTafParts() {
    }
    
    /**
     * Create a new TAFParts instance with specified header and body parts.
     * @param header
     * @param body
     */
    public NcTafParts(String header, String body) {
        tafHeader = header;
        tafBody = body;
    }
    
    /**
     * @return the tafHeader
     */
    public String getTafHeader() {
        return tafHeader;
    }

    /**
     * @param tafHeader the tafHeader to set
     */
    public void setTafHeader(String tafHeader) {
        this.tafHeader = tafHeader;
    }

    /**
     * @return the tafBody
     */
    public String getTafBody() {
        return tafBody;
    }

    /**
     * @param body
     *            The tafBody to set
     */
    public void setTafBody(String body) {
        this.tafBody = body;
    }

    public String toString() {
        return "header{" + tafHeader + "}: body{" + tafBody + "}";
    }
}
