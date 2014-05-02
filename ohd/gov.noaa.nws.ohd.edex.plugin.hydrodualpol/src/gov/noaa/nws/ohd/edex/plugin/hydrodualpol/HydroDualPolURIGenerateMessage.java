/**
 * This software was developed and / or modified by HSEB, OHD
 * 
 **/
package gov.noaa.nws.ohd.edex.plugin.hydrodualpol;

import com.raytheon.edex.urifilter.URIGenerateMessage;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 13, 2013            jtDeng     Initial creation
 * 
 * </pre>
 * 
 * @author deng2
 * @version 1.0
 */
@DynamicSerialize
public class HydroDualPolURIGenerateMessage extends URIGenerateMessage {
    @DynamicSerializeElement
    protected String icao;

    public HydroDualPolURIGenerateMessage() {
    }

    public HydroDualPolURIGenerateMessage(HydroDualPolURIFilter filter) {
        super(filter);
        setIcao(filter.getIcao());
    }

    public String getIcao() {
        return icao;
    }

    public void setIcao(String icao) {
        this.icao = icao;
    }

}
