package gov.noaa.nws.ncep.common.dataplugin.pgen.request;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.serialization.comm.IServerRequest;

/**
 * 
 * Request for a PGEN Activity XML and all derived products for the given
 * dataURI
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 22, 2013            sgilbert     Initial creation
 * 
 * </pre>
 * 
 * @author sgilbert
 * @version 1.0
 */
@DynamicSerialize
public class RetrieveAllProductsRequest implements IServerRequest {

    @DynamicSerializeElement
    private String dataURI;

    public RetrieveAllProductsRequest() {
    }

    public RetrieveAllProductsRequest(String dataURI) {
        super();
        this.dataURI = dataURI;
    }

    public String getDataURI() {
        return dataURI;
    }

    public void setDataURI(String dataURI) {
        this.dataURI = dataURI;
    }

}
