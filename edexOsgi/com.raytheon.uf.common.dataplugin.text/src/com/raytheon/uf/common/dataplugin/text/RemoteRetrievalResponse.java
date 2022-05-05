package com.raytheon.uf.common.dataplugin.text;

import java.util.ArrayList;
import java.util.List;

import com.raytheon.uf.common.dataplugin.text.db.StdTextProduct;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Remote Retrieval Response
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- -------------------
 * Jul 15, 2015  7286     dfriedma  Initial creation
 * Aug 28, 2018  7134     randerso  Added product list
 *
 * </pre>
 *
 * @author randerso
 */
@DynamicSerialize
public class RemoteRetrievalResponse {
    @DynamicSerializeElement
    private boolean ok;

    @DynamicSerializeElement
    private String statusType;

    @DynamicSerializeElement
    private String statusMessage;

    @DynamicSerializeElement
    private List<StdTextProduct> productList = new ArrayList<>();

    /**
     * Nullary constructor for serialization
     */
    public RemoteRetrievalResponse() {

    }

    /**
     * Constructor
     *
     * @param ok
     * @param statusType
     * @param statusMessage
     */
    public RemoteRetrievalResponse(boolean ok, String statusType,
            String statusMessage) {
        this.ok = ok;
        this.statusType = statusType;
        this.statusMessage = statusMessage;
    }

    /**
     * @return the ok
     */
    public boolean isOk() {
        return ok;
    }

    /**
     * @param ok
     *            the ok to set
     */
    public void setOk(boolean ok) {
        this.ok = ok;
    }

    /**
     * @return the statusType
     */
    public String getStatusType() {
        return statusType;
    }

    /**
     * @param statusType
     *            the statusType to set
     */
    public void setStatusType(String statusType) {
        this.statusType = statusType;
    }

    /**
     * @return the statusMessage
     */
    public String getStatusMessage() {
        return statusMessage;
    }

    /**
     * @param statusMessage
     *            the statusMessage to set
     */
    public void setStatusMessage(String statusMessage) {
        this.statusMessage = statusMessage;
    }

    /**
     * @return the productList
     */
    public List<StdTextProduct> getProductList() {
        return productList;
    }

    /**
     * @param productList
     *            the productList to set
     */
    public void setProductList(List<StdTextProduct> productList) {
        this.productList = productList;
    }

    @Override
    public String toString() {
        StringBuilder builder = new StringBuilder();
        builder.append("RemoteRetrievalResponse [ok=");
        builder.append(ok);
        builder.append(", statusType=");
        builder.append(statusType);
        builder.append(", statusMessage=");
        builder.append(statusMessage);
        builder.append("]");
        return builder.toString();
    }
}
