package gov.noaa.nws.ncep.common.dataplugin.pgen;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.serialization.comm.IServerRequest;

/**
 * 
 * Can contain any product derived from a PGEN Activity
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
public class DerivedProduct implements IServerRequest {

    @DynamicSerializeElement
    private String name;

    @DynamicSerializeElement
    private String productType;

    @DynamicSerializeElement
    private Object product;

    public DerivedProduct() {
    }

    public DerivedProduct(String name, String productType, Object product) {
        super();
        this.name = name;
        this.productType = productType;
        this.product = product;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getProductType() {
        return productType;
    }

    public void setProductType(String productType) {
        this.productType = productType;
    }

    public Object getProduct() {
        return product;
    }

    public void setProduct(Object product) {
        this.product = product;
    }

}
