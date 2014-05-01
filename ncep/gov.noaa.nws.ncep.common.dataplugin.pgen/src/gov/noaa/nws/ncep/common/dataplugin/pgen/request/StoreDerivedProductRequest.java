package gov.noaa.nws.ncep.common.dataplugin.pgen.request;

import gov.noaa.nws.ncep.common.dataplugin.pgen.DerivedProduct;

import java.util.ArrayList;
import java.util.List;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.serialization.comm.IServerRequest;

/**
 * 
 * Request to store a derived product for a given PGEN activity
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
public class StoreDerivedProductRequest implements IServerRequest {

    @DynamicSerializeElement
    private String dataURI;

    @DynamicSerializeElement
    private List<DerivedProduct> productList;

    public StoreDerivedProductRequest() {
    }

    /**
     * Convenience constructor for one Derived Product
     * 
     * @param dataURI
     * @param name
     * @param productType
     * @param product
     */
    public StoreDerivedProductRequest(String dataURI, String name,
            String productType, Object product) {
        this.dataURI = dataURI;
        productList = new ArrayList<DerivedProduct>();
        DerivedProduct prod = new DerivedProduct(name, productType, product);
        productList.add(prod);
    }

    public String getDataURI() {
        return dataURI;
    }

    public void setDataURI(String dataURI) {
        this.dataURI = dataURI;
    }

    public List<DerivedProduct> getProductList() {
        return productList;
    }

    public void setProductList(List<DerivedProduct> productList) {
        this.productList = productList;
    }

    public void addToProductList(DerivedProduct prod) {
        if (productList == null)
            productList = new ArrayList<DerivedProduct>();
        productList.add(prod);
    }
}
