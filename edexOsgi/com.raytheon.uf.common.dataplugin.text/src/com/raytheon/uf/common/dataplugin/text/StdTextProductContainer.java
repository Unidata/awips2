package com.raytheon.uf.common.dataplugin.text;

import java.util.ArrayList;
import java.util.List;

import com.raytheon.uf.common.dataplugin.text.db.StdTextProduct;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Translates an AFOS id to a wmo ttaaii cccc and vice versa.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date           Ticket#    Engineer    Description
 * ------------   ---------- ----------- --------------------------
 * March 24, 2010 4734       M. Huang    Initial creation
 * May 15, 2014   2536       bclement    removed ISerializableObject
 * 
 * </pre>
 * 
 * @author mhuang
 * @version 1.0
 */
@DynamicSerialize
public class StdTextProductContainer {
    @DynamicSerializeElement
    private static List<StdTextProduct> productList = new ArrayList<StdTextProduct>();

    @DynamicSerializeElement
    private String errorMessage = null;

    public List<StdTextProduct> getProductList() {
        return productList;
    }

    @SuppressWarnings("static-access")
	public void setProductList(List<StdTextProduct> productList) {
        this.productList = productList;
    }

    public void add(StdTextProduct product) {
        productList.add(product);
    }

    public String getErrorMessage() {
        return errorMessage;
    }

    public void setErrorMessage(String errorMessage) {
        this.errorMessage = errorMessage;
    }
    
}
