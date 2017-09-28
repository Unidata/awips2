package com.raytheon.uf.edex.plugin.goesr.dmw.description;

import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

/**
 * A list of Derived Motion Winds (DMW) Product Descriptions. 
 * See {@link PointSetProductDescriptions}, after which this class is modeled,
 * for more information.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 07/13/2016   19051   mcomerford   Initial creation
 *
 * </pre>
 *
 * @author matt.comerford
 * @version 1.0
 */

@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
public class ProductDescriptions {

    @XmlElement(name = "description")
    private List<ProductDescription> descriptions;

    public List<ProductDescription> getDescriptions() {
        return descriptions;
    }

    public String listDescriptions() {
        return descriptions.toArray().toString();
    }

    public void setDescriptions(List<ProductDescription> descriptions) {
        this.descriptions = descriptions;
    }

    public void addDescription(ProductDescription description) {
        this.descriptions.add(description);
    }

    public void addDescriptions(ProductDescriptions descriptions) {
        if (this.descriptions == null) {
            this.descriptions = new ArrayList<>();
        }
        this.descriptions.addAll(descriptions.getDescriptions());
    }
}
