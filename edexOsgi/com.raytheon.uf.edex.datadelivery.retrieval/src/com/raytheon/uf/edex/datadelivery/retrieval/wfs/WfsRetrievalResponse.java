package com.raytheon.uf.edex.datadelivery.retrieval.wfs;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.datadelivery.retrieval.xml.RetrievalAttribute;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.edex.datadelivery.retrieval.response.RetrievalResponse;

/**
 * {@link RetrievalResponse} for WFS.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 12, 2013 753        dhladky     Initial creation
 * May 31, 2013 2038       djohnson    Move to correct repo.
 * Oct 04, 2013 2267       bgonzale    Added default constructor.
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */

@DynamicSerialize
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
public class WfsRetrievalResponse extends RetrievalResponse {

    @XmlElement
    private String payload;

    /**
     * Default constructor.
     */
    public WfsRetrievalResponse() {
    }
    
    /**
     * Initialization constructor.
     * 
     * @param attribute
     */
    public WfsRetrievalResponse(RetrievalAttribute attribute) {
    }

    @Override
    public void setPayLoad(Object payLoad) {
        this.payload = (String) payLoad;
    }

    @Override
    public String getPayLoad() {
        return payload;
    }

}
