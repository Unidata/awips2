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
    
    public WfsRetrievalResponse(RetrievalAttribute attribute) {
        
    }

    @Override
    public void setPayLoad(Object payLoad) {
        this.payload = (String)payload;
    }

    @Override
    public String getPayLoad() {
        return payload;
    }

}
