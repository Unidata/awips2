package com.raytheon.uf.edex.datadelivery.retrieval.handlers;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
/**
 * Wrapper for retrieval primary keys
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 30, 2014 2686       dhladky      refactor of retrieval.
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class RetrievalRequestWrapper {
    
    @XmlAttribute
    @DynamicSerializeElement
    private Object payload;

    /**
     * Constructor.
     */
    public RetrievalRequestWrapper() {
        
    }

    /**
     * Constructor
     * 
     * @param payload
     */
    public RetrievalRequestWrapper(Object payload) {
        this.payload = payload;
    }
    
    public void setPayload(Object payload) {
        this.payload = payload;
    }
    
    public Object getPayload() {
        return payload;
    }

}
