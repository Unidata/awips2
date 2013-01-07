package com.raytheon.uf.common.datadelivery.registry;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Handle Ensemble models
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 20, 2011    357      dhladky     Initial creation
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */

@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class Ensemble implements ISerializableObject {

    public Ensemble() {

    }

    /**
     * ensemble models set this
     */
    @DynamicSerializeElement
    @XmlAttribute
    private String name;

    /**
     * ensemble models set this
     */
    @DynamicSerializeElement
    @XmlAttribute
    private String length;

    /**
     * ensemble models set this
     */
    @DynamicSerializeElement
    @XmlAttribute
    private String init;

    /**
     * ensemble models set this
     */
    @DynamicSerializeElement
    @XmlAttribute
    private Integer size;

    public void setName(String name) {
        this.name = name;
    }

    public String getName() {
        return name;
    }

    public void setSize(Integer size) {
        this.size = size;
    }

    public Integer getSize() {
        return size;
    }

    public void setLength(String length) {
        this.length = length;
    }

    public String getLength() {
        return length;
    }

    public void setInit(String init) {
        this.init = init;
    }

    public String getInit() {
        return init;
    }
}
