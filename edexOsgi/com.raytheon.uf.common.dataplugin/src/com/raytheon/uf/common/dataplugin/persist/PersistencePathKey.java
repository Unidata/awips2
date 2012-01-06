package com.raytheon.uf.common.dataplugin.persist;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;

@XmlRootElement(name = "pathKey")
@XmlAccessorType(XmlAccessType.NONE)
public class PersistencePathKey implements Comparable<PersistencePathKey>,ISerializableObject {

    @XmlElement
    private String key;

    @XmlElement
    private int order;

    public PersistencePathKey() {

    }

    public PersistencePathKey(String key, int order) {
        this.key = key;
        this.order = order;
    }

    /**
     * @return the key
     */
    public String getKey() {
        return key;
    }

    /**
     * @param key
     *            the key to set
     */
    public void setKey(String key) {
        this.key = key;
    }

    /**
     * @return the order
     */
    public int getOrder() {
        return order;
    }

    /**
     * @param order
     *            the order to set
     */
    public void setOrder(int order) {
        this.order = order;
    }

    @Override
    public int compareTo(PersistencePathKey o) {
        // Less than
        if (this.getOrder() < o.getOrder()) {
            return -1;
        }
        // Equal
        else if (this.getOrder() == o.getOrder()) {
            return 0;
        }
        // Greater than
        else if (this.getOrder() > o.getOrder()) {
            return 1;
        }

        // Should never get here
        return 0;
    }

}
