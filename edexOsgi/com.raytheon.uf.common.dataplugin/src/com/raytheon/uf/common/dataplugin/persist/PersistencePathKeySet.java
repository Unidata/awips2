package com.raytheon.uf.common.dataplugin.persist;

import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElements;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;

@XmlRootElement(name = "pathKeySet")
@XmlAccessorType(XmlAccessType.NONE)
public class PersistencePathKeySet implements ISerializableObject{

    @XmlElements({ @XmlElement(name = "pathKey", type = PersistencePathKey.class) })
    private List<PersistencePathKey> pathKeys;

    /**
     * @return the pathKeys
     */
    public List<PersistencePathKey> getPathKeys() {
        return pathKeys;
    }

    /**
     * @param pathKeys
     *            the pathKeys to set
     */
    public void setPathKeys(List<PersistencePathKey> pathKeys) {
        this.pathKeys = pathKeys;
    }

}
