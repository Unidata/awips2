package com.raytheon.uf.featureexplorer.jaxb;

import jakarta.xml.bind.annotation.XmlAccessOrder;
import jakarta.xml.bind.annotation.XmlAccessType;
import jakarta.xml.bind.annotation.XmlAccessorOrder;
import jakarta.xml.bind.annotation.XmlAccessorType;
import jakarta.xml.bind.annotation.XmlAttribute;

@XmlAccessorType(XmlAccessType.NONE)
@XmlAccessorOrder(XmlAccessOrder.UNDEFINED)
public class Includes {

    @XmlAttribute
    private String id;

    @XmlAttribute
    private String version;

    @XmlAttribute
    private boolean optional = false;

    public Includes() {

    }

    public Includes(String anId, String aVersion, boolean isOptional) {
        this.id = anId;
        this.version = aVersion;
        this.optional = isOptional;
    }

    public void setId(String anId) {
        this.id = anId;
    }

    public void setVersion(String aVersion) {
        this.version = aVersion;
    }

    public String getId() {
        return this.id;
    }

    public String getVersion() {
        return this.version;
    }

    public boolean getOptional() {
        return this.optional;
    }
}
