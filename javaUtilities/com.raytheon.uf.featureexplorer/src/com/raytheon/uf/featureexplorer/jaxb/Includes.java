package com.raytheon.uf.featureexplorer.jaxb;

import javax.xml.bind.annotation.XmlAccessOrder;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorOrder;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;

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
