package com.raytheon.uf.edex.datadelivery.retrieval.mapping;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;

@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
public class ParameterNameAlias implements ISerializableObject {
    @XmlAttribute(required = true)
    private String name;

    @XmlElement(name = "alias")
    private String[] aliases;

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String[] getAliases() {
        return aliases;
    }

    public void setAliases(String[] aliases) {
        this.aliases = aliases;
    }
}
