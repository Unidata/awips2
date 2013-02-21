package com.raytheon.uf.edex.datadelivery.retrieval.mapping;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
public class ParameterNameAliasList {
    @XmlElement(name = "parameterNameAlias")
    private ParameterNameAlias[] parameterNames;

    public ParameterNameAlias[] getParameterNames() {
        return parameterNames;
    }

    public void setParameterNames(ParameterNameAlias[] parameterNames) {
        this.parameterNames = parameterNames;
    }
}
