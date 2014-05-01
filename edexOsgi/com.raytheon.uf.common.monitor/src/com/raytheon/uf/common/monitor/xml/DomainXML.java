package com.raytheon.uf.common.monitor.xml;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;

@XmlRootElement(name = "FFMPRunXML")
@XmlAccessorType(XmlAccessType.NONE)

public class DomainXML {
    
    @XmlAttribute(name = "primary")
    private boolean primary;
    
    @XmlAttribute(name = "cwa")
    private String cwa;
 
    public boolean isPrimary() {
        return primary;
    }

    public void setPrimary(boolean primary) {
        this.primary = primary;
    }
    
    public String getCwa() {
        return cwa;
    }
    
    public void setCwa(String cwa) {
        this.cwa = cwa;
    }
}
