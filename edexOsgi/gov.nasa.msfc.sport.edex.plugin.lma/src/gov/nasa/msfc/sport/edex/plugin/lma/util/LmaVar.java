package gov.nasa.msfc.sport.edex.plugin.lma.util;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;


@XmlAccessorType(XmlAccessType.NONE)
@XmlRootElement
public class LmaVar {

    @XmlAttribute
    private String name;
    
    @XmlAttribute
    private String avar;
    
    @XmlAttribute
    private String longname;
    
    public LmaVar() {
        
    }
    
    public String getName() {
        return name;
    }
    public void setName(String name) {
        this.name = name;
    }
    public String getAvar() {
        return avar;
    }
    public void setAvar(String avar) {
        this.avar = avar;
    }
    public String getLongname() {
        return longname;
    }
    public void setLongname(String longname) {
        this.longname = longname;
    }
}
