package gov.nasa.msfc.sport.edex.plugin.lma.util;

import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;


@DynamicSerialize
@XmlAccessorType(XmlAccessType.NONE)
@XmlRootElement(name = "lmaVarsContainer")
public class LmaVarsContainer {
    
    @XmlElement(name = "lmaVar")
    @DynamicSerializeElement
    private List<LmaVar> lmaVars;

    public LmaVarsContainer() {
        lmaVars = new ArrayList<LmaVar>();
    }

    public LmaVarsContainer(int initialSize) {
        lmaVars = new ArrayList<LmaVar>(initialSize);
    }

    public List<LmaVar> getLmaVars() {
        return lmaVars;
    }

    public void setLmaVar(List<LmaVar> lmaVars) {
        this.lmaVars = lmaVars;
    }

    public void add(LmaVar lmaVar) {
        lmaVars.add(lmaVar);
    }
}
