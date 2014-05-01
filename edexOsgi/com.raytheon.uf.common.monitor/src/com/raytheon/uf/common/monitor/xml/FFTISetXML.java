package com.raytheon.uf.common.monitor.xml;

import java.util.ArrayList;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElements;

@XmlAccessorType(XmlAccessType.NONE)
public class FFTISetXML {

    @XmlElement(name = "type")
    private String type;

    @XmlElement(name = "yellowThreshold")
    private double yellowThreshold;

    @XmlElement(name = "redThreshold")
    private double redThreshold;

    @XmlElements({ @XmlElement(name = "sourceSet", type = FFTISourceSetXML.class) })
    private ArrayList<FFTISourceSetXML> sourceSets;

    public void setType(String type) {
        this.type = type;
    }

    public String getType() {
        return type;
    }

    public void setYellowThreshold(double yellowThreshold) {
        this.yellowThreshold = yellowThreshold;
    }

    public double getYellowThreshold() {
        return yellowThreshold;
    }

    public void setRedThreshold(double redThreshold) {
        this.redThreshold = redThreshold;
    }

    public double getRedThreshold() {
        return redThreshold;
    }

    public void setSourceSets(ArrayList<FFTISourceSetXML> sourceSets) {
        this.sourceSets = sourceSets;
    }

    public ArrayList<FFTISourceSetXML> getSourceSets() {
        return sourceSets;
    }

    public FFTISourceSetXML getQPESet() {
        for (FFTISourceSetXML thisSet : sourceSets)
            if (thisSet.getSourceType().equalsIgnoreCase("qpe"))
                return thisSet;
        return null;
    }

    public FFTISourceSetXML getGUIDSet() {
        for (FFTISourceSetXML thisSet : sourceSets)
            if (thisSet.getSourceType().equalsIgnoreCase("guid"))
                return thisSet;
        return null;
    }

    public FFTISourceSetXML getQPFSet() {
        for (FFTISourceSetXML thisSet : sourceSets)
            if (thisSet.getSourceType().equalsIgnoreCase("qpf"))
                return thisSet;
        return null;
    }

}
