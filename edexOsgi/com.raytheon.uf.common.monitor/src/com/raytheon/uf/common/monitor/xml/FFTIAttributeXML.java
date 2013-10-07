package com.raytheon.uf.common.monitor.xml;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;

@XmlAccessorType(XmlAccessType.NONE)
public class FFTIAttributeXML {
    @XmlElement(name = "Name")
    protected String attribName;

    @XmlElement(name = "yellowThreshold")
    protected double yellowThreshold;

    @XmlElement(name = "redThreshold")
    protected double redThreshold;

    public enum ATTRIBUTE {

        ACCUM("Accum"), RATIO("Ratio"), DIFF("Diff");

        private final String attribute;

        private ATTRIBUTE(String name) {
            attribute = name;
        }

        public String getAttribute() {
            return attribute;
        }
    };

    /**
     * get the attribute name
     * 
     * @return the attribName
     */
    public String getAttributeName() {
        return attribName;
    }

    /**
     * set the attrib name
     * 
     * @param attribName
     */
    public void setAttributeName(String attribName) {
        this.attribName = attribName;
    }

    /**
     * get the yellow threshold
     * 
     * @return the yellowThreshold
     */
    public double getYellowThrshld() {
        return yellowThreshold;
    }

    /**
     * set the yellow threshold
     * 
     * @param yellowThreshold
     */
    public void setYellowThrshld(double yellowThreshold) {
        this.yellowThreshold = yellowThreshold;
    }

    /**
     * get the red threshold
     * 
     * @return the redThreshold
     */
    public double getRedThrshld() {
        return redThreshold;
    }

    /**
     * set the red threshold
     * 
     * @param redThreshold
     */
    public void setRedThrshld(double redThreshold) {
        this.redThreshold = redThreshold;
    }
}
