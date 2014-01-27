package com.raytheon.uf.common.monitor.xml;

import java.util.ArrayList;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;

@XmlAccessorType(XmlAccessType.NONE)
public class FFTISettingXML {

    @XmlElement(name = "attribute", type = FFTIAttributeXML.class)
    private FFTIAttributeXML attribute;

    @XmlElement(name = "QPESource", type = FFTISourceXML.class)
    private FFTISourceXML qpeSource;

    @XmlElement(name = "GUIDSource", type = FFTISourceXML.class)
    private FFTISourceXML guidSource;

    @XmlElement(name = "QPFSource", type = FFTISourceXML.class)
    private FFTISourceXML qpfSource;

    public FFTISettingXML() {
        attribute = new FFTIAttributeXML();
        qpeSource = new FFTISourceXML();
        guidSource = new FFTISourceXML();
        qpfSource = new FFTISourceXML();
    }

    /**
     * @return the attribute
     */
    public FFTIAttributeXML getAttribute() {
        return attribute;
    }

    /**
     * @param attribute
     *            the attribute to set
     */
    public void setAttribute(FFTIAttributeXML attribute) {
        this.attribute = attribute;
    }

    /**
     * @return the qpeSource
     */
    public FFTISourceXML getQpeSource() {
        return qpeSource;
    }

    /**
     * @param qpeSource
     *            the qpeSource to set
     */
    public void setQpeSource(FFTISourceXML qpeSource) {
        this.qpeSource = qpeSource;
    }

    /**
     * @return the guidSource
     */
    public FFTISourceXML getGuidSource() {
        return guidSource;
    }

    /**
     * @param guidSource
     *            the guidSource to set
     */
    public void setGuidSource(FFTISourceXML guidSource) {
        this.guidSource = guidSource;
    }

    /**
     * @return the qpfSource
     */
    public FFTISourceXML getQpfSource() {
        return qpfSource;
    }

    /**
     * @param qpfSource
     *            the qpfSource to set
     */
    public void setQpfSource(FFTISourceXML qpfSource) {
        this.qpfSource = qpfSource;
    }

    /**
     * Get settings display names
     * 
     * @return
     */
    public ArrayList<String> getSettingDisplayNames() {

        ArrayList<String> displayNames = new ArrayList<String>();
        ArrayList<String> guidList = getGuidSource().getDisplayNameList();
        ArrayList<String> qpeList = getQpeSource().getDisplayNameList();
        ArrayList<String> qpfList = getQpfSource().getDisplayNameList();

        if (guidList != null) {
            displayNames.addAll(guidList);
        }
        if (qpeList != null) {
            displayNames.addAll(qpeList);
        }
        if (qpfList != null) {
            displayNames.addAll(qpfList);
        }

        return displayNames;

    }
}
