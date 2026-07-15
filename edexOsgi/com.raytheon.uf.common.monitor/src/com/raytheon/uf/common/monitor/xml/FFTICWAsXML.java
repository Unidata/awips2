package com.raytheon.uf.common.monitor.xml;

import java.util.ArrayList;

import jakarta.xml.bind.annotation.XmlAccessType;
import jakarta.xml.bind.annotation.XmlAccessorType;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlElements;

@XmlAccessorType(XmlAccessType.NONE)
public class FFTICWAsXML {

    @XmlElements({ @XmlElement(name = "CWA", type = String.class) })
    private ArrayList<String> cwaList;

    public FFTICWAsXML() {

    }

    /**
     * @return the cwa
     */
    public ArrayList<String> getCwaList() {
        return cwaList;
    }

    /**
     * @param cwa
     *            the cwa to set
     */
    public void setCwaList(ArrayList<String> cwaList) {
        this.cwaList = cwaList;
    }

    public void addCwa(String cwa) {
        this.cwaList.add(cwa);
    }
}