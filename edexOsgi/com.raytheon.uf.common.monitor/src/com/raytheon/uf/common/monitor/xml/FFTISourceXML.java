package com.raytheon.uf.common.monitor.xml;

import java.util.ArrayList;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElements;

@XmlAccessorType(XmlAccessType.NONE)
public class FFTISourceXML {

    @XmlElements({ @XmlElement(name = "DisplayName", type = String.class) })
    protected ArrayList<String> displayNameList;

    @XmlElement(name = "DurationHour")
    protected double DurationHour;

    /**
     * get the Duration Hour
     * 
     * @return the DurationHour
     */
    public double getDurationHour() {
        return DurationHour;
    }

    /**
     * set the Duration Hour
     * 
     * @param DurationHour
     */
    public void setDurationHour(double DurationHour) {
        this.DurationHour = DurationHour;
    }

    /**
     * @return the displayNameList
     */
    public ArrayList<String> getDisplayNameList() {
        return displayNameList;
    }

    /**
     * @param displayNameList
     *            the displayNameList to set
     */
    public void setDisplayNameList(ArrayList<String> displayNameList) {
        this.displayNameList = displayNameList;
    }

    public void addDisplayName(String name) {
        if (displayNameList == null) {
            displayNameList = new ArrayList<String>();
        }
        this.displayNameList.add(name);
    }
}
