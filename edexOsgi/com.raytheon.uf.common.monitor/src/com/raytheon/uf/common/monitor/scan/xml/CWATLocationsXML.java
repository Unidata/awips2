package com.raytheon.uf.common.monitor.scan.xml;

import java.util.ArrayList;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElements;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.monitor.scan.ThreatLocation;

/**
 * CWAT locations Configuration XML .
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 01, 2010            dhladky     Initial creation
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */

@XmlRootElement(name = "CWATLocationsXML")
@XmlAccessorType(XmlAccessType.NONE)
public class CWATLocationsXML {

    @XmlElements({ @XmlElement(name = "threats", type = ThreatLocation.class) })
    private ArrayList<ThreatLocation> threats;

    public ArrayList<ThreatLocation> getThreats() {
        return threats;
    }

    public void setThreats(ArrayList<ThreatLocation> threats) {
        this.threats = threats;
    }
}
