/**
 * This software was developed and / or modified by Raytheon Company,
 * pursuant to Contract DG133W-05-CQ-1067 with the US Government.
 * 
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * This software product contains export-restricted data whose
 * export/transfer/disclosure is restricted by U.S. law. Dissemination
 * to non-U.S. persons whether in the United States or abroad requires
 * an export license or other authorization.
 * 
 * Contractor Name:        Raytheon Company
 * Contractor Address:     6825 Pine Street, Suite 340
 *                         Mail Stop B8
 *                         Omaha, NE 68106
 *                         402.291.0100
 * 
 * See the AWIPS II Master Rights File ("Master Rights File.pdf") for
 * further licensing information.
 **/
package com.raytheon.uf.common.monitor.cpg;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;

/**
 * Monitor EDEX plugins on/off.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 05, 2009   3963      dhladky     Initial creation
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */
@XmlRootElement(name = "MonitorState")
@XmlAccessorType(XmlAccessType.NONE)
public class MonitorStateXML implements ISerializableObject {
       
    @XmlElement(name = "ffmp")
    private boolean ffmp = false;
    
    @XmlElement(name = "qpf")
    private boolean qpf = false;
    
    @XmlElement(name = "cwat")
    private boolean cwat = false;
    
    @XmlElement(name = "vil")
    private boolean vil = false;
    
    @XmlElement(name = "fog")
    private boolean fog = false;
    
    @XmlElement(name = "preciprate")
    private boolean preciprate = false;
    
    @XmlElement(name = "hydrodualpol")
    private boolean hydrodualpol = false;
    
    @XmlElement(name = "scan")
    private boolean scan = false;
    
    @XmlElement(name = "fssobs")
    private boolean fssobs = false;

    public boolean isFfmp() {
        return ffmp;
    }

    public void setFfmp(boolean ffmp) {
        this.ffmp = ffmp;
    }

    public boolean isQpf() {
        return qpf;
    }

    public void setQpf(boolean qpf) {
        this.qpf = qpf;
    }

    public boolean isCwat() {
        return cwat;
    }

    public void setCwat(boolean cwat) {
        this.cwat = cwat;
    }

    public boolean isVil() {
        return vil;
    }

    public void setVil(boolean vil) {
        this.vil = vil;
    }

    public boolean isFog() {
        return fog;
    }

    public void setFog(boolean fog) {
        this.fog = fog;
    }
    
    public void setPrecipRate(boolean preciprate) {
        this.preciprate = preciprate;
    }

    public boolean isPrecipRate() {
        return preciprate;
    }
  
    
    public void setHydroDualPol(boolean hydrodualpol) {
        this.hydrodualpol = hydrodualpol;
    }
    
    public boolean isHydroDualPol() {
        return hydrodualpol;
    }
    
    public void setScan(boolean scan) {
        this.scan = scan;
    }
    
    public boolean isScan() {
        return scan;
    }

    /**
     * @param fssobs the fssobs to set
     */
    public void setFssobs(boolean fssobs) {
        this.fssobs = fssobs;
    }

    /**
     * @return the fssobs
     */
    public boolean isFssobs() {
        return fssobs;
    }
 
}

