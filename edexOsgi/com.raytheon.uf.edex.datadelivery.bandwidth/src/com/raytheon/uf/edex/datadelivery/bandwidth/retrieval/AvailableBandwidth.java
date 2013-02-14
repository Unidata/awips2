package com.raytheon.uf.edex.datadelivery.bandwidth.retrieval;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;

/**
 * Class to describe available bandwidth for {@link BandwidthRoute} to generate
 * a profile of available network resources for any given time.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 27, 2012 726        jspinks     Initial release.
 * 
 * </pre>
 * 
 * @version 1.0
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
public class AvailableBandwidth extends RelativeTime {

    @XmlAttribute(name = "bandwidth")
    private int bandwidth;

    @XmlAttribute(name = "duration", required = false)
    private String duration;

    @XmlAttribute(name = "description", required = false)
    private String description;

    /**
     * @param bandwidth
     *            the bandwidth to set
     */
    public void setBandwidth(int bandwidth) {
        this.bandwidth = bandwidth;
    }

    /**
     * @return the bandwidth
     */
    public int getBandwidth() {
        return bandwidth;
    }

    /**
     * @param description
     *            the description to set
     */
    public void setDescription(String description) {
        this.description = description;
    }

    /**
     * @return the description
     */
    public String getDescription() {
        return description;
    }

    /**
     * @param duration
     *            the duration to set
     */
    public void setDuration(String duration) {
        this.duration = duration;
    }

    /**
     * @return the duration
     */
    public String getDuration() {
        return duration;
    }
}
