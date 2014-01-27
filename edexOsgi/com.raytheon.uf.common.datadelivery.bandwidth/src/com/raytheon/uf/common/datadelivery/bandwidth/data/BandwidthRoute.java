package com.raytheon.uf.common.datadelivery.bandwidth.data;

import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;

import com.raytheon.uf.common.datadelivery.registry.Network;

/**
 * Class to describe available bandwidth for edex to manage network load using
 * {@link IBandwidthManager}. BandwidthRoutes describe the map of network
 * resource availability for a scheduling various tasks that require network
 * resources. BandwidthRoutes contain a default profile of available network
 * resources and an optional list of modifications to the default network
 * profile using {@link AvailableBandwidth} entries to produce a
 * {@link BandwidthMap}
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 27, 2012 726        jspinks     Initial release.
 * Nov 27, 2013 1736       dhladky     Moved to common plugin.
 * 
 * </pre>
 * 
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
public class BandwidthRoute {

    @XmlAttribute(name = "bucketSizeMinutes", required = true)
    private int bucketSizeMinutes;

    @XmlAttribute(name = "defaultBandwidth", required = true)
    private int defaultBandwidth;

    @XmlElement(name = "modification", nillable = true)
    private List<AvailableBandwidth> modifications;

    @XmlAttribute(name = "network", required = true)
    private Network network;

    @XmlAttribute(name = "planDays", required = true)
    private int planDays;

    /**
     * @return the bucketSizeMinutes
     */
    public int getBucketSizeMinutes() {
        return bucketSizeMinutes;
    }

    /**
     * @return the defaultBandwidth
     */
    public int getDefaultBandwidth() {
        return defaultBandwidth;
    }

    /**
     * @return the modifications
     */
    public List<AvailableBandwidth> getModifications() {
        return modifications;
    }

    /**
     * @return the network
     */
    public Network getNetwork() {
        return network;
    }

    /**
     * @return the planHours
     */
    public int getPlanDays() {
        return planDays;
    }

    /**
     * @param bucketSizeMinutes
     *            the bucketSizeMinutes to set
     */
    public void setBucketSizeMinutes(int bucketSizeMinutes) {
        this.bucketSizeMinutes = bucketSizeMinutes;
    }

    /**
     * @param defaultBandwidth
     *            the defaultBandwidth to set
     */
    public void setDefaultBandwidth(int defaultBandwidth) {
        this.defaultBandwidth = defaultBandwidth;
    }

    /**
     * @param modifications
     *            the modifications to set
     */
    public void setModifications(List<AvailableBandwidth> modifications) {
        this.modifications = modifications;
    }

    /**
     * @param network
     *            the name to set
     */
    public void setNetwork(Network network) {
        this.network = network;
    }

    /**
     * @param planDays
     *            the planDays to set
     */
    public void setPlanDays(int planDays) {
        this.planDays = planDays;
    }

}
