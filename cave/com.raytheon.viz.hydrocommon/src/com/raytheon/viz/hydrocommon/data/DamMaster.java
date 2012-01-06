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
package com.raytheon.viz.hydrocommon.data;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

/**
 * Dam Catalog DamMaster data object.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 8, 2009  2216       mpduff     Initial creation.
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlRootElement(name = "damMaster")
public class DamMaster {
    private String nidid = null;

    private String damName = null;

    private String county = null;

    private String river = null;

    private String downStreamHazard = null;

    private double max_storage;

    private String hsa = null;

    private String rfc = null;

    private double latitudeDam;

    private double longitudeDam;

    /**
     * @return the nidid
     */
    public String getNidid() {
        return nidid;
    }

    /**
     * @param nidid
     *            the nidid to set
     */
    public void setNidid(String nidid) {
        this.nidid = nidid;
    }

    /**
     * @return the damName
     */
    public String getDamName() {
        return damName;
    }

    /**
     * @param damName
     *            the damName to set
     */
    public void setDamName(String damName) {
        this.damName = damName;
    }

    /**
     * @return the county
     */
    public String getCounty() {
        return county;
    }

    /**
     * @param county
     *            the county to set
     */
    public void setCounty(String county) {
        this.county = county;
    }

    /**
     * @return the river
     */
    public String getRiver() {
        return river;
    }

    /**
     * @param river
     *            the river to set
     */
    public void setRiver(String river) {
        this.river = river;
    }

    /**
     * @return the downStreamHazard
     */
    public String getDownStreamHazard() {
        return downStreamHazard;
    }

    /**
     * @param downStreamHazard
     *            the downStreamHazard to set
     */
    public void setDownStreamHazard(String downStreamHazard) {
        this.downStreamHazard = downStreamHazard;
    }

    /**
     * @return the max_storage
     */
    public double getMax_storage() {
        return max_storage;
    }

    /**
     * @param max_storage
     *            the max_storage to set
     */
    public void setMax_storage(double max_storage) {
        this.max_storage = max_storage;
    }

    /**
     * @return the hsa
     */
    public String getHsa() {
        return hsa;
    }

    /**
     * @param hsa
     *            the hsa to set
     */
    public void setHsa(String hsa) {
        this.hsa = hsa;
    }

    /**
     * @return the rfc
     */
    public String getRfc() {
        return rfc;
    }

    /**
     * @param rfc
     *            the rfc to set
     */
    public void setRfc(String rfc) {
        this.rfc = rfc;
    }

    /**
     * @return the latitudeDam
     */
    public double getLatitudeDam() {
        return latitudeDam;
    }

    /**
     * @param latitudeDam
     *            the latitudeDam to set
     */
    public void setLatitudeDam(double latitudeDam) {
        this.latitudeDam = latitudeDam;
    }

    /**
     * @return the longitudeDam
     */
    public double getLongitudeDam() {
        return longitudeDam;
    }

    /**
     * @param longitudeDam
     *            the longitudeDam to set
     */
    public void setLongitudeDam(double longitudeDam) {
        this.longitudeDam = longitudeDam;
    }

}
