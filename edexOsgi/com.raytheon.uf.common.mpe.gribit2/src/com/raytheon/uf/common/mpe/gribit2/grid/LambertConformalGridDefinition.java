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
package com.raytheon.uf.common.mpe.gribit2.grid;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

/**
 * Lambert Conformal Grid Definition. Based on:
 * /rary.ohd.pproc.gribit/TEXT/w3fi71.f
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 14, 2016 4619       bkowal      Initial creation
 * 
 * </pre>
 * 
 * @author bkowal
 */
@XmlAccessorType(XmlAccessType.NONE)
@XmlRootElement(name = "lambertConformalGrid")
public class LambertConformalGridDefinition extends
        AbstractConformalGridDefinition {

    /**
     * FIRST LATITUDE FROM THE POLE AT WHICH THE SECANT CONE CUTS THE SPERICAL
     * EARTH
     */
    @XmlElement(required = true)
    private int firstLatConeCut;

    /**
     * SECOND LATITUDE ...
     */
    @XmlElement(required = true)
    private int secondLat;

    /**
     * LATITUDE OF SOUTH POLE (MILLIDEGREES)
     */
    @XmlElement(required = true)
    private int latSouthPole;

    /**
     * LONGITUDE OF SOUTH POLE (MILLIDEGREES)
     */
    @XmlElement(required = true)
    private int lonSouthPole;

    public int getFirstLatConeCut() {
        return firstLatConeCut;
    }

    public void setFirstLatConeCut(int firstLatConeCut) {
        this.firstLatConeCut = firstLatConeCut;
    }

    public int getSecondLat() {
        return secondLat;
    }

    public void setSecondLat(int secondLat) {
        this.secondLat = secondLat;
    }

    public int getLatSouthPole() {
        return latSouthPole;
    }

    public void setLatSouthPole(int latSouthPole) {
        this.latSouthPole = latSouthPole;
    }

    public int getLonSouthPole() {
        return lonSouthPole;
    }

    public void setLonSouthPole(int lonSouthPole) {
        this.lonSouthPole = lonSouthPole;
    }

    public String toString() {
        StringBuilder sb = new StringBuilder("LambertConformalGridDefinition [");
        sb.append(super.toString());
        sb.append(", firstLatConeCut=").append(firstLatConeCut);
        sb.append(", secondLat=").append(secondLat);
        sb.append(", latSouthPole=").append(latSouthPole);
        sb.append(", lonSouthPole=").append(lonSouthPole);
        sb.append("]");
        return sb.toString();
    }
}