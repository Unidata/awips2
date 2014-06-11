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
package com.raytheon.edex.plugin.binlightning.filter;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;

/**
 * JAXB POJO for filter bounding box
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 11, 2014 3226       bclement     Initial creation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
public class GeoFilterBbox {

    @XmlAttribute(required = true)
    private double minx;

    @XmlAttribute(required = true)
    private double maxx;

    @XmlAttribute(required = true)
    private double miny;

    @XmlAttribute(required = true)
    private double maxy;

    /**
     * 
     */
    public GeoFilterBbox() {
    }

    /**
     * @param minx
     * @param maxx
     * @param miny
     * @param maxy
     */
    public GeoFilterBbox(double minx, double maxx, double miny, double maxy) {
        this.minx = minx;
        this.maxx = maxx;
        this.miny = miny;
        this.maxy = maxy;
    }

    /**
     * @return the minx
     */
    public double getMinx() {
        return minx;
    }

    /**
     * @param minx
     *            the minx to set
     */
    public void setMinx(double minx) {
        this.minx = minx;
    }

    /**
     * @return the maxx
     */
    public double getMaxx() {
        return maxx;
    }

    /**
     * @param maxx
     *            the maxx to set
     */
    public void setMaxx(double maxx) {
        this.maxx = maxx;
    }

    /**
     * @return the miny
     */
    public double getMiny() {
        return miny;
    }

    /**
     * @param miny
     *            the miny to set
     */
    public void setMiny(double miny) {
        this.miny = miny;
    }

    /**
     * @return the maxy
     */
    public double getMaxy() {
        return maxy;
    }

    /**
     * @param maxy
     *            the maxy to set
     */
    public void setMaxy(double maxy) {
        this.maxy = maxy;
    }

}
