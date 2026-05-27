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
package com.raytheon.viz.hydrocommon.radaroverlay;

import org.eclipse.swt.graphics.RGB;

/**
 * Radar Ring Overlay Data Object.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 19, 2010 1783       mpduff      Initial creation.
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class RadarRingOverlayData {
    private double lat;

    private double lon;

    private String radId = null;

    private boolean radAvail = true;

    private RGB color;

    public RadarRingOverlayData() {

    }

    /**
     * @return the lat
     */
    public double getLat() {
        return lat;
    }

    /**
     * @param lat
     *            the lat to set
     */
    public void setLat(double lat) {
        this.lat = lat;
    }

    /**
     * @return the lon
     */
    public double getLon() {
        return lon;
    }

    /**
     * @param lon
     *            the lon to set
     */
    public void setLon(double lon) {
        this.lon = lon;
    }

    /**
     * @return the radId
     */
    public String getRadId() {
        return radId;
    }

    /**
     * @param radId
     *            the radId to set
     */
    public void setRadId(String radId) {
        this.radId = radId;
    }

    /**
     * @return the radAvail
     */
    public boolean isRadAvail() {
        return radAvail;
    }

    /**
     * @param radAvail
     *            the radAvail to set
     */
    public void setRadAvail(boolean radAvail) {
        this.radAvail = radAvail;
    }

    /**
     * @return the color
     */
    public RGB getColor() {
        return color;
    }

    /**
     * @param color
     *            the color to set
     */
    public void setColor(RGB color) {
        this.color = color;
    }
}
