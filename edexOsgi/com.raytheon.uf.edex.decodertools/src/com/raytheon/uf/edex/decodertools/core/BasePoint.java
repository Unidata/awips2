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
package com.raytheon.uf.edex.decodertools.core;

import com.vividsolutions.jts.geom.Coordinate;


/**
 * BasePoint provides a simple 3D (latitude, longitude, elevation) base class
 * for observation data. Sub-classes of BasePoint are responsible for adding
 * additional checking or using specific units as required. Most attributes of
 * this class do not have units.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 27 July 2007        411 jkorman     Initial Development
 * 20070911            379 jkorman     Added comments.
 * 20070912            379 jkorman     Code review cleanup.
 * Jun 05, 2014 3226       bclement    deprecated class, removed time
 * Sep 18, 2014 3627       mapeters    Removed unused methods/fields.
 * </pre>
 * 
 * @deprecated use {@link Coordinate} instead
 * @author jkorman
 * @version 1
 */
@Deprecated
public class BasePoint {
    private double latitude;

    private double longitude;

    /**
     * Create a base point at a given location.
     * 
     * @param latitude
     * @param longitude
     */
    public BasePoint(double latitude, double longitude) {
        this.latitude = latitude;
        this.longitude = longitude;
    }

    /**
     * Get the latitude of this point.
     * 
     * @return The latitude.
     */
    public double getLatitude() {
        return latitude;
    }

    /**
     * Set the latitude for this point.
     * 
     * @param latitude
     *            The latitude.
     */
    public void setLatitude(double latitude) {
        this.latitude = latitude;
    }

    /**
     * Get the longitude of this point.
     * 
     * @return The longitude.
     */
    public double getLongitude() {
        return longitude;
    }

    /**
     * Set the longitude for this point.
     * 
     * @param longitude
     *            The longitude.
     */
    public void setLongitude(double longitude) {
        this.longitude = longitude;
    }
}
