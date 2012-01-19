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
package com.raytheon.uf.common.dataplugin.bufrsigwx;
/**
 * 
 * 
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 *  09/24/2009             jsanchez    Initial creation.
 * 
 * 
 * </pre>
 * 
 * @author jsanchez
 * @version 1.0
 */
public class JetStreamData {
    
    private double jetSpeed;
    
    private double latitude;
    
    private double longitude;
    
    private double jetAltitude;

    private double baseHeight;
    
    private double topHeight;
    
    public JetStreamData(double lon, double lat, double speed, 
                        double altitude, double baseHgt, double topHgt) {
        latitude = lat;
        longitude = lon;
        jetSpeed = speed;
        jetAltitude = altitude;
        topHeight = topHgt;
        baseHeight = baseHgt;
    }

    public double getJetSpeed() {
        return jetSpeed;
    }

    public void setJetSpeed(double jetSpeed) {
        this.jetSpeed = jetSpeed;
    }

    public double getJetAltitude() {
        return jetAltitude;
    }

    public void setJetAltitude(double jetAltitude) {
        this.jetAltitude = jetAltitude;
    }
    
    public double getLatitude() {
        return latitude;
    }

    public void setLatitude(double latitude) {
        this.latitude = latitude;
    }

    public double getLongitude() {
        return longitude;
    }

    public void setLongitude(double longitude) {
        this.longitude = longitude;
    }
    
    public double getBaseHeight() {
        return baseHeight;
    }

    public void setBaseHeight(double baseHeight) {
        this.baseHeight = baseHeight;
    }

    public double getTopHeight() {
        return topHeight;
    }

    public void setTopHeight(double topHeight) {
        this.topHeight = topHeight;
    }
}
