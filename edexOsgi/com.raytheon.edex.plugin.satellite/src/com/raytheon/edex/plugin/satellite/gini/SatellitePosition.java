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
package com.raytheon.edex.plugin.satellite.gini;

import java.io.Serializable;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;

import com.raytheon.uf.common.dataplugin.persist.PersistableDataObject;

/**
 * Object describing the position of a geostationary satellite.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 01/22/2010   4335       bphillip    Initial Creation
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */
@Entity
@Table(name = "satellite_geostationary_positions")
public class SatellitePosition extends PersistableDataObject implements
        Serializable {

    private static final long serialVersionUID = 1854243110517231052L;

    /** The name of the satellite */
    @Id
    private String satelliteName;

    /** The height in km of the satellite above the earth's surface */
    @Column
    private int height;

    /** The latitude sub point of the satellite */
    @Column
    private float latitude;

    /** The longitude sub point of the satellite */
    @Column
    private float longitude;

    /**
     * Constructs an empty SatellitePosition
     */
    public SatellitePosition() {

    }

    /**
     * Constructs a SatellitePosition
     * 
     * @param satelliteName
     *            The satellite name
     * @param height
     *            The height in km of the satellite above the earth's surface
     * @param latitude
     *            The latitude sub point of the satellite
     * @param longitude
     *            The longitude sub point of the satellite
     */
    public SatellitePosition(String satelliteName, int height, float latitude,
            float longitude) {
        this.satelliteName = satelliteName;
        this.height = height;
        this.latitude = latitude;
        this.longitude = longitude;
    }

    /**
     * Gets the satellite name
     * 
     * @return The satellite name
     */
    public String getSatelliteName() {
        return satelliteName;
    }

    /**
     * Sets the satellite name
     * 
     * @param satelliteName
     *            The satellite name
     */
    public void setSatelliteName(String satelliteName) {
        this.satelliteName = satelliteName;
    }

    /**
     * Gets the satellite height
     * 
     * @return The satellite height
     */
    public int getHeight() {
        return height;
    }

    /**
     * Sets the satellite height
     * 
     * @param height
     *            The satellite height
     */
    public void setHeight(int height) {
        this.height = height;
    }

    /**
     * Gets the satellite's latitude sub point
     * 
     * @return The latitude sub point
     */
    public float getLatitude() {
        return latitude;
    }

    /**
     * Sets the satellite's latitude sub point
     * 
     * @param latitude
     *            The latitude sub point
     */
    public void setLatitude(float latitude) {
        this.latitude = latitude;
    }

    /**
     * Gets the satellite's longitude sub point
     * 
     * @return The satellite's longitude sub point
     */
    public float getLongitude() {
        return longitude;
    }

    /**
     * Sets the satellite's longitude sub point
     * 
     * @param longitude
     *            The satellite's longitude sub point
     */
    public void setLongitude(float longitude) {
        this.longitude = longitude;
    }
}
