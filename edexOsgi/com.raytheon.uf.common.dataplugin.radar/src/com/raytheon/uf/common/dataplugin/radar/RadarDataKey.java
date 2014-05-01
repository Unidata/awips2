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
package com.raytheon.uf.common.dataplugin.radar;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Represents an i,j point on the Radar screen. Used to sort and maintain the
 * relationship of radar data.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 17, 2009 2000       askripsk     Initial creation
 * 
 * </pre>
 * 
 * @author askripsk
 * @version 1.0
 */

@DynamicSerialize
public class RadarDataKey implements ISerializableObject {

    @DynamicSerializeElement
    private double lon;

    @DynamicSerializeElement
    private double lat;

    /**
     * @return the longitude
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
     * @return the j
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

    @Override
    public boolean equals(Object o) {
        boolean rval = false;

        RadarDataKey that = (RadarDataKey) o;

        if (this == that) {
            rval = true;
        } else if (lon == that.lon && lat == that.lat) {
            rval = true;
        }

        return rval;

    }

    @Override
    public int hashCode() {
        int hash = 7;
        hash = 31 * hash + ((Double) lon).hashCode();
        hash = 31 * hash + ((Double) lat).hashCode();
        return hash;
    }

    @Override
    public String toString() {
        return String.format("lon: %s lat: %s", lon, lat);
    }
}
