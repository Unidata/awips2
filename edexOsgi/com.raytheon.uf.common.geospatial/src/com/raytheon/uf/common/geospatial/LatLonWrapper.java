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
package com.raytheon.uf.common.geospatial;

/**
 * Lat/Lon data can be wrapped in here for ease of retrieval
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 21, 2013            mnash     Initial creation
 * 
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */

public class LatLonWrapper {

    private float[] lats;

    private float[] lons;

    /**
     * 
     */
    public LatLonWrapper(float[] lats, float[] lons) {
        this.lats = lats;
        this.lons = lons;
    }

    /**
     * @return the lats
     */
    public float[] getLats() {
        return lats;
    }

    /**
     * @return the lons
     */
    public float[] getLons() {
        return lons;
    }

}
