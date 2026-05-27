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
package com.raytheon.uf.edex.plugin.mpe.geo;

import org.locationtech.jts.geom.Coordinate;

/**
 * POJO representation of a gage location read from the gage locations geo data
 * ascii file. Many of the ASCII files have a different layout. So, specific
 * objects have been defined for each file individually. The expectation is that
 * these objects will be indexed by the lid field for a quick and convenient
 * lookup of latitude/longitude. Based on: hpe_fieldgen/TEXT/get_loc_latlon.c.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 13, 2016 5631       bkowal      Initial creation
 *
 * </pre>
 *
 * @author bkowal
 */

public class GageLocation {

    private final String lid;

    private final Coordinate latLonCoord;

    public GageLocation(final String lid, final float lon, final float lat) {
        this.lid = lid;
        this.latLonCoord = new Coordinate(lon, lat);
    }

    public String getLid() {
        return lid;
    }

    public Coordinate getLatLonCoord() {
        return latLonCoord;
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder("GageLocation [");
        sb.append("lid=").append(lid);
        sb.append(", latLonCoord=").append(latLonCoord.toString());
        sb.append("]");
        return sb.toString();
    }
}