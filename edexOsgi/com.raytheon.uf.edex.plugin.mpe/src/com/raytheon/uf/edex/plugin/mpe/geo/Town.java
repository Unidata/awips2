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
 * POJO representation of a town read from the town geo data ascii file. Many of
 * the ASCII files have a different layout. So, specific objects have been
 * defined for each file individually.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 29, 2016 5631       bkowal      Initial creation
 * 
 * </pre>
 * 
 * @author bkowal
 */

public class Town {

    private final String name;

    private final Coordinate hrapCoord;

    private final Coordinate latLonCoord;

    public Town(final String name, final Coordinate hrapCoord,
            final Coordinate latLonCoord) {
        this.name = name;
        this.hrapCoord = hrapCoord;
        this.latLonCoord = latLonCoord;
    }

    public String getName() {
        return name;
    }

    public Coordinate getHrapCoord() {
        return hrapCoord;
    }

    public Coordinate getLatLonCoord() {
        return latLonCoord;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + ((name == null) ? 0 : name.hashCode());
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        Town other = (Town) obj;
        if (name == null) {
            if (other.name != null)
                return false;
        } else if (!name.equals(other.name))
            return false;
        return true;
    }
}