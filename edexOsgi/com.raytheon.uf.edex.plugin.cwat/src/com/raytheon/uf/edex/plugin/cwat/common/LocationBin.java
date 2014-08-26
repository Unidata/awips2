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
package com.raytheon.uf.edex.plugin.cwat.common;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * CWAT grib binning object
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 11/09/10      7441       D. Hladky   Initial release
 * Aug 26, 2014  3503       bclement    removed ISerializableObject
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1
 */
@DynamicSerialize
public class LocationBin {
    @DynamicSerializeElement
    private Coordinate cellCoord;

    @DynamicSerializeElement
    private Coordinate[] latLons;

    public Coordinate getCellCoord() {
        return cellCoord;
    }

    public void setCellCoord(Coordinate cellCoord) {
        this.cellCoord = cellCoord;
    }

    public Coordinate[] getLatLons() {
        return latLons;
    }

    public void setLatLons(Coordinate[] latLons) {
        this.latLons = latLons;
    }
}
