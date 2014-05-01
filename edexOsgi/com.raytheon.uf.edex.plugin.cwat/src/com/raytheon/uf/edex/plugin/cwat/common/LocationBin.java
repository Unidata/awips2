package com.raytheon.uf.edex.plugin.cwat.common;

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
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1
 */
import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.vividsolutions.jts.geom.Coordinate;

@DynamicSerialize
public class LocationBin implements ISerializableObject {
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
