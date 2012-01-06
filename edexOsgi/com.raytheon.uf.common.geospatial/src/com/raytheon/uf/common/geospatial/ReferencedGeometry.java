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

import org.geotools.coverage.grid.GeneralGridGeometry;
import org.opengis.referencing.operation.MathTransform;
import org.opengis.referencing.operation.TransformException;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Geometry;

/**
 * Represents a coordinate in any reference system
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 29, 2008            chammack     Initial creation
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */

public class ReferencedGeometry extends ReferencedObject<Geometry> {

    /**
     * Create geometry as grid crs native or grid cell native
     * 
     * @param obj
     * @param geometry
     * @param type
     */
    public ReferencedGeometry(Geometry obj, GeneralGridGeometry geometry,
            Type type) {
        super(obj, geometry, type);
    }

    /**
     * Create map crs native geometry
     * 
     * @param obj
     * @param mapGeometry
     */
    public ReferencedGeometry(Geometry obj, GeneralGridGeometry mapGeometry) {
        super(obj, mapGeometry);
    }

    /**
     * Create lat lon geometry
     * 
     * @param coord
     */
    public ReferencedGeometry(Geometry coord) {
        super(coord);
    }

    /**
     * Create pixel geometry
     * 
     * @param mapGeometry
     * @param coord
     */
    public ReferencedGeometry(GeneralGridGeometry mapGeometry, Geometry coord) {
        super(mapGeometry, coord);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.geospatial.ReferencedObject#transform(org.opengis
     * .referencing.operation.MathTransform)
     */
    @Override
    protected Geometry transform(MathTransform mt) throws TransformException {
        // The following appears to be very slow due to object creation:
        // return JTS.transform(this.internalCoordinate, mt);

        // Faster, but destructive version:
        Coordinate[] coords = this.internalObject.getCoordinates();
        int size = coords.length * 2;
        double[] out = new double[size];
        double[] in = new double[size];
        int index = 0;
        for (int i = 0; i < coords.length; i++) {
            in[index++] = coords[i].x;
            in[index++] = coords[i].y;
        }

        try {
            mt.transform(in, 0, out, 0, coords.length);
            index = 0;
            for (int i = 0; i < coords.length; i++) {
                coords[i].x = out[index++];
                coords[i].y = out[index++];
            }
        } catch (Exception e) {
            e.printStackTrace();
        }

        return this.internalObject;

    }

}
