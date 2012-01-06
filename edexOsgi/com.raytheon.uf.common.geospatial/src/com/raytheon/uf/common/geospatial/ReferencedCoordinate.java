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

public class ReferencedCoordinate extends ReferencedObject<Coordinate> {

    /**
     * Create coordinate as grid crs native or grid cell native
     * 
     * @param obj
     * @param geometry
     * @param type
     */
    public ReferencedCoordinate(Coordinate obj, GeneralGridGeometry geometry,
            Type type) {
        super(obj, geometry, type);
    }

    /**
     * Create lat lon coordinate
     * 
     * @param coord
     */
    public ReferencedCoordinate(Coordinate coord) {
        super(coord);
    }

    /**
     * Create pixel coordinate
     * 
     * @param descriptor
     * @param coord
     */
    public ReferencedCoordinate(GeneralGridGeometry mapGeometry,
            Coordinate coord) {
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
    protected Coordinate transform(MathTransform mt) throws TransformException {

        double[] in = new double[] { this.internalObject.x,
                this.internalObject.y };
        double[] out = new double[2];

        mt.transform(in, 0, out, 0, 1);

        return new Coordinate(out[0], out[1]);

    }

}
