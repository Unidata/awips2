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
import org.geotools.geometry.jts.CoordinateSequenceTransformer;
import org.geotools.geometry.jts.DefaultCoordinateSequenceTransformer;
import org.opengis.referencing.operation.MathTransform;
import org.opengis.referencing.operation.TransformException;

import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.impl.PackedCoordinateSequenceFactory;

/**
 * Represents a coordinate in any reference system
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 29, 2008            chammack    Initial creation
 * Feb 18, 2014  #2819     randerso    Made transform non-destructive
 * Apr 16, 2014  #2997     randerso    Changed to use our GeometryTransformer
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
        CoordinateSequenceTransformer t1 = new DefaultCoordinateSequenceTransformer(
                PackedCoordinateSequenceFactory.DOUBLE_FACTORY);
        final GeometryTransformer transformer = new GeometryTransformer(t1);
        transformer.setMathTransform(mt);

        return transformer.transform(this.internalObject);

    }
}
