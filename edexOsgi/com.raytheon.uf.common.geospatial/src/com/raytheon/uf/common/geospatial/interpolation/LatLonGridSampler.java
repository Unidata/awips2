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
package com.raytheon.uf.common.geospatial.interpolation;

import org.geotools.coverage.grid.GeneralGridGeometry;
import org.geotools.coverage.grid.GridGeometry2D;
import org.geotools.geometry.DirectPosition2D;
import org.geotools.referencing.operation.DefaultMathTransformFactory;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.operation.MathTransform;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.numeric.source.DataSource;

/**
 * Similar to a regular GridSampler but inputs to sample method are Lat/Lon
 * coordinates that get transformed to grid space.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 5, 2013            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

public class LatLonGridSampler {

    private final GridSampler sampler;

    private final MathTransform fromLatLon;

    public LatLonGridSampler(GeneralGridGeometry sourceGeometry,
            DataSource source, Interpolation interpolation)
            throws FactoryException {
        sampler = new GridSampler(source, interpolation);
        MathTransform ll2crs = MapUtil.getTransformFromLatLon(sourceGeometry
                .getCoordinateReferenceSystem());
        MathTransform crs2grid = GridGeometry2D.wrap(sourceGeometry)
                .getCRSToGrid2D();
        fromLatLon = new DefaultMathTransformFactory()
                .createConcatenatedTransform(ll2crs, crs2grid);
    }

    public double sample(double lon, double lat) throws TransformException {
        DirectPosition2D pt = new DirectPosition2D(lon, lat);
        fromLatLon.transform(pt, pt);
        return sampler.sample(pt.x, pt.y);
    }

}
