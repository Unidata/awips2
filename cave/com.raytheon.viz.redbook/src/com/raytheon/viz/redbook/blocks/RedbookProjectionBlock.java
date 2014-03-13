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
package com.raytheon.viz.redbook.blocks;

import java.nio.ByteBuffer;

import org.geotools.coverage.grid.GeneralGridEnvelope;
import org.geotools.coverage.grid.GridGeometry2D;
import org.geotools.geometry.GeneralEnvelope;
import org.geotools.referencing.operation.DefaultMathTransformFactory;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.crs.ProjectedCRS;
import org.opengis.referencing.datum.PixelInCell;
import org.opengis.referencing.operation.MathTransform;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.uf.common.dataplugin.redbook.blocks.Block_004_017;
import com.raytheon.uf.common.dataplugin.redbook.blocks.RedbookBlockHeader;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.Point;

/**
 * Implements the Redbook projection block
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------	----------	-----------	--------------------------
 * May 27, 2008 1162        chammack    Initial creation
 * Apr 29, 2013 1958        bgonzale    New class RedbookBlockHeader
 * Mar 13, 2014	2907      	njensen    	split edex.redbook plugin into common and
 *                                      edex redbook plugins
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */

public class RedbookProjectionBlock extends Block_004_017 {

    /**
     * 
     * @param separator
     */
    public RedbookProjectionBlock(RedbookBlockHeader header, ByteBuffer data) {
        super(header, data);

    }

    public MathTransform getMathTransform(int m, int n)
            throws TransformException, FactoryException {
        ProjectedCRS crs = MapUtil.constructNorthPolarStereo(
                MapUtil.AWIPS_EARTH_RADIUS, MapUtil.AWIPS_EARTH_RADIUS,
                getLat1(), -getLon1());

        Point[] pts = new Point[4];
        GeometryFactory gf = new GeometryFactory();
        pts[0] = gf.createPoint(new Coordinate(-getUrLon(), getUrLat()));
        pts[1] = gf.createPoint(new Coordinate(-getLrLon(), getLrLat()));
        pts[2] = gf.createPoint(new Coordinate(-getLlLon(), getLlLat()));
        pts[3] = gf.createPoint(new Coordinate(-getUlLon(), getUlLat()));

        MathTransform toProj = MapUtil.getTransformFromLatLon(crs);

        double[] ll = new double[2];
        double[] ur = new double[2];
        toProj.transform(new double[] { -getLlLon(), getLlLat() }, 0, ll, 0, 1);
        toProj.transform(new double[] { -getUrLon(), getUrLat() }, 0, ur, 0, 1);

        GeneralEnvelope env = new GeneralEnvelope(2);
        env.setCoordinateReferenceSystem(crs);
        env.setRange(0, ll[0], ur[0]);
        env.setRange(1, ll[1], ur[1]);

        GridGeometry2D gg = new GridGeometry2D(new GeneralGridEnvelope(
                new int[] { 0, 0 }, new int[] { m, n }, false), env);

        MathTransform mt2 = gg.getGridToCRS(PixelInCell.CELL_CENTER);

        DefaultMathTransformFactory dmtf = new DefaultMathTransformFactory();
        MathTransform mt = dmtf.createConcatenatedTransform(mt2,
                toProj.inverse());

        return mt;
    }
}
