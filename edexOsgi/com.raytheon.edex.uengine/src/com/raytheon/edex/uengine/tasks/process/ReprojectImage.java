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

package com.raytheon.edex.uengine.tasks.process;

import java.awt.image.BufferedImage;
import java.awt.image.RenderedImage;
import java.awt.image.renderable.ParameterBlock;

import javax.media.jai.JAI;
import javax.media.jai.PlanarImage;
import javax.media.jai.RenderedOp;

import org.geotools.coverage.grid.GridCoverage2D;
import org.geotools.coverage.grid.GridGeometry2D;
import org.geotools.geometry.Envelope2D;
import org.opengis.referencing.crs.CoordinateReferenceSystem;
import org.opengis.referencing.operation.MathTransform;

import com.raytheon.edex.uengine.exception.MicroEngineException;
import com.raytheon.edex.uengine.tasks.ScriptTask;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.Point;

/**
 * TODO
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date             PR#             Engineer            Description
 * -----------      ----------      ------------        --------------------------
 * Apr 10, 2007                     njensen             Initial Creation
 * 
 * </PRE>
 * 
 */
public class ReprojectImage extends ScriptTask {
    private BufferedImage image;

    private CoordinateReferenceSystem crs;

    private GridGeometry2D gridGeometry;

    public ReprojectImage(BufferedImage anImage, GridGeometry2D aGridGeometry,
            CoordinateReferenceSystem aCrs) {
        image = anImage;
        gridGeometry = aGridGeometry;
        crs = aCrs;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.edex.uengine.js.tasks.ScriptTask#execute()
     */
    @Override
    public Object execute() {
        BufferedImage reprojectedImage = null;
        logger.info("execute() reprojecting image" + ", image size="
                + (image.getHeight() * image.getWidth()));
        Envelope2D e = gridGeometry.getEnvelope2D();
        // crs

        GeometryFactory gf = new GeometryFactory();
        try {
            MathTransform toLatLon = MapUtil.getTransformToLatLon(gridGeometry
                    .getCoordinateReferenceSystem());

            double[] p1 = { e.getMinX(), e.getMinY() };
            double[] p2 = { e.getMaxX(), e.getMaxY() };

            toLatLon.transform(p1, 0, p1, 0, 1);
            toLatLon.transform(p2, 0, p2, 0, 1);
            Point[] p = new Point[2];
            p[0] = gf.createPoint(new Coordinate(p1[0], p1[1]));
            p[1] = gf.createPoint(new Coordinate(p2[0], p2[1]));

            // TODO: implement argument for other CRSes
            RenderedImage workImage = image;
            if (!workImage.getColorModel().hasAlpha()) {
                // Add alpha or we'll get ugly projection "bars"
                workImage = addAlpha(workImage);
            }
            GridCoverage2D coverage = MapUtil.constructGridCoverage(
                    "MicroEngine Grid", workImage, crs, p);

            GridCoverage2D projCoverage = MapUtil.reprojectCoverage(coverage,
                    MapUtil.LATLON_PROJECTION);

            gridGeometry = (GridGeometry2D) projCoverage.getGridGeometry();
            crs = gridGeometry.getCoordinateReferenceSystem();

            if (projCoverage.getRenderedImage() instanceof RenderedOp) {
                reprojectedImage = ((RenderedOp) projCoverage
                        .getRenderedImage()).getAsBufferedImage();
            } else {
                reprojectedImage = (BufferedImage) projCoverage
                        .getRenderedImage();
            }
        } catch (Exception e1) {
            e1.printStackTrace();
            throw new MicroEngineException("Error projecting image", e1);
        }

        JAI.getDefaultInstance().getTileCache().flush();
        return reprojectedImage;
    }

    /**
     * Create an Alpha layer in images without one (jpegs)
     * 
     * @param img
     *            the image to modify
     * @return the image
     */
    private RenderedImage addAlpha(RenderedImage img) {
        ParameterBlock pb = new ParameterBlock();

        Byte[] bandValues = new Byte[] { new Byte((byte) 255) };

        pb.add(new Float(img.getWidth()));
        pb.add(new Float(img.getHeight()));
        pb.add(bandValues);
        PlanarImage alphaPlane = JAI.create("Constant", pb, null);

        ParameterBlock pb2 = new ParameterBlock();
        pb2.removeParameters();
        pb2.removeSources();
        pb2.addSource(img);
        pb2.addSource(alphaPlane);

        long t0 = System.currentTimeMillis();
        PlanarImage ro = JAI.create("BandMerge", pb2, null);
        long tDelta = System.currentTimeMillis() - t0;

        logger.debug("Adding alpha layer took: " + tDelta + " (ms)");

        return ro;

    }

    public GridGeometry2D getGridGeometry() {
        return gridGeometry;
    }

    public void setGridGeometry(GridGeometry2D aGridGeometry) {
        gridGeometry = aGridGeometry;
    }

    public BufferedImage getImage() {
        return image;
    }

    public void setImage(BufferedImage aImage) {
        image = aImage;
    }

    public CoordinateReferenceSystem getCrs() {
        return crs;
    }

    public void setCrs(CoordinateReferenceSystem aCrs) {
        crs = aCrs;
    }

}
