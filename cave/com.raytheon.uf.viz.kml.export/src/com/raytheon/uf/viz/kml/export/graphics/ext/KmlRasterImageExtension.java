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
package com.raytheon.uf.viz.kml.export.graphics.ext;

import org.geotools.coverage.grid.GridCoverage2D;
import org.geotools.coverage.grid.GridCoverageFactory;
import org.geotools.coverage.processing.Operations;
import org.geotools.geometry.DirectPosition2D;
import org.geotools.geometry.Envelope2D;
import org.geotools.geometry.jts.ReferencedEnvelope;
import org.opengis.referencing.operation.MathTransform;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.DrawableImage;
import com.raytheon.uf.viz.core.PixelCoverage;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.drawables.ext.GraphicsExtension;
import com.raytheon.uf.viz.core.drawables.ext.IImagingExtension;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.kml.export.graphics.KmlGraphicsTarget;
import com.raytheon.uf.viz.kml.export.io.KmlOutputManager;

/**
 * Extension for creating KML Ground Overlay from raster images.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 11, 2012            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

public class KmlRasterImageExtension extends
        GraphicsExtension<KmlGraphicsTarget> implements IImagingExtension {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(KmlRasterImageExtension.class);

    @Override
    public boolean drawRasters(PaintProperties paintProps,
            DrawableImage... images) throws VizException {
        target.addGenerator(new Generator(paintProps.getAlpha(), images));
        return true;
    }

    @Override
    public int getCompatibilityValue(KmlGraphicsTarget target) {
        return Compatibilty.TARGET_COMPATIBLE;
    }

    private static class Generator extends KmlGroundOverlayGenerator {

        public Generator(float alpha, DrawableImage[] images) {
            super(alpha, images);
        }

        @Override
        public void addFeature(KmlOutputManager outputManager) {
            GridCoverageFactory coverageFactory = new GridCoverageFactory();
            for (DrawableImage image : images) {
                KmlRasterImage kmlImage = (KmlRasterImage) image.getImage();
                PixelCoverage coverage = image.getCoverage();
                MathTransform transform = gridGeometry.getGridToCRS();
                DirectPosition2D min = new DirectPosition2D(coverage.getMinX(),
                        coverage.getMinY());
                DirectPosition2D max = new DirectPosition2D(coverage.getMaxX(),
                        coverage.getMaxY());
                try {
                    transform.transform(min, min);
                    transform.transform(max, max);
                    ReferencedEnvelope env = new ReferencedEnvelope(
                            new Envelope2D(min, max),
                            gridGeometry.getCoordinateReferenceSystem());
                    GridCoverage2D source = coverageFactory.create("Test",
                            kmlImage.getImage(), env);
                    GridCoverage2D result = (GridCoverage2D) new Operations(
                            null).resample(source, MapUtil.LATLON_PROJECTION);
                    makeOverlay(outputManager, result.getRenderedImage(),
                            result.getEnvelope2D());
                } catch (VizException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            e.getLocalizedMessage(), e);

                } catch (TransformException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            e.getLocalizedMessage(), e);
                }
            }
        }
    }

}
