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

import java.util.Arrays;
import java.util.Comparator;

import org.geotools.coverage.grid.GridEnvelope2D;
import org.geotools.coverage.grid.GridGeometry2D;
import org.geotools.geometry.Envelope2D;
import org.geotools.geometry.jts.ReferencedEnvelope;
import org.opengis.coverage.grid.GridEnvelope;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.uf.common.colormap.prefs.ColorMapParameters;
import com.raytheon.uf.common.geospatial.interpolation.Interpolation;
import com.raytheon.uf.common.geospatial.interpolation.NearestNeighborInterpolation;
import com.raytheon.uf.common.numeric.buffer.BufferWrapper;
import com.raytheon.uf.common.numeric.buffer.FloatBufferWrapper;
import com.raytheon.uf.common.numeric.dest.DataDestination;
import com.raytheon.uf.common.numeric.source.DataSource;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.DrawableImage;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.drawables.ext.GraphicsExtension;
import com.raytheon.uf.viz.core.drawables.ext.IMosaicImageExtension;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.kml.export.graphics.KmlGraphicsTarget;
import com.raytheon.uf.viz.kml.export.io.KmlOutputManager;

/**
 * Generates a KMLGroundOverlay for mosaic by reprojecting all the images to a
 * common Lat/Lon space and performing a MaxVal algorithm.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 8, 2012            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

public class KmlMosaicImageExtension extends
        GraphicsExtension<KmlGraphicsTarget> implements IMosaicImageExtension {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(KmlMosaicImageExtension.class);

    @Override
    public boolean drawRasters(PaintProperties paintProps,
            DrawableImage... images) throws VizException {
        for (DrawableImage image : images) {
            target.addGenerator(new Generator(paintProps.getAlpha(),
                    (KmlMosaicImage) image.getImage()));
        }
        return true;
    }

    @Override
    public IMosaicImage initializeRaster(int[] imageBounds,
            IExtent imageExtent, ColorMapParameters params) throws VizException {
        return new KmlMosaicImage(imageBounds, imageExtent, params,
                getMosaicComparator());
    }

    /**
     * The mosaic comparator is used to implement different mosaicing
     * algorithms. The first Double will be the current value, and the second
     * double will be the dataValue, the dataValue will only be used if it is
     * greater than the existing value.
     * 
     * @return
     */
    protected Comparator<Double> getMosaicComparator() {
        return new Comparator<Double>() {
            @Override
            public int compare(Double o1, Double o2) {
                // always use data value.
                return -1;
            }

        };
    }

    @Override
    public int getCompatibilityValue(KmlGraphicsTarget target) {
        return Compatibilty.TARGET_COMPATIBLE;
    }

    private static class Generator extends KmlGroundOverlayGenerator {

        private final Comparator<Double> mosaicComparator;

        private final ColorMapParameters parameters;

        public Generator(float alpha, KmlMosaicImage image) {
            super(alpha, image.getImagesToMosaic());
            this.parameters = image.getColorMapParameters();
            this.mosaicComparator = image.getMosaicComparator();
        }

        @Override
        public void addFeature(KmlOutputManager outputManager) {
            if (images.length == 0) {
                return;
            }
            long startTime = System.currentTimeMillis();
            // make a big envelope in lat lon space that is large enough to hold
            // all radar sites.
            ReferencedEnvelope bigenv = null;
            for (DrawableImage image : images) {
                KmlMesh mesh = (KmlMesh) image.getCoverage().getMesh();
                try {
                    ReferencedEnvelope env = new ReferencedEnvelope(
                            mesh.getLatLonEnvelope());
                    if (bigenv == null) {
                        bigenv = env;
                    } else {
                        bigenv.expandToInclude(env);
                    }
                } catch (TransformException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            e.getLocalizedMessage(), e);
                }
            }
            // create a range that preserves aspect ratio with the larger
            // dimension as 1024 pixels, 1024 is a randomly chosen number.
            GridEnvelope2D range = new GridEnvelope2D(0, 0, 1024, 1024);
            if (bigenv.getWidth() > bigenv.getHeight()) {
                range.height = (int) (range.width * bigenv.getHeight() / bigenv
                        .getWidth());
            } else {
                range.width = (int) (range.height * bigenv.getWidth() / bigenv
                        .getHeight());
            }
            GridGeometry2D geom = new GridGeometry2D(range, bigenv);
            // fullDest is the end location of reprojected data values
            FloatBufferWrapper fullDest = new FloatBufferWrapper(range);
            Arrays.fill(fullDest.getArray(), Float.NaN);
            Interpolation interp = new NearestNeighborInterpolation();
            for (DrawableImage image : images) {
                KmlMesh mesh = (KmlMesh) image.getCoverage().getMesh();
                KmlColormappedImage kmlImage = (KmlColormappedImage) image
                        .getImage();
                try {
                    // Reproject a subgrid rather than the full grid because it
                    // is faster. The MosaicDataDestination maps the subgrid to
                    // the larger grid.
                    GridEnvelope2D gridenv = geom.worldToGrid(new Envelope2D(
                            mesh.getLatLonEnvelope()));
                    DataDestination dest = new MosaicDataDestination(
                            mosaicComparator, fullDest, gridenv.clone());
                    Envelope2D env = geom.gridToWorld(gridenv);
                    gridenv.x = 0;
                    gridenv.y = 0;
                    GridGeometry2D newGeom = new GridGeometry2D(
                            (GridEnvelope) gridenv, env);
                    GridGeometry2D oldGeom = mesh.getImageGeometry();
                    DataSource source = kmlImage.getData(oldGeom);
                    getReprojection(oldGeom, newGeom).reprojectedGrid(interp,
                            source, dest);
                } catch (VizException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            e.getLocalizedMessage(), e);
                } catch (TransformException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            e.getLocalizedMessage(), e);
                } catch (FactoryException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            e.getLocalizedMessage(), e);
                }
            }
            makeOverlay(outputManager, geom, fullDest.getArray(), parameters);
            System.out.println(System.currentTimeMillis() - startTime);
        }
    }

    /**
     * 
     * Provides a mapping into a subgrid of a larger destination and performs
     * maxVal mosaic algorithm when multiple radars map t a single grid cell.
     * 
     * <pre>
     * 
     * SOFTWARE HISTORY
     * 
     * Date         Ticket#    Engineer    Description
     * ------------ ---------- ----------- --------------------------
     * Jun 25, 2012            bsteffen     Initial creation
     * 
     * </pre>
     * 
     * @author bsteffen
     * @version 1.0
     */
    private static class MosaicDataDestination implements DataDestination {

        private final Comparator<Double> mosaicComparator;

        private final BufferWrapper fullDest;

        private final GridEnvelope2D envelope;

        public MosaicDataDestination(Comparator<Double> mosaicComparator,
                BufferWrapper fullDest, GridEnvelope2D envelope) {
            this.mosaicComparator = mosaicComparator;
            this.fullDest = fullDest;
            this.envelope = envelope;
        }

        @Override
        public void setDataValue(double dataValue, int x, int y) {
            x = x + envelope.x;
            y = y + envelope.y;
            // shift the data and apply maxVal algorithm
            double oldValue = fullDest.getDataValue(x, y);
            if (mosaicComparator.compare(oldValue, dataValue) < 0) {
                fullDest.setDataValue(dataValue, x, y);
            }
        }

    }

}
