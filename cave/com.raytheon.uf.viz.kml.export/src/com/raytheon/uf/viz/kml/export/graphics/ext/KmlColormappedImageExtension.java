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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import org.geotools.coverage.grid.GridEnvelope2D;
import org.geotools.coverage.grid.GridGeometry2D;
import org.geotools.coverage.grid.InvalidGridGeometryException;
import org.geotools.geometry.DirectPosition2D;
import org.geotools.geometry.Envelope2D;
import org.opengis.coverage.grid.GridEnvelope;
import org.opengis.geometry.Envelope;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.crs.CoordinateReferenceSystem;
import org.opengis.referencing.operation.MathTransform;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.uf.common.colormap.prefs.ColorMapParameters;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.geospatial.interpolation.data.DataSource;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.DrawableImage;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.PixelCoverage;
import com.raytheon.uf.viz.core.data.IColorMapDataRetrievalCallback;
import com.raytheon.uf.viz.core.drawables.IColormappedImage;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.drawables.ext.GraphicsExtension;
import com.raytheon.uf.viz.core.drawables.ext.colormap.IColormappedImageExtension;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.kml.export.graphics.KmlGraphicsTarget;
import com.raytheon.uf.viz.kml.export.io.KmlOutputManager;

/**
 * 
 * Responsible for creating KML Ground Overlays from colormapped images.
 * Converts all raw data to floats, reproject to LatLon using an Interpolation
 * and use the Colormapper to generate a RenderedImage.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Jun 01, 2012           bsteffen    Initial creation
 * Jan 23, 2014  2703     bsteffen    Enable drawing with no mesh.
 * 
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class KmlColormappedImageExtension extends
        GraphicsExtension<KmlGraphicsTarget> implements
        IColormappedImageExtension {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(KmlColormappedImageExtension.class);

    @Override
    public boolean drawRasters(PaintProperties paintProps,
            DrawableImage... images) throws VizException {
        target.addGenerator(new Generator(paintProps.getAlpha(), images));
        return true;
    }

    @Override
    public IColormappedImage initializeRaster(
            IColorMapDataRetrievalCallback dataCallback,
            ColorMapParameters colorMapParameters) {
        return new KmlColormappedImage(dataCallback, colorMapParameters);
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
            drawRasters(outputManager, Arrays.asList(images));
        }

        private void drawRasters(KmlOutputManager out,
                List<DrawableImage> images) {
            Map<GridGeometry2D, DataSource> matches = new HashMap<GridGeometry2D, DataSource>();
            List<DrawableImage> others = new ArrayList<DrawableImage>();
            ColorMapParameters params = null;
            boolean interpolated = false;
            // find candidates for merging tiles, matches must have the same
            // color map parameters and interpolations state.
            for (DrawableImage image : images) {
                KmlColormappedImage kmlImage = (KmlColormappedImage) image
                        .getImage();
                PixelCoverage coverage = image.getCoverage();
                KmlMesh mesh = (KmlMesh) coverage.getMesh();
                if (mesh == null){
                    try {
                        kmlImage.loadData();
                    } catch (VizException e) {
                        statusHandler.handle(Priority.PROBLEM,
                                e.getLocalizedMessage(), e);
                        continue;
                    }
                    MathTransform grid2crs = gridGeometry.getGridToCRS();
                    IExtent extent = coverage.getExtent();
                    DirectPosition2D minCorner = new DirectPosition2D(
                            extent.getMinX(), extent.getMinY());
                    DirectPosition2D maxCorner = new DirectPosition2D(
                            extent.getMaxX(), extent.getMaxY());
                    try {
                        grid2crs.transform(minCorner, minCorner);
                        grid2crs.transform(maxCorner, maxCorner);
                    } catch (TransformException e) {
                        statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
                        continue;
                    }
                    
                    CoordinateReferenceSystem crs = gridGeometry
                            .getCoordinateReferenceSystem();
                    minCorner.setCoordinateReferenceSystem(crs);
                    maxCorner.setCoordinateReferenceSystem(crs);
                    Envelope userRange = new Envelope2D(minCorner, maxCorner);
                    GridEnvelope gridRange = new GridEnvelope2D(0, 0,
                            kmlImage.getWidth(), kmlImage.getHeight());

                    GridGeometry2D imageGeometry = new GridGeometry2D(
                            gridRange, userRange);
                    
                    mesh = new KmlMesh(imageGeometry);
                }
                try {
                    if (params == null) {
                        params = kmlImage.getColorMapParameters();
                        interpolated = kmlImage.isInterpolated();
                        matches.put(mesh.getImageGeometry(),
                                kmlImage.getData(mesh.getImageGeometry()));
                    } else if (!params.equals(kmlImage.getColorMapParameters())) {
                        others.add(image);
                    } else if (interpolated != kmlImage.isInterpolated()) {
                        others.add(image);
                    } else {
                        matches.put(mesh.getImageGeometry(),
                                kmlImage.getData(mesh.getImageGeometry()));
                    }
                } catch (VizException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            e.getLocalizedMessage(), e);
                }
            }
            if (!others.isEmpty()) {
                drawRasters(out, others);
            }
            try {
                drawRastersInternal(out, matches, params, interpolated);
            } catch (FactoryException e) {
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                        e);
            } catch (TransformException e) {
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                        e);
            }
        }

        /**
         * images must all have the same colormap parameters and interpolation
         * 
         * @param paintProps
         * @param images
         * @return
         * @throws TransformException
         * @throws FactoryException
         * @throws VizException
         */
        private void drawRastersInternal(KmlOutputManager out,
                Map<GridGeometry2D, DataSource> map,
                ColorMapParameters parameters, boolean interpolated)
                throws FactoryException, TransformException {
            // attempt to merge
            map = mergeTiles(map);
            // and then draw any that were merged.
            for (Entry<GridGeometry2D, DataSource> entry : map.entrySet()) {
                reprojectAndMakeOverlay(out, entry.getKey(), entry.getValue(),
                        parameters, interpolated);
            }
        }

        /**
         * make the kml overlay object.
         * 
         * @param paintProps
         * @param geometry
         * @param images
         * @throws TransformException
         * @throws FactoryException
         * @throws VizException
         */
        private void reprojectAndMakeOverlay(KmlOutputManager out,
                GridGeometry2D geometry, DataSource data,
                ColorMapParameters parameters, boolean interpolated)
                throws FactoryException, TransformException {
            Envelope env = new Envelope2D(MapUtil.LATLON_PROJECTION, -180, -90,
                    360, 180);
            GridGeometry2D projectedGeometry;
            projectedGeometry = GridGeometry2D.wrap(MapUtil.reprojectGeometry(
                    geometry, env));
            float[] fdata = reproject(geometry, projectedGeometry, data,
                    interpolated);
            makeOverlay(out, projectedGeometry, fdata, parameters);
        }

        private Map<GridGeometry2D, DataSource> mergeTiles(
                Map<GridGeometry2D, DataSource> map) {
            Map<GridGeometry2D, DataSource> newmap = new HashMap<GridGeometry2D, DataSource>();
            while (!map.isEmpty()) {
                // each iteration removes a single entry and also any entries
                // that are representable in the same grid space.
                Iterator<Entry<GridGeometry2D, DataSource>> it = map.entrySet()
                        .iterator();
                Entry<GridGeometry2D, DataSource> entry = it.next();
                it.remove();
                GridGeometry2D geom = entry.getKey();
                CoordinateReferenceSystem crs = geom
                        .getCoordinateReferenceSystem();
                Envelope2D env = geom.getEnvelope2D();
                // bigenv will be an envelope big enopugh to hold all compatible
                // grids.
                Envelope2D bigenv = null;
                Map<Envelope2D, DataSource> envmap = new HashMap<Envelope2D, DataSource>();
                envmap.put(env, entry.getValue());
                float dx = (float) (env.width / geom.getGridRange2D().width);
                float dy = (float) (env.height / geom.getGridRange2D().height);
                // loop through remaining entries to find any matches, a match
                // will have the same crs, grid spacing and the distance between
                // the envelopes will be a multiple of the grid spacing.
                while (it.hasNext()) {
                    entry = it.next();
                    GridGeometry2D geom2 = entry.getKey();
                    CoordinateReferenceSystem crs2 = geom2
                            .getCoordinateReferenceSystem();
                    Envelope2D env2 = geom2.getEnvelope2D();
                    float dx2 = (float) (env2.width / geom2.getGridRange2D().width);
                    float dy2 = (float) (env2.height / geom2.getGridRange2D().height);
                    // numeric comparisons are done using floats because float
                    // precision is considered "close enough" and double
                    // precision results in very small inconsitencies.
                    if (!crs.equals(crs2) || dx != dx2 || dy != dy2) {
                        continue;
                    }
                    // Make sure that the two grids line up, the distance
                    // between the envelopes should be an even multiple of dx
                    // and dy otherwise the grids are slightly offset from
                    // eachother and incompatible.
                    float xoffset = (float) ((env.x - env2.x) / dx);
                    float yoffset = (float) ((env.y - env2.y) / dy);
                    if (xoffset != (int) xoffset || yoffset != (int) yoffset) {
                        continue;
                    }
                    // at this point the two grids are compatible so we add them
                    // to the bigenv.
                    if (bigenv == null) {
                        bigenv = new Envelope2D(env);
                    }
                    it.remove();
                    envmap.put(env2, entry.getValue());
                    bigenv.add(env2);
                }
                if (bigenv != null) {
                    // determine GridEnvelope mapping needed to put all the
                    // matching grids into the same space.
                    try {
                        GridEnvelope2D range = geom.worldToGrid(bigenv);
                        range.x = 0;
                        range.y = 0;
                        GridGeometry2D newGeom = new GridGeometry2D(
                                (GridEnvelope) range, bigenv);
                        Map<GridEnvelope2D, DataSource> rangemap = new HashMap<GridEnvelope2D, DataSource>();
                        for (Entry<Envelope2D, DataSource> envent : envmap
                                .entrySet()) {
                            rangemap.put(newGeom.worldToGrid(envent.getKey()),
                                    envent.getValue());
                        }
                        newmap.put(newGeom, new MergedDataSource(rangemap));
                    } catch (InvalidGridGeometryException e) {
                        statusHandler.handle(Priority.PROBLEM,
                                e.getLocalizedMessage(), e);
                    } catch (TransformException e) {
                        statusHandler.handle(Priority.PROBLEM,
                                e.getLocalizedMessage(), e);
                    }
                } else {
                    // if there are no compatible grids than this grid must be
                    // handled all alone.
                    newmap.put(geom, envmap.get(env));
                }
            }
            return newmap;
        }
    }

    /**
     * 
     * Wraps multiple data sources that each cover a tile into a single data
     * source so it can all be reprojected in a single pass.
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
    private static class MergedDataSource implements DataSource {

        private final Map<GridEnvelope2D, DataSource> sources;

        public MergedDataSource(Map<GridEnvelope2D, DataSource> sources) {
            this.sources = sources;
        }

        @Override
        public double getDataValue(int x, int y) {
            for (Entry<GridEnvelope2D, DataSource> entry : sources.entrySet()) {
                GridEnvelope2D env = entry.getKey();
                if (env.contains(x, y)) {
                    x -= env.x;
                    y -= env.y;
                    return entry.getValue().getDataValue(x, y);
                }
            }

            return Double.NaN;
        }

    }

}
