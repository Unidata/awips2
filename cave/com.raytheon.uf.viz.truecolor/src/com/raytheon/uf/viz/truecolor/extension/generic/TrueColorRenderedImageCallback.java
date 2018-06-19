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
package com.raytheon.uf.viz.truecolor.extension.generic;

import java.awt.Color;
import java.awt.image.BufferedImage;
import java.awt.image.RenderedImage;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.measure.unit.Unit;

import org.geotools.coverage.grid.GridEnvelope2D;
import org.geotools.coverage.grid.GridGeometry2D;
import org.geotools.geometry.DirectPosition2D;
import org.geotools.geometry.Envelope2D;
import org.opengis.coverage.grid.GridEnvelope;
import org.opengis.referencing.operation.MathTransform;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.uf.common.colormap.image.Colormapper;
import com.raytheon.uf.common.geospatial.interpolation.GridReprojection;
import com.raytheon.uf.common.geospatial.interpolation.GridReprojectionDataSource;
import com.raytheon.uf.common.geospatial.interpolation.GridSampler;
import com.raytheon.uf.common.geospatial.interpolation.NearestNeighborInterpolation;
import com.raytheon.uf.common.numeric.filter.DataFilter;
import com.raytheon.uf.common.numeric.source.DataSource;
import com.raytheon.uf.common.numeric.source.FilteredDataSource;
import com.raytheon.uf.common.numeric.source.OffsetDataSource;
import com.raytheon.uf.viz.core.DrawableImage;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.IGridMesh;
import com.raytheon.uf.viz.core.IMesh;
import com.raytheon.uf.viz.core.data.IRenderedImageCallback;
import com.raytheon.uf.viz.core.drawables.IColormappedImage;
import com.raytheon.uf.viz.core.drawables.IImage;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.truecolor.extension.ITrueColorImagingExtension.Channel;
import com.raytheon.uf.viz.truecolor.extension.ITrueColorImagingExtension.ITrueColorImage;

/**
 * 
 * {@link IRenderedImageCallback} that takes all of the information within a
 * {@link ITrueColorImage} and is able to generate a {@link RenderedImage}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- -----------------
 * Apr 06, 2016  5400     bsteffen  Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 */
public class TrueColorRenderedImageCallback implements IRenderedImageCallback {

    private final int[] bounds;

    private final IExtent imageExtent;

    private final Map<Channel, DrawableImage[]> channelMap;

    private final Map<Channel, Double> gammaMap;

    public TrueColorRenderedImageCallback(int[] bounds, IExtent imageExtent,
            Map<Channel, DrawableImage[]> channelMap,
            Map<Channel, Double> gammaMap) {
        this.bounds = new int[] { bounds[0], bounds[1] };
        this.imageExtent = imageExtent;
        this.channelMap = new HashMap<>(channelMap);
        this.gammaMap = new HashMap<>(gammaMap);
    }

    @Override
    public RenderedImage getImage() throws VizException {
        int width = bounds[0];
        int height = bounds[1];
        int[] pixels = new int[width * height];
        DataSource redSource = getDataSource(Channel.RED);
        DataSource greenSource = getDataSource(Channel.GREEN);
        DataSource blueSource = getDataSource(Channel.BLUE);
        for (int y = 0; y < height; y += 1) {
            for (int x = 0; x < width; x += 1) {
                int index = y * width + x;
                double red = redSource.getDataValue(x, y);
                if (Double.isNaN(red)) {
                    continue;
                }
                double green = greenSource.getDataValue(x, y);
                if (Double.isNaN(green)) {
                    continue;
                }
                double blue = blueSource.getDataValue(x, y);
                if (Double.isNaN(blue)) {
                    continue;
                }
                pixels[index] = new Color((int) red, (int) green, (int) blue)
                        .getRGB();
            }
        }
        BufferedImage bi = new BufferedImage(width, height,
                BufferedImage.TYPE_INT_ARGB);
        bi.setRGB(0, 0, width, height, pixels, 0, width);
        return bi;
    }

    private DataSource getDataSource(Channel channel) throws VizException {
        CompositeDataSource compositeSource = new CompositeDataSource();
        DrawableImage[] images = channelMap.get(channel);
        if (images == null) {
            images = new DrawableImage[0];
        }
        for (DrawableImage image : images) {
            IMesh rawMesh = image.getCoverage().getMesh();
            if (!(rawMesh instanceof IGridMesh)) {
                throw new IllegalStateException(rawMesh.getClass()
                        .getSimpleName()
                        + " cannot be used within a true color image.");
            }
            IImage rawImage = image.getImage();
            if (!(rawImage instanceof IColormappedImage)) {
                throw new IllegalStateException(rawImage.getClass()
                        .getSimpleName()
                        + " cannot be used within a true color image.");
            }
            IGridMesh gridMesh = (IGridMesh) rawMesh;
            GridGeometry2D imageGeom = GridGeometry2D.wrap(gridMesh
                    .getSourceGeometry());
            GridEnvelope2D imageRange = imageGeom.getGridRange2D();
            GridGeometry2D targetGeom = GridGeometry2D.wrap(gridMesh
                    .getTargetGeometry());

            DirectPosition2D minPoint = new DirectPosition2D(imageExtent.getMinX(), imageExtent.getMinY());
            DirectPosition2D maxPoint = new DirectPosition2D(imageExtent.getMaxX(), imageExtent.getMaxY());
            try {
                MathTransform gridToCrs = targetGeom.getGridToCRS();
                gridToCrs.transform(minPoint, minPoint);
                gridToCrs.transform(maxPoint, maxPoint);
                Envelope2D env = new Envelope2D(minPoint, maxPoint);
                env.setCoordinateReferenceSystem(targetGeom
                        .getCoordinateReferenceSystem());
                GridEnvelope gridEnv = new GridEnvelope2D(0, 0, bounds[0],
                        bounds[1]);
                targetGeom = new GridGeometry2D(gridEnv, env);
            } catch (TransformException e) {
                throw new VizException("Error creating true color image.", e);
            }

            DataSource source = new ColorMappedImageSource(
                    (IColormappedImage) rawImage);
            source = new OffsetDataSource(source, -1 * imageRange.getLow(0), -1
                    * imageRange.getLow(1));
            GridSampler sampler = new GridSampler(source,
                    new NearestNeighborInterpolation());
            GridReprojection reprojection = new GridReprojection(imageGeom,
                    targetGeom);
            source = new GridReprojectionDataSource(reprojection, sampler);
            compositeSource.add(source);
        }
        return FilteredDataSource.addFilters(compositeSource,
                new IndexToColorFilter(gammaMap.get(channel)));
    }

    /**
     * Filter for transforming a colormap index(from 0.0 to 1.0) to a color
     * value(from 0 to 255). Also applies gamma correction.
     */
    private static class IndexToColorFilter implements DataFilter {

        private final double gamma;

        public IndexToColorFilter(double gamma) {
            this.gamma = gamma;
        }

        @Override
        public double filter(double value) {
            if (value > 1.0) {
                value = 1.0;
            } else if (value < 0.0) {
                value = 0.0;
            }
            value = Math.pow(value, gamma);
            return value * 255;
        }

    }

    /**
     * Combines multiple data sources for the same area into a single data
     * source.
     */
    private static class CompositeDataSource implements DataSource {

        private DataSource lastSource = null;

        private List<DataSource> sources = new ArrayList<>();

        public void add(DataSource source) {
            sources.add(0, source);
        }

        @Override
        public double getDataValue(int x, int y) {
            /*
             * This implementation deviates slightly from the reference
             * implementation(in GL) to get better performance. The GL
             * implementation checks all sources and uses the source with the
             * greatest no data value, however since each source requires a
             * reproject it is much faster to always use the value from the
             * first source that has a value. Since sources do not usually
             * overlap the resulting image looks identical to the GL
             * implementation.
             */
            /*
             * A majority of the time the previous source will have data for the
             * current point so always try it first.
             */
            if (lastSource != null) {
                double value = lastSource.getDataValue(x, y);
                if (!Double.isNaN(value)) {
                    return value;
                }
            }
            for (DataSource source : sources) {
                double value = source.getDataValue(x, y);
                if (!Double.isNaN(value)) {
                    return value;
                }
            }
            return Double.NaN;
        }

    }

    /**
     * Uses the API available for getting data from an {@link IColormappedImage}
     * and makes that data available through the {@link DataSource} interface.
     */
    private static class ColorMappedImageSource implements DataSource {

        private final IColormappedImage image;

        public ColorMappedImageSource(IColormappedImage image) {
            this.image = image;
        }

        @Override
        public double getDataValue(int x, int y) {
            if (x >= 0 && y >= 0 && image.getWidth() > x
                    && image.getHeight() > y) {
                double value = image.getValue(x, y);
                if (value == image.getColorMapParameters().getNoDataValue()) {
                    return Double.NaN;
                }
                Unit<?> dataUnit = image.getDataUnit();
                Unit<?> colorMapUnit = image.getColorMapParameters()
                        .getColorMapUnit();
                if (dataUnit != null && colorMapUnit != null
                        && dataUnit.isCompatible(colorMapUnit)) {
                    value = dataUnit.getConverterTo(colorMapUnit)
                            .convert(value);
                }
                double index = Colormapper.getColorMappingIndex(value,
                        image.getColorMapParameters());
                return index;
            } else {
                return Double.NaN;
            }
        }

    }

}
