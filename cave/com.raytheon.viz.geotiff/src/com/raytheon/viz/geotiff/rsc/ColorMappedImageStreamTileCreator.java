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
package com.raytheon.viz.geotiff.rsc;

import java.awt.image.ColorModel;
import java.awt.image.DataBuffer;
import java.awt.image.Raster;
import java.awt.image.RenderedImage;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.nio.DoubleBuffer;
import java.nio.FloatBuffer;
import java.util.ArrayList;
import java.util.List;

import javax.imageio.ImageReadParam;
import javax.imageio.stream.FileImageInputStream;

import org.geotools.coverage.grid.GeneralGridGeometry;
import org.geotools.coverage.grid.GridEnvelope2D;

import com.raytheon.uf.common.colormap.ColorMap;
import com.raytheon.uf.common.colormap.image.ColorMapData;
import com.raytheon.uf.common.colormap.prefs.ColorMapParameters;
import com.raytheon.uf.viz.core.DrawableImage;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IMesh;
import com.raytheon.uf.viz.core.PixelCoverage;
import com.raytheon.uf.viz.core.data.IColorMapDataRetrievalCallback;
import com.raytheon.uf.viz.core.drawables.IImage;
import com.raytheon.uf.viz.core.drawables.ext.colormap.IColormappedImageExtension;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.IMapMeshExtension;
import com.raytheon.uf.viz.core.tile.Tile;

/**
 * Tile image creator that gets tiles from a FileImageInputStream and applies a
 * color map. Must be closed after use.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- -----------------
 * Oct 25, 2017  5773     bsteffen  Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 */
public class ColorMappedImageStreamTileCreator extends ImageStreamTileCreator {

    /**
     * There is no standard for what value to use for missing data. These are
     * common values for no data. The no data value is usually significantly
     * outside the range of valid data so if the data range is naturally lower
     * than any of these values they will not be used as the no data value.
     * Otherwise whichever value occurs most frequently in the data will be
     * used, although there can be only one no data value in the
     * {@link ColorMapParameters} so hopefully only one is found. This array
     * only contains numbers which might be below the minimum valid range. If
     * there is ever a no data value that is higher than the valid data range it
     * will require different checks to detect it.
     * 
     * -999 and -9999 have been encountered in real tiff files. -999,999 is
     * commonly used in numerical weather data sets. -99,999 is included only
     * because it seems logical.
     * 
     */
    private static final float[] POTENTIAL_NO_DATA_VALUES = { -999, -9999,
            -99_999, -999_999 };

    protected final ColorMapParameters colorMapParameters;

    public ColorMappedImageStreamTileCreator(File imageFile,
            ColorMapParameters colorMapParameters)
            throws FileNotFoundException, IOException {
        super(imageFile);
        this.colorMapParameters = colorMapParameters;
    }

    public ColorMappedImageStreamTileCreator(FileImageInputStream in,
            ColorMapParameters colorMapParameters) throws IOException {
        super(in);
        this.colorMapParameters = colorMapParameters;
    }

    @Override
    public DrawableImage createTileImage(IGraphicsTarget target, Tile tile,
            GeneralGridGeometry targetGeometry) throws VizException {
        if (tile.tileLevel != 0) {
            throw new VizException(getClass().getSimpleName()
                    + " only supports single level tiled data");
        }
        final GridEnvelope2D env = tile.tileGeometry.getGridRange2D();
        final ImageReadParam param = imgReader.getDefaultReadParam();
        param.setSourceRegion(env);
        IMesh mesh = target.getExtension(IMapMeshExtension.class)
                .constructMesh(tile.tileGeometry, targetGeometry);
        IImage img = target.getExtension(IColormappedImageExtension.class)
                .initializeRaster(new IColorMapDataRetrievalCallback() {

                    @Override
                    public ColorMapData getColorMapData() throws VizException {
                        try {
                            synchronized (imgReader) {
                                RenderedImage image = imgReader.read(0, param);
                                Raster data = image.getData();
                                Object dataObj = data.getDataElements(0, 0,
                                        env.width, env.height, null);
                                if (dataObj instanceof float[]) {
                                    return new ColorMapData(
                                            FloatBuffer.wrap((float[]) dataObj),
                                            new int[] { env.width,
                                                    env.height });
                                } else if (dataObj instanceof double[]) {
                                    return new ColorMapData(
                                            DoubleBuffer
                                                    .wrap((double[]) dataObj),
                                            new int[] { env.width,
                                                    env.height });
                                } else {
                                    throw new VizException(
                                            "Unsupported data of type "
                                                    + dataObj.getClass()
                                                            .getSimpleName());
                                }
                            }
                        } catch (IOException e) {
                            throw new VizException(
                                    "Unable to read tile from image stream for bounds: "
                                            + env,
                                    e);
                        }
                    }
                }, colorMapParameters);
        return new DrawableImage(img, new PixelCoverage(mesh));
    }

    public static ColorMapParameters initializeParameters(RenderedImage image) {
        float min = Float.POSITIVE_INFINITY;
        float max = Float.NEGATIVE_INFINITY;

        int[] noDataCounts = new int[POTENTIAL_NO_DATA_VALUES.length];

        Raster raster = image.getData();
        DataBuffer buffer = raster.getDataBuffer();
        for (int i = 0; i < buffer.getSize(); i += 1) {
            float f = buffer.getElemFloat(i);

            if (f < min) {
                boolean isNoData = false;
                for (int c = 0; c < noDataCounts.length; c += 1) {
                    if (f == POTENTIAL_NO_DATA_VALUES[c]) {
                        isNoData = true;
                        noDataCounts[c] += 1;
                        break;
                    }
                }
                if (!isNoData) {
                    min = f;
                }
            } else if (f > max) {
                max = f;
            }
        }
        ColorMapParameters parameters = new ColorMapParameters();
        parameters.setColorMapMin(min);
        parameters.setColorMapMax(max);

        int maxCount = 0;
        for (int c = 0; c < noDataCounts.length; c += 1) {
            if (min > POTENTIAL_NO_DATA_VALUES[c]
                    && noDataCounts[c] > maxCount) {
                parameters.setNoDataValue(POTENTIAL_NO_DATA_VALUES[c]);
                maxCount = noDataCounts[c];
            }
        }
        /* 1024 colors that is just a rainbow from blue(440) to red(780) */
        parameters.setColorMap(
                new ColorMap("<Default Colormap>", 1024, 440, 780, false));
        addLabels(parameters);
        return parameters;
    }

    private static void addLabels(ColorMapParameters parameters) {
        double min = parameters.getColorMapMin();
        double max = parameters.getColorMapMax();
        double range = max - min;
        /*
         * This will get the next smallest power of 10, powers of 10 make nice
         * round label spacing
         */
        double inc = Math.pow(10, (int) Math.log10(range));
        /*
         * This is an approximation, the actual number will be ±1 for rounding
         */
        double numLabels = range / inc;

        /*
         * There should be about 10 labels and they should be round numbers that
         * feel nice. The code above will get us an increment that is a perfect
         * power of 10 and provides between 1 and 10 labels. If there is less
         * than 7 labels then the code below will add extra labels by reducing
         * the increment.
         * 
         * The cutoff points are chosen arbitrarily to try to balance the
         * minimum number of labels and the maximum number of labels for edge
         * cases. For example 1.3 labels gets increased to 13(1.3*10) but 1.4
         * labels only gets increased to 5(1.4*4 = 5.6). 5 seems like too few
         * labels but if we increase the threshold then 1.4 labels gets
         * increased to 14(1.4*10), which seems like too many. I am more worried
         * about a cluttered display being hard to use so I chose the cutoff of
         * 1.4, but it is entirely unscientific.
         */
        if (numLabels < 1.4) {
            /* Use the next power of 10 */
            inc /= 10;
        } else if (numLabels < 3.1) {
            /* Use a quarter of a power of 10, like 2.5 or 2500 */
            inc /= 4;
        } else if (numLabels < 7.0) {
            /* Half a power of 10, like 5 or 500 */
            inc /= 2;
        }

        min = inc * Math.ceil(min / inc);
        max = inc * Math.ceil(max / inc);
        List<Double> intervals = new ArrayList<>();
        for (double v = min; v < max; v += inc) {
            intervals.add(v);
        }

        float[] primivals = new float[intervals.size()];
        for (int i = 0; i < primivals.length; i += 1) {
            primivals[i] = intervals.get(i).floatValue();
        }

        parameters.setColorBarIntervals(primivals);
    }

    public static boolean checkForRawRaster(RenderedImage image) {
        ColorModel colorModel = image.getColorModel();
        if (colorModel.getNumComponents() > 1) {
            /* Assume anything with multiple components is a real image. */
            return false;
        }
        if (colorModel.getTransferType() == DataBuffer.TYPE_FLOAT) {
            return true;
        }
        if (colorModel.getTransferType() == DataBuffer.TYPE_DOUBLE) {
            return true;
        }
        return false;
    }
}
