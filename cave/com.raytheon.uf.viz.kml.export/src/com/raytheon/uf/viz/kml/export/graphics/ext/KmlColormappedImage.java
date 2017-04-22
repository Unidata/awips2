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

import java.awt.Rectangle;
import java.nio.ByteBuffer;
import java.nio.FloatBuffer;
import java.nio.IntBuffer;
import java.nio.ShortBuffer;

import javax.measure.unit.Unit;

import org.geotools.coverage.grid.GridGeometry2D;

import com.raytheon.uf.common.colormap.image.ColorMapData;
import com.raytheon.uf.common.colormap.prefs.ColorMapParameters;
import com.raytheon.uf.common.geospatial.data.GeographicDataSource;
import com.raytheon.uf.common.numeric.buffer.ByteBufferWrapper;
import com.raytheon.uf.common.numeric.buffer.FloatBufferWrapper;
import com.raytheon.uf.common.numeric.buffer.IntBufferWrapper;
import com.raytheon.uf.common.numeric.buffer.ShortBufferWrapper;
import com.raytheon.uf.common.numeric.filter.UnsignedFilter;
import com.raytheon.uf.common.numeric.source.DataSource;
import com.raytheon.uf.viz.core.data.IColorMapDataRetrievalCallback;
import com.raytheon.uf.viz.core.drawables.IColormappedImage;
import com.raytheon.uf.viz.core.drawables.ext.IImagingExtension;
import com.raytheon.uf.viz.core.exception.VizException;

/**
 * 
 * Implements the interface and converts all data callbacks into raw float
 * arrays.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Jun 01, 2012  704      bsteffen  Initial creation
 * Jan 23, 2014  2703     bsteffen  Use data to get width and height.
 * Mar 07, 2014  2791     bsteffen  Move Data Source/Destination to numeric
 *                                  plugin.
 * Apr 06, 2016  5400     bsteffen  Allow getters to be called before getData
 * 
 * </pre>
 * 
 * @author bsteffen
 */
public class KmlColormappedImage extends KmlImage implements IColormappedImage {

    private final IColorMapDataRetrievalCallback dataCallback;

    private ColorMapData data;

    private ColorMapParameters colorMapParameters;

    public KmlColormappedImage(IColorMapDataRetrievalCallback dataCallback,
            ColorMapParameters colorMapParameters) {
        this.dataCallback = dataCallback;
        this.colorMapParameters = colorMapParameters;
    }

    /**
     * Use the callback to retrieve the data, width, height, and dataUnit. If
     * the data has not been loaded {@link #getDataUnit()}, {@link #getWidth()}.
     * {@link #getHeight()} will not return correct values.
     * 
     * @throws VizException
     */
    public void loadData() throws VizException {
        if (this.data == null) {
            this.data = dataCallback.getColorMapData();
        }
    }

    /**
     * An alternative to {@link #loadData()} that doesn't throw a VizException.
     * This method can be used from methods which need the data but cannot throw
     * a VizException because the interface methods they are implementing do not
     * throw an exception. This method will throw an
     * {@link IllegalStateException} on failure.
     */
    private ColorMapData forceLoadData() {
        try {
            loadData();
        } catch (VizException e) {
            throw new IllegalStateException(
                    "Unable to load colormapped data for KML image.", e);
        }
        return data;
    }

    public GeographicDataSource getData(GridGeometry2D geometry)
            throws VizException {
        loadData();
        return new GeographicDataSource(getSimpleDataSource(), geometry);
    }

    /**
     * Converts {@link #data} to a {@link DataSource}. The data must mbe loaded
     * before calling this method.
     */
    private DataSource getSimpleDataSource() {
        int[] dim = forceLoadData().getDimensions();
        Rectangle dimensions = new Rectangle(dim[0], dim[1]);
        switch (data.getDataType()) {
        case FLOAT:
            return new FloatBufferWrapper(((FloatBuffer) data.getBuffer()),
                    dimensions);
        case BYTE: {
            return UnsignedFilter.apply(new ByteBufferWrapper(
                    ((ByteBuffer) data.getBuffer()), dimensions));
        }
        case SIGNED_BYTE: {
            return new ByteBufferWrapper(((ByteBuffer) data.getBuffer()),
                    dimensions);
        }
        case INT: {
            return new IntBufferWrapper(((IntBuffer) data.getBuffer()),
                    dimensions);
        }
        case SHORT: {
            return new ShortBufferWrapper(((ShortBuffer) data.getBuffer()),
                    dimensions);
        }
        case UNSIGNED_SHORT: {
            return UnsignedFilter.apply(new ShortBufferWrapper(
                    ((ShortBuffer) data.getBuffer()), dimensions));
        }
        default: {
            throw new UnsupportedOperationException(
                    "Kml Export does not support image type: "
                            + data.getDataType());
        }
        }
    }

    @Override
    public Class<? extends IImagingExtension> getExtensionClass() {
        return KmlColormappedImageExtension.class;
    }

    @Override
    public ColorMapParameters getColorMapParameters() {
        return colorMapParameters;
    }

    @Override
    public void setColorMapParameters(ColorMapParameters params) {
        this.colorMapParameters = params;
    }

    @Override
    public double getValue(int x, int y) {
        forceLoadData();
        return getSimpleDataSource().getDataValue(x, y);
    }

    @Override
    public Unit<?> getDataUnit() {
        Unit<?> dataUnit = forceLoadData().getDataUnit();
        return dataUnit == null ? getColorMapParameters().getDataUnit()
                : dataUnit;
    }

    @Override
    public int getWidth() {
        return forceLoadData().getDimensions()[0];
    }

    @Override
    public int getHeight() {
        return forceLoadData().getDimensions()[1];
    }

}
