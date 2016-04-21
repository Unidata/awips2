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
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Jun 01, 2012           bsteffen    Initial creation
 * Jan 23, 2014  2703     bsteffen    Use data to get width and height.
 * Mar 07, 2014  2791     bsteffen    Move Data Source/Destination to numeric
 *                                    plugin.
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
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

    public DataSource getData(GridGeometry2D geometry) throws VizException {
        loadData();
        Rectangle gridRange = geometry.getGridRange2D();
        DataSource source = null;
        switch (data.getDataType()) {
        case FLOAT:
            source = new FloatBufferWrapper(((FloatBuffer) data.getBuffer()),
                    gridRange);
            break;
        case BYTE: {
            source = UnsignedFilter.apply(new ByteBufferWrapper(
                    ((ByteBuffer) data.getBuffer()), gridRange));
            break;
        }
        case SIGNED_BYTE: {
            source = new ByteBufferWrapper(((ByteBuffer) data.getBuffer()),
                    gridRange);
            break;
        }
        case INT: {
            source = new IntBufferWrapper(((IntBuffer) data.getBuffer()),
                    gridRange);
            break;
        }
        case SHORT: {
            source = new ShortBufferWrapper(((ShortBuffer) data.getBuffer()),
                    gridRange);
            break;
        }
        case UNSIGNED_SHORT: {
            source = UnsignedFilter.apply(new ShortBufferWrapper(
                    ((ShortBuffer) data.getBuffer()), gridRange));
            break;
        }
        default: {
            throw new UnsupportedOperationException(
                    "Kml Export does not supprt image type: "
                            + data.getDataType());
        }
        }
        return new GeographicDataSource(source, geometry);

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
        return 0;
    }

    @Override
    public Unit<?> getDataUnit() {
        Unit<?> dataUnit = null;
        if (data != null) {
            dataUnit = data.getDataUnit();
        }
        return dataUnit == null ? getColorMapParameters().getDataUnit()
                : dataUnit;
    }

    @Override
    public int getWidth() {
        if (data != null) {
            return data.getDimensions()[0];
        } else {
            return super.getWidth();
        }
    }

    @Override
    public int getHeight() {
        if (data != null) {
            return data.getDimensions()[1];
        } else {
            return super.getHeight();
        }
    }

}
