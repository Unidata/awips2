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

import java.nio.ByteBuffer;
import java.nio.FloatBuffer;
import java.nio.IntBuffer;
import java.nio.ShortBuffer;

import javax.measure.unit.Unit;

import org.geotools.coverage.grid.GridGeometry2D;

import com.raytheon.uf.common.colormap.image.ColorMapData;
import com.raytheon.uf.common.colormap.prefs.ColorMapParameters;
import com.raytheon.uf.common.geospatial.interpolation.data.ByteBufferWrapper;
import com.raytheon.uf.common.geospatial.interpolation.data.DataSource;
import com.raytheon.uf.common.geospatial.interpolation.data.FloatBufferWrapper;
import com.raytheon.uf.common.geospatial.interpolation.data.IntBufferWrapper;
import com.raytheon.uf.common.geospatial.interpolation.data.ShortBufferWrapper;
import com.raytheon.uf.common.geospatial.interpolation.data.UnsignedByteBufferWrapper;
import com.raytheon.uf.common.geospatial.interpolation.data.UnsignedShortBufferWrapper;
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
        switch (data.getDataType()) {
        case FLOAT:
            return new FloatBufferWrapper(((FloatBuffer) data.getBuffer()),
                    geometry);
        case BYTE: {
            return new UnsignedByteBufferWrapper(
                    ((ByteBuffer) data.getBuffer()), geometry);
        }
        case SIGNED_BYTE: {
            return new ByteBufferWrapper(((ByteBuffer) data.getBuffer()),
                    geometry);
        }
        case INT: {
            return new IntBufferWrapper(((IntBuffer) data.getBuffer()),
                    geometry);
        }
        case SHORT: {
            return new ShortBufferWrapper(((ShortBuffer) data.getBuffer()),
                    geometry);
        }
        case UNSIGNED_SHORT: {
            return new UnsignedShortBufferWrapper(
                    ((ShortBuffer) data.getBuffer()), geometry);
        }
        }
        throw new UnsupportedOperationException(
                "Kml Export does not supprt image type: " + data.getDataType());

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
