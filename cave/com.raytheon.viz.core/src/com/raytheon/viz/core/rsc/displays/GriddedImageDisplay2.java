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
package com.raytheon.viz.core.rsc.displays;

import java.awt.Rectangle;
import java.nio.Buffer;
import java.nio.FloatBuffer;
import java.nio.ShortBuffer;

import org.geotools.coverage.grid.GridGeometry2D;

import com.raytheon.uf.common.datastorage.StorageException;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.data.IDataPreparer;
import com.raytheon.uf.viz.core.data.prep.CMDataPreparerManager;
import com.raytheon.uf.viz.core.drawables.IImage;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.IMapDescriptor;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorMapCapability;
import com.raytheon.viz.core.rsc.hdf5.AbstractTileSet;

/**
 * TODO Add Description
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 27, 2008            randerso     Initial creation
 * Feb 01, 2011 #4237      ekladstrup   Rewrite to extend AbstractTileSet
 * </pre>
 * 
 * @author randerso
 * @version 2.0
 */

public class GriddedImageDisplay2 extends AbstractTileSet {

    private Buffer data;

    private DataType dataType = null;

    private static enum DataType {
        BYTE, FLOAT, INT, SHORT
    }

    /**
     * 
     * @param size
     *            size of imageTile, default to 512
     * @param data
     * @param descriptor
     * @param gridGeometry
     * @param rsc
     * @param viewType
     * @throws VizException
     */
    public GriddedImageDisplay2(int size, Buffer data,
            GridGeometry2D gridGeometry,
            AbstractVizResource<?, ? extends IMapDescriptor> rsc,
            String viewType) throws VizException {
        super(1, size, gridGeometry, rsc, viewType);
        this.setMapDescriptor(rsc.getDescriptor());
        this.data = data;
        setDataType();
    }

    /**
     * 
     * @param data
     * @param descriptor
     * @param gridGeometry
     * @param rsc
     * @param viewType
     * @throws VizException
     */
    public GriddedImageDisplay2(Buffer data, GridGeometry2D gridGeometry,
            AbstractVizResource<?, ? extends IMapDescriptor> rsc,
            String viewType) throws VizException {
        super(1, 512, gridGeometry, rsc, viewType);
        this.setMapDescriptor(rsc.getDescriptor());
        this.data = data;
        setDataType();
    }

    private void setDataType() {
        Object arr = data.array();

        if (arr instanceof byte[]) {
            dataType = DataType.BYTE;
        } else if (arr instanceof float[]) {
            dataType = DataType.FLOAT;
        } else if (arr instanceof int[]) {
            dataType = DataType.INT;
        } else if (arr instanceof short[]) {
            dataType = DataType.SHORT;
        }
    }

    @Override
    protected void preloadDataObject(int level) throws StorageException {
        // TODO Auto-generated method stub

    }

    @Override
    public boolean hasDataPreloaded(int level) {
        // TODO Auto-generated method stub
        if (level == 1) {
            return true;
        }
        return false;
    }

    @Override
    protected IImage createTile(IGraphicsTarget target, int level, int i, int j)
            throws VizException {

        if (dataType == null) {
            throw new VizException("Unsupported buffer type used");
        }

        // tile bounds
        Rectangle rect = this.tileSet.getTileSet().get(level)[i][j].rect;
        // total width
        int width = gridGeometry[level].getGridRange().getSpan(0);
        IImage image = null;
        switch (dataType) {
        case BYTE: {
            throw new VizException("Unsupported buffer type used (BYTE)");
            // image = createByteTile(target, rect, width, i, j);
            // break;
        }
        case FLOAT: {
            image = createFloatTile(target, rect, width, i, j);
            break;
        }
        case INT: {
            throw new VizException("Unsupported buffer type used (INT)");
            // image = createIntTile(target, rect, width, i, j);
            // break;
        }
        case SHORT: {
            // throw new VizException("Unsupported buffer type used (SHORT)");
            image = createShortTile(target, rect, width, i, j);
            break;
        }
        }
        return image;
    }

    private IImage createShortTile(IGraphicsTarget target, Rectangle rect,
            int width, int i, int j) {
        // short is 2 bytes
        int elemSize = 2;
        // buffer to copy into
        short[] dest = new short[rect.width * rect.height * elemSize];
        ShortBuffer srcBuff = (ShortBuffer) data;

        short[] tmp = new short[rect.width];
        int initSize = tileSize * j * width;

        for (int r = 0; r < rect.height; r++) {
            int curY = (r * width) + initSize;
            synchronized (this) {
                srcBuff.position(curY + rect.x);

                srcBuff.get(tmp);
            }

            for (int destIndex = r * rect.width; destIndex < (r * rect.width)
                    + rect.width; ++destIndex) {
                dest[destIndex] = tmp[destIndex - (r * rect.width)];
            }
        }

        ShortBuffer destBuff = ShortBuffer.wrap(dest);

        return generateImage(destBuff, target, rect);
    }

    private IImage createIntTile(IGraphicsTarget target, Rectangle rect,
            int width, int i, int j) {
        return null;
    }

    private IImage createFloatTile(IGraphicsTarget target, Rectangle rect,
            int width, int i, int j) {

        // buffer to copy into
        FloatBuffer destBuff = FloatBuffer.allocate(rect.width * rect.height);
        FloatBuffer srcBuff = (FloatBuffer) data;

        float[] tmp = new float[rect.width];
        int initSize = tileSize * j * width;

        for (int r = 0; r < rect.height; r++) {
            int curY = (r * width) + initSize;
            synchronized (this) {
                srcBuff.position(curY + rect.x);

                srcBuff.get(tmp);
            }

            destBuff.put(tmp);
        }

        return generateImage(destBuff, target, rect);
    }

    private IImage createByteTile(IGraphicsTarget target, Rectangle rect,
            int width, int i, int j) {
        return null;
    }

    protected IImage generateImage(Buffer tileData, IGraphicsTarget target,
            Rectangle rect) {

        // create image at i,j index
        int[] dims = { rect.width, rect.height };
        IDataPreparer preparer = CMDataPreparerManager.getDataPreparer(
                tileData, rect, dims);

        return target.initializeRaster(preparer,
                rsc.getCapability(ColorMapCapability.class)
                        .getColorMapParameters());
    }

    @Override
    public void cancelRequest(int level, int i, int j) {
        // TODO Auto-generated method stub

    }

    /**
     * @return the data
     */
    public Buffer getData() {
        return data;
    }
}