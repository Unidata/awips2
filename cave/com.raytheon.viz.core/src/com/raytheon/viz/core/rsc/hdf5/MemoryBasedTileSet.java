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
package com.raytheon.viz.core.rsc.hdf5;

import java.io.File;
import java.io.FileNotFoundException;

import javax.measure.converter.UnitConverter;

import org.geotools.coverage.grid.GridGeometry2D;
import org.opengis.referencing.datum.PixelInCell;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.datastorage.DataStoreFactory;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.datastorage.Request;
import com.raytheon.uf.common.datastorage.StorageException;
import com.raytheon.uf.common.datastorage.records.FloatDataRecord;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.data.IDataPreparer;
import com.raytheon.uf.viz.core.data.prep.CMDataPreparerManager;
import com.raytheon.uf.viz.core.datastructure.DataCubeContainer;
import com.raytheon.uf.viz.core.datastructure.VizDataCubeException;
import com.raytheon.uf.viz.core.drawables.IImage;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorMapCapability;

/**
 * Memory based tileset.
 * 
 * This memory-based tileset pulls a small raster from an hdf5 file and
 * interpolates it to a larger size. The raster is then split once it is already
 * loaded in memory.git pull
 * 
 * <pre>
 * 
 *  SOFTWARE HISTORY
 * 
 *  Date         Ticket#     Engineer    Description
 *  ------------ ----------  ----------- --------------------------
 *  Feb 15, 2007             chammack    Initial Creation.
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1
 */
public class MemoryBasedTileSet extends AbstractTileSet {

    protected Object[] loadedData;

    protected int[][] dims;

    protected boolean[] isLoaded;

    protected UnitConverter converter;

    protected PluginDataObject pdo;

    protected String hdf5File;

    protected String group;

    protected String dataset;

    public MemoryBasedTileSet(String hdf5File, String group, String dataset,
            AbstractTileSet sharedGeometryTileset, UnitConverter converter,
            PluginDataObject pdo) throws VizException {
        super(sharedGeometryTileset);
        this.converter = converter;
        this.isLoaded = new boolean[sharedGeometryTileset.levels];
        this.pdo = pdo;
        this.hdf5File = hdf5File;
        this.group = group;
        this.dataset = dataset;
    }

    public MemoryBasedTileSet(String hdf5File, String group, String dataset,
            int levels, int tileSize, GridGeometry2D gridGeometry,
            AbstractVizResource<?, ?> rsc, UnitConverter converter,
            PixelInCell orientation, PluginDataObject pdo, String viewType)
            throws VizException {
        super(levels, tileSize, gridGeometry, rsc, orientation, viewType);
        this.converter = converter;
        this.isLoaded = new boolean[levels];
        this.pdo = pdo;
        this.hdf5File = hdf5File;
        this.group = group;
        this.dataset = dataset;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.core.rsc.tiling.AbstractTileSet#preloadDataObject(int)
     */
    @Override
    protected void preloadDataObject(int level) throws StorageException {
        synchronized (isLoaded) {
            if (isLoaded[level]) {
                return;
            }
            IDataRecord rec = getDataRecord();

            if (loadedData == null) {
                loadedData = new Object[levels];
                dims = new int[levels][];
            }

            if (rec != null) {

                loadedData[level] = rec.getDataObject();

                long[] d = rec.getSizes();
                dims[level] = new int[] { (int) d[0], (int) d[1] };

            }

            isLoaded[level] = true;
        }
    }

    protected IDataRecord getDataRecord() throws StorageException {
        if (pdo != null) {
            try {
                IDataRecord[] records = DataCubeContainer.getDataRecord(pdo);
                if (records != null && records.length > 0) {
                    return records[0];
                }
            } catch (VizDataCubeException e) {
                // TODO Auto-generated catch block
                e.printStackTrace();
            }
        } else {
            try {
                IDataStore ds = DataStoreFactory
                        .getDataStore(new File(hdf5File));
                return ds.retrieve("", group + "/" + dataset, Request.ALL);
                // (float) this.interpolationFactorOnLoad
                // * (float) (Math.pow(2, this.levels
                // - level - 1)));
            } catch (FileNotFoundException e) {
                throw new StorageException("Unable to open file", null, e);
            }
        }
        return null;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.core.rsc.tiling.AbstractTileSet#createTile(com.raytheon
     * .viz.core.IGraphicsTarget, int, int, int)
     */
    @Override
    protected IImage createTile(IGraphicsTarget target, int level, int i, int j)
            throws VizException {
        IDataPreparer preparer = CMDataPreparerManager.getDataPreparer(
                loadedData[level], this.tileSet.getTile(level, i, j)
                        .getRectangle(), dims[level]);
        return target.initializeRaster(preparer,
                rsc.getCapability(ColorMapCapability.class)
                        .getColorMapParameters());
    }

    public float getDataMin() {
        float dataMin = Float.POSITIVE_INFINITY;

        if (loadedData[0] instanceof float[]) {
            float[] floatData = (float[]) loadedData[0];
            for (int i = 0; i < floatData.length; i++) {
                if (!Float.isNaN(floatData[i]) && floatData[i] != -999999) {
                    dataMin = Math.min(dataMin, floatData[i]);
                }
            }
        }
        return dataMin;

    }

    public float getDataMax() {
        float dataMax = Float.NEGATIVE_INFINITY;

        if (loadedData[0] instanceof float[]) {
            float[] floatData = (float[]) loadedData[0];
            for (int i = 0; i < floatData.length; i++) {
                if (!Float.isNaN(floatData[i]) && floatData[i] != -999999) {
                    dataMax = Math.max(dataMax, floatData[i]);
                }
            }
        }
        return dataMax;

    }

    protected IDataRecord getFullDataSet() throws StorageException,
            FileNotFoundException {

        IDataStore ds = DataStoreFactory.getDataStore(new File(hdf5File));
        IDataRecord dr = ds.retrieve(group, dataset, Request.ALL);

        if (converter == null) {
            return dr;
        }

        if (dr instanceof FloatDataRecord) {
            float[] fd = ((FloatDataRecord) dr).getFloatData();
            for (int i = 0; i < fd.length; i++) {
                fd[i] = (float) converter.convert(fd[i]);
            }
        }

        return dr;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.core.rsc.tiling.AbstractTileSet#hasDataPreloaded(int)
     */
    @Override
    public boolean hasDataPreloaded(int level) {
        return this.isLoaded[level];
    }

    @Override
    public void cancelRequest(int level, int i, int j) {
        // TODO Auto-generated method stub

    }

}