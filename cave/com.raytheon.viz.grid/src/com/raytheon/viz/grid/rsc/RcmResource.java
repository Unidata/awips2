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
package com.raytheon.viz.grid.rsc;

import javax.measure.converter.UnitConverter;

import org.geotools.coverage.grid.GridGeometry2D;
import org.opengis.referencing.datum.PixelInCell;

import com.raytheon.uf.common.dataplugin.grib.GribRecord;
import com.raytheon.uf.common.datastorage.StorageException;
import com.raytheon.uf.common.geospatial.ReferencedCoordinate;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.viz.core.rsc.hdf5.AbstractTileSet;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 16, 2009            mnash     Initial creation
 * 
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */

public class RcmResource extends GridResource {

    /**
     * Extends the MemoryBasedTileSet class so that we can have direct access to
     * the loadedData
     */
    private class RcmMemoryBasedTileSet extends GridMemoryBasedTileSet {

        /**
         * @param group
         * @param dataset
         * @param sharedGeometryTileset
         * @param converter
         * @param pdo
         * @throws VizException
         */
        public RcmMemoryBasedTileSet(String group, String dataset,
                AbstractTileSet sharedGeometryTileset, UnitConverter converter,
                GribRecord pdo) throws VizException {
            super(group, dataset, sharedGeometryTileset, converter, pdo);
        }

        /**
         * @param dataURI
         * @param string
         * @param numLevels
         * @param i
         * @param gridGeometry2D
         * @param gridResource
         * @param conversion
         * @param cellCorner
         * @param record
         * @param viewType
         * @throws VizException
         */
        public RcmMemoryBasedTileSet(String dataURI, String string,
                int numLevels, int i, GridGeometry2D gridGeometry2D,
                GridResource gridResource, UnitConverter conversion,
                PixelInCell cellCorner, GribRecord record, String viewType)
                throws VizException {
            super(dataURI, string, numLevels, i, gridGeometry2D, gridResource,
                    conversion, cellCorner, record, viewType);
        }

        @Override
        public float[] getLoadedData() {
            return (float[]) loadedData[0];
        }

        /*
         * (non-Javadoc)
         * 
         * @see
         * com.raytheon.viz.core.rsc.hdf5.MemoryBasedTileSet#preloadDataObject
         * (int)
         */
        @Override
        protected void preloadDataObject(int level) throws StorageException {
            // TODO Auto-generated method stub
            super.preloadDataObject(level);
            float[] data = new float[((float[]) loadedData[0]).length];
            for (int i = 0; i < data.length; i++) {
                data[i] = ((float[]) loadedData[0])[i];
            }
            data = transformRCMData(data);
            for (int i = 0; i < data.length; i++) {
                ((float[]) loadedData[0])[i] = data[i];
            }
        }
    }

    /**
     * @param data
     * @param props
     */
    public RcmResource(RcmResourceData data, LoadProperties props) {
        super(data, props);
        this.units = "dBZ";
    }

    @Override
    public String inspect(ReferencedCoordinate coord) throws VizException {
        String tmp = super.inspect(coord);
        if ("No Data".equals(tmp)) {
            return tmp;
        }
        tmp = tmp.replace("dBZ", "");
        float x = Float.parseFloat(tmp);
        return dBZasString(x);
    }

    /**
     * Takes the value of the cell and changes it to be a range for sampling
     * 
     * @param val
     * @return
     */
    private String dBZasString(float val) {
        String sampleVal = "";
        if (val < 1f) {
            sampleVal = "No Data";
        } else if (val < 48f) {
            sampleVal = "< 15dBZ";
        } else if (val < 96f) {
            sampleVal = "15-29dBZ";
        } else if (val < 128f) {
            sampleVal = "30-39dBZ";
        } else if (val < 144f) {
            sampleVal = "40-44dBZ";
        } else if (val < 160f) {
            sampleVal = "45-49dBZ";
        } else if (val < 176f) {
            sampleVal = "50-54dBZ";
        } else {
            sampleVal = ">= 55dBZ";
        }
        return sampleVal;
    }

    /**
     * Takes the value of the cell and changes it to be a range for sampling
     * 
     * @param val
     * @return
     */
    private float[] transformRCMData(float[] rcmData) {
        for (int i = 0; i < rcmData.length; i++) {
            if (rcmData[i] < 1f) {
                rcmData[i] = 1f;
            } else if (rcmData[i] < 2) {
                rcmData[i] = 48f;
            } else if (rcmData[i] < 3) {
                rcmData[i] = 96f;
            } else if (rcmData[i] < 4) {
                rcmData[i] = 128f;
            } else if (rcmData[i] < 5) {
                rcmData[i] = 144f;
            } else if (rcmData[i] < 6) {
                rcmData[i] = 160f;
            } else if (rcmData[i] < 7) {
                rcmData[i] = 176f;
            } else {
                rcmData[i] = 0f;
            }
        }
        return rcmData;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.grid.rsc.GridResource#createTile(com.raytheon.uf.common
     * .dataplugin.grib.GribRecord, java.lang.String)
     */
    @Override
    public GridMemoryBasedTileSet createTile(GribRecord record,
            GridMemoryBasedTileSet commonTile) throws VizException {
        if (commonTile != null) {
            return new RcmMemoryBasedTileSet(record.getDataURI(), "Data",
                    commonTile, conversion, record);
        }

        GridGeometry2D gridGeometry2D = record.getModelInfo().getLocation()
                .getGridGeometry();
        return new RcmMemoryBasedTileSet(record.getDataURI(), "Data",
                numLevels, 256, gridGeometry2D, this, conversion,
                PixelInCell.CELL_CORNER, record, viewType);
    }
}
