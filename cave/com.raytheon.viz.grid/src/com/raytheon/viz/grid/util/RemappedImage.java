package com.raytheon.viz.grid.util;

import org.geotools.coverage.grid.GridGeometry2D;

import com.raytheon.uf.common.datastorage.records.FloatDataRecord;

/**
 * Remapped image data and geometry
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 17, 2010            rgeorge     Initial creation
 * 
 * </pre>
 * 
 * @author rgeorge
 * @version 1.0
 */
public class RemappedImage {

    private FloatDataRecord floatDataRecord;

    private GridGeometry2D gridGeometry;

    /**
     * @param floatDataRecord
     */
    public RemappedImage(FloatDataRecord floatDataRecord) {
        this.floatDataRecord = floatDataRecord;
    }

    /**
     * @param inputData
     * @param gridGeometry
     */
    public RemappedImage(FloatDataRecord floatDataRecord,
            GridGeometry2D gridGeometry) {
        this.floatDataRecord = floatDataRecord;
        this.gridGeometry = gridGeometry;
    }

    /**
     * @return the floatDataRecord
     */
    public FloatDataRecord getFloatDataRecord() {
        return floatDataRecord;
    }

    /**
     * @param floatDataRecord
     *            the floatDataRecord to set
     */
    public void setFloatDataRecord(FloatDataRecord floatDataRecord) {
        this.floatDataRecord = floatDataRecord;
    }

    /**
     * @return the gridGeometry
     */
    public GridGeometry2D getGridGeometry() {
        return gridGeometry;
    }

    /**
     * @param gridGeometry
     *            the gridGeometry to set
     */
    public void setGridGeometry(GridGeometry2D gridGeometry) {
        this.gridGeometry = gridGeometry;
    }

}
