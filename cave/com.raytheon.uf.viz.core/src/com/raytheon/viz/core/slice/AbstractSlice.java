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
package com.raytheon.viz.core.slice;

import org.opengis.coverage.grid.GridGeometry;

import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.interp.InterpolationResult;
import com.raytheon.uf.viz.core.style.level.SingleLevel;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * A slice of data
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 28, 2007            njensen     Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public abstract class AbstractSlice {

    protected GridGeometry gridGeometry;

    protected DataTime[] dataTimes;

    protected float interpolationFactor = 1.0f;

    protected SingleLevel[] levels;

    protected InterpolationResult interpResult;

    protected Coordinate[] coordinates;

    public abstract Object getSliceData();

    public abstract void setSliceData(Object data);

    public InterpolationResult getInterpResult() {
        return interpResult;
    }

    public void setInterpResult(InterpolationResult interpResult) {
        this.interpResult = interpResult;
    }

    /**
     * @return the gridGeometry
     */
    public GridGeometry getGridGeometry() {
        return gridGeometry;
    }

    public void setLevels(SingleLevel[] levels) {
        this.levels = levels;
    }

    public SingleLevel[] getLevels() {
        return this.levels;
    }

    /**
     * @param gridGeometry
     *            the gridGeometry to set
     */
    public void setGridGeometry(GridGeometry gridGeometry) {
        this.gridGeometry = gridGeometry;
    }

    /**
     * @return the dataTime
     */
    public DataTime[] getDataTime() {
        return dataTimes;
    }

    /**
     * @param dataTime
     *            the dataTime to set
     */
    public void setDataTime(DataTime[] dataTimes) {
        this.dataTimes = dataTimes;
    }

    /**
     * @return the interpolationFactor
     */
    public float getInterpolationFactor() {
        return interpolationFactor;
    }

    /**
     * @param interpolationFactor
     *            the interpolationFactor to set
     */
    public void setInterpolationFactor(float interpolationFactor) {
        this.interpolationFactor = interpolationFactor;
    }

    /**
     * @return the coordinates
     */
    public Coordinate[] getCoordinates() {
        return coordinates;
    }

    /**
     * @param coordinates
     *            the coordinates to set
     */
    public void setCoordinates(Coordinate[] coordinates) {
        this.coordinates = coordinates;
    }

}
