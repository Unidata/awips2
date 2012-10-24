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
package com.raytheon.uf.viz.remote.graphics.events.shapes;

import org.geotools.coverage.grid.GeneralGridGeometry;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.PixelExtent;
import com.raytheon.uf.viz.remote.graphics.events.AbstractDispatchingObjectEvent;
import com.raytheon.uf.viz.remote.graphics.events.ICreationEvent;

/**
 * Event to create a new wireframe shape object
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 27, 2012            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */
@DynamicSerialize
public class CreateWireframeShapeEvent extends AbstractDispatchingObjectEvent
        implements ICreationEvent {

    @DynamicSerializeElement
    private GeneralGridGeometry gridGeometry;

    @DynamicSerializeElement
    private boolean mutable;

    @DynamicSerializeElement
    private Float simplificationLevel;

    @DynamicSerializeElement
    private Boolean spatialChopFlag;

    @DynamicSerializeElement
    private double[] extent;

    /**
     * @return the gridGeometry
     */
    public GeneralGridGeometry getGridGeometry() {
        return gridGeometry;
    }

    /**
     * @param gridGeometry
     *            the gridGeometry to set
     */
    public void setGridGeometry(GeneralGridGeometry gridGeometry) {
        this.gridGeometry = gridGeometry;
    }

    /**
     * @return the mutable
     */
    public boolean isMutable() {
        return mutable;
    }

    /**
     * @param mutable
     *            the mutable to set
     */
    public void setMutable(boolean mutable) {
        this.mutable = mutable;
    }

    /**
     * @return the simplificationLevel
     */
    public Float getSimplificationLevel() {
        return simplificationLevel;
    }

    /**
     * @param simplificationLevel
     *            the simplificationLevel to set
     */
    public void setSimplificationLevel(Float simplificationLevel) {
        this.simplificationLevel = simplificationLevel;
    }

    /**
     * @return the spatialChopFlag
     */
    public Boolean isSpatialChopFlag() {
        return spatialChopFlag;
    }

    /**
     * @param spatialChopFlag
     *            the spatialChopFlag to set
     */
    public void setSpatialChopFlag(Boolean spatialChopFlag) {
        this.spatialChopFlag = spatialChopFlag;
    }

    /**
     * @return the extent
     */
    public double[] getExtent() {
        return extent;
    }

    /**
     * @param extent
     *            the extent to set
     */
    public void setExtent(double[] extent) {
        this.extent = extent;
    }

    public void setIExtent(IExtent extent) {
        if (extent != null) {
            setExtent(new double[] { extent.getMinX(), extent.getMaxX(),
                    extent.getMinY(), extent.getMaxY() });
        }
    }

    public IExtent getIExtent() {
        return new PixelExtent(extent[0], extent[1], extent[2], extent[3]);
    }
}
