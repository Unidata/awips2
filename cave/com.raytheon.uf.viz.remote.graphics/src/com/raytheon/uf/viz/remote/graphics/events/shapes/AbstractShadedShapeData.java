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

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.vividsolutions.jts.geom.LineString;

/**
 * Abstract class for shaded shape data. Accommodates colormapped shaded shapes
 * and regular shaded shapes
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 22, 2012            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */
@DynamicSerialize
public abstract class AbstractShadedShapeData<T> {

    public static enum DataSpace {
        PIXEL, WORLD;
    }

    @DynamicSerializeElement
    private T info;

    @DynamicSerializeElement
    private LineString[] contour;

    @DynamicSerializeElement
    private DataSpace dataSpace;

    /**
     * @return the info
     */
    public T getInfo() {
        return info;
    }

    /**
     * @param info
     *            the info to set
     */
    public void setInfo(T info) {
        this.info = info;
    }

    /**
     * @return the contour
     */
    public LineString[] getContour() {
        return contour;
    }

    /**
     * @param contour
     *            the contour to set
     */
    public void setContour(LineString[] contour) {
        this.contour = contour;
    }

    /**
     * @return the dataSpace
     */
    public DataSpace getDataSpace() {
        return dataSpace;
    }

    /**
     * @param dataSpace
     *            the dataSpace to set
     */
    public void setDataSpace(DataSpace dataSpace) {
        this.dataSpace = dataSpace;
    }

}
