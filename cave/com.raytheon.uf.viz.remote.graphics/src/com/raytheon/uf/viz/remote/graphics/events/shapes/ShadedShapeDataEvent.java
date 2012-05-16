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

import java.util.LinkedList;
import java.util.List;

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.viz.remote.graphics.events.AbstractDispatchingObjectEvent;
import com.vividsolutions.jts.geom.LineString;

/**
 * Event for shaded shape data for display
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 15, 2012            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */
@DynamicSerialize
public class ShadedShapeDataEvent extends AbstractDispatchingObjectEvent {

    public static enum DataSpace {
        WORLD, PIXEL;
    }

    @DynamicSerialize
    public static class ShadedShapeData {

        @DynamicSerializeElement
        private DataSpace dataSpace;

        @DynamicSerializeElement
        private RGB dataColor;

        @DynamicSerializeElement
        private LineString[] data;

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

        /**
         * @return the dataColor
         */
        public RGB getDataColor() {
            return dataColor;
        }

        /**
         * @param dataColor
         *            the dataColor to set
         */
        public void setDataColor(RGB dataColor) {
            this.dataColor = dataColor;
        }

        /**
         * @return the data
         */
        public LineString[] getData() {
            return data;
        }

        /**
         * @param data
         *            the data to set
         */
        public void setData(LineString[] data) {
            this.data = data;
        }
    }

    @DynamicSerializeElement
    private List<ShadedShapeData> shapeData = new LinkedList<ShadedShapeData>();

    @DynamicSerializeElement
    private boolean compile = false;

    /**
     * @return the shapeData
     */
    public List<ShadedShapeData> getShapeData() {
        return shapeData;
    }

    /**
     * @param shapeData
     *            the shapeData to set
     */
    public void setShapeData(List<ShadedShapeData> shapeData) {
        this.shapeData = shapeData;
    }

    /**
     * @return the compile
     */
    public boolean isCompile() {
        return compile;
    }

    /**
     * @param compile
     *            the compile to set
     */
    public void setCompile(boolean compile) {
        this.compile = compile;
    }

    public void addShapeData(DataSpace dataSpace, LineString[] data, RGB color) {
        ShadedShapeData shapeDataItem = new ShadedShapeData();
        shapeDataItem.setDataSpace(dataSpace);
        shapeDataItem.setData(data);
        shapeDataItem.setDataColor(color);
        shapeData.add(shapeDataItem);
    }
}
