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

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.viz.remote.graphics.events.AbstractDispatchingObjectEvent;
import com.raytheon.uf.viz.remote.graphics.events.shapes.AbstractShadedShapeData.DataSpace;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.LineString;

/**
 * Event for colormapped shaded shape data for display
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
public class ColormappedShadedShapeDataEvent extends
        AbstractDispatchingObjectEvent {

    @DynamicSerialize
    public static class ColormappedShadedShapeData extends
            AbstractShadedShapeData<Integer> {

    }

    @DynamicSerialize
    public static class ColormappedShadedGeometryData {

        @DynamicSerializeElement
        private Geometry geometry;

        @DynamicSerializeElement
        private Integer colorKey;

        /**
         * @return the geometry
         */
        public Geometry getGeometry() {
            return geometry;
        }

        /**
         * @param geometry
         *            the geometry to set
         */
        public void setGeometry(Geometry geometry) {
            this.geometry = geometry;
        }

        /**
         * @return the colorKey
         */
        public Integer getColorKey() {
            return colorKey;
        }

        /**
         * @param colorKey
         *            the colorKey to set
         */
        public void setColorKey(Integer colorKey) {
            this.colorKey = colorKey;
        }

    }

    @DynamicSerializeElement
    private boolean compile = false;

    @DynamicSerializeElement
    private List<ColormappedShadedShapeData> shapeData = new LinkedList<ColormappedShadedShapeData>();

    @DynamicSerializeElement
    private List<ColormappedShadedGeometryData> geometryData = new LinkedList<ColormappedShadedShapeDataEvent.ColormappedShadedGeometryData>();

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

    /**
     * @return the shapeData
     */
    public List<ColormappedShadedShapeData> getShapeData() {
        return shapeData;
    }

    /**
     * @param shapeData
     *            the shapeData to set
     */
    public void setShapeData(List<ColormappedShadedShapeData> shapeData) {
        this.shapeData = shapeData;
    }

    /**
     * @return the geometryData
     */
    public List<ColormappedShadedGeometryData> getGeometryData() {
        return geometryData;
    }

    /**
     * @param geometryData
     *            the geometryData to set
     */
    public void setGeometryData(List<ColormappedShadedGeometryData> geometryData) {
        this.geometryData = geometryData;
    }

    public void addShapeData(DataSpace dataSpace, LineString[] contours,
            Integer colorKey) {
        ColormappedShadedShapeData shapeDataItem = new ColormappedShadedShapeData();
        shapeDataItem.setDataSpace(dataSpace);
        shapeDataItem.setInfo(colorKey);
        shapeDataItem.setContour(contours);
        shapeData.add(shapeDataItem);
    }

    public void addGeometryData(Geometry data, Integer colorKey) {
        ColormappedShadedGeometryData shapeDataItem = new ColormappedShadedGeometryData();
        shapeDataItem.setColorKey(colorKey);
        shapeDataItem.setGeometry(data);
        geometryData.add(shapeDataItem);
    }
}
