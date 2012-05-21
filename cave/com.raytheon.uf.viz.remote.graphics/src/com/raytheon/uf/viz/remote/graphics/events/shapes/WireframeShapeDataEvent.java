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

import com.raytheon.uf.common.serialization.IDeserializationContext;
import com.raytheon.uf.common.serialization.ISerializationContext;
import com.raytheon.uf.common.serialization.ISerializationTypeAdapter;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeTypeAdapter;
import com.raytheon.uf.viz.remote.graphics.events.AbstractDispatchingObjectEvent;
import com.raytheon.uf.viz.remote.graphics.events.shapes.WireframeShapeDataEvent.WireframeShapeDataAdapter;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Wireframe shape data event which contains coordinates and labels to add to
 * the wireframe shape referenced by this event
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 26, 2012            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */
@DynamicSerialize
@DynamicSerializeTypeAdapter(factory = WireframeShapeDataAdapter.class)
public class WireframeShapeDataEvent extends AbstractDispatchingObjectEvent {

    public static class WireframeShapeDataAdapter implements
            ISerializationTypeAdapter<WireframeShapeDataEvent> {
        /*
         * (non-Javadoc)
         * 
         * @see
         * com.raytheon.uf.common.serialization.ISerializationTypeAdapter#serialize
         * (com.raytheon.uf.common.serialization.ISerializationContext,
         * java.lang.Object)
         */
        @Override
        public void serialize(ISerializationContext serializer,
                WireframeShapeDataEvent object) throws SerializationException {
            serializer.writeI32(object.getDisplayId());
            serializer.writeI32(object.getObjectId());
            serializer.writeI32(object.labels.size());
            for (Label l : object.labels) {
                serializer.writeString(l.getText());
                serializer.writeDoubleArray(l.getPoint());
            }
            serializer.writeI32(object.pixelCoordinates.size());
            for (double[][] coords : object.pixelCoordinates) {
                serializer.writeI32(coords.length);
                for (double[] coord : coords) {
                    serializer.writeDoubleArray(coord);
                }
            }
            serializer.writeI32(object.worldCoordiantes.size());
            for (Coordinate[] coordArray : object.worldCoordiantes) {
                double[] packedCoords = new double[coordArray.length * 3];
                int i = 0;
                for (Coordinate coord : coordArray) {
                    packedCoords[i++] = coord.x;
                    packedCoords[i++] = coord.y;
                    packedCoords[i++] = coord.z;
                }
                serializer.writeDoubleArray(packedCoords);
            }
        }

        /*
         * (non-Javadoc)
         * 
         * @see com.raytheon.uf.common.serialization.ISerializationTypeAdapter#
         * deserialize
         * (com.raytheon.uf.common.serialization.IDeserializationContext)
         */
        @Override
        public WireframeShapeDataEvent deserialize(
                IDeserializationContext deserializer)
                throws SerializationException {
            WireframeShapeDataEvent data = new WireframeShapeDataEvent();
            data.setDisplayId(deserializer.readI32());
            data.setObjectId(deserializer.readI32());
            int size = deserializer.readI32();
            for (int i = 0; i < size; ++i) {
                data.addLabel(deserializer.readString(),
                        deserializer.readDoubleArray());
            }

            size = deserializer.readI32();
            for (int i = 0; i < size; ++i) {
                int size2 = deserializer.readI32();
                double[][] coords = new double[size2][];
                for (int j = 0; j < size2; ++j) {
                    coords[j] = deserializer.readDoubleArray();
                }
                data.addPixelCoordinates(coords);
            }

            size = deserializer.readI32();
            for (int i = 0; i < size; ++i) {
                double[] packedCoords = deserializer.readDoubleArray();
                Coordinate[] worldCoords = new Coordinate[packedCoords.length / 3];
                for (int j = 0, k = 0; j < worldCoords.length; j++) {
                    worldCoords[j] = new Coordinate(packedCoords[k++],
                            packedCoords[k++], packedCoords[k++]);
                }
                data.addWorldCoordinates(worldCoords);
            }

            return data;
        }
    }

    @DynamicSerialize
    public static class Label {

        @DynamicSerializeElement
        private String text;

        @DynamicSerializeElement
        private double[] point;

        private Label(String text, double[] point) {
            this.text = text;
            this.point = point;
        }

        public Label() {

        }

        /**
         * @return the text
         */
        public String getText() {
            return text;
        }

        /**
         * @param text
         *            the text to set
         */
        public void setText(String text) {
            this.text = text;
        }

        /**
         * @return the point
         */
        public double[] getPoint() {
            return point;
        }

        /**
         * @param point
         *            the point to set
         */
        public void setPoint(double[] point) {
            this.point = point;
        }
    }

    @DynamicSerializeElement
    private List<double[][]> pixelCoordinates = new LinkedList<double[][]>();

    @DynamicSerializeElement
    private List<Coordinate[]> worldCoordiantes = new LinkedList<Coordinate[]>();

    @DynamicSerializeElement
    private List<Label> labels = new LinkedList<Label>();

    @DynamicSerializeElement
    private boolean compile = false;

    /**
     * @return the labels
     */
    public List<Label> getLabels() {
        return labels;
    }

    /**
     * @param labels
     *            the labels to set
     */
    public void setLabels(List<Label> labels) {
        this.labels = labels;
    }

    /**
     * @return the pixelCoordinates
     */
    public List<double[][]> getPixelCoordinates() {
        return pixelCoordinates;
    }

    /**
     * @param pixelCoordinates
     *            the pixelCoordinates to set
     */
    public void setPixelCoordinates(List<double[][]> pixelCoordinates) {
        this.pixelCoordinates = pixelCoordinates;
    }

    /**
     * @return the worldCoordiantes
     */
    public List<Coordinate[]> getWorldCoordiantes() {
        return worldCoordiantes;
    }

    /**
     * @param worldCoordiantes
     *            the worldCoordiantes to set
     */
    public void setWorldCoordiantes(List<Coordinate[]> worldCoordiantes) {
        this.worldCoordiantes = worldCoordiantes;
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

    public void addLabel(String text, double[] point) {
        labels.add(new Label(text, point));
    }

    public void addPixelCoordinates(double[][] points) {
        pixelCoordinates.add(points);
    }

    public void addWorldCoordinates(Coordinate[] points) {
        worldCoordiantes.add(points);
    }
}
