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
package com.raytheon.uf.common.dataplugin.radar.level3.generic;

import java.io.DataInputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 19, 2009            askripsk     Initial creation
 * 03/04/2013   DCS51      zwang        Handle product GFM
 * </pre>
 * 
 * @author askripsk
 * @version 1.0
 */

@DynamicSerialize
public class AreaComponent extends GenericDataComponent {
    @DynamicSerialize
    public static enum AreaPointFormat {
        LAT_LON(0, "Lat/Lon"), X_Y(1, "X/Y"), AZ_RANGE(2, "Az/Range");

        @DynamicSerializeElement
        private int value;

        @DynamicSerializeElement
        private String name;

        private AreaPointFormat(int value, String name) {
            this.value = value;
            this.name = name;
        }

        public static AreaPointFormat valueOf(int value) {
            AreaPointFormat rval = null;

            for (AreaPointFormat curr : AreaPointFormat.values()) {
                if (curr.getValue() == value) {
                    rval = curr;
                    break;
                }
            }

            return rval;
        }

        /**
         * @return the name
         */
        public String getName() {
            return name;
        }

        /**
         * @param name
         *            the name to set
         */
        public void setName(String name) {
            this.name = name;
        }

        /**
         * @return the value
         */
        public int getValue() {
            return value;
        }

        /**
         * @param value
         *            the value to set
         */
        public void setValue(int value) {
            this.value = value;
        }

        @Override
        public String toString() {
            return name;
        }
    }

    @DynamicSerialize
    public static enum AreaPointType {
        POINT(1, "Point"), AREA(2, "Area"), POLYLINE(3, "PolyLine");

        @DynamicSerializeElement
        private int value;

        @DynamicSerializeElement
        private String name;

        private AreaPointType(int value, String name) {
            this.value = value;
            this.name = name;
        }

        public static AreaPointType valueOf(int value) {
            AreaPointType rval = null;

            for (AreaPointType curr : AreaPointType.values()) {
                if (curr.getValue() == value) {
                    rval = curr;
                    break;
                }
            }

            return rval;
        }

        /**
         * @return the name
         */
        public String getName() {
            return name;
        }

        /**
         * @param name
         *            the name to set
         */
        public void setName(String name) {
            this.name = name;
        }

        /**
         * @return the value
         */
        public int getValue() {
            return value;
        }

        /**
         * @param value
         *            the value to set
         */
        public void setValue(int value) {
            this.value = value;
        }

        @Override
        public String toString() {
            return name;
        }
    }

    @DynamicSerialize
    public static class AreaPoint implements ISerializableObject {
        @DynamicSerializeElement
        private float coordinate1;

        @DynamicSerializeElement
        private float coordinate2;

        public AreaPoint() {

        }

        public void parseData(DataInputStream in) throws IOException {
            setCoordinate1(in.readFloat());
            setCoordinate2(in.readFloat());
        }

        /**
         * @return the coordinate1
         */
        public float getCoordinate1() {
            return coordinate1;
        }

        /**
         * @param coordinate1
         *            the coordinate1 to set
         */
        public void setCoordinate1(float coordinate1) {
            this.coordinate1 = coordinate1;
        }

        /**
         * @return the coordinate2
         */
        public float getCoordinate2() {
            return coordinate2;
        }

        /**
         * @param coordinate2
         *            the coordinate2 to set
         */
        public void setCoordinate2(float coordinate2) {
            this.coordinate2 = coordinate2;
        }

        @Override
        public String toString() {
            return "Coord 1: " + coordinate1 + " Coord 2: " + coordinate2;
        }
    }

    @DynamicSerializeElement
    private List<AreaPoint> points = new ArrayList<AreaPoint>();

    @DynamicSerializeElement
    private AreaPointType type;

    @DynamicSerializeElement
    private AreaPointFormat format;

    @Override
    public void parseData(DataInputStream in) throws IOException {
    	this.setParameters(GenericUtil.parseParameters(in));

        // Set the format of the component (0x?000?)
        this.setFormat(AreaPointFormat.valueOf(in.readShort()));
        
        // Read in the byte that determines the type
        this.setType(AreaPointType.valueOf(in.readShort()));
        
        // Get the number of points in the component
        int pointCount = in.readInt();
        
        if (pointCount != 0) {
            // redundant
            pointCount = in.readInt();
        }
        
        AreaPoint currPoint;
        for (int i = 0; i < pointCount; i++) {
            currPoint = new AreaPoint();

            currPoint.parseData(in);

            points.add(currPoint);
        }        
    }

    /**
     * @return the points
     */
    public List<AreaPoint> getPoints() {
        return points;
    }

    /**
     * @param points
     *            the points to set
     */
    public void setPoints(List<AreaPoint> points) {
        this.points = points;
    }

    /**
     * @return the type
     */
    public AreaPointType getType() {
        return type;
    }

    /**
     * @param type
     *            the type to set
     */
    public void setType(AreaPointType type) {
        this.type = type;
    }

    /**
     * @return the format
     */
    public AreaPointFormat getFormat() {
        return format;
    }

    /**
     * @param format
     *            the format to set
     */
    public void setFormat(AreaPointFormat format) {
        this.format = format;
    }

    @Override
    public String toString() {
        StringBuffer rval = new StringBuffer(super.toString());
        rval.append("Area Component: \n\tType: " + type + "\n\tFormat: "
                + format);

        rval.append("\nPoints:");
        for (AreaPoint currPoint : points) {
            rval.append("\n" + currPoint);
        }

        return rval.toString();
    }
}
