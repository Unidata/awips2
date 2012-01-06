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
package com.raytheon.uf.common.dataplugin.radar.level3;

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
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 5, 2008            mnash     Initial creation
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */
@DynamicSerialize
public class SpecialGraphicSymbolPacket extends SymbologyPacket implements
        ISerializableObject {

    /**
     * @param packetId
     * @param in
     * @throws IOException
     */
    public SpecialGraphicSymbolPacket(int packetId, DataInputStream in)
            throws IOException {
        super(packetId, in);
    }

    public SpecialGraphicSymbolPacket() {

    }

    private static final int SPECIAL_GRAPHIC_SYMBOL20 = 20;

    static {
        PacketFactory.registerPacketType(SpecialGraphicSymbolPacket.class,
                SPECIAL_GRAPHIC_SYMBOL20);
    }

    @DynamicSerialize
    public static class SpecialGraphicPoint implements SymbologyPoint,
            ISerializableObject {
        @DynamicSerializeElement
        public int i;

        @DynamicSerializeElement
        public int j;

        @DynamicSerializeElement
        public int pointFeatureType;

        @DynamicSerializeElement
        public int pointFeatureAttr;

        /**
         * @return the i
         */
        public int getI() {
            return i;
        }

        /**
         * @param i
         *            the i to set
         */
        public void setI(int i) {
            this.i = i;
        }

        /**
         * @return the j
         */
        public int getJ() {
            return j;
        }

        /**
         * @param j
         *            the j to set
         */
        public void setJ(int j) {
            this.j = j;
        }

        /**
         * 
         * @return
         */
        public int getPointFeatureType() {
            return pointFeatureType;
        }

        /**
         * 
         * @param pointFeatureType
         */
        public void setPointFeatureType(int pointFeatureType) {
            this.pointFeatureType = pointFeatureType;
        }

        /**
         * 
         * @return
         */
        public int getPointFeatureAttr() {
            return pointFeatureAttr;
        }

        /**
         * 
         * @param pointFeatureAttr
         */
        public void setPointFeatureAttr(int pointFeatureAttr) {
            this.pointFeatureAttr = pointFeatureAttr;
        }

        @Override
        public String toString() {
            return String.format("(%d,%d) : %d:%d", i, j, pointFeatureType,
                    pointFeatureAttr);
        }
    }

    @DynamicSerializeElement
    protected int pointFeatureType;

    @DynamicSerializeElement
    protected List<SpecialGraphicPoint> points;

    /**
     * @return the theColor
     */
    public int getPointFeatureType() {
        return pointFeatureType;
    }

    /**
     * @param theColor
     *            the theColor to set
     */
    public void setPointFeatureType(int pointFeatureType) {
        this.pointFeatureType = pointFeatureType;
    }

    /**
     * @return the vectors
     */
    public List<SpecialGraphicPoint> getPoints() {
        return points;
    }

    /**
     * 
     * @param points
     */
    public void setPoints(List<SpecialGraphicPoint> points) {
        this.points = points;
    }

    /**
     * @param vectors
     *            the vectors to set
     */
    public void setVectors(List<SpecialGraphicPoint> points) {
        this.points = points;
    }

    /*
     * (non-Javadoc)
     * 
     * @seecom.raytheon.uf.common.dataplugin.radar.level3.SymbologyPacket#init(java
     * .io. DataInputStream)
     */
    @Override
    protected void init(DataInputStream in) throws IOException {
        int blockLen = in.readUnsignedShort();

        points = new ArrayList<SpecialGraphicPoint>();
        for (int i = 0; i < blockLen; i += 8) {
            SpecialGraphicPoint pnt = new SpecialGraphicPoint();
            pnt.i = in.readShort();
            pnt.j = in.readShort();
            pnt.pointFeatureType = in.readShort();
            pnt.pointFeatureAttr = in.readShort();
            points.add(pnt);
        }
    }

    @Override
    public String toString() {
        String s = super.toString() + " SpecialGraphicPoint";

        for (SpecialGraphicPoint pnt : points) {
            s += "\n\t" + pnt;
        }
        return s;
    }
}
