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
 * Decodes the vector arrow packet as described by packet code 5
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 19, 2009            mnash     Initial creation
 * 
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */

@DynamicSerialize
public class VectorArrowPacket extends SymbologyPacket implements
        ISerializableObject {

    public VectorArrowPacket(int packetId, DataInputStream in)
            throws IOException {
        super(packetId, in);
    }

    public VectorArrowPacket() {

    }

    private static final int VECTOR_ARROW_PACKET5 = 5;

    static {
        PacketFactory.registerPacketType(VectorArrowPacket.class,
                VECTOR_ARROW_PACKET5);
    }

    @DynamicSerialize
    public static class VectorArrow implements ISerializableObject {

        @DynamicSerializeElement
        protected int arrowDirection;

        @DynamicSerializeElement
        protected int arrowLength;

        @DynamicSerializeElement
        protected int arrowHeadLength;

        @DynamicSerializeElement
        public int i;

        @DynamicSerializeElement
        public int j;

        public int getI() {
            return i;
        }

        public void setI(int i) {
            this.i = i;
        }

        public int getJ() {
            return j;
        }

        public void setJ(int j) {
            this.j = j;
        }

        /**
         * @return the arrowDirection
         */
        public int getArrowDirection() {
            return arrowDirection;
        }

        /**
         * @param arrowDirection
         *            the arrowDirection to set
         */
        public void setArrowDirection(int arrowDirection) {
            this.arrowDirection = arrowDirection;
        }

        /**
         * @return the arrowLength
         */
        public int getArrowLength() {
            return arrowLength;
        }

        /**
         * @param arrowLength
         *            the arrowLength to set
         */
        public void setArrowLength(int arrowLength) {
            this.arrowLength = arrowLength;
        }

        /**
         * @return the arrowHeadLength
         */
        public int getArrowHeadLength() {
            return arrowHeadLength;
        }

        /**
         * @param arrowHeadLength
         *            the arrowHeadLength to set
         */
        public void setArrowHeadLength(int arrowHeadLength) {
            this.arrowHeadLength = arrowHeadLength;
        }

        @Override
        public String toString() {
            return String.format(
                    "(%d,%d  Direction: %d  Length: %d  Head Length: %d)", i,
                    j, arrowDirection, arrowLength, arrowHeadLength);
        }
    }

    @DynamicSerializeElement
    protected VectorArrow[] arrows;

    /**
     * @return the points
     */
    public VectorArrow[] getArrows() {
        return arrows;
    }

    /**
     * @param points
     *            the points to set
     */
    public void setArrows(VectorArrow[] arrows) {
        this.arrows = arrows;
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

        List<VectorArrow> arrows = new ArrayList<VectorArrow>();
        for (int i = 0; i < blockLen; i += 10) {
            VectorArrow arr = new VectorArrow();
            arr.setI(in.readShort());
            arr.setJ(in.readShort());
            arr.setArrowDirection(in.readShort());
            arr.setArrowLength(in.readShort());
            arr.setArrowHeadLength(in.readShort());
            arrows.add(arr);
        }

        this.arrows = arrows.toArray(new VectorArrow[arrows.size()]);
    }

    @Override
    public String toString() {
        String s = super.toString() + " VectorArrowPacket";
        for (VectorArrow arrow : arrows) {
            s += "\n\t" + arrow;
        }
        return s;
    }
}
