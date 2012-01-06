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
public class HdaHailPacket extends SymbologyPacket implements
        ISerializableObject {

    public HdaHailPacket(int packetId, DataInputStream in) throws IOException {
        super(packetId, in);
    }

    public HdaHailPacket() {

    }

    private static final int HDA_HAIL_PACKET19 = 19;

    static {
        PacketFactory
                .registerPacketType(HdaHailPacket.class, HDA_HAIL_PACKET19);
    }

    @DynamicSerialize
    public static class HdaHailPoint implements SymbologyPoint,
            ISerializableObject {

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

        @DynamicSerializeElement
        public int i;

        @DynamicSerializeElement
        public int j;

        public double getProbHail() {
            return probHail;
        }

        public void setProbHail(double probHail) {
            this.probHail = probHail;
        }

        public double getProbSevereHail() {
            return probSevereHail;
        }

        public void setProbSevereHail(double probSevereHail) {
            this.probSevereHail = probSevereHail;
        }

        public double getMaxHailSize() {
            return maxHailSize;
        }

        public void setMaxHailSize(double maxHailSize) {
            this.maxHailSize = maxHailSize;
        }

        @DynamicSerializeElement
        protected double probHail;

        @DynamicSerializeElement
        protected double probSevereHail;

        @DynamicSerializeElement
        protected double maxHailSize;

        @Override
        public String toString() {
            return String.format("(%d,%d)", i, j);
        }
    }

    @DynamicSerializeElement
    protected List<HdaHailPoint> points;

    /**
     * @return the points
     */
    public List<HdaHailPoint> getPoints() {
        return points;
    }

    /**
     * @param points
     *            the points to set
     */
    public void setPoints(List<HdaHailPoint> points) {
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

        points = new ArrayList<HdaHailPoint>();
        for (int i = 0; i < blockLen; i += 10) {
            HdaHailPoint pnt = new HdaHailPoint();
            pnt.setI(in.readShort());
            pnt.setJ(in.readShort());

            pnt.setProbHail(in.readShort());
            pnt.setProbSevereHail(in.readShort());
            pnt.setMaxHailSize(in.readShort());
            points.add(pnt);
        }
    }

    @Override
    public String toString() {
        String s = super.toString() + " HdaHailPacket";
        for (HdaHailPoint point : points) {
            s += "\n\t" + point;
        }
        return s;
    }
}
