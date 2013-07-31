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
 * 07/29/2013   2148       mnash       Refactor registering of packets to Spring
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */

@DynamicSerialize
public class StormIDPacket extends SymbologyPacket implements
        ISerializableObject {

    public StormIDPacket(int packetId, DataInputStream in) throws IOException {
        super(packetId, in);
    }

    public StormIDPacket() {

    }

    @DynamicSerialize
    public static class StormIDPoint implements SymbologyPoint,
            ISerializableObject {

        @DynamicSerializeElement
        public int i;

        @DynamicSerializeElement
        public int j;

        @DynamicSerializeElement
        protected String stormID;

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
         * @return the stormID
         */
        public String getStormID() {
            return stormID;
        }

        /**
         * @param stormID
         *            the stormID to set
         */
        public void setStormID(String stormID) {
            this.stormID = stormID;
        }

        @Override
        public String toString() {
            return String.format("(%d,%d %s)", getI(), getJ(), getStormID());
        }
    }

    @DynamicSerializeElement
    protected StormIDPoint[] points;

    /**
     * @return the points
     */
    public StormIDPoint[] getPoints() {
        return points;
    }

    /**
     * @param points
     *            the points to set
     */
    public void setPoints(StormIDPoint[] points) {
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

        List<StormIDPoint> points = new ArrayList<StormIDPoint>();
        for (int i = 0; i < blockLen; i += 6) {
            StormIDPoint pnt = new StormIDPoint();
            pnt.setI(in.readShort());
            pnt.setJ(in.readShort());

            char c1 = (char) in.readByte();
            char c2 = (char) in.readByte();

            pnt.setStormID("" + c1 + c2);
            points.add(pnt);
        }
        this.points = points.toArray(new StormIDPoint[points.size()]);
    }

    @Override
    public String toString() {
        String s = super.toString() + " StormIDPacket";
        for (StormIDPoint point : points) {
            s += "\n\t" + point;
        }
        return s;
    }
}
