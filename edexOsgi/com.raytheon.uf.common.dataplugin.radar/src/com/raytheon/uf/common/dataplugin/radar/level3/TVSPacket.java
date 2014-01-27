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
 * Decodes the TVS packet.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 13, 2009            chammack     Initial creation
 * 07/29/2013   2148       mnash       Refactor registering of packets to Spring
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */

@DynamicSerialize
public class TVSPacket extends SymbologyPacket implements ISerializableObject {

    public TVSPacket(int packetId, DataInputStream in) throws IOException {
        super(packetId, in);
    }

    public TVSPacket() {

    }

    protected final boolean isElevated = false;

    @DynamicSerialize
    public static class TVSPoint implements SymbologyPoint, ISerializableObject {

        @DynamicSerializeElement
        public boolean elevated = false;

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
         * @return the elevated
         */
        public boolean isElevated() {
            return elevated;
        }

        /**
         * @param elevated
         *            the elevated to set
         */
        public void setElevated(boolean elevated) {
            this.elevated = elevated;
        }

        @Override
        public String toString() {
            return String.format("TVS(%d,%d)", i, j);
        }
    }

    @DynamicSerializeElement
    protected TVSPoint[] points;

    /**
     * @return the points
     */
    public TVSPoint[] getPoints() {
        return points;
    }

    /**
     * @param points
     *            the points to set
     */
    public void setPoints(TVSPoint[] points) {
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

        List<TVSPoint> points = new ArrayList<TVSPoint>();
        for (int i = 0; i < blockLen; i += 4) {
            TVSPoint pnt = new TVSPoint();
            pnt.setI(in.readShort());
            pnt.setJ(in.readShort());

            pnt.setElevated(this.isElevated);

            points.add(pnt);
        }
        this.points = points.toArray(new TVSPoint[points.size()]);
    }

    @Override
    public String toString() {
        String s = super.toString() + " TVSPacket";
        for (TVSPoint point : points) {
            s += "\n\t" + point;
        }
        return s;
    }
}
