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

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * A {@link SymbologyPacket} containing multiple {@link STICirclePoint}s.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Jan 26, 2009           mnash    Initial creation
 * Jul 29, 2013  2148     mnash       Refactor registering of packets to Spring
 * Jun 04, 2014  3232     bsteffen    Remove ISerializableObject
 * 
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */
@DynamicSerialize
public class STICirclePacket extends SymbologyPacket {

    /**
     * @param packetId
     * @param in
     * @throws IOException
     */
    public STICirclePacket(int packetId, DataInputStream in) throws IOException {
        super(packetId, in);
    }

    public STICirclePacket() {

    }

    @DynamicSerialize
    public static class STICirclePoint {
        @DynamicSerializeElement
        public int i;

        @DynamicSerializeElement
        public int j;

        @DynamicSerializeElement
        public int radius;

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
         * @return the radius
         */
        public int getRadius() {
            return radius;
        }

        /**
         * @param theColor
         *            the theColor to set
         */
        public void setRadius(int radius) {
            this.radius = radius;
        }

        @Override
        public String toString() {
            return String.format("(%d,%d) Radius : %d", i, j, radius);
        }
    }

    @DynamicSerializeElement
    protected List<STICirclePoint> points;

    /**
     * @return the points
     */
    public List<STICirclePoint> getPoints() {
        return points;
    }

    /**
     * @param points
     *            the points to set
     */
    public void setPoints(List<STICirclePoint> points) {
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
        points = new ArrayList<STICirclePoint>();
        for (int i = 0; i < blockLen; i += 6) {
            STICirclePoint pnt = new STICirclePoint();
            pnt.i = in.readShort();
            pnt.j = in.readShort();
            pnt.radius = in.readShort();
            points.add(pnt);
        }
    }

    @Override
    public String toString() {
        String s = super.toString() + " SCITCirclePoint: ";
        for (STICirclePoint pnt : points) {
            s += "\n\t" + pnt;
        }

        return s;
    }
}
