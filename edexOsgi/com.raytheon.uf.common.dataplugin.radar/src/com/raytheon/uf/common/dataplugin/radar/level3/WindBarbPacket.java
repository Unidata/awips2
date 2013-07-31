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
 * Decodes the (non-DMD) mesocyclone packet
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 1-23-2009               mnash     Initial creation
 * 07/29/2013   2148       mnash       Refactor registering of packets to Spring
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */

@DynamicSerialize
public class WindBarbPacket extends SymbologyPacket implements
        ISerializableObject {

    public WindBarbPacket(int packetId, DataInputStream in) throws IOException {
        super(packetId, in);
    }

    public WindBarbPacket() {

    }

    @DynamicSerialize
    public static class WindBarbPoint implements SymbologyPoint,
            ISerializableObject {

        @DynamicSerializeElement
        protected int colorValue;

        @DynamicSerializeElement
        protected int windBarbDir;

        @DynamicSerializeElement
        protected int windBarbSpd;

        @DynamicSerializeElement
        public int i;

        @DynamicSerializeElement
        public int j;

        public int getColorValue() {
            return colorValue;
        }

        public void setColorValue(int colorValue) {
            this.colorValue = colorValue;
        }

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
         * 
         * @return
         */
        public int getWindBarbDir() {
            return windBarbDir;
        }

        /**
         * 
         * @param windBarbDir
         */
        public void setWindBarbDir(int windBarbDir) {
            this.windBarbDir = windBarbDir;
        }

        /**
         * 
         * @return
         */
        public int getWindBarbSpd() {
            return windBarbSpd;
        }

        /**
         * 
         * @param windBarbSpd
         */
        public void setWindBarbSpd(int windBarbSpd) {
            this.windBarbSpd = windBarbSpd;
        }

        @Override
        public String toString() {
            return String.format("(%d,%d)  Direction: %d ; Speed: %d ", i, j,
                    windBarbDir, windBarbSpd);
        }

    }

    @DynamicSerializeElement
    protected WindBarbPoint[] points;

    /**
     * @return the points
     */
    public WindBarbPoint[] getPoints() {
        return points;
    }

    /**
     * @param points
     *            the points to set
     */
    public void setPoints(WindBarbPoint[] points) {
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

        // skipping extra because packet contains information from first packet
        // again, block length and packet code
        List<WindBarbPoint> points = new ArrayList<WindBarbPoint>();
        for (int i = 0; i < blockLen; i += 14) {
            WindBarbPoint pnt = new WindBarbPoint();

            pnt.setColorValue(in.readShort());
            pnt.setI(in.readShort());
            pnt.setJ(in.readShort());

            pnt.setWindBarbDir(in.readShort());
            pnt.setWindBarbSpd(in.readShort());
            points.add(pnt);
        }

        this.points = points.toArray(new WindBarbPoint[points.size()]);
    }

    @Override
    public String toString() {
        String s = super.toString() + " WindBarbPoint";
        for (WindBarbPoint point : points) {
            s += "\n\t" + point;
        }
        return s;
    }
}
