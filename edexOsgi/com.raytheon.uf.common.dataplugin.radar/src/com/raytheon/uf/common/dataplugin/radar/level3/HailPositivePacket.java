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
 * decode the hail points
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Feb 19, 2009           mnash       Initial creation
 * Jul 29, 2013  2148     mnash       Refactor registering of packets to Spring
 * Jun 04, 2014  3232     bsteffen    Remove ISerializableObject
 * 
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */

@DynamicSerialize
public class HailPositivePacket extends SymbologyPacket {

    public HailPositivePacket(int packetId, DataInputStream in)
            throws IOException {
        super(packetId, in);
    }

    public HailPositivePacket() {

    }

    @DynamicSerialize
    public static class HailPoint {

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

        @Override
        public String toString() {
            return String.format("Hail(%d,%d)", i, j);
        }
    }

    @DynamicSerializeElement
    protected HailPoint[] points;

    /**
     * @return the points
     */
    public HailPoint[] getPoints() {
        return points;
    }

    /**
     * @param points
     *            the points to set
     */
    public void setPoints(HailPoint[] points) {
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

        List<HailPoint> points = new ArrayList<HailPoint>();
        for (int i = 0; i < blockLen; i += 4) {
            HailPoint pnt = new HailPoint();
            pnt.setI(in.readShort());
            pnt.setJ(in.readShort());

            points.add(pnt);
        }
        this.points = points.toArray(new HailPoint[points.size()]);
    }

    @Override
    public String toString() {
        String s = super.toString() + " HailPacket";
        for (HailPoint point : points) {
            s += "\n\t" + point;
        }
        return s;
    }
}
