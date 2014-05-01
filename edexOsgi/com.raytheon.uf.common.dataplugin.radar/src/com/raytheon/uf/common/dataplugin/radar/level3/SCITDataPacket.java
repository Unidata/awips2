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

import com.raytheon.uf.common.dataplugin.radar.level3.STICirclePacket.STICirclePoint;
import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Decodes the SCITData
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 28, 2009            mnash     Initial creation
 * 07/29/2013   2148       mnash       Refactor registering of packets to Spring
 * 
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */

@DynamicSerialize
public class SCITDataPacket extends SymbologyPacket implements
        ISerializableObject {

    public SCITDataPacket(int packetId, DataInputStream in) throws IOException {
        super(packetId, in);
    }

    public SCITDataPacket() {

    }

    /**
     * Class holding the SCIT information - LinkedVectors,Strings,and
     * STIDataPoints
     */
    @DynamicSerialize
    public static class SCITDataCell implements ISerializableObject {
        @DynamicSerializeElement
        public List<LinkedVector> vectors = new ArrayList<LinkedVector>();

        @DynamicSerializeElement
        public String text = new String();

        @DynamicSerializeElement
        public int i;

        @DynamicSerializeElement
        public int j;

        @DynamicSerializeElement
        public List<STICirclePoint> points = new ArrayList<STICirclePoint>();

        public List<LinkedVector> getVectors() {
            return vectors;
        }

        public void setVectors(List<LinkedVector> vectors) {
            this.vectors = vectors;
        }

        /**
         * 
         * @return
         */
        public String getText() {
            return text;
        }

        /**
         * 
         * @param text
         */
        public void setText(String text) {
            this.text = text;
        }

        /**
         * 
         * @return
         */
        public List<STICirclePoint> getPoints() {
            return points;
        }

        /**
         * 
         * @param points
         */
        public void setPoints(List<STICirclePoint> points) {
            this.points = points;
        }

        /**
         * 
         * @return
         */
        public int getI() {
            return i;
        }

        /**
         * 
         * @param i
         */
        public void setI(int i) {
            this.i = i;
        }

        /**
         * 
         * @return
         */
        public int getJ() {
            return j;
        }

        /**
         * 
         * @param j
         */
        public void setJ(int j) {
            this.j = j;
        }

        @Override
        public String toString() {
            return String
                    .format("(%d,%d) LinkedVectors : %s \n\t STICirclePoints : %s \n\t Text : %s \n",
                            i, j, vectors.toString(), points.toString(), text);
        }
    }

    @DynamicSerializeElement
    List<SCITDataCell> points;

    /**
     * 
     * @return
     */
    public List<SCITDataCell> getPoints() {
        return points;
    }

    /**
     * 
     * @param cells
     */
    public void setPoints(List<SCITDataCell> points) {
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
        int increment = 0;
        points = new ArrayList<SCITDataCell>();
        for (int i = 0; i < blockLen; i += increment) {
            SCITDataCell pnt = new SCITDataCell();
            int packetType = in.readShort();

            in.mark(2);
            increment = in.readShort();
            in.reset();

            if (packetType == 2) {
                TextSymbolPacket packet = new TextSymbolPacket(packetType, in);
                pnt.text = packet.getTheText();
                pnt.i = packet.getI();
                pnt.j = packet.getJ();
            } else if (packetType == 6) {
                LinkedVectorPacket packet = new LinkedVectorPacket(packetType,
                        in);
                pnt.vectors = packet.getVectors();
            } else if (packetType == 25) {
                STICirclePacket packet = new STICirclePacket(packetType, in);
                pnt.points = packet.getPoints();
            }
            points.add(pnt);
            // Need to count the ID and BlockLength bytes
            increment += 4;
        }
    }

    @Override
    public String toString() {
        String s = super.toString() + " SCITDataPacket";
        for (SCITDataCell cell : points) {
            s += "\n\t" + cell;
        }
        return s;
    }
}
