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
 * 1-26-2009    465         mnash    Initial Creation
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */
@DynamicSerialize
public class SuperObWindDataPacket extends SymbologyPacket implements
        ISerializableObject {

    /**
     * @param packetId
     * @param in
     * @throws IOException
     */
    public SuperObWindDataPacket(int packetId, DataInputStream in)
            throws IOException {
        super(packetId, in);
    }

    public SuperObWindDataPacket() {

    }

    private static final int SUPER_OB_WIND_DATA_PACKET27 = 27;

    static {
        PacketFactory.registerPacketType(SuperObWindDataPacket.class,
                SUPER_OB_WIND_DATA_PACKET27);
    }

    @DynamicSerialize
    public static class SuperObWindDataCell implements ISerializableObject {
        @DynamicSerializeElement
        public int latitudeLSW;

        @DynamicSerializeElement
        public int longitudeMSW;

        @DynamicSerializeElement
        public int longitudeLSW;

        @DynamicSerializeElement
        public int radius;

        @DynamicSerializeElement
        public int height;

        @DynamicSerializeElement
        public int avgRadialVel;

        @DynamicSerializeElement
        public int stdAvgRadialVel;

        @DynamicSerializeElement
        public int timeDeviation;

        @DynamicSerializeElement
        public int avgAzimuth;

        @DynamicSerializeElement
        public int latitudeMSW;

        @DynamicSerializeElement
        public int elevation;

        public int getLatitudeMSW() {
            return latitudeMSW;
        }

        public void setLatitudeMSW(int latitudeMSW) {
            this.latitudeMSW = latitudeMSW;
        }

        public int getLatitudeLSW() {
            return latitudeLSW;
        }

        public void setLatitudeLSW(int latitudeLSW) {
            this.latitudeLSW = latitudeLSW;
        }

        public int getLongitudeMSW() {
            return longitudeMSW;
        }

        public void setLongitudeMSW(int longitudeMSW) {
            this.longitudeMSW = longitudeMSW;
        }

        public int getLongitudeLSW() {
            return longitudeLSW;
        }

        public void setLongitudeLSW(int longitudeLSW) {
            this.longitudeLSW = longitudeLSW;
        }

        public int getRadius() {
            return radius;
        }

        public void setRadius(int radius) {
            this.radius = radius;
        }

        public int getHeight() {
            return height;
        }

        public void setHeight(int height) {
            this.height = height;
        }

        public int getAvgRadialVel() {
            return avgRadialVel;
        }

        public void setAvgRadialVel(int avgRadialVel) {
            this.avgRadialVel = avgRadialVel;
        }

        public int getStdAvgRadialVel() {
            return stdAvgRadialVel;
        }

        public void setStdAvgRadialVel(int stdAvgRadialVel) {
            this.stdAvgRadialVel = stdAvgRadialVel;
        }

        public int getTimeDeviation() {
            return timeDeviation;
        }

        public void setTimeDeviation(int timeDeviation) {
            this.timeDeviation = timeDeviation;
        }

        public int getAvgAzimuth() {
            return avgAzimuth;
        }

        public void setAvgAzimuth(int avgAzimuth) {
            this.avgAzimuth = avgAzimuth;
        }

        public void setElevation(int elevation) {
            this.elevation = elevation;
        }

        public int getElevation() {
            return elevation;
        }

        @Override
        public String toString() {
            return String.format("(%d,%d)", latitudeMSW, latitudeLSW);
        }
    }

    @DynamicSerializeElement
    protected List<SuperObWindDataCell> points;

    /**
     * @return the points
     */
    public List<SuperObWindDataCell> getPoints() {
        return points;
    }

    /**
     * @param points
     *            the points to set
     */
    public void setPoints(List<SuperObWindDataCell> points) {
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
        int blockLen = in.readInt();
        blockLen -= 4;
        int elevationAngle = in.readShort();
        blockLen -= 2;
        points = new ArrayList<SuperObWindDataCell>();
        for (int i = 0; i < blockLen; i += 26) {
            SuperObWindDataCell pnt = new SuperObWindDataCell();
            pnt.setElevation(elevationAngle);
            pnt.setLatitudeMSW(in.readInt());
            pnt.setLatitudeLSW(in.readInt());
            pnt.setLongitudeMSW(in.readInt());
            pnt.setLongitudeLSW(in.readInt());
            pnt.setHeight(in.readShort());
            pnt.setAvgRadialVel(in.readShort());
            pnt.setStdAvgRadialVel(in.readShort());
            pnt.setTimeDeviation(in.readShort());
            pnt.setAvgAzimuth(in.readShort());
            points.add(pnt);
        }
    }

    @Override
    public String toString() {
        String s = super.toString() + " SuperObWindDataCell: ";
        for (SuperObWindDataCell pnt : points) {
            s += "\n\t" + pnt;
        }

        return s;
    }
}
