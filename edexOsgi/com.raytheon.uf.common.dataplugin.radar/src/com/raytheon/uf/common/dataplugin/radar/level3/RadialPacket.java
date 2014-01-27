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

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;

/**
 * RadialPacket is a class that will allow access to the actual radar data
 * contained in a radial product.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 10/25/2007   #465       randerso    initial creation
 * 07/29/2013   2148       mnash       Refactor registering of packets to Spring
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */
public class RadialPacket extends SymbologyPacket {
    private final static IUFStatusHandler handler = UFStatus
            .getHandler(RadialPacket.class);

    protected int firstBinIndex;

    protected int numBins;

    protected int theICenter;

    protected int theJCenter;

    protected double scaleFactor;

    protected int numRadials;

    protected byte[] radialData;

    protected float[] angleData;

    /**
     * Construct takes a byte array containing a radial symbology layer.
     * 
     */
    public RadialPacket(int packetId, DataInputStream in) throws IOException {
        super(packetId, in);
    }

    /**
     * Returns the number of range bins in each radial
     * 
     * @return An int which will be from 1 to 460
     */
    public int getNumBins() {
        return numBins;
    }

    /**
     * Returns the I coordinate of the center of sweep
     * 
     * @return An int which will be from -2048 to 2048
     */
    public int getICenter() {
        return theICenter;
    }

    /**
     * Returns the J coordinate of the center of sweep
     * 
     * @return An int which will be from -2048 to 2048
     */
    public int getJCenter() {
        return theJCenter;
    }

    /**
     * Returns the total number of radials in the product.
     * 
     * @return An int which will be from 1 to 400
     */
    public int getNumRadials() {
        return numRadials;
    }

    /**
     * Returns the number of pixels per range bin.
     * 
     * @return A double from .001 to 8.000
     */
    public double getScaleFactor() {
        return scaleFactor;
    }

    /**
     * Parses the radial header
     */
    @Override
    protected void init(DataInputStream in) throws IOException {
        firstBinIndex = in.readUnsignedShort();
        numBins = in.readUnsignedShort();
        theICenter = in.readUnsignedShort();
        theJCenter = in.readUnsignedShort();
        scaleFactor = in.readUnsignedShort() * 0.001;
        numRadials = in.readUnsignedShort();
        radialData = new byte[numRadials * numBins];
        angleData = new float[numRadials];

        readRadialData(in);
    }

    /**
     * @param in
     * @throws IOException
     */
    protected void readRadialData(DataInputStream in) throws IOException {
        for (int radial = 0; radial < numRadials; radial++) {
            int remainingBytes = in.readUnsignedShort() * 2;
            angleData[radial] = (in.readUnsignedShort() * 0.1f) % 360.0f;
            in.skip(2); // radial angle delta not used

            int bin = 0;
            for (int b = 0; b < remainingBytes; b++) {
                byte dataByte = in.readByte();
                int runLength = 0x0F & (dataByte >> 4);
                byte value = (byte) (0x0F & dataByte);
                for (int i = 0; i < runLength; ++i) {
                    setRadialDataValue(radial, bin++, value);
                }
            }
        }
    }

    /**
     * @param radial
     * @param bin
     * @param value
     */
    protected void setRadialDataValue(int radial, int bin, byte value) {
        if ((radial < numRadials) && (bin < numBins)) {
            radialData[radial * numBins + bin] = value;
        } else {
            handler.handle(Priority.WARN, "Index out of range. Radial: "
                    + radial + "  Bin: " + bin
                    + ".  Excess data will be discarded.");
        }
    }

    @Override
    public String toString() {
        String s = super.toString() + " Radial Data";
        s += "\n\t\tNum Radials: " + numRadials;
        s += "\n\t\tNum Bins: " + numBins;

        return s;
    }

    public byte[] getRadialData() {
        return radialData;
    }

    public float[] getAngleData() {
        return angleData;
    }

    /**
     * @return the firstBinIndex
     */
    public int getFirstBinIndex() {
        return firstBinIndex;
    }

}
