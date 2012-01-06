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

/**
 * RadialPacket is a class that will allow access to the actual radar data
 * contained in a RPG created radial product.
 * 
 * 
 * @author Bryan Rockwood
 * @version 1.0
 */
public class RadialPacket8bit extends RadialPacket {
    public static final int RADIAL_DATA_PACKET_8BIT = 16;
    static {
        PacketFactory.registerPacketType(RadialPacket8bit.class,
                RADIAL_DATA_PACKET_8BIT);
    }

    /**
     * An extension to the RadialPacket class that supports RPG generated
     * reflectivity and velocity data.
     * 
     * @param symbologyLayer
     *            The symbology layer to process
     * @throws IOException
     */
    public RadialPacket8bit(int packetId, DataInputStream in)
            throws IOException {
        super(packetId, in);
    }

    @Override
    protected void readRadialData(DataInputStream in) throws IOException {
        for (int radial = 0; radial < numRadials; radial++) {
            int remainingBytes = in.readUnsignedShort();
            angleData[radial] = (in.readUnsignedShort() * 0.1f) % 360.0f;
            in.skip(2); // radial angle delta not used

            for (int bin = 0; bin < numBins; bin++) {
                byte value = in.readByte();
                setRadialDataValue(radial, bin, value);
            }

            // allow for odd number of bins
            if (remainingBytes > numBins) {
                in.skip(1);
            }
        }
    }

}
