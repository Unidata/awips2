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
 * Contour Vector Packet to decode packets 0E03,0802,3501
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Mar 09, 2009           mnash       Initial creation
 * Jul 29, 2013  2148     mnash       Refactor registering of packets to Spring
 * Jun 04, 2014  3232     bsteffen    Remove ISerializableObject
 * 
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */

@DynamicSerialize
public class LinkedContourVectorPacket extends SymbologyPacket {

    private static final int CONTOUR_VECTOR_PACKET0E03 = 0x0E03;

    private static final int CONTOUR_VECTOR_PACKET0802 = 0x0802;

    private static int color = -1;

    /**
     * @param packetId
     * @param in
     * @throws IOException
     */
    public LinkedContourVectorPacket(int packetId, DataInputStream in)
            throws IOException {
        super(packetId, in);
    }

    public LinkedContourVectorPacket() {

    }

    @DynamicSerializeElement
    protected List<LinkedVector> vectors;

    /**
     * @return the vectors
     */
    public List<LinkedVector> getVectors() {
        return vectors;
    }

    /**
     * @param vectors
     *            the vectors to set
     */
    public void setVectors(List<LinkedVector> vectors) {
        this.vectors = vectors;
    }

    /*
     * (non-Javadoc)
     * 
     * @seecom.raytheon.uf.common.dataplugin.radar.level3.SymbologyPacket#init(java
     * .io. DataInputStream)
     */
    @Override
    protected void init(DataInputStream in) throws IOException {
        int blockLen = 0;

        vectors = new ArrayList<LinkedVector>();
        LinkedVector vec;
        int prevI = 0;
        int prevJ = 0;

        if (packetId == CONTOUR_VECTOR_PACKET0E03) {
            if (in.readUnsignedShort() == 0x8000) {
                prevI = in.readShort();
                prevJ = in.readShort();
                blockLen = in.readUnsignedShort();
                for (int i = 0; i < blockLen; i += 4) {
                    vec = new LinkedVector();
                    if (i == 0) {
                        vec.i1 = in.readShort();
                        vec.j1 = in.readShort();
                        i += 4;
                    } else {
                        vec.i1 = prevI;
                        vec.j1 = prevJ;
                    }
                    vec.i2 = in.readShort();
                    vec.j2 = in.readShort();

                    prevI = vec.i2;
                    prevJ = vec.j2;
                    vec.theColor = color;
                    vectors.add(vec);
                }
            }
        } else if (packetId == CONTOUR_VECTOR_PACKET0802) {
            if (in.readUnsignedShort() == 0x0002) {
                color = in.readShort();
            }
        }
    }

    @Override
    public String toString() {
        String s = super.toString() + " LinkedContourVector";
        for (LinkedVector vec : vectors) {
            s += "\n\t" + vec;
        }

        return s;
    }
}
