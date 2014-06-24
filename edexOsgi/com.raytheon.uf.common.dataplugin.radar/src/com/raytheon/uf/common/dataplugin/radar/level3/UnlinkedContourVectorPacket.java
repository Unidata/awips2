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
 * {@link SymbologyPacket} containing contours of {@link UnlinkedVector}s.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Sep 14, 2010           mnash       Initial creation
 * Jul 29, 2013  2148     mnash       Refactor registering of packets to Spring
 * Jun 04, 2014  3232     bsteffen    Remove ISerializableObject
 * Jun 24, 2014  3311     njensen     Add DynamicSerialize annotation
 * 
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */

@DynamicSerialize
public class UnlinkedContourVectorPacket extends SymbologyPacket {

    private static final int CONTOUR_VECTOR_PACKET3501 = 0x3501;

    /**
     * @param packetId
     * @param in
     * @throws IOException
     */
    public UnlinkedContourVectorPacket(int packetId, DataInputStream in)
            throws IOException {
        super(packetId, in);
    }

    public UnlinkedContourVectorPacket() {

    }

    @DynamicSerializeElement
    protected List<UnlinkedVector> vectors;

    /**
     * @return the vectors
     */
    public List<UnlinkedVector> getVectors() {
        return vectors;
    }

    /**
     * @param vectors
     *            the vectors to set
     */
    public void setVectors(List<UnlinkedVector> vectors) {
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

        vectors = new ArrayList<UnlinkedVector>();
        UnlinkedVector vec;

        if (packetId == CONTOUR_VECTOR_PACKET3501) {
            blockLen = in.readUnsignedShort();
            for (int i = 0; i < blockLen; i += 8) {
                vec = new UnlinkedVector();
                vec.i1 = in.readShort();
                vec.j1 = in.readShort();
                vec.i2 = in.readShort();
                vec.j2 = in.readShort();
                vectors.add(vec);
            }
        }

    }

    @Override
    public String toString() {
        String s = super.toString() + " UnlinkedContourVector";
        for (UnlinkedVector vec : vectors) {
            s += "\n\t" + vec;
        }

        return s;
    }

}
