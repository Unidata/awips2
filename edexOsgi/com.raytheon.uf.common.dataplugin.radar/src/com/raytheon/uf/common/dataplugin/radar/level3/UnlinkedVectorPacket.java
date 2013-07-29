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
 * 11-05-2007	465        randerso    Initial Creation
 * 07/29/2013   2148       mnash       Refactor registering of packets to Spring
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */
@DynamicSerialize
public class UnlinkedVectorPacket extends SymbologyPacket implements
        ISerializableObject {

    private static final int UNLINKED_VECTOR_PACKET10 = 10;

    /**
     * @param packetId
     * @param in
     * @throws IOException
     */
    public UnlinkedVectorPacket(int packetId, DataInputStream in)
            throws IOException {
        super(packetId, in);
    }

    public UnlinkedVectorPacket() {

    }

    @DynamicSerializeElement
    protected int theColor;

    @DynamicSerializeElement
    protected List<UnlinkedVector> vectors;

    /**
     * @return the theColor
     */
    public int getTheColor() {
        return theColor;
    }

    /**
     * @param theColor
     *            the theColor to set
     */
    public void setTheColor(int theColor) {
        this.theColor = theColor;
    }

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
        int blockLen = in.readUnsignedShort();

        theColor = -1;
        if (packetId == UNLINKED_VECTOR_PACKET10) {
            theColor = in.readUnsignedShort();
            blockLen -= 2;
        }

        vectors = new ArrayList<UnlinkedVector>();
        for (int i = 0; i < blockLen; i += 8) {
            UnlinkedVector vec = new UnlinkedVector();
            vec.i1 = in.readShort();
            vec.j1 = in.readShort();
            vec.i2 = in.readShort();
            vec.j2 = in.readShort();
            vec.setTheColor(theColor);
            vectors.add(vec);
        }
    }

    @Override
    public String toString() {
        String s = super.toString() + " UnlinkedVector";
        if (packetId == UNLINKED_VECTOR_PACKET10) {
            s += " Color: " + theColor;
        }
        for (UnlinkedVector vec : vectors) {
            s += "\n\t" + vec;
        }

        return s;
    }
}
