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
 * 
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */
@DynamicSerialize
public class TextSymbolPacket extends SymbologyPacket implements
        ISerializableObject {
    public static final int TEXT_SYMBOL_PACKET1 = 1;

    public static final int TEXT_SYMBOL_PACKET2 = 2;

    public static final int TEXT_SYMBOL_PACKET8 = 8;

    static {
        PacketFactory.registerPacketType(TextSymbolPacket.class,
                TEXT_SYMBOL_PACKET1, TEXT_SYMBOL_PACKET2, TEXT_SYMBOL_PACKET8);
    }

    @DynamicSerializeElement
    protected int theColor;

    @DynamicSerializeElement
    protected int i;

    @DynamicSerializeElement
    protected int j;

    @DynamicSerializeElement
    protected String theText;

    /**
     * @param layerArray
     * @throws IOException
     */
    public TextSymbolPacket(int packetId, DataInputStream in)
            throws IOException {
        super(packetId, in);
    }

    public TextSymbolPacket() {

    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.edex.util.radar.SymbologyLayer#parseHeader()
     */
    @Override
    protected void init(DataInputStream in) throws IOException {
        int blockLen = in.readUnsignedShort();

        theColor = -1;
        if (packetId == TEXT_SYMBOL_PACKET8) {
            theColor = in.readUnsignedShort();
            blockLen -= 2;
        }

        i = in.readShort();
        j = in.readShort();
        blockLen -= 4;
        if (blockLen >= 0) {
            byte[] buf = new byte[blockLen];
            in.readFully(buf);
            theText = new String(buf);
        }
    }

    @Override
    public String toString() {
        String s = super.toString() + " Text/Symbology";
        if (packetId == TEXT_SYMBOL_PACKET8) {
            s += " Color: " + theColor;
        }
        s += "\n\t\tI: " + i + "  J: " + j;
        s += "  Text: \"" + theText + "\"";

        return s;
    }

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
     * @return the theText
     */
    public String getTheText() {
        return theText;
    }

    /**
     * @param theText
     *            the theText to set
     */
    public void setTheText(String theText) {
        this.theText = theText;
    }

}
