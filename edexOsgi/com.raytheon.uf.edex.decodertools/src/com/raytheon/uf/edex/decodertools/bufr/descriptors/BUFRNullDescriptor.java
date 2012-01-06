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
package com.raytheon.uf.edex.decodertools.bufr.descriptors;

import com.raytheon.uf.edex.decodertools.bufr.io.BUFRBitInputStream;
import com.raytheon.uf.edex.decodertools.bufr.packets.BUFRUnknownPacket;
import com.raytheon.uf.edex.decodertools.bufr.packets.IBUFRDataPacket;

/**
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 20071127            382 jkorman     Initial Coding.
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */
public class BUFRNullDescriptor extends BUFRDescriptor {
    
    private int bitWidth;
    
    /**
     * 
     * @param descriptor
     */
    public BUFRNullDescriptor(int descriptor) {
        super(descriptor);
    }

    /**
     * 
     * @param f
     * @param x
     * @param y
     */
    public BUFRNullDescriptor(int f, int x, int y) {
        super(f, x, y);

    }

    /**
     * Read bit width number of bits from the input stream and create an
     * unknown type data packet to receive the data.
     * @param bitStream The bit input stream to read data from. 
     */
    @Override
    public IBUFRDataPacket execute(BUFRBitInputStream bitStream) {
        long data = bitStream.read(bitWidth);
        IBUFRDataPacket packet = new BUFRUnknownPacket(data,this);
        
        return packet;
    }

    /**
     * Get the bit width of this descriptor.
     * @return the bitWidth
     */
    public int getBitWidth() {
        return bitWidth;
    }

    /**
     * Set the bit width of this descriptor.
     * @param bitWidth The desired bit width.
     */
    public void setBitWidth(int bitWidth) {
        this.bitWidth = bitWidth;
    }
    
    /**
     * 
     * @param descriptor
     * @return
     */
    @Override
    public BUFRDescriptor copy() {

        BUFRNullDescriptor newDescriptor = new BUFRNullDescriptor(getF(), getX(), getY());
        newDescriptor.bitWidth = bitWidth;
        newDescriptor.setDefined(isDefined());

        return newDescriptor;
    }
    
}
