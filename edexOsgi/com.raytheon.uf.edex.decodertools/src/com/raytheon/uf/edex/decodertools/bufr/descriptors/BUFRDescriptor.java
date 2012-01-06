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

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import com.raytheon.uf.edex.decodertools.bufr.io.BUFRBitInputStream;
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
public abstract class BUFRDescriptor implements
Iterable<BUFRDescriptor> {

    private final int desciptor_f;

    private final int desciptor_x;

    private final int desciptor_y;

    private final int descriptorValue;
    
    private List<BUFRDescriptor> subList = null;

    private boolean defined = true;
    
    /**
     * Construct a descriptor using the number descriptor number. If the
     * descriptor defines a Table D descriptor, the defined flag is set to
     * false.
     * 
     * @param descriptor
     *            The number descriptor number.
     */
    public BUFRDescriptor(int descriptor) {
        descriptorValue = descriptor;
        desciptor_f = (descriptor & 0xC000) >> 14;
        desciptor_x = (descriptor & 0x03F00) >> 8;
        desciptor_y = (descriptor & 0x00FF);
        defined = !(desciptor_f == 3);
    }

    /**
     * Construct a descriptor using the descriptor parts. If the descriptor
     * defines a Table D descriptor, the defined flag is set to false.
     * 
     * @param f
     *            This should be 3 for table D.
     * @param x
     *            Value with range [0..63].
     * @param y
     *            Value with range [0..255].
     */
    public BUFRDescriptor(int f, int x, int y) {
        descriptorValue = createDescriptor(f, x, y);
        desciptor_f = f;
        desciptor_x = x;
        desciptor_y = y;
        defined = !(desciptor_f == 3);
    }

    /**
     * 
     * @param descriptor
     */
    public abstract BUFRDescriptor copy();
    
    /**
     * Get the value of the descriptor "F" part.
     * @return The descriptor "F" value.
     */
    public int getF() {
        return desciptor_f;
    }

    /**
     * Get the value of the descriptor "X" part.
     * @return The descriptor "X" value.
     */
    public int getX() {
        return desciptor_x;
    }

    /**
     * Get the value of the descriptor "Y" part.
     * @return The descriptor "Y" value.
     */
    public int getY() {
        return desciptor_y;
    }

    /**
     * Get the descriptor value.
     * @return The descriptor value.
     */
    public int getDescriptor() {
        return descriptorValue;
    }

    /**
     * Add a descriptor to this descriptors' sublist. If no sublist is defined
     * for this descriptor, the sublist is created.
     * @param descriptor A descriptor to add.
     */
    public void addDescriptor(BUFRDescriptor descriptor) {
        if(subList == null) {
            subList = new ArrayList<BUFRDescriptor>();
        }
        subList.add(descriptor);
    }
    
    /**
     * Get the sublist for this descriptor. If no sublist is defined, a null
     * reference is returned. This can also be used to "clear" an existing
     * sublist.
     * @param subList The sublist to be associated with this descriptor.
     */
    public void setSubList(List<BUFRDescriptor> subList) {
        this.subList = subList;
    }
    
    /**
     * Get the sublist for this descriptor. If no sublist is defined, a null
     * reference is returned. 
     * @return The sublist associated with this descriptor.
     */
    public List<BUFRDescriptor> getSubList() {
        return subList;
    }
    
    /**
     * Returns a list of descriptors if this descriptor defines a sublist. If
     * no sublist is defined, a null iterator is returned.
     * @return An iterator to a possible sublist.
     */
    @Override
    public Iterator<BUFRDescriptor> iterator() {
        Iterator<BUFRDescriptor> it = null;
        if(subList != null) {
            it = subList.iterator();
        }
        return it;
    }

    /**
     * Has this descriptor been defined. When true, a subclass should set defined
     * to true if the descriptors context is known.
     * @return Is this descriptor defined.
     */
    public boolean isDefined() {
        return defined;
    }

    /**
     * Set whether this descriptor is defined.
     * @param defined Set whether this descriptor is defined.
     */
    public void setDefined(boolean defined) {
        this.defined = defined;
    }

    /**
     * Create a string representation of the descriptor value only as a formatted
     * String.
     * @return The String representation of the descriptor value.
     */
    public String getStringDescriptor() {
        return String.format("%1d %02d %03d", desciptor_f, desciptor_x,
                desciptor_y);
    }
    
    /**
     * Create a string representation of this class using StringBuilder.
     * @param A StringBuilder buffer to receive data. If this reference is null,
     * an instance is created.
     * @return The String representation of this class as a StringBuilder reference.
     */
    public StringBuilder getStringData(StringBuilder buffer) {
        if (buffer == null) {
            buffer = new StringBuilder();
        }
        buffer.append(getStringDescriptor());

        return buffer;
    }

    /**
     * Create a string representation of this class.
     * @return The String representation of this class.
     */
    public String toString() {
        return getStringData(null).toString();
    }

    /**
     * The expected execute behavior is that a descriptor may read some number
     * of bits from the supplied Bit Input Stream, construct a data packet, and
     * return that packet to the client. If the descriptor does not intend to
     * perform any execute behavior, no data should be read from the stream and
     * a null reference should be returned.
     * @param bitStream The bit input stream to read from.
     * @return The constructed data packet. This reference may be null.
     */
    public abstract IBUFRDataPacket execute(BUFRBitInputStream bitStream);

    /**
     * Create the descriptor value based on the descriptor parts.
     * @param f The "f" part. Range [0..3].
     * @param x The "x" part. Range [0..63].
     * @param y The "y" part. Range [0..255].
     * @return
     */
    public static final int createDescriptor(int f, int x, int y) {
        int descriptorValue = (f << 14) | (x << 8) | y;
        
        return descriptorValue; 
    }
    
}
