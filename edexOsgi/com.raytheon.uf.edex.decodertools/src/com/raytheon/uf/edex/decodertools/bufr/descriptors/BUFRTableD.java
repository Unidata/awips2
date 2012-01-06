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
import java.util.List;

import com.raytheon.uf.edex.decodertools.bufr.io.BUFRBitInputStream;
import com.raytheon.uf.edex.decodertools.bufr.packets.IBUFRDataPacket;

/**
 * A table D descriptor is used to define a sequence of descriptors that will be
 * expanded prior to descriptor execution. When created, table D descriptors are
 * set to a notDefined state. The user defining this table entry is responsible
 * for setting the defined state.
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
public class BUFRTableD extends BUFRDescriptor {

    /**
     * Construct a Table D descriptor using the number descriptor number.
     * @param descriptor The number descriptor number.
     */
    public BUFRTableD(int descriptor) {
        super(descriptor);
    }

    /**
     * Construct a Table D descriptor using the descriptor parts.
     * @param f This should be 3 for table D.
     * @param x Value with range [0..63].
     * @param y Value with range [0..255].
     */
    public BUFRTableD(int f, int x, int y) {
        super(f, x, y);
    }

    /**
     * This descriptor performs no action and returns a null reference.
     * @param bitStream The bit input stream to read from.
     * @return The constructed data packet. This reference may be null.
     */
    @Override
    public IBUFRDataPacket execute(BUFRBitInputStream bitStream) {
        return null;
    }

    /**
     * 
     * @param descriptor
     * @return
     */
    @Override
    public BUFRDescriptor copy() {

        BUFRTableD newDescriptor = new BUFRTableD(getF(), getX(), getY());
        newDescriptor.setDefined(isDefined());

        // Table D descriptors shouldn't have sublists, doing this for
        // completeness.
        List<BUFRDescriptor> repList = getSubList();
        if(repList != null) {
            List<BUFRDescriptor> newList = new ArrayList<BUFRDescriptor>();
            for(BUFRDescriptor d : repList) {
                BUFRDescriptor nd = d.copy();
                newList.add(nd);
            }
            newDescriptor.setSubList(newList);
        }

        return newDescriptor;
    }

}
