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
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 20071127            382 jkorman     Initial Coding.
 * 20080214            862 jkorman     BUFRMOS implementation changes.
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */
public class BUFRReplicationDescriptor extends BUFRDescriptor {
    
    private BUFRTableB delayedSuffix = null;
    
    /**
     * 
     * @param descriptor
     */
    public BUFRReplicationDescriptor(int descriptor) {
        super(descriptor);
    }

    /**
     * 
     * @param f
     * @param x
     * @param y
     */
    public BUFRReplicationDescriptor(int f, int x, int y) {
        super(f, x, y);
    }
    
    /**
     * Get the delayed suffix (Table B, class 31 descriptor).
     * @return The delayedSuffix descriptor.
     */
    public BUFRTableB getDelayedSuffix() {
        return delayedSuffix;
    }

    /**
     * Get the delayed operator (Table B, class 31 descriptor).
     * @param delayedOperator The delayedSuffix descriptor
     */
    public void setDelayedSuffix(BUFRTableB delayedSuffix) {
        this.delayedSuffix = delayedSuffix;
    }

    /**
     * 
     */
    @Override
    public IBUFRDataPacket execute(BUFRBitInputStream bitStream) {
        IBUFRDataPacket packet = null;
        return packet;
    }

    public StringBuilder getStringData(StringBuilder buffer) {
        String fmt = "\n   Replication [%3d] descriptors [%6d] times";
        buffer = super.getStringData(buffer);
        if (buffer == null) {
            buffer = new StringBuilder();
        }
        buffer.append(String.format(fmt, getX(), getY()));
        return buffer;
    }

    /**
     * 
     * @param descriptor
     * @return
     */
    @Override
    public BUFRDescriptor copy() {

        BUFRReplicationDescriptor newDescriptor = new BUFRReplicationDescriptor(getF(), getX(), getY());
        
        if(delayedSuffix != null) {
            newDescriptor.delayedSuffix = (BUFRTableB) delayedSuffix.copy();
        } else {
            newDescriptor.delayedSuffix = null;
        }
        List<BUFRDescriptor> repList = getSubList();
        if(repList != null) {
            List<BUFRDescriptor> newList = new ArrayList<BUFRDescriptor>();
            for(BUFRDescriptor d : repList) {
                BUFRDescriptor nd = d.copy();
                newList.add(nd);
            }
            newDescriptor.setSubList(newList);
        }
        newDescriptor.setDefined(isDefined());

        return newDescriptor;
    }

}
