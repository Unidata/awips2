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
package com.raytheon.uf.edex.decodertools.bufr.packets;

import java.util.List;

import com.raytheon.uf.edex.decodertools.bufr.descriptors.BUFRDescriptor;
import com.raytheon.uf.edex.decodertools.bufr.io.BUFRBitInputStream;

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
public class BUFRSublistPacket implements IBUFRDataPacket {
    private BUFRDescriptor referenceDescriptor = null;

    private final List<IBUFRDataPacket> value;

    private final String units;

    private boolean missingData;
    
    public BUFRSublistPacket(List<IBUFRDataPacket> subList, String units,
            BUFRDescriptor refDescriptor) {
        this.value = subList;
        this.units = units;
        missingData = ((value == null) || (value.size() == 0));
        referenceDescriptor = refDescriptor;
    }

    @Override
    public String getUnits() {
        return units;
    }

    @Override
    public Object getValue() {
        return value;
    }

    /**
     * Is the value for this packet missing.
     * @return Is the value missing.
     */
    public boolean isMissing() {
        return missingData;
    }

    public String toString() {
        return value + ":" + units;
    }

    public BUFRDescriptor getReferencingDescriptor() {
        return referenceDescriptor;
    }

    public void readCompressed(int numSubsets, BUFRBitInputStream bitStrm) {
    }

    /**
     * Get the count of data subset elements.
     * @return The number of data subset elements.
     */
    public int getSubsetDataCount() {
        return 1;
    }
    
    /**
     * 
     * @return
     */
    public IBUFRDataPacket getSubsetData(int index) {
        return this;
    }

}
