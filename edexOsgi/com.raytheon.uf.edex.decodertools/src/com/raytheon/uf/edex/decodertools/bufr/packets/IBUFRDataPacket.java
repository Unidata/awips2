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
public interface IBUFRDataPacket {

    /**
     * 
     * @return
     */
    public String getUnits();

    /**
     * 
     * @return
     */
    public Object getValue();

    /**
     * Is the value for this packet missing.
     * @return Is the value missing.
     */
    public boolean isMissing();

    /**
     * 
     * @return
     */
    public BUFRDescriptor getReferencingDescriptor();
    
    /**
     * 
     * @param numSubsets
     * @param bitStrm
     */
    public void readCompressed(int numSubsets, BUFRBitInputStream bitStrm);
    
    /**
     * Get the count of data subset elements.
     * @return The number of data subset elements.
     */
    public int getSubsetDataCount();
    
    /**
     * 
     * @param index
     * @return
     */
    public IBUFRDataPacket getSubsetData(int index);
}
