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
package com.raytheon.uf.edex.bufrtools;

import java.util.List;

import com.raytheon.uf.edex.bufrtools.descriptors.BUFRDescriptor;
import com.raytheon.uf.edex.bufrtools.packets.IBUFRDataPacket;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 *                         jkorman     Initial creation.
 * 9/16/2014    #3628      mapeters    Moved from uf.edex.decodertools plugin.
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */

public class BUFRSearchContext {

    private List<IBUFRDataPacket> targetDocument = null;

    private int currPos = -1;

    // private boolean isValid = false;

    private IBUFRDataPacket currFindData = null;

    private int descriptorTarget = -1;

    public BUFRSearchContext(List<IBUFRDataPacket> document, int descriptor) {
        if (document != null) {
            targetDocument = document;
            currPos = 0;
            descriptorTarget = descriptor;
            // isValid = true;
        }
    }

    public BUFRSearchContext(List<IBUFRDataPacket> document, int f, int x, int y) {
        this(document, BUFRDescriptor.createDescriptor(f, x, y));
    }

    /**
     * Get the last found data packet.
     * 
     * @return
     */
    public IBUFRDataPacket get() {
        return currFindData;
    }

    /**
     * Searches the current packet list to find a packet that has a specified
     * descriptor reference. The search begins at the current location.
     * 
     * @return Was the find successful?
     */
    public boolean find() {
        boolean found = false;
        // clear the last found data.
        currFindData = null;
        while (!found && (currPos < targetDocument.size())) {
            IBUFRDataPacket packet = targetDocument.get(currPos);

            if ((packet != null) && (packet.getReferencingDescriptor() != null)) {
                if (packet.getReferencingDescriptor().getDescriptor() == descriptorTarget) {
                    currFindData = packet;
                    found = true;
                }
            }
            if (!found) {
                currPos++;
            }
        }
        return found;
    }
}
