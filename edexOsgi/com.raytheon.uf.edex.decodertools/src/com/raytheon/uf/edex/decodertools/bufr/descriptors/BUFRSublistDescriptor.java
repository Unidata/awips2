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
 * TODO Add Description
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 	
 * 
 * </pre>
 *
 * @author jkorman
 * @version 1.0	
 */

public class BUFRSublistDescriptor extends BUFRDescriptor {

    public static final int REP_LIST_DESC = 
        BUFRDescriptor.createDescriptor(0, 0, 1);
    public static final int REP_SUBLIST_DESC = 
        BUFRDescriptor.createDescriptor(0, 0, 2);
    public static final int SUBSET_LIST_DESC = 
        BUFRDescriptor.createDescriptor(0, 0, 3);
    
    private int listCount = -1;
    
    /**
     * 
     * @param descriptor
     */
    public BUFRSublistDescriptor(int descriptor, int count) {
        super(descriptor);
    }

    /**
     * 
     * @param f
     * @param x
     * @param y
     */
    public BUFRSublistDescriptor(int f, int x, int y, int count) {
        super(f, x, y);
        listCount = count;
    }

    /**
     * 
     * @see com.raytheon.edex.tools.bufr.descriptors.BUFRDescriptor#filter(com.raytheon.edex.tools.bufr.io.BUFRBitInputStream)
     */
    @Override
    public IBUFRDataPacket execute(BUFRBitInputStream bitStream) {
        return null;
    }

    /**
     * @return the listCount
     */
    public int getListCount() {
        return listCount;
    }
    
    /**
     * 
     * @param descriptor
     * @return
     */
    @Override
    public BUFRDescriptor copy() {

        BUFRSublistDescriptor newDescriptor = new BUFRSublistDescriptor(getF(), getX(), getY(), getListCount());
        List<BUFRDescriptor> repList = getSubList();
        if(repList != null) {
            List<BUFRDescriptor> newList = new ArrayList<BUFRDescriptor>();
            for(BUFRDescriptor d : repList) {
                BUFRDescriptor nd = d.copy();
                newList.add(nd);
            }
            newDescriptor.setSubList(newList);
        }
        newDescriptor.listCount = listCount;
        newDescriptor.setDefined(isDefined());
        return newDescriptor;
    }

}
