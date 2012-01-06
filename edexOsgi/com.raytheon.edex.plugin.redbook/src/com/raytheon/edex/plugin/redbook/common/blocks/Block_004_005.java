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
package com.raytheon.edex.plugin.redbook.common.blocks;

import java.nio.ByteBuffer;

/**TODO Add Description
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 20080512           1131 jkorman     Initial implementation.
 * 
 * </pre>
 *
 * @author jkorman
 * @version 1.0	
 */

public class Block_004_005 extends RedbookBlock {

    /**
     * 
     * @param separator
     */
    public Block_004_005(ByteBuffer data) {
        super(data);
        populate(data);
        if(hasChkSum()) {
            data.getShort();
        }
    }

    private void populate(ByteBuffer data) {
        if(hasLength()) {
            for(int i = 0;i < getLength()-2;i++) {
                data.getShort();
            }
        }
    }
    
    /**
     * 
     */
    public StringBuilder toString(StringBuilder sb) {
        sb = super.toString(sb);
        
        return sb;
    }
}
