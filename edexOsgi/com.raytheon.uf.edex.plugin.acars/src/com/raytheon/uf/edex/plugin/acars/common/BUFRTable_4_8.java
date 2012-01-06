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
package com.raytheon.uf.edex.plugin.acars.common;

/**
 * TODO Add Description
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 23, 2009       1939 jkorman     Initial creation
 *
 * </pre>
 *
 * @author jkorman
 * @version 1.0	
 */

public enum BUFRTable_4_8 {

    UNS(2), LVR(3), LVW(4), ASC(5), DES(6), MSG(7);
    
    private final int value;
    
    private BUFRTable_4_8(int tableValue) {
        value = tableValue;
    }
    
    /**
     * 
     * @return
     */
    public int getValue() {
        return value;
    }
    
    /**
     * 
     * @param index
     * @return
     */
    public static BUFRTable_4_8 getEntry(int index) {
        BUFRTable_4_8 phase = null;
        switch(index) {
        case 2 : {
            phase = UNS;
            break;
        }
        case 3 : {
            phase = LVR;
            break;
        }
        case 4 : {
            phase = LVW;
            break;
        }
        case 5 : {
            phase = ASC;
            break;
        }
        case 6 : {
            phase = DES;
            break;
        }
        case 7 : {
            phase = MSG;
            break;
        }
        default : {
            phase = null;
        }
        }
        
        return phase;
    }
}
