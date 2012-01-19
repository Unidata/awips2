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

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

/**
 * 
 * 
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

public abstract class RedbookBlock {

    @SuppressWarnings("unused")
    private final Log logger = LogFactory.getLog(getClass());

    private static final int LEN_MASK = 0x8000;
    
    private static final int CHKSUM_MASK = 0x4000;
    
    private static final int LENGTH_MASK = 0x3FFF;
    
    private boolean hasLength = false;
    
    private boolean hasChkSum = false;
    
    private final int length;
    
    private final int mode;
    
    private final int subMode;
    
    /**
     * 
     * @param separator
     */
    public RedbookBlock(ByteBuffer data) {
        
        int hdr = (data.getShort() & 0xFFFF);
        
        hasLength = (hdr & LEN_MASK) == 0;
        
        hasChkSum = (hdr & CHKSUM_MASK) == 0;

        mode = (data.get() & 0xFF);
        subMode = (data.get() & 0xFF);

        length = (hasLength) ? (hdr & LENGTH_MASK) : -1;
    }

    public boolean isEndBlock() {
        return false;
    }

    /**
     * @return the length
     */
    public int getLength() {
        return length;
    }

    /**
     * @return the mode
     */
    public int getMode() {
        return mode;
    }

    /**
     * @return the subMode
     */
    public int getSubMode() {
        return subMode;
    }

    /**
     * @return the hasLength
     */
    public boolean hasLength() {
        return hasLength;
    }

    /**
     * @param hasLength the hasLength to set
     */
    public void setHasLength(boolean hasLength) {
        this.hasLength = hasLength;
    }

    /**
     * @return the hasChkSum
     */
    public boolean hasChkSum() {
        return hasChkSum;
    }

    /**
     * @param hasChkSum the hasChkSum to set
     */
    public void setHasChkSum(boolean hasChkSum) {
        this.hasChkSum = hasChkSum;
    }
    
    public StringBuilder toString(StringBuilder sb) {
        if(sb == null) {
            sb = new StringBuilder();
        }
        sb.append((hasLength) ? 'L' : '.');
        sb.append((hasChkSum) ? 'C' : '.');
        sb.append(':');
        
        sb.append(String.format("%05d:mode=%02X:submode=%02X",length,mode,subMode));
        
        return sb;
    }
    
    /**
     * 
     */
    public String toString() {
        return toString((StringBuilder) null).toString();
    }
    
    public static float getFloat2(ByteBuffer dataBuf) {
        float f = Float.NaN;
        
        if(dataBuf.remaining() >= 2) {
            
            short s = dataBuf.getShort();
            
            f = s / 100.0f;
        }
        return f;
    }
    
}
