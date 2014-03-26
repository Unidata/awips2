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
package com.raytheon.uf.common.numeric;


/**
 * Utility methods for unsigned numbers
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 25, 2014 2905       bclement     Initial creation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class UnsignedNumbers {

    public static final short UNSIGNED_BYTE_MASK = 0xFF;

    public static final int UNSIGNED_SHORT_MASK = 0xFFFF;

    public static final long UNSIGNED_INT_MASK = 0xFFFFFFFFL;

    private UnsignedNumbers() {
    }

    /**
     * Promote unsigned byte value to short
     * 
     * @param unsigned
     * @return
     */
    public static short ubyteToShort(byte unsigned) {
        return (short) (unsigned & UNSIGNED_BYTE_MASK);
    }

    /**
     * Promote unsigned short value to int
     * 
     * @param unsigned
     * @return
     */
    public static int ushortToInt(short unsigned) {
        return unsigned & UNSIGNED_SHORT_MASK;
    }

    /**
     * Promote unsigned int value to long
     * 
     * @param unsigned
     * @return
     */
    public static long uintToLong(int unsigned) {
        return unsigned & UNSIGNED_INT_MASK;
    }

}
