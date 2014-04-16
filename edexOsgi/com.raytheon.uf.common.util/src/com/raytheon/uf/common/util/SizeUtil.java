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
package com.raytheon.uf.common.util;

/**
 * Utilities for calculating the size of objects
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 24, 2012            njensen     Initial creation
 * Jun 12, 2013   2064     mpduff      Add prettyKiloByteSize.
 * Apr 10, 2014   3023     rferrel     Properly handle negative numbers.
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class SizeUtil {
    private static final int BYTES_PER = 1024;

    private static final String[] REP_PREFIX = new String[] { "B", "kB", "MB",
            "GB", "TB", "PB", "EB", "ZB", "YB" };

    public static final long BYTES_PER_KB = BYTES_PER;

    public static final long BYTES_PER_MB = BYTES_PER_KB * BYTES_PER;

    /**
     * Transforms a number of bytes to a pretty string based on the total number
     * of bytes, e.g. B, kB, MB, or GB as fitting. For example: 1000 -> 1000B,
     * 10000 -> 9.7kB, 100000 -> 97.6kB, 1000000 -> 976.5kB, 10000000 -> 9.5MB
     * 
     * @param numberOfBytes
     *            the number to transform to a pretty string
     * @return the pretty String representation of the byte size
     */
    public static String prettyByteSize(long numberOfBytes) {
        StringBuilder sb = new StringBuilder();
        float n;
        if (numberOfBytes < 0) {
            sb.append("-");
            n = -numberOfBytes;
        } else {
            n = numberOfBytes;
        }

        int reps = 0;
        while ((n > BYTES_PER) && (reps < (REP_PREFIX.length - 1))) {
            reps++;
            n /= BYTES_PER;
        }
        int tenth = ((int) (n * 10)) % 10;
        sb.append((int) n).append(".").append(tenth);
        sb.append(REP_PREFIX[reps]);
        return sb.toString();
    }

    /**
     * Transforms a number of kilobytes to a pretty string based on the total
     * number of bytes, e.g. B, kB, MB, or GB as fitting. For example: 1000 ->
     * 1000B, 10000 -> 9.7kB, 100000 -> 97.6kB, 1000000 -> 976.5kB, 10000000 ->
     * 9.5MB
     * 
     * @param numberOfKiloBytes
     *            * the number to transform to a pretty string
     * @return the pretty String representation of the byte size
     */
    public static String prettyKiloByteSize(long numberOfKiloBytes) {
        return prettyByteSize(numberOfKiloBytes * BYTES_PER);
    }
}
