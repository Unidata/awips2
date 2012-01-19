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
package com.raytheon.edex.util;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 27, 2009            randerso     Initial creation
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class MathUtil {

    /**
     * private constructor (do not instantiate)
     */
    private MathUtil() {
    }

    /**
     * Computes the greatest common divisor of a and b
     * 
     * @param a
     * @param b
     * @return gcd(a,b)
     */
    public static int gcd(int a, int b) {
        if (b == 0) {
            return Math.abs(a);
        }
        return gcd(b, a % b);
    }

    /**
     * Computes the greatest common divisor of a list of integers
     * 
     * @param a0
     *            a0
     * @param a1
     *            a1..an
     * @return gcd(a0..an)
     */
    public static int gcd(int a0, int... a1) {
        int result = a0;
        for (int b : a1) {
            result = gcd(result, b);
        }
        return result;
    }

    /**
     * Computes the least common multiple of a and b
     * 
     * @param a
     * @param b
     * @return lcm(a,b)
     */
    public static int lcm(int a, int b) {
        return a * b / gcd(a, b);
    }

    /**
     * Computes the least common multiple of a list of integers
     * 
     * @param a0
     *            a0
     * @param a1
     *            a1..an
     * @return lcm(a0..an)
     */
    public static int lcm(int a0, int... a1) {
        int result = a0;
        for (int b : a1) {
            result = lcm(result, b);
        }
        return result;
    }
}
