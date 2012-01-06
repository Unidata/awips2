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
package com.raytheon.viz.gfe.contours.util;

/**
 * Provides a enumeration of search directions for Contour Analysis. This
 * enumeration is based on the legacy C++ enum and related methods from
 * {@code ContourAnalyzer.H} and {@code ContourAnalyzer.C}.
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 13Mar2008	999        MW Fegan    Initial implementation.
 * 
 * </pre>
 *
 * @author mfegan
 * @version 1.0	
 */

public enum SearchDir {
    /**
     * North East.
     */
    NE, 
    /**
     * East.
     */
    E, 
    /**
     * South East
     */
    SE, 
    /**
     * South.
     */
    S, 
    /**
     * South West.
     */
    SW, 
    /**
     * West.
     */
    W, 
    /**
     * North West.
     */
    NW, 
    /**
     * North
     */
    N;
    /**
     * Provides the ordinal value of the element. This is intended to
     * emulate the casting of enum elements to int's in C/C++.
     */
    public final int ord;
    /**
     * An array containing the elements indexed by their ordinal values.
     */
    /**
     * Constructor. Sets the ordinal value of the element and adds the 
     * element to the reverse translation hash.
     */
    private SearchDir() {
        this.ord = this.ordinal();
    }
    /**
     * Returns the SearchDir element representing a specific ordinal
     * value. 
     * 
     * @param ord the ordinal value of the required element
     * 
     * @return the element with the specified ordinal value
     * 
     * @throws IllegalArgumentException if the ordinal value is not valid
     */
    public static SearchDir translate(int ord) {
        /* make sure input is valid. If not, throw exception */
        if (ord < 0 || ord > SearchDir.values().length - 1) {
            throw new IllegalArgumentException("Invalid ordinal value " + ord);
        }
        return SearchDir.values()[ord];
    }
    /**
     * Finds the SearchDir element representing the opposite direction.
     * E.g., SW is the opposite direction of NE, so {@code NE.reverse()}
     *  returns {@code SW}.
     *  
     * @return the opposite of this SearchDir
     */
    public SearchDir reverse() {
        int opp = (this.ord + 4) % 8;
        return values()[opp];
    }
}
