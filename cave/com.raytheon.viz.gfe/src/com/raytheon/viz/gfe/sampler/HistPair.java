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

package com.raytheon.viz.gfe.sampler;

/**
 * Container for a count and a HistValue
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * May 21, 2008 1167        mnash       Initial creation
 * Aug 6, 2008  1283        njensen     Implemented comparable
 * 
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */
public class HistPair implements Comparable<HistPair>, Cloneable {

    private int _count;

    private HistValue _value = null;

    /**
     * Default constructor setting count to 0
     */
    public HistPair() {
        _count = 0;
    }

    /**
     * Constructor taking a HistValue, stores that value, sets the count to 1
     * 
     * @param value
     */
    public HistPair(HistValue value) {
        _count = 1;
        _value = value;
    }

    /**
     * Constructor taking a count and the HistValue, stores the values in
     * private data
     * 
     * @param count
     * @param value
     */

    public HistPair(int count, HistValue value) {
        _count = count;
        _value = value;
    }

    /**
     * returns the count associated with this HistPair
     * 
     * @return
     */
    public int count() {
        return _count;
    }

    /**
     * Returns the value associated with this HistPair
     * 
     * @return
     */
    public final HistValue value() {
        return _value;
    }

    /**
     * Increments the count for this HistPair by amount
     * 
     * @param amount
     */
    public void incrementCount(int amount) {
        _count += amount;
    }

    /**
     * Outputs information about the class
     * 
     * @param o
     */
    public String printOn() {
        return "[" + _count + "," + _value + "]";
    }

    @Override
    public int compareTo(HistPair o) {
        return _value.compareTo(o._value);
    }

    public String toString() {
        return printOn();
    }

    @Override
    public HistPair clone() {
        return new HistPair(this.count(), this.value());
    }
}
