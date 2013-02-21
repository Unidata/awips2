package com.raytheon.uf.featureexplorer;

import java.util.Comparator;

/**
 * This class is available for the sort function of Collections. The idea is
 * that it compares the Versions backwards so that the sort creates a descending
 * order Collection.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 3, 2008             dglazesk    Initial creation
 * </pre>
 * 
 * @author dglazesk
 * @version 1.0
 */
public class ReverseVersionComparator implements Comparator<Version> {

    /**
     * This comparator is purposefully backwards so that the highest version
     * number can be sorted to the front of an array.
     * 
     * @param left
     *            The left hand side of the version comparison
     * @param right
     *            The right hand side of the version comparison
     * @return -1 if left is bigger than right, 1 if right is bigger, and 0 if
     *         equal
     */
    @Override
    public int compare(Version left, Version right) {
        return right.compareTo(left);
    }

}
