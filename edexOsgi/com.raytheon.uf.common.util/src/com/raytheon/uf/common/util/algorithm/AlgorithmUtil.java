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
package com.raytheon.uf.common.util.algorithm;

import java.util.Iterator;
import java.util.SortedSet;

/**
 * Provide reusable implementations of commonly used algorithms.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 30, 2012 1389       djohnson     Initial creation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public class AlgorithmUtil {

    /**
     * Response interface for a binary search.
     */
    public static interface IBinarySearchResponse<T extends Comparable<T>> {
        /**
         * Get the item matching the function, or null, if no item matching the function's criteria was found.
         * @return
         *  the item, or null
         */
        T getItem();

        /**
         * Get the number of iterations that were performed.
         * 
         * @return the number of iterations
         */
        int getIterations();
    }

    /**
     * Simple implementation ofa binary search response. Should remain private,
     * and not part of the public API so it can change later if required.
     */
    private static class BinarySearchResponse<T extends Comparable<T>>
            implements IBinarySearchResponse<T> {

        private final T item;

        private final int iterations;

        /**
         * Constructor.
         * 
         * @param item
         *            the item
         * @param iterations
         *            the iterations
         */
        private BinarySearchResponse(T item, int iterations) {
            this.item = item;
            this.iterations = iterations;
        }

        /**
         * {@inheritDoc}
         */
        @Override
        public T getItem() {
            return item;
        }

        /**
         * {@inheritDoc}
         */
        @Override
        public int getIterations() {
            return iterations;
        }
    }

    /**
     * Performs a binary search of a {@link SortedSet} of {@link Comparable}s,
     * applying the specified {@link Comparable} function on each item until it
     * returns an equals response, via the integer 0.
     * 
     * @param <T>
     *            the comparable type
     * @param items
     *            the items
     * @param function
     *            The function to apply to each item. Should return a number
     *            less than zero if the argument provides an answer too low,
     *            greater than zero if the argument provides an answer too high,
     *            and zero if the argument provides the expected answer.
     * @return the response
     */
    public static <T extends Comparable<T>> IBinarySearchResponse<T> binarySearch(
            SortedSet<T> items, Comparable<T> function) {
        int start = 0;
        int end = items.size() - 1;
        int iterations = 0;

        while (start <= end) {
            iterations++;
            int midPt = (start + end) / 2;

            int i = 0;
            T midValue = null;
            for (Iterator<T> iter = items.iterator(); i <= midPt; i++) {
                midValue = iter.next();
            }

            final int compareResult = function.compareTo(midValue);
            if (compareResult == 0) {
                return new BinarySearchResponse<T>(midValue, iterations);
            } else if (compareResult < 0) {
                start = midPt + 1;
            } else {
                end = midPt - 1;
            }
        }
        return new BinarySearchResponse<T>(null, iterations);
    }

}
