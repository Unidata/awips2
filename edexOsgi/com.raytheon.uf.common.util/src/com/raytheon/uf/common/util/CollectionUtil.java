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

import java.util.Arrays;
import java.util.Collection;
import java.util.HashSet;
import java.util.Set;


/**
 * Utility class for collection types. 
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 13, 2012 740        djohnson     Initial creation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public final class CollectionUtil {

    private CollectionUtil() {

    }

    /**
     * Check whether an array is null or empty.
     * 
     * @param array
     *            the array
     * @return true if the array is null or empty
     */
    public static <T> boolean isNullOrEmpty(T[] array) {
        return array == null || array.length == 0;
    }

    /**
     * Check whether a collection is null or empty.
     * 
     * @param coll
     *            the collection
     * @return true if the collection is null or empty
     */
    public static boolean isNullOrEmpty(Collection<?> coll) {
        return coll == null || coll.isEmpty();
    }

    /**
     * Create a set from the specified items. Assumes the items can be stored in
     * a {@link HashSet}.
     * 
     * @param items
     *            the items
     * @return the set
     */
    public static <T> Set<T> asSet(T... items) {
        return new HashSet<T>(Arrays.asList(items));
    }
}
