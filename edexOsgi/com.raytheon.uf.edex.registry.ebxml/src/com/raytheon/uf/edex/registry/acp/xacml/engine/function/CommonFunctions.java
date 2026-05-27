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
package com.raytheon.uf.edex.registry.acp.xacml.engine.function;

import java.util.ArrayList;
import java.util.List;

import com.raytheon.uf.edex.registry.acp.xacml.exception.XACMLProcessingException;

/**
 * 
 * Functions that are common across most data types supported by the XACML spec
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 8/17/2012    724          bphillip    Initial Coding
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
public class CommonFunctions {

    /**
     * Checks to see if at least one member of the first list is in the second
     * list
     * 
     * @param <T>
     *            The object type
     * @param list1
     *            The first list
     * @param list2
     *            The second list
     * @return True if an item in the first list exists in the second list
     */
    public static <T extends Object> Boolean atLeastOneMemberOf(List<T> list1,
            List<T> list2) {
        for (T obj : list1) {
            if (list2.contains(obj)) {
                return true;
            }
        }
        return false;
    }

    /**
     * Converts an array of objects to a list of objects
     * 
     * @param <T>
     *            The object type
     * @param objs
     *            The array of objects
     * @return A list containing the objects from the input array
     */
    @SuppressWarnings("unchecked")
    public static <T extends Object> List<T> createBag(Object[] objs) {
        List<T> retVal = new ArrayList<T>();
        for (Object obj : objs) {
            retVal.add((T) obj);
        }
        return retVal;
    }

    /**
     * Gets the intersection of the first and second lists
     * 
     * @param <T>
     *            The object type
     * @param list1
     *            The first list
     * @param list2
     *            The second list
     * @return The intersection of the two lists
     */
    public static <T extends Object> List<T> intersection(List<T> list1,
            List<T> list2) {
        List<T> retVal = new ArrayList<T>();
        for (T obj : list1) {
            if (list2.contains(obj)) {
                retVal.add(obj);
            }
        }
        return retVal;
    }

    /**
     * Verifies that the provided list contains only one element
     * 
     * @param <T>
     *            The object type
     * @param list
     *            The list to check
     * @return Null if the list is null or the one item in the list if the list
     *         is of size 1
     * @throws XACMLProcessingException
     *             If the list size is not 1
     */
    public static <T extends Object> T oneAndOnly(List<T> list)
            throws XACMLProcessingException {
        if (list == null) {
            return null;
        }
        if (list.size() != 1) {
            throw new XACMLProcessingException(
                    "Bag contains more than one value");
        }
        return list.get(0);
    }

    /**
     * Checks if the first list is a subset of the second list
     * 
     * @param <T>
     *            The object type
     * @param list1
     *            The list used to check if it's a subset of the second list
     * @param list2
     *            The list to check to see if the first list is a subset
     * @return True if the first list is a subset of the second, else false
     */
    public static <T extends Object> Boolean isSubset(List<T> list1,
            List<T> list2) {
        for (T obj : list1) {
            if (!list2.contains(obj)) {
                return false;
            }
        }
        return true;
    }

    /**
     * Gets the union of the provided lists
     * 
     * @param <T>
     *            The object type
     * @param list1
     *            The first list
     * @param list2
     *            The second list
     * @return The union of the provided lists
     */
    public static <T extends Object> List<T> union(List<T> list1, List<T> list2) {
        List<T> retVal = new ArrayList<T>(list1.size() + list2.size());
        retVal.addAll(list1);
        for (T elem : list2) {
            if (!retVal.contains(elem)) {
                retVal.add(elem);
            }
        }
        return retVal;
    }
}
