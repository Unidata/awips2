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
package com.raytheon.uf.edex.registry.ebxml.services.query;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import oasis.names.tc.ebxml.regrep.xsd.rim.v4.SlotType;

/**
 * 
 * Container class used to hold query parameters extracted from slots
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 21, 2012 #184       bphillip     Initial creation
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */
public class QueryParameters {

    /** The map of parameters */
    private Map<String, List<Object>> parameterMap;

    /**
     * Creates a new empty set of query parameters
     */
    public QueryParameters() {
        parameterMap = new HashMap<String, List<Object>>();
    }

    /**
     * Adds a parameter to the set
     * 
     * @param parameterName
     *            The parameter name
     * @param parameterValue
     *            The parameter value
     */
    public void addParameter(String parameterName, Object parameterValue) {
        if (parameterValue instanceof Collection<?>) {
            for (Object obj : ((Collection<?>) parameterValue)) {
                addParameter(parameterName, obj);
            }
        } else {
            if (!containsParameter(parameterName)) {
                parameterMap.put(parameterName, new ArrayList<Object>());
            }
            parameterMap.get(parameterName).add(parameterValue);
        }
    }

    /**
     * Adds a parameter contained within a slot to the set
     * 
     * @param slot
     *            The slot containing the parameter value
     */
    public void addParameter(SlotType slot) {
        addParameter(slot.getName(), slot.getSlotValue().getValue());
    }

    /**
     * Gets the values associated with this parameter
     * 
     * @param parameterName
     *            The parameter to get the values for
     * @return The values
     */
    public List<Object> getParameter(String parameterName) {
        return parameterMap.get(parameterName);
    }

    /**
     * Gets the item at the specified index in the list
     * 
     * @param <T>
     *            An object
     * @param parameterName
     *            The parameter name to get
     * @param index
     *            The value index to get
     * @return The value at the specified index
     */
    @SuppressWarnings("unchecked")
    public <T extends Object> T getParameter(String parameterName, int index) {
        if (containsParameter(parameterName)) {
            return (T) parameterMap.get(parameterName).get(index);
        }
        return null;
    }

    /**
     * Gets the first value in the value list for a given parameter
     * 
     * @param <T>
     *            An object class
     * @param parameterName
     *            The parameter name to get
     * @return The first value in the value list for the given parameter
     */
    public <T extends Object> T getFirstParameter(String parameterName) {
        return getParameter(parameterName, 0);
    }

    /**
     * Gets the first value in the value list for a given parameter. If not
     * found, return the default value provided
     * 
     * @param <T>
     *            An object class
     * @param parameterName
     *            The parameter name to get
     * @param defaultValue
     *            The default value to return if not found
     * @return The first value in the value list for the given parameter, or the
     *         default value if not found
     */
    public <T extends Object> T getFirstParameter(String parameterName,
            T defaultValue) {
        T param = getParameter(parameterName, 0);
        if (param == null) {
            return defaultValue;
        } else {
            return param;
        }
    }

    /**
     * Checks if a parameter is contained in this set
     * 
     * @param parameterName
     *            The parameter name to check for
     * @return True if the parameter name is contained in this set, else false
     */
    public boolean containsParameter(String parameterName) {
        return parameterMap.containsKey(parameterName);
    }

    /**
     * Checks if the given parameter list is contained in the set
     * 
     * @param parameterNames
     *            The parameter names to check for
     * @return True if all the parameters provided are in the list, else false
     */
    public boolean containsParameters(String... parameterNames) {
        return parameterMap.keySet().containsAll(Arrays.asList(parameterNames));
    }

    /**
     * Removes the parameter value list from this set
     * 
     * @param parameterName
     *            The parameter to remove the values for
     * @return The values removed
     */
    public List<Object> removeParameter(String parameterName) {
        return parameterMap.remove(parameterName);
    }

    /**
     * Checks if this parameter set is empty
     * 
     * @return True if empty
     */
    public boolean isEmpty() {
        return parameterMap.isEmpty();
    }

    /**
     * Gets the underlying parameter map object
     * 
     * @return The parameter map
     */
    public Map<String, List<Object>> getParameterMap() {
        return parameterMap;
    }

    /**
     * Sets the parameter map
     * 
     * @param parameterMap
     *            The parameter map to set
     */
    public void setParameterMap(Map<String, List<Object>> parameterMap) {
        this.parameterMap = parameterMap;
    }

}
