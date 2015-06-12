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
package com.raytheon.uf.common.datalisting;

import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.SortedMap;

import com.raytheon.uf.common.dataquery.requests.DbQueryRequest;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;

/**
 * 
 * This class provides a mechanism for interactively querying plugin metadata to
 * determine what data is available. Metadata is described using key value
 * pairs. To use a listing you need to know what keys are available for that
 * listing, this can be found using {@link #getKeys()}. For the typical use
 * case, the first key is passed to {@link #getValues(String, Map)} along with
 * an empty map. This will return all the options available for that key. A
 * value for that key should be chosen and the key/value pair placed in a map,
 * then all available values for the next key can be found using that map. Once
 * a value has been chosen for all keys a Map<String, RequestConstraint> can be
 * created using {@link #getRequestConstraints(Map)} and this can be used to
 * retrieve data using a {@link DbQueryRequest} or other query objects. Here is
 * an example of how a listing may be used:
 * 
 * <pre>
 * <code>
 * DataListing listing = getDataListing();
 * Map<String,String> keyVals = new HashMap<>();
 * for(String key: listing.getKeys()){ 
 *     Collection<String> values = listing.getValues(key, keyVals);
 *     String selectedValue = selectValue(values);
 *     keyVals.put(key, selectedValue);
 * }
 * Map<String,RequestConstraint> rcMap = listing.getRequestConstraints(keyVals);
 * doSomethingAmazing(rcMap);
 * </code>
 * </pre>
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------
 * Jun 02, 2015  4153     bsteffen  Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public interface DataListing {

    /**
     * @return the name of the plugin that is providing the data for this
     *         listing.
     */
    public String getPluginName();

    /**
     * @return the keys that have metadata values that can be listed.
     */
    public Collection<String> getKeys();

    /**
     * Get all the values for a specific key, constrained by other key/values.
     * 
     * @param key
     *            a key(chosen from those returned by {@link #getKeys()}) that
     *            should be listed.
     * @param keyVals
     *            a map of keys to values(chosen from previous calls to
     *            {@link #getKeys()} and {@link #getValues(String, Map)}
     * @return a set of values available for the specified key
     * @throws Exception
     */
    public Collection<String> getValues(String key, Map<String, String> keyVals) throws Exception;

    /**
     * Performs the same functionality as {@link #getValues(String, Map)} but
     * formats the result in a way that may be easier for users to understand.
     * Each value is mapped to a formatted value. Additionally the returned map
     * should contain a logical ordering of values when iterated, for this
     * reasons most implementations will use a {@link LinkedHashMap} or a
     * {@link SortedMap}.
     */
    public Map<String, String> getFormattedValues(String key, Map<String, String> keyVals) throws Exception;

    /**
     * Provides a mechanism to convert a listing into a Map<String,
     * RequestConstraint> which can be used to perform further data query
     * operations.
     * 
     * @param keyVals
     *            a map of keys to values(chosen from previous calls to
     *            {@link #getKeys()} and {@link #getValues(String, Map)}
     * @return request constraints that can be used as part of a
     *         {@link DbQueryRequest} to get data for the provided key/val
     *         pairs.
     */
    public Map<String, RequestConstraint> getRequestConstraints(Map<String, String> keyVals);

}
