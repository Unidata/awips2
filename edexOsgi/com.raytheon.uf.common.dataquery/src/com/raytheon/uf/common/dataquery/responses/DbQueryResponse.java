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
package com.raytheon.uf.common.dataquery.responses;

import java.lang.reflect.Array;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * DbQueryResponse object is returned from DbQueryRequest. Contains a
 * List<Map<String,Object>> where each Map in the List is a row returned from
 * the query and you can use the field strings from the request to get the
 * object out of the Map
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Jan 21, 2010           mschenke    Initial creation
 * Dec 18, 2013  2579     bsteffen    Remove ISerializableObject
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */
@DynamicSerialize
public class DbQueryResponse {

    public static final String ENTITY_RESULT_KEY = null;

    @DynamicSerializeElement
    private List<Map<String, Object>> results;

    public List<Map<String, Object>> getResults() {
        return results == null ? new ArrayList<Map<String, Object>>() : results;
    }

    public void setResults(List<Map<String, Object>> results) {
        this.results = results;
    }

    public int getNumResults() {
        return getResults().size();
    }

    public <T> T[] getEntityObjects(Class<T> entityType) {
        return getFieldObjects(ENTITY_RESULT_KEY, entityType);
    }

    @SuppressWarnings("unchecked")
    public <T> T[] getFieldObjects(String fieldKey, Class<T> fieldType) {
        List<Map<String, Object>> results = getResults();
        T[] entities = (T[]) Array.newInstance(fieldType, results.size());
        int i = 0;
        for (Map<String, Object> result : results) {
            entities[i++] = fieldType.cast(result.get(fieldKey));
        }
        return entities;
    }
}
