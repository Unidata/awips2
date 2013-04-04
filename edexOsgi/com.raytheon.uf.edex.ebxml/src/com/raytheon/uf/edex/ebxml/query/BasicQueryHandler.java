/**
 * 
 */
package com.raytheon.uf.edex.ebxml.query;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import oasis.names.tc.ebxml.regrep.xsd.rim.v4.QueryType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryObjectType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.SlotType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.StringValueType;

import com.raytheon.uf.edex.ebxml.registry.IRegistry;
import com.raytheon.uf.edex.ebxml.registry.RegistryManager;

/**
 * Handles query logic for "BasicQuery" queries.
 * 
 * @author jsherida
 */
public class BasicQueryHandler implements IQueryHandler {

    public static final String QUERY_DEFINITION = "urn:oasis:names:tc:ebxml-regrep:query:BasicQuery";

    private static final List<String> BASIC_QUERY_PARAMETERS = new ArrayList<String>();
    static {
        BASIC_QUERY_PARAMETERS.add("name");
        BASIC_QUERY_PARAMETERS.add("description");
        BASIC_QUERY_PARAMETERS.add("type");
        BASIC_QUERY_PARAMETERS.add("status");
        BASIC_QUERY_PARAMETERS.add("classifications");
        BASIC_QUERY_PARAMETERS.add("matchOnAnyParameter");
    }

    /** {@inheritDoc} */
    @Override
    public String getQueryDefinition() {
        return QUERY_DEFINITION;
    }

    @Override
    public List<RegistryObjectType> handleQuery(QueryType query,
            boolean federated, String federation, long startIndex,
            long maxResults, long depth, boolean matchOlderVersions)
            throws IOException {
        Map<String, List<Object>> baseParameters = new HashMap<String, List<Object>>();
        Boolean matchAny = false;

        Collection<SlotType> slots = query.getSlot();
        for (SlotType slot : slots) {
            String param = slot.getName();
            if (BASIC_QUERY_PARAMETERS.contains(param)) {
                if (param.equals("matchOnAnyParameter")) {
                    matchAny = true;
                    continue;
                }
                List<Object> values = baseParameters.get(param);
                if (values == null) {
                    values = new ArrayList<Object>();
                    baseParameters.put(param, values);
                }

                values.add(((StringValueType) slot.getSlotValue()).getValue());
            }
        }

        IRegistry registry = RegistryManager.getRegistryInstance();
        return registry.query(baseParameters, matchAny, startIndex, maxResults,
                depth, matchOlderVersions);
    }

}
