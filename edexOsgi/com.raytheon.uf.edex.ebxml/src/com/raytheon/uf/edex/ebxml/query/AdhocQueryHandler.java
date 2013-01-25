/*
 * The following software products were developed by Raytheon:
 *
 * ADE (AWIPS Development Environment) software
 * CAVE (Common AWIPS Visualization Environment) software
 * EDEX (Environmental Data Exchange) software
 * uFrame™ (Universal Framework) software
 *
 * Copyright (c) 2010 Raytheon Co.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/org/documents/epl-v10.php
 *
 *
 * Contractor Name: Raytheon Company
 * Contractor Address:
 * 6825 Pine Street, Suite 340
 * Mail Stop B8
 * Omaha, NE 68106
 * 402.291.0100
 *
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 25, 2011            jsherida     Initial creation
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

import com.raytheon.uf.edex.ebxml.registry.IRegistry;
import com.raytheon.uf.edex.ebxml.registry.RegistryManager;

/**
 * Handles query logic for "AdhocQuery" queries.
 * 
 * @author jsherida
 * @version 1.0
 */
public class AdhocQueryHandler implements IQueryHandler {

    public static final String QUERY_DEFINITION = "urn:oasis:names:tc:ebxml-regrep:query:AdhocQuery";

    /** {@inheritDoc} */
    @Override
    public String getQueryDefinition() {
        return QUERY_DEFINITION;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.ebxml.query.IQueryHandler#handleQuery(oasis.names
     * .tc.ebxml_regrep.xsd.rim._4.QueryType, boolean, java.lang.String, long,
     * long, long, boolean)
     */
    @Override
    public List<RegistryObjectType> handleQuery(QueryType query,
            boolean federated, String federation, long startIndex,
            long maxResults, long depth, boolean matchOlderVersions)
            throws IOException {
        Map<String, List<Object>> parameters = new HashMap<String, List<Object>>();
        Collection<SlotType> slots = query.getSlot();

        for (SlotType slot : slots) {
            String param = slot.getName();
            List<Object> values = parameters.get(param);
            if (values == null) {
                values = new ArrayList<Object>();
                parameters.put(param, values);
            }

            // if (IRegistry.QUERY_EXPR_PARAM.equals(param)
            // || IRegistry.QUERY_LANG_PARAM.equals(param)) {
            // String classValue = ((StringValueType)
            // slot.getSlotValue()).getValue();
            // values.add(classValue);
            // }
        }

        IRegistry registry = RegistryManager.getRegistryInstance();
        return registry.query(parameters, false, startIndex, maxResults, depth,
                matchOlderVersions);
    }

}
