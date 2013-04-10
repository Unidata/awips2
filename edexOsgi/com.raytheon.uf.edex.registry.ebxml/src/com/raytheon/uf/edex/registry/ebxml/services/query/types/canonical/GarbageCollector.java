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
package com.raytheon.uf.edex.registry.ebxml.services.query.types.canonical;

import java.util.Collections;
import java.util.List;

import oasis.names.tc.ebxml.regrep.xsd.query.v4.QueryResponse;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.QueryType;

import com.raytheon.uf.common.registry.constants.CanonicalQueryTypes;
import com.raytheon.uf.edex.registry.ebxml.exception.EbxmlRegistryException;
import com.raytheon.uf.edex.registry.ebxml.services.query.types.CanonicalEbxmlQuery;

/**
 * Garbage collector implementation to purge items from the registry
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 18, 2012            bphillip     Initial creation
 * 4/9/2013     1802       bphillip     Changed abstract method signature, modified return processing, and changed static variables
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */

public class GarbageCollector extends CanonicalEbxmlQuery {

    @Override
    public String getQueryDefinition() {
        return CanonicalQueryTypes.GARBAGE_COLLECTOR;
    }

    @Override
    protected void query(QueryType queryType, QueryResponse queryResponse)
            throws EbxmlRegistryException {
        // TODO:Implement
    }

    @Override
    protected List<String> getValidParameters() {
        return Collections.emptyList();
    }

}
