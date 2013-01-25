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
package com.raytheon.uf.edex.registry.ebxml.services.query.types;

import oasis.names.tc.ebxml.regrep.xsd.query.v4.QueryResponse;
import oasis.names.tc.ebxml.regrep.xsd.query.v4.ResponseOptionType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.QueryType;
import oasis.names.tc.ebxml.regrep.xsd.rs.v4.UnsupportedCapabilityExceptionType;

import com.raytheon.uf.edex.registry.ebxml.constants.ErrorSeverity;
import com.raytheon.uf.edex.registry.ebxml.util.EbxmlExceptionUtil;

/**
 * Implementation of a custom ebXML query type
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 18, 2012 184        bphillip     Initial creation
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */

public abstract class ExtendedEbxmlQuery extends AbstractEbxmlQuery {

    protected static final String QUERY_EXTENSION_PREFIX = "urn:raytheon:names:tc:ebxml-regrep:query:";

    public String getQueryDefinitionPrefix() {
        return QUERY_EXTENSION_PREFIX;
    }

    public String getQueryDefinition() {
        return QUERY_EXTENSION_PREFIX + this.getClass().getSimpleName();
    }

    public boolean isCanonical() {
        return false;
    }

    public void execute(QueryResponse response,
            ResponseOptionType responseOption, QueryType query, int depth,
            boolean matchOlderVersions, int maxResults, int startIndex) {
        response.getException()
                .add(EbxmlExceptionUtil
                        .createRegistryException(
                                UnsupportedCapabilityExceptionType.class,
                                "",
                                "This query type is currently unsupported",
                                "This exception is thrown if the query type has not been registered or properly implemented",
                                ErrorSeverity.ERROR, statusHandler));
    }

}
