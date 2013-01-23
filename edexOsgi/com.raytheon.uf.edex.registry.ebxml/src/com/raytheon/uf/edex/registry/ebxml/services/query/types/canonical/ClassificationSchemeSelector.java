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

import java.util.ArrayList;
import java.util.List;
import java.util.Set;

import oasis.names.tc.ebxml.regrep.xsd.query.v4.QueryResponse;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.ClassificationNodeType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.ClassificationSchemeType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.QueryType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryObjectType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.TaxonomyElementType;

import com.raytheon.uf.edex.registry.ebxml.dao.RegistryObjectTypeDao;
import com.raytheon.uf.edex.registry.ebxml.exception.EbxmlRegistryException;
import com.raytheon.uf.edex.registry.ebxml.services.query.QueryConstants;
import com.raytheon.uf.edex.registry.ebxml.services.query.QueryParameters;
import com.raytheon.uf.edex.registry.ebxml.services.query.types.CanonicalEbxmlQuery;

/**
 * The canonical query ClassificationSchemeSelector allows clients to create a
 * Subscription to a remote server to replicate a remote ClassificationScheme.
 * This query may be used as Selector query in the subscription as defined in
 * the object replication feature.
 * <p>
 * <b>Parameter Summary:</b> <br>
 * · <b><i>classificationSchemeId</i></b> -- Matches
 * rim:RegistryObject[@xsi:type="rim:ClassificationSchemeType"]/@id.<br>
 * 
 * Does not allow wildcards.
 * <p>
 * · The server MUST return the specified ClassificationScheme and all
 * ClassificationNodes that are descendants of that ClassificationScheme.
 * <p>
 * 
 * · The ClassificationNodes MUST NOT be returned as nested elements inside
 * their parent Taxonomy element. Instead they MUST be returned as sibling
 * elements with the RegistryObjectList element of the QueryResponse.
 * <p>
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 2/13/2012    #184       bphillip    Initial creation
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */

public class ClassificationSchemeSelector extends CanonicalEbxmlQuery {

    public static final String QUERY_DEFINITION = QUERY_CANONICAL_PREFIX
            + "ClassificationSchemeSelector";

    /** The valid query parameter for this query **/
    private static final List<String> QUERY_PARAMETERS = new ArrayList<String>();
    static {
        QUERY_PARAMETERS.add(QueryConstants.CLASSIFICATION_SCHEME_ID);
    }

    @Override
    protected List<RegistryObjectType> query(QueryType queryType,
            QueryResponse queryResponse) throws EbxmlRegistryException {
        List<RegistryObjectType> retVal = new ArrayList<RegistryObjectType>();
        QueryParameters parameters = this.getParameterMap(queryType.getSlot(),
                queryResponse);

        // The client did not specify the required parameter
        if (parameters.isEmpty()) {
            throw new EbxmlRegistryException("Canonical query ["
                    + this.getQueryDefinition()
                    + "] is missing required parameter ["
                    + QUERY_PARAMETERS.get(0) + "]");
        }

        /*
         * Get the classification scheme and recursively extract the nodes into
         * a list as per the requirements of this canonical query described in
         * the class description
         */
        RegistryObjectTypeDao registryObjectDao = new RegistryObjectTypeDao(
                ClassificationSchemeType.class);
        ClassificationSchemeType classificationScheme = registryObjectDao
                .getById((String) parameters
                        .getFirstParameter(QueryConstants.CLASSIFICATION_SCHEME_ID));
        getNodeList(classificationScheme, retVal);
        return retVal;
    }

    /**
     * Recursive method to extract the ClassificationNodeType objects from a
     * TaxonomyElementType object and place them into a list.
     * 
     * @param node
     *            The node for which to get the classificationNodes
     * @param retVal
     *            The list of ClassificationNodeType objects
     */
    private void getNodeList(TaxonomyElementType node,
            List<RegistryObjectType> retVal) {
        if (node == null) {
            return;
        }
        retVal.add(node);
        Set<ClassificationNodeType> nodes = node.getClassificationNode();
        if (nodes != null) {
            for (ClassificationNodeType nodeItem : nodes) {
                getNodeList(nodeItem, retVal);
            }
        }
    }

    @Override
    protected List<String> getValidParameters() {
        return QUERY_PARAMETERS;
    }

    @Override
    public String getQueryDefinition() {
        return QUERY_DEFINITION;
    }
}
