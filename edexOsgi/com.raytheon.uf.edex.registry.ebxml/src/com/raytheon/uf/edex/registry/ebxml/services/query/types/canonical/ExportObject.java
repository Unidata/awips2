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

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;

import oasis.names.tc.ebxml.regrep.xsd.query.v4.QueryResponse;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.ClassificationNodeType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.OrganizationType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.QueryType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryObjectType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.TaxonomyElementType;

import com.raytheon.uf.edex.registry.ebxml.dao.RegistryObjectTypeDao;
import com.raytheon.uf.edex.registry.ebxml.exception.EbxmlRegistryException;
import com.raytheon.uf.edex.registry.ebxml.services.query.QueryConstants;
import com.raytheon.uf.edex.registry.ebxml.services.query.QueryParameters;
import com.raytheon.uf.edex.registry.ebxml.services.query.types.CanonicalEbxmlQuery;

/**
 * Parameterized Query to export an Object and its descendent tree. Used by tree
 * structures such as RegistryPackage, TaxonomyElement, Organization
 * <p>
 * <b>Parameter Summary:</b> <br>
 * · <b><i>id</i></b> -- ID of desired object. Use '%' and '_' as wildcard to
 * match multiple and single characters respectively.
 * <p>
 * · <b><i>depth</i></b> -- Specifies the depth or number of levels of children
 * to fetch. A depth of 0 (default) indicates that the server MUST return only
 * those objects that match the id parameter (root objects). A depth of N where
 * N is greater that 0 indicates that the server MUST also return objects that
 * are descendants of root objects upto N levels. A depth of -1 indicates that
 * the server MUST return all descendent objects as well. Descendents MUST be
 * returned using the nested syntax supported by objects capable of having
 * children.
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

public class ExportObject extends CanonicalEbxmlQuery {

    public static final String QUERY_DEFINITION = QUERY_CANONICAL_PREFIX
            + "ExportObject";

    private static final BigInteger DEFAULT_DEPTH = new BigInteger("0");

    /** The list of valid parameters for this query */
    private static final List<String> QUERY_PARAMETERS = new ArrayList<String>();

    private RegistryObjectTypeDao registryObjectDao = new RegistryObjectTypeDao();

    /* Initializes the list of parameters */
    static {
        QUERY_PARAMETERS.add(QueryConstants.ID);
        QUERY_PARAMETERS.add(QueryConstants.DEPTH);
    }

    @Override
    protected List<RegistryObjectType> query(QueryType queryType,
            QueryResponse queryResponse) throws EbxmlRegistryException {
        List<RegistryObjectType> retVal = new ArrayList<RegistryObjectType>();

        QueryParameters parameters = getParameterMap(queryType.getSlot(),
                queryResponse);
        if (!parameters.containsParameter(QueryConstants.ID)) {
            throw new EbxmlRegistryException("Canonical query ["
                    + this.getQueryDefinition()
                    + "] is missing required parameter [" + QueryConstants.ID
                    + "]");
        }
        String id = parameters.getFirstParameter(QueryConstants.ID);
        int depth = parameters.getFirstParameter(QueryConstants.DEPTH,
                DEFAULT_DEPTH).intValue();

        List<String> ids = registryObjectDao.getMatchingIds(id);

        if (!ids.isEmpty()) {
            List<RegistryObjectType> objects = registryObjectDao.getById(ids);

            /*
             * Indicates that the server MUST return only those objects that
             * match the id parameter (root objects)
             */
            if (depth == 0) {
                for (RegistryObjectType obj : objects) {
                    getChildren(obj, retVal, 0, 1);
                    retVal.add(obj);
                }
            }
            /*
             * indicates that the server MUST return all descendant objects as
             * well. Descendants MUST be returned using the nested syntax
             * supported by objects capable of having children.
             */
            else if (depth == -1) {
                for (RegistryObjectType obj : objects) {
                    getChildren(obj, retVal, depth, -1);
                    retVal.add(obj);
                }
            }
            /*
             * Indicates that the server MUST also return objects that are
             * descendants of root objects up to depth levels
             */
            else if (depth > 0) {
                for (RegistryObjectType obj : objects) {
                    getChildren(obj, retVal, depth, 1);
                    retVal.add(obj);
                }
            } else {
                throw new EbxmlRegistryException("Canonical query ["
                        + this.getQueryDefinition()
                        + "] received invalid value for the ["
                        + QueryConstants.DEPTH + "] parameter. " + depth
                        + " is not a valid value.  Values must be >= -1");
            }
            for (int i = 0; i < retVal.size(); i++) {
                TaxonomyElementType element = null;
                if (retVal.get(i) instanceof TaxonomyElementType) {
                    element = (TaxonomyElementType) retVal.get(i);
                    registryObjectDao.evict(element);
                    element.getClassificationNode().clear();
                }
            }
        }
        return retVal;
    }

    @Override
    protected List<String> getValidParameters() {
        return QUERY_PARAMETERS;
    }

    /**
     * Recursively removes the children objects of the given object if the
     * targetLevel is equal to the current level.
     * 
     * @param obj
     *            The current object
     * @param targetLevel
     *            The level at which to remove children
     * @param currentLevel
     *            The current level in the object tree
     * @throws EbxmlRegistryException
     *             If the properties of the object are unable to be examined.
     */
    private void getChildren(RegistryObjectType obj,
            List<RegistryObjectType> objList, int targetLevel, int currentLevel)
            throws EbxmlRegistryException {
        if (obj instanceof TaxonomyElementType) {
            TaxonomyElementType taxObj = (TaxonomyElementType) obj;
            Set<ClassificationNodeType> nodes = taxObj.getClassificationNode();
            for (ClassificationNodeType node : nodes) {
                if (currentLevel <= targetLevel || targetLevel == -1) {
                    objList.add(node);
                }
                if (currentLevel < targetLevel || targetLevel == -1) {
                    getChildren(node, objList, targetLevel, currentLevel + 1);
                }
            }
        } else if (obj instanceof OrganizationType) {
            OrganizationType taxObj = (OrganizationType) obj;
            List<OrganizationType> orgs = taxObj.getOrganization();
            for (OrganizationType node : orgs) {
                if (currentLevel <= targetLevel || targetLevel == -1) {
                    objList.add(node);
                }
                if (currentLevel < targetLevel || targetLevel == -1) {
                    getChildren(node, objList, targetLevel, currentLevel + 1);
                }
            }
            orgs.clear();
        }
    }

    @Override
    public String getQueryDefinition() {
        return QUERY_DEFINITION;
    }
}
