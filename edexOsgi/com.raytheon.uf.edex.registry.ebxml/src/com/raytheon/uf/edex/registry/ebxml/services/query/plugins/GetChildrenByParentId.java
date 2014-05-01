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
package com.raytheon.uf.edex.registry.ebxml.services.query.plugins;

import java.util.ArrayList;
import java.util.List;

import javax.jws.WebMethod;
import javax.jws.WebParam;
import javax.jws.WebResult;

import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.MsgRegistryException;
import oasis.names.tc.ebxml.regrep.xsd.query.v4.QueryRequest;
import oasis.names.tc.ebxml.regrep.xsd.query.v4.QueryResponse;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.OrganizationType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.QueryType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryObjectListType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryObjectType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryPackageType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.TaxonomyElementType;

import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import com.raytheon.uf.common.registry.EbxmlNamespaces;
import com.raytheon.uf.common.registry.constants.CanonicalQueryTypes;
import com.raytheon.uf.common.util.CollectionUtil;
import com.raytheon.uf.edex.registry.ebxml.dao.RegistryObjectDao;
import com.raytheon.uf.edex.registry.ebxml.dao.RegistryPackageDao;
import com.raytheon.uf.edex.registry.ebxml.services.query.QueryConstants;

/**
 * The canonical query GetChildrenByParentId allows clients to get the children
 * of a RegistryObject whose Id attribute value is the same as the value
 * specified for the parentId parameter. This query is used to query objects
 * hierarchies with parent-child relationships such as the following:
 * <p>
 * ClassificationScheme – Child ClassificationNodes
 * <p>
 * Organization – Child Organizations
 * <p>
 * RegistryPackage – RegistryPackage Members
 * <p>
 * <b>Query Semantics</b>
 * <p>
 * If objectType and parentId are both unspecified the server MUST return all
 * RegistryObjects that are not members of a RegistryPackage (root level
 * objects)
 * <p>
 * If parentId parameter is unspecified and objectType parameter is specified
 * the server MUST re - turn all root level objects for the object hierarchy
 * identified by the objectType as follows:
 * <p>
 * If objectType parameter value contains the string “ClassificationScheme” the
 * server MUST return all ClassificationSchemes
 * <p>
 * If objectType parameter value contains the string “Organization” the server
 * MUST return all Organizations that are not a member of another Organization
 * (root level Organizations)
 * <p>
 * If objectType parameter value contains the string “RegistryPackage” the
 * server MUST return all RegistryPackages that are not a member of another
 * RegistryPackage (root level RegistryPackages)
 * <p>
 * <p>
 * If parentId parameter is specified then the behavior is as follows:
 * <p>
 * If objectType parameter value is unspecified or if its value contains the
 * string “RegistryPackage” the server MUST return all RegistryObjects that are
 * member of a RegistryPackage whose id is the same as the value of the parentId
 * attribute
 * <p>
 * If objectType parameter is specified and its value contains the string
 * “ClassificationScheme” the server MUST return all ClassificationNodes that
 * are children of a TaxonomyElementType instance whose id is the same as the
 * value of the parentId attribute
 * <p>
 * If objectType parameter is specified and its value contains the string
 * “Organization” the server MUST return all Organizations that are members of
 * an Organization whose id is the same as the value of the parentId attribute
 * <p>
 * If depth parameter is specified then the server MUST also return all
 * descendants upto the specified depth as described by the definition of the
 * depth parameter above
 * <p>
 * If exclusiveChildrenOnly is specified with a true value then the server MUST
 * not return any descendants that have multiple parents
 * 
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 18, 2012            bphillip     Initial creation
 * 4/9/2013     1802       bphillip     Changed abstract method signature, modified return processing, and changed static variables
 * 10/8/2013    1682       bphillip    Refactored querying
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */
public class GetChildrenByParentId extends RegistryQueryPlugin {

    /** Data access object for accessing registry objects */
    protected RegistryObjectDao registryObjectDao;

    /** Data access object for getting registry packages */
    private RegistryPackageDao registryPackageDao;

    /** ClassificationScheme token */
    private static final String CLASSIFICATION_SCHEME = "ClassificationScheme";

    /** Organization token */
    private static final String ORGANIZATION = "Organization";

    /** RegistryPackage token */
    private static final String REGISTRY_PACKAGE = "RegistryPackage";

    /** Root level object query */
    private static final String ROOT_LEVEL_OBJECT_QUERY = "FROM RegistryObjectType obj where obj.id NOT IN (:ids)";

    private static final String ROOT_LEVEL_PACKAGE_QUERY = "FROM RegistryPackageType obj where obj.id NOT IN (:ids)";

    private static final String ROOT_LEVEL_ORG_QUERY = "FROM OrganizationType org where org.id NOT IN (:ids)";

    private static final String EXCLUSIVE_REGISTRY_PACKAGE_QUERY = ""
            + "FROM RegistryPackageType pack "
            + "INNER JOIN pack.registryObjectList as objList "
            + "INNER JOIN objList.registryObject as registryObject "
            + "WHERE registryObject.id = :id";

    private static final String EXCLUSIVE_CLASSIFICATION_QUERY = ""
            + "FROM ClassificationSchemeType obj"
            + "INNER JOIN obj.classificationNode as nodes "
            + "WHERE nodes.id = :id";

    private static final String EXCLUSIVE_ORGANIZATION_QUERY = ""
            + "FROM OrganizationType org"
            + "INNER JOIN org.organization as orgs " + "WHERE orgs.id = :id";

    @Override
    @WebMethod(action = EXECUTE_QUERY_ACTION)
    @WebResult(name = "QueryResponse", targetNamespace = EbxmlNamespaces.QUERY_URI, partName = "partQueryResponse")
    @Transactional(propagation = Propagation.MANDATORY, readOnly = true)
    public QueryResponse executeQuery(
            @WebParam(name = "QueryRequest", targetNamespace = EbxmlNamespaces.QUERY_URI, partName = "partQueryRequest") QueryRequest queryRequest)
            throws MsgRegistryException {
        QueryType queryType = queryRequest.getQuery();
        List<RegistryObjectType> retVal = new ArrayList<RegistryObjectType>();
        Integer depth = queryType.getSlotValue(QueryConstants.DEPTH);
        Boolean exclusiveChildrenOnly = queryType
                .getSlotValue(QueryConstants.EXCLUSIVE_CHILDREN_ONLY);
        String objectType = queryType.getSlotValue(QueryConstants.OBJECT_TYPE);
        String parentId = queryType.getSlotValue(QueryConstants.PARENT_ID);

        if (depth == null) {
            depth = 1;
        }
        if (exclusiveChildrenOnly == null) {
            exclusiveChildrenOnly = false;
        }

        if (parentId == null && objectType == null) {
            return createResponse(getRootLevelPackageObjects(false));
        } else if (parentId == null && objectType != null) {
            if (objectType.contains(CLASSIFICATION_SCHEME)) {
                retVal = getClassificationSchemes();
            } else if (objectType.contains(ORGANIZATION)) {
                retVal = getRootLevelOrganizationObjects();
            } else if (objectType.contains(REGISTRY_PACKAGE)) {
                retVal = getRootLevelPackageObjects(true);
            }
        } else if (parentId != null) {
            if (objectType == null) {
                objectType = REGISTRY_PACKAGE;
            }
            if (depth <= 0) {
                depth = Integer.MAX_VALUE;
            }
            List<String> parentIds = new ArrayList<String>();
            parentIds.add(parentId);
            for (int i = depth; i > 0; i--) {
                List<RegistryObjectType> descendants = getDescendants(
                        objectType, parentIds);
                retVal.addAll(descendants);
                parentIds.clear();
                for (RegistryObjectType descendant : descendants) {
                    parentIds.add(descendant.getId());
                }
            }
            if (exclusiveChildrenOnly) {
                retVal = getExclusiveChildren(objectType, retVal);
            }
        }

        return createResponse(retVal);
    }

    private List<RegistryObjectType> getDescendants(String objectType,
            List<String> parentIds) {
        List<RegistryObjectType> retVal = new ArrayList<RegistryObjectType>();
        for (String parentId : parentIds) {
            if (objectType.contains(REGISTRY_PACKAGE)) {
                RegistryPackageType registryPackage = registryPackageDao
                        .getById(parentId);
                if (registryPackage != null) {
                    RegistryObjectListType objList = registryPackage
                            .getRegistryObjectList();
                    if (objList != null) {
                        retVal.addAll(objList.getRegistryObject());
                    }
                }
            } else if (objectType.contains(CLASSIFICATION_SCHEME)) {
                TaxonomyElementType taxonomyElement = (TaxonomyElementType) registryObjectDao
                        .getById(parentId);
                if (taxonomyElement != null) {
                    retVal.addAll(taxonomyElement.getClassificationNode());
                }
            } else if (objectType.contains(ORGANIZATION)) {
                OrganizationType org = (OrganizationType) registryObjectDao
                        .getById(parentId);
                if (org != null) {
                    retVal.addAll(org.getOrganization());
                }
            }
        }
        return retVal;
    }

    private List<RegistryObjectType> getExclusiveChildren(String objectType,
            List<RegistryObjectType> objs) {
        List<RegistryObjectType> exclusiveChildren = new ArrayList<RegistryObjectType>();
        if (objectType.equals(REGISTRY_PACKAGE)) {
            for (RegistryObjectType obj : objs) {
                List<RegistryPackageType> parents = registryObjectDao
                        .executeHQLQuery(EXCLUSIVE_REGISTRY_PACKAGE_QUERY,
                                "id", obj.getId());
                if (parents.size() < 2) {
                    exclusiveChildren.add(obj);
                }
            }
        } else if (objectType.contains(CLASSIFICATION_SCHEME)) {
            for (RegistryObjectType obj : objs) {
                List<RegistryPackageType> parents = registryObjectDao
                        .executeHQLQuery(EXCLUSIVE_CLASSIFICATION_QUERY, "id",
                                obj.getId());
                if (parents.size() < 2) {
                    exclusiveChildren.add(obj);
                }
            }
        } else if (objectType.contains(ORGANIZATION)) {
            for (RegistryObjectType obj : objs) {
                List<RegistryPackageType> parents = registryObjectDao
                        .executeHQLQuery(EXCLUSIVE_ORGANIZATION_QUERY, "id",
                                obj.getId());
                if (parents.size() < 2) {
                    exclusiveChildren.add(obj);
                }
            }
        }
        return exclusiveChildren;

    }

    private List<RegistryObjectType> getRootLevelPackageObjects(
            boolean packagesOnly) {
        List<RegistryPackageType> registryPackages = registryPackageDao
                .getAll();
        List<String> nonRootLevelObjects = new ArrayList<String>();
        for (RegistryPackageType registryPackage : registryPackages) {
            RegistryObjectListType objList = registryPackage
                    .getRegistryObjectList();
            if (objList != null) {
                for (RegistryObjectType obj : objList.getRegistryObject()) {
                    nonRootLevelObjects.add(obj.getId());
                }
            }
        }
        if (nonRootLevelObjects.isEmpty()) {
            nonRootLevelObjects.add("---dummyentry---");
        }
        if (packagesOnly) {
            return registryObjectDao.executeHQLQuery(ROOT_LEVEL_PACKAGE_QUERY,
                    "ids", nonRootLevelObjects);
        } else {
            return registryObjectDao.executeHQLQuery(ROOT_LEVEL_OBJECT_QUERY,
                    "ids", nonRootLevelObjects);
        }
    }

    private List<RegistryObjectType> getRootLevelOrganizationObjects() {
        List<OrganizationType> orgs = registryObjectDao
                .executeHQLQuery("FROM OrganizationType");
        List<String> nonRootLevelObjects = new ArrayList<String>();
        for (OrganizationType org : orgs) {
            List<OrganizationType> subOrgs = org.getOrganization();
            if (!CollectionUtil.isNullOrEmpty(subOrgs)) {
                for (OrganizationType subOrg : subOrgs) {
                    nonRootLevelObjects.add(subOrg.getId());
                }
            }
        }
        if (nonRootLevelObjects.isEmpty()) {
            nonRootLevelObjects.add("---dummyentry---");
        }
        return registryObjectDao.executeHQLQuery(ROOT_LEVEL_ORG_QUERY, "ids",
                nonRootLevelObjects);
    }

    private List<RegistryObjectType> getClassificationSchemes() {
        return registryObjectDao
                .executeHQLQuery("FROM ClassificationSchemeType");

    }

    @Override
    public String getQueryDefinition() {
        return CanonicalQueryTypes.GET_CHILDREN_BY_PARENT_ID;
    }

    public void setRegistryPackageDao(RegistryPackageDao registryPackageDao) {
        this.registryPackageDao = registryPackageDao;
    }

    public void setRegistryObjectDao(RegistryObjectDao registryObjectDao) {
        this.registryObjectDao = registryObjectDao;
    }

}
