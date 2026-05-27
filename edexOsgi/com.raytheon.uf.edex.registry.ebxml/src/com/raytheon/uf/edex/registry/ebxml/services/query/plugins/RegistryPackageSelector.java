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

import jakarta.jws.WebMethod;
import jakarta.jws.WebParam;
import jakarta.jws.WebResult;

import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import com.raytheon.uf.common.registry.constants.AssociationTypes;
import com.raytheon.uf.common.registry.constants.CanonicalQueryTypes;
import com.raytheon.uf.common.registry.schemas.ebxml.util.EbxmlNamespaces;
import com.raytheon.uf.edex.registry.ebxml.dao.RegistryObjectDao;
import com.raytheon.uf.edex.registry.ebxml.dao.RegistryPackageDao;
import com.raytheon.uf.edex.registry.ebxml.services.query.QueryConstants;

import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.MsgRegistryException;
import oasis.names.tc.ebxml.regrep.xsd.query.v4.QueryRequest;
import oasis.names.tc.ebxml.regrep.xsd.query.v4.QueryResponse;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.AssociationType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.QueryType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryObjectType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryPackageType;

/**
 * Query type allowing the client to select registry packages
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 18, 2012            bphillip    Initial creation
 * 3/18/2013    1802       bphillip    Modified to use transaction boundaries and spring dao injection
 * 4/9/2013     1802       bphillip    Changed abstract method signature, modified return processing, and changed static variables
 * 10/8/2013    1682       bphillip    Refactored querying
 * 12/09/2021   7849       mapeters    Remove unreachable catch block
 *
 * </pre>
 *
 * @author bphillip
 */
public class RegistryPackageSelector extends RegistryQueryPlugin {

    /** Data access object for accessing registry objects */
    private RegistryObjectDao registryObjectDao;

    /** Registry package data access object */
    private RegistryPackageDao registryPackageDao;

    /** Gets a package based on association type and the source object */
    private static final String PACKAGE_ASSOCIATION_QUERY = "FROM AssociationType obj "
            + "WHERE obj.type=:associationType "
            + "AND obj.sourceObject=:packageSource";

    @Override
    @WebMethod(action = EXECUTE_QUERY_ACTION)
    @WebResult(name = "QueryResponse", targetNamespace = EbxmlNamespaces.QUERY_URI, partName = "partQueryResponse")
    @Transactional(propagation = Propagation.MANDATORY, readOnly = true)
    public QueryResponse executeQuery(
            @WebParam(name = "QueryRequest", targetNamespace = EbxmlNamespaces.QUERY_URI, partName = "partQueryRequest")
            QueryRequest queryRequest) throws MsgRegistryException {
        QueryType queryType = queryRequest.getQuery();
        List<String> packageIds = queryType
                .getSlotValueAsList(QueryConstants.REGISTRY_PACKAGE_IDS);

        List<RegistryObjectType> retVal = new ArrayList<>();

        List<RegistryPackageType> registryPackages = registryPackageDao
                .getById(packageIds);

        for (RegistryPackageType registryPackage : registryPackages) {
            if (registryPackage.getRegistryObjectList() != null) {
                retVal.addAll(registryPackage.getRegistryObjectList()
                        .getRegistryObject());
            }
            registryPackage.setRegistryObjectList(null);
            retVal.add(registryPackage);

            List<AssociationType> associations = registryObjectDao
                    .executeHQLQuery(PACKAGE_ASSOCIATION_QUERY,
                            "associationType", AssociationTypes.HAS_MEMBER,
                            "packageSource", registryPackage.getId());
            retVal.addAll(associations);
        }

        return createResponse(retVal);
    }

    @Override
    public String getQueryDefinition() {
        return CanonicalQueryTypes.REGISTRY_PACKAGE_SELECTOR;
    }

    public void setRegistryPackageDao(RegistryPackageDao registryPackageDao) {
        this.registryPackageDao = registryPackageDao;
    }

    public void setRegistryObjectDao(RegistryObjectDao registryObjectDao) {
        this.registryObjectDao = registryObjectDao;
    }
}