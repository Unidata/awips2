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
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.AssociationType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryObjectType;

import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import com.raytheon.uf.common.registry.EbxmlNamespaces;
import com.raytheon.uf.edex.registry.ebxml.dao.RegistryObjectDao;

/**
 * Garbage collector implementation to purge items from the registry
 * <p>
 * The Garbage collector operates on these rules: <br>
 * The server MAY return any objects it considers as garbage or no longer
 * relavant or needed<br>
 * The definition of what objects are garbage may be implementation, profile, or
 * deployment specific<br>
 * The server MUST return the following types of objects<br>
 * --Dangling Associations - AssociationType instances that have an unresolvable
 * or null sourceObject or targetObject attribute
 * <p>
 * This implementation only checks for dangling associations
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
public class GarbageCollectorQueryPlugin extends RegistryQueryPlugin {

    /** Data access object for accessing registry objects */
    protected RegistryObjectDao registryObjectDao;

    @Override
    @WebMethod(action = EXECUTE_QUERY_ACTION)
    @WebResult(name = "QueryResponse", targetNamespace = EbxmlNamespaces.QUERY_URI, partName = "partQueryResponse")
    @Transactional(propagation = Propagation.MANDATORY, readOnly = true)
    public QueryResponse executeQuery(
            @WebParam(name = "QueryRequest", targetNamespace = EbxmlNamespaces.QUERY_URI, partName = "partQueryRequest") QueryRequest queryRequest)
            throws MsgRegistryException {
        List<RegistryObjectType> retVal = new ArrayList<RegistryObjectType>();
        List<AssociationType> associations = registryObjectDao
                .executeHQLQuery("FROM AssociationType");

        for (AssociationType association : associations) {
            String sourceObject = association.getSourceObject();
            String targetObject = association.getTargetObject();
            if ((sourceObject == null || registryObjectDao
                    .getById(sourceObject) == null)
                    || (targetObject == null || registryObjectDao
                            .getById(targetObject) == null)) {
                retVal.add(association);
            }
        }
        return createResponse(retVal);
    }

    public void setRegistryObjectDao(RegistryObjectDao registryObjectDao) {
        this.registryObjectDao = registryObjectDao;
    }

}
