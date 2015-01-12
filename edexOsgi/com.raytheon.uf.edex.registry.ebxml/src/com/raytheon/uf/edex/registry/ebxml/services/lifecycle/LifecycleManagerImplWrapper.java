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
package com.raytheon.uf.edex.registry.ebxml.services.lifecycle;

import javax.jws.WebMethod;
import javax.jws.WebParam;
import javax.jws.WebResult;

import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.LifecycleManager;
import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.MsgRegistryException;
import oasis.names.tc.ebxml.regrep.xsd.lcm.v4.RemoveObjectsRequest;
import oasis.names.tc.ebxml.regrep.xsd.lcm.v4.SubmitObjectsRequest;
import oasis.names.tc.ebxml.regrep.xsd.lcm.v4.UpdateObjectsRequest;
import oasis.names.tc.ebxml.regrep.xsd.rs.v4.RegistryResponseType;

import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import com.raytheon.uf.common.registry.schemas.ebxml.util.EbxmlNamespaces;

/**
 * 
 * Wrapper for the lifecyclemanager service to be used with the SOAP interface.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 7/11/2013    1707        bphillip    Initial implementation
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
@Transactional(propagation = Propagation.REQUIRED)
public class LifecycleManagerImplWrapper implements LifecycleManager {

    private LifecycleManagerImpl lifecycleManager;

    public LifecycleManagerImplWrapper() {

    }

    public LifecycleManagerImplWrapper(LifecycleManagerImpl lifecycleManager) {
        this.lifecycleManager = lifecycleManager;
    }

    @Override
    @WebMethod(action = REMOVE_OBJECTS_ACTION)
    @WebResult(name = "RegistryResponse", targetNamespace = EbxmlNamespaces.RS_URI, partName = "partRegistryResponse")
    public RegistryResponseType removeObjects(
            @WebParam(name = "RemoveObjectsRequest", targetNamespace = EbxmlNamespaces.LCM_URI, partName = "partRemoveObjectsRequest") RemoveObjectsRequest partRemoveObjectsRequest)
            throws MsgRegistryException {
        return lifecycleManager.removeObjects(partRemoveObjectsRequest);
    }

    @Override
    @WebMethod(action = SUBMIT_OBJECTS_ACTION)
    @WebResult(name = "RegistryResponse", targetNamespace = EbxmlNamespaces.RS_URI, partName = "partRegistryResponse")
    public RegistryResponseType submitObjects(
            @WebParam(name = "SubmitObjectsRequest", targetNamespace = EbxmlNamespaces.LCM_URI, partName = "partSubmitObjectsRequest") SubmitObjectsRequest partSubmitObjectsRequest)
            throws MsgRegistryException {
        return lifecycleManager.submitObjects(partSubmitObjectsRequest);
    }

    @Override
    @WebMethod(action = UPDATE_OBJECTS_ACTION)
    @WebResult(name = "RegistryResponse", targetNamespace = EbxmlNamespaces.RS_URI, partName = "partRegistryResponse")
    public RegistryResponseType updateObjects(
            @WebParam(name = "UpdateObjectsRequest", targetNamespace = EbxmlNamespaces.LCM_URI, partName = "partUpdateObjectsRequest") UpdateObjectsRequest partUpdateObjectsRequest)
            throws MsgRegistryException {
        return lifecycleManager.updateObjects(partUpdateObjectsRequest);
    }

}
