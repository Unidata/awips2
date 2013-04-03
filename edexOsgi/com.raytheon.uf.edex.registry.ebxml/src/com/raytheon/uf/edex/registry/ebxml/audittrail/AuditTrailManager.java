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
package com.raytheon.uf.edex.registry.ebxml.audittrail;

import java.util.ArrayList;
import java.util.List;

import javax.xml.datatype.DatatypeConfigurationException;

import oasis.names.tc.ebxml.regrep.xsd.rim.v4.ActionType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.AuditableEventType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.ObjectRefListType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.ObjectRefType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryObjectListType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryObjectType;
import oasis.names.tc.ebxml.regrep.xsd.rs.v4.RegistryResponseType;

import com.raytheon.uf.edex.registry.ebxml.constants.RegistryObjectTypes;
import com.raytheon.uf.edex.registry.ebxml.constants.StatusTypes;
import com.raytheon.uf.edex.registry.ebxml.dao.RegistryObjectTypeDao;
import com.raytheon.uf.edex.registry.ebxml.exception.EbxmlRegistryException;
import com.raytheon.uf.edex.registry.ebxml.util.EbxmlObjectUtil;

/**
 * 
 * Class used for submitting auditable events.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 28, 2012 #363       bphillip     Initial creation
 * 8/3/2012     724        bphillip    Modified to use predefined constants
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */
public class AuditTrailManager {

    /** The data access object used for manipulating registry objects */
    private RegistryObjectTypeDao dao;

    /**
     * Creates an audit trail entry from a RegistryResponseType
     * 
     * @param response
     *            The RegistryResponseType object from which to get the
     *            auditable information
     * @param actionType
     *            The action taken on the objects referenced by the response
     *            object
     * @throws EbxmlRegistryException
     *             If errors occur while querying or submitting the auditable
     *             event
     */
    public void createAuditTrailFromResponse(RegistryResponseType response,
            String actionType) throws EbxmlRegistryException {
        ObjectRefListType refList = response.getObjectRefList();
        RegistryObjectListType regObjList = response.getRegistryObjectList();

        List<RegistryObjectType> objs = new ArrayList<RegistryObjectType>();
        if (refList != null) {
            List<String> objIds = new ArrayList<String>();
            for (ObjectRefType ref : refList.getObjectRef()) {
                objIds.add(ref.getId());
            }
            objs.addAll(dao.getById(objIds));
        }

        if (regObjList != null) {
            objs.addAll(regObjList.getRegistryObject());
        }
        createAuditTrail(objs, actionType, response.getRequestId(),
                System.currentTimeMillis());
    }

    /**
     * Creates an audit trail entry from a list of registry objects
     * 
     * @param objs
     *            The list of objects to generate an audit trail for
     * @param actionType
     *            The action taken on the objects in the registry object list
     * @param requestId
     *            The requestId of the request which modified the objects in the
     *            registry object list
     * @throws EbxmlRegistryException
     *             If errors occur during the submission of the audit trail
     *             entry
     */
    public void createAuditTrail(List<RegistryObjectType> objs,
            String actionType, String requestId) throws EbxmlRegistryException {
        createAuditTrail(objs, actionType, requestId,
                System.currentTimeMillis());
    }

    /**
     * Creates an audit trail entry from a list of registry objects
     * 
     * @param objs
     *            The list of objects to generate an audit trail for
     * @param actionType
     *            The action taken on the objects in the registry object list
     * @param requestId
     *            The requestId of the request which modified the objects in the
     *            registry object list
     * @param time
     *            The time at which the modifications to the registry objects
     *            occurred
     * @throws EbxmlRegistryException
     *             If errors occur during the submission of the audit trail
     *             entry
     */
    public void createAuditTrail(List<RegistryObjectType> objs,
            String actionType, String requestId, long time)
            throws EbxmlRegistryException {
        List<RegistryObjectType> objsToRemove = new ArrayList<RegistryObjectType>();
        for (RegistryObjectType obj : objs) {
            if (obj instanceof AuditableEventType) {
                objsToRemove.add(obj);
            }
        }
        objs.removeAll(objsToRemove);
        if (objs.isEmpty()) {
            return;
        }
        AuditableEventType event = EbxmlObjectUtil.rimObjectFactory
                .createAuditableEventType();

        event.setId(EbxmlObjectUtil.getUUID());
        event.setLid(EbxmlObjectUtil.getUUID());
        event.setOwner(objs.get(0).getOwner());
        event.setObjectType(RegistryObjectTypes.AUDITABLE_EVENT);
        ActionType action = EbxmlObjectUtil.rimObjectFactory.createActionType();
        action.setAffectedObjects(EbxmlObjectUtil
                .createRegistryObjectList(objs));
        action.setEventType(actionType);
        event.getAction().add(action);
        event.setRequestId(requestId);
        try {
            event.setTimestamp(EbxmlObjectUtil.getTime(time));
        } catch (DatatypeConfigurationException e) {
            throw new EbxmlRegistryException(
                    "Error creating timestamp for auditable event", e);
        }
        event.setUser("Client");
        event.setStatus(StatusTypes.APPROVED);
        event.setVersionInfo(EbxmlObjectUtil.newVersionObject());
        dao.save(event);
    }

    /**
     * Sets the data access object
     * 
     * @param dao
     *            The data access object to set
     */
    public void setDao(RegistryObjectTypeDao dao) {
        this.dao = dao;
    }

}
