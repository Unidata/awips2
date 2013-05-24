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
package com.raytheon.uf.edex.registry.ebxml.services.notification;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import javax.annotation.Resource;
import javax.xml.ws.WebServiceContext;

import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.MsgRegistryException;
import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.NotificationListener;
import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.QueryManager;
import oasis.names.tc.ebxml.regrep.xsd.lcm.v4.Mode;
import oasis.names.tc.ebxml.regrep.xsd.lcm.v4.RemoveObjectsRequest;
import oasis.names.tc.ebxml.regrep.xsd.lcm.v4.SubmitObjectsRequest;
import oasis.names.tc.ebxml.regrep.xsd.query.v4.QueryRequest;
import oasis.names.tc.ebxml.regrep.xsd.query.v4.QueryResponse;
import oasis.names.tc.ebxml.regrep.xsd.query.v4.ResponseOptionType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.ActionType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.AuditableEventType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.NotificationType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.ObjectRefListType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.ObjectRefType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.QueryType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryObjectListType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryObjectType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.SlotType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.StringValueType;

import org.springframework.transaction.annotation.Transactional;

import com.raytheon.uf.common.registry.constants.ActionTypes;
import com.raytheon.uf.common.registry.constants.CanonicalQueryTypes;
import com.raytheon.uf.common.registry.constants.DeletionScope;
import com.raytheon.uf.common.registry.constants.Format;
import com.raytheon.uf.common.registry.constants.Languages;
import com.raytheon.uf.common.registry.constants.QueryReturnTypes;
import com.raytheon.uf.common.registry.services.RegistrySOAPServices;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.util.CollectionUtil;
import com.raytheon.uf.edex.registry.ebxml.dao.ClassificationNodeDao;
import com.raytheon.uf.edex.registry.ebxml.dao.RegistryDao;
import com.raytheon.uf.edex.registry.ebxml.dao.RegistryObjectDao;
import com.raytheon.uf.edex.registry.ebxml.exception.EbxmlRegistryException;
import com.raytheon.uf.edex.registry.ebxml.services.lifecycle.LifecycleManagerImpl;
import com.raytheon.uf.edex.registry.ebxml.util.EbxmlObjectUtil;

/**
 * Implementation of the ebxml notificationListenerImpl.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 3/18/2013    1082       bphillip    Initial creation
 * 4/9/2013     1802       bphillip    Implemented notification handling
 * 5/21/2013    2022       bphillip    Reworked how notifications are handled
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */
@Transactional
public class NotificationListenerImpl implements NotificationListener {

    /** The logger */
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(NotificationListenerImpl.class);

    /**
     * The web service context information populated when this class is called
     * via a web service invocation
     */
    @Resource
    private WebServiceContext wsContext;

    /** The local lifecyclemanager instance */
    private LifecycleManagerImpl lcm;

    /** Data access object for getting registry objects */
    private RegistryObjectDao registryObjectDao;

    /** Data access object for getting RegistryType objects */
    private RegistryDao registryDao;

    /** The classification node data access object */
    private ClassificationNodeDao classificationNodeDao;

    @Override
    public void onNotification(NotificationType notification) {
        String clientBaseURL = EbxmlObjectUtil.getClientHost(wsContext);
        RegistryType sourceRegistry = registryDao
                .getRegistryByBaseURL(clientBaseURL);
        if (sourceRegistry == null) {
            statusHandler.info("Received notification from unknown source at "
                    + clientBaseURL);
        } else {
            statusHandler
                    .info("Received notification from Registry Federation member at ["
                            + sourceRegistry.getId() + "]");
        }

        List<AuditableEventType> events = notification.getEvent();

        // Process the received auditable events and add them to the appropriate
        // list based on the action performed
        RegistryObjectActionList objActionList = new RegistryObjectActionList();
        for (AuditableEventType event : events) {
            List<ActionType> actions = event.getAction();
            for (ActionType action : actions) {
                String eventType = action.getEventType();

                // Verify this is a valid event type
                if (!classificationNodeDao.isValidNode(eventType)) {
                    statusHandler.info("Unknown event type [" + eventType
                            + "] received in notification");
                    continue;
                }

                if (action.getAffectedObjectRefs() != null) {
                    for (ObjectRefType ref : action.getAffectedObjectRefs()
                            .getObjectRef()) {
                        objActionList.addAction(eventType, ref.getId());
                    }
                } else if (action.getAffectedObjects() != null) {
                    for (RegistryObjectType regObj : action
                            .getAffectedObjects().getRegistryObject()) {
                        objActionList.addAction(eventType, regObj.getId());
                    }
                } else {
                    statusHandler.info("Event " + event.getId()
                            + " contains 0 affected objects ");
                }
            }
        }

        for (RegistryObjectAction regObjAction : objActionList.getActionList()) {
            String action = regObjAction.getAction();
            try {
                if (action.equals(ActionTypes.create)
                        || action.equals(ActionTypes.update)) {
                    SubmitObjectsRequest submitRequest = createSubmitObjectsRequest(
                            clientBaseURL, notification.getId(),
                            regObjAction.getObjIds(), Mode.CREATE_OR_REPLACE);
                    lcm.submitObjects(submitRequest);
                } else if (action.equals(ActionTypes.delete)) {
                    ObjectRefListType refList = new ObjectRefListType();
                    for (String id : regObjAction.getObjIds()) {
                        RegistryObjectType object = registryObjectDao
                                .getById(id);
                        if (object != null) {
                            String replicaHome = object
                                    .getSlotValue(EbxmlObjectUtil.HOME_SLOT_NAME);
                            if (clientBaseURL.equals(replicaHome)) {
                                ObjectRefType ref = new ObjectRefType();
                                ref.setId(id);
                                refList.getObjectRef().add(ref);
                            }
                        }
                    }
                    RemoveObjectsRequest request = new RemoveObjectsRequest(
                            "Remove Objects for notification ["
                                    + notification.getId() + "]",
                            "Notification delete object submission", null,
                            null, refList, false, false,
                            DeletionScope.DELETE_ALL);
                    lcm.removeObjects(request);
                }

            } catch (EbxmlRegistryException e) {
                statusHandler
                        .error("Error getting remote objects to create", e);
            } catch (MsgRegistryException e) {
                statusHandler.error("Error creating objects in registry!", e);
            }
        }

    }

    /**
     * This method queries the client server to get the current state of the
     * affected objects and creates a SubmitObjectRequest
     * 
     * @param clientIP
     *            The remote server to get the current state of the registry
     *            objects from
     * @param notificationId
     *            The id of the received notification object
     * @param objIds
     *            The ids of the objects that were affected
     * @param mode
     *            The insert mode to be used
     * @return The SubmitObjectsRequest
     * @throws EbxmlRegistryException
     *             If errors occur while creating the SubmitObjectsRequest
     */
    private SubmitObjectsRequest createSubmitObjectsRequest(
            String clientBaseURL, String notificationId,
            Collection<String> objIds, Mode mode) throws EbxmlRegistryException {
        try {
            // Get a the remote query service
            QueryManager queryManager = RegistrySOAPServices
                    .getQueryServiceForHost(clientBaseURL);
            // Create a query to get the current state of the affected objects
            QueryRequest queryRequest = createGetCurrentStateQuery(
                    notificationId, objIds);
            // Query the remote server
            QueryResponse response = queryManager.executeQuery(queryRequest);
            List<RegistryObjectType> remoteObjects = response
                    .getRegistryObjectList().getRegistryObject();
            // Set the home server slot on the object denoting the home server
            // of the received object.
            for (RegistryObjectType object : remoteObjects) {
                object.updateSlot(EbxmlObjectUtil.HOME_SLOT_NAME, clientBaseURL);
            }

            /*
             * Attach a slot on the submit objects request that will later be
             * attached to the generated auditable event. This slot is used so
             * notfications will not ping pong back and forth between servers if
             * they have identical subscriptions with one another
             */
            // Create the submit objects request object
            SubmitObjectsRequest request = new SubmitObjectsRequest(
                    "Submit Objects for notification [" + notificationId + "]",
                    "Notification object submission",
                    CollectionUtil.asSet(new SlotType(
                            EbxmlObjectUtil.HOME_SLOT_NAME,
                            new StringValueType(clientBaseURL))),
                    new RegistryObjectListType(remoteObjects), false,
                    Mode.CREATE_OR_REPLACE);
            return request;
        } catch (Exception e) {
            throw new EbxmlRegistryException("Error processing notification", e);
        }
    }

    /**
     * Generates a QueryRequest to retrieve the current state of the objects
     * from a remote server.
     * 
     * @param notificationId
     *            The id of the received notification
     * @param ids
     *            The ids of the affected objects for which we are getting the
     *            current state
     * @return The QueryRequest object
     * @throws Exception
     *             If errors occur while creating the query
     */
    private QueryRequest createGetCurrentStateQuery(String notificationId,
            Collection<String> ids) {
        StringBuffer queryExpression = new StringBuffer();
        queryExpression.append("FROM RegistryObjectType obj WHERE obj.id in (");
        int idx = 0;
        for (String id : ids) {
            queryExpression.append("'").append(id).append("'");
            if (idx != ids.size() - 1) {
                queryExpression.append(",");
            }
            idx++;
        }
        queryExpression.append(")");

        ResponseOptionType responseOption = new ResponseOptionType();
        responseOption.setReturnComposedObjects(true);
        responseOption.setReturnType(QueryReturnTypes.REGISTRY_OBJECT);

        SlotType queryLanguageSlot = new SlotType("queryLanguage",
                new StringValueType("HQL"));
        SlotType queryExpressionSlot = new SlotType("queryExpression",
                new StringValueType(queryExpression.toString()));
        QueryType selectorQuery = new QueryType(
                CanonicalQueryTypes.ADHOC_QUERY, queryLanguageSlot,
                queryExpressionSlot);
        return new QueryRequest("NotificationListener object update",
                "NotificationListener object Update", new ResponseOptionType(
                        QueryReturnTypes.REGISTRY_OBJECT, true), selectorQuery,
                false, null, Format.EBRIM, Languages.EN_US, 0, 0, 0, false);
    }

    public void setLcm(LifecycleManagerImpl lcm) {
        this.lcm = lcm;
    }

    public void setRegistryObjectDao(RegistryObjectDao registryObjectDao) {
        this.registryObjectDao = registryObjectDao;
    }

    public void setClassificationNodeDao(
            ClassificationNodeDao classificationNodeDao) {
        this.classificationNodeDao = classificationNodeDao;
    }

    public void setRegistryDao(RegistryDao registryDao) {
        this.registryDao = registryDao;
    }

    private class RegistryObjectActionList {

        List<RegistryObjectAction> actionList = new ArrayList<RegistryObjectAction>();

        public void addAction(String action, String objId) {
            if (actionList.isEmpty()
                    || !actionList.get(actionList.size() - 1).getAction()
                            .equals(action)) {
                RegistryObjectAction newAction = new RegistryObjectAction(
                        action);
                newAction.addObj(objId);
                actionList.add(newAction);
            } else {
                actionList.get(actionList.size() - 1).addObj(objId);
            }
        }

        public List<RegistryObjectAction> getActionList() {
            return actionList;
        }
    }

    private class RegistryObjectAction {

        private String action;

        private List<String> objIds = new ArrayList<String>();

        public RegistryObjectAction(String action) {
            this.action = action;
        }

        public void addObj(String obj) {
            this.objIds.add(obj);
        }

        public String getAction() {
            return action;
        }

        public List<String> getObjIds() {
            return objIds;
        }
    }

}
