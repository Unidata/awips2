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
import java.util.LinkedList;
import java.util.List;

import javax.annotation.Resource;
import javax.jws.WebMethod;
import javax.jws.WebParam;
import javax.jws.WebResult;
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
import oasis.names.tc.ebxml.regrep.xsd.rs.v4.RegistryResponseStatus;
import oasis.names.tc.ebxml.regrep.xsd.rs.v4.RegistryResponseType;

import org.springframework.transaction.annotation.Transactional;

import com.raytheon.uf.common.registry.constants.ActionTypes;
import com.raytheon.uf.common.registry.constants.CanonicalQueryTypes;
import com.raytheon.uf.common.registry.constants.DeletionScope;
import com.raytheon.uf.common.registry.constants.QueryLanguages;
import com.raytheon.uf.common.registry.constants.QueryReturnTypes;
import com.raytheon.uf.common.registry.ebxml.RegistryUtil;
import com.raytheon.uf.common.registry.schemas.ebxml.util.EbxmlNamespaces;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.common.util.CollectionUtil;
import com.raytheon.uf.edex.registry.ebxml.dao.RegistryDao;
import com.raytheon.uf.edex.registry.ebxml.dao.RegistryObjectDao;
import com.raytheon.uf.edex.registry.ebxml.exception.EbxmlRegistryException;
import com.raytheon.uf.edex.registry.ebxml.services.lifecycle.LifecycleManagerImpl;
import com.raytheon.uf.edex.registry.ebxml.services.query.QueryConstants;
import com.raytheon.uf.edex.registry.ebxml.services.soap.RegistrySOAPServices;
import com.raytheon.uf.edex.registry.ebxml.util.EbxmlExceptionUtil;
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
 * 9/11/2013    2254       bphillip    Cleaned up handling of notifications and removed unneccessary code
 * 10/20/2013    1682       bphillip    Added synchronous notification delivery
 * 10/23/2013   1538       bphillip    Added log message denoting when processing is complete and time duration
 * 10/30/2013   1538       bphillip    Changed to use non-static registry soap service client
 * 12/2/2013    1829       bphillip    Added getIdsFrom action method and changed how slots are added to objects
 * 1/15/2014    2613       bphillip    Added batching of notification update queries to reduce number of web service calls
 * 01/21/2014   2613       bphillip    Added home slot to remove objects request so delete events are properly handled
 * 2/4/2014     2769        bphillip    Removed flush and clear call
 * Mar 31, 2014 2889       dhladky      Added username for notification center tracking.
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

    /** Registry soap service client */
    private RegistrySOAPServices registrySoapClient;

    @Override
    public void onNotification(NotificationType notification) {
        long startTime = TimeUtil.currentTimeMillis();

        String clientBaseURL = EbxmlObjectUtil.getClientHost(wsContext);
        RegistryType sourceRegistry = registryDao
                .getRegistryByBaseURL(clientBaseURL);
        if (sourceRegistry == null) {
            statusHandler.info("Received notification from unknown source at "
                    + clientBaseURL);
        } else {
            statusHandler
                    .info("Received notification from Registry Federation member at ["
                            + sourceRegistry.getId()
                            + "("
                            + clientBaseURL
                            + ")]");
        }

        // Creates a new action list from the received events
        ProcessActionList actionList = new ProcessActionList(
                notification.getEvent());

        /*
         * Iterate through each action and process them in the order in which
         * the were received. Notification types currently supported are create,
         * update and delete. All other action types will be ignored and a
         * warning message is output into the log
         */
        for (ProcessAction action : actionList.getActions()) {
            String actionType = action.getActionType();

            /*
             * Process creates and updates
             */
            if (ActionTypes.create.equals(actionType)
                    || ActionTypes.update.equals(actionType)) {
                try {
                    SubmitObjectsRequest submitRequest = createSubmitObjectsRequest(
                            clientBaseURL, notification.getId(),
                            action.getIdList(), Mode.CREATE_OR_REPLACE);
                    lcm.submitObjects(submitRequest);
                } catch (MsgRegistryException e) {
                    throw new RuntimeException(
                            "Error creating objects in registry!", e);
                } catch (EbxmlRegistryException e) {
                    throw new RuntimeException(
                            "Error creating submit objects request!", e);
                }
            }
            /*
             * Process deletes
             */
            else if (ActionTypes.delete.equals(actionType)) {
                ObjectRefListType refList = new ObjectRefListType();
                for (String id : action.getIdList()) {
                    RegistryObjectType object = registryObjectDao.getById(id);
                    if (object != null) {
                        refList.getObjectRef().add(new ObjectRefType(id));
                    }
                }
                if (!CollectionUtil.isNullOrEmpty(refList.getObjectRef())) {
                    RemoveObjectsRequest request = new RemoveObjectsRequest(
                            "Remove Objects for notification ["
                                    + notification.getId() + "]",
                            "Notification delete object submission", null,
                            null, refList, false, true,
                            DeletionScope.DELETE_ALL);
                    request.getSlot().add(
                            new SlotType(EbxmlObjectUtil.HOME_SLOT_NAME,
                                    new StringValueType(clientBaseURL)));
                    try {
                        lcm.removeObjects(request);
                    } catch (MsgRegistryException e) {
                        throw new RuntimeException(
                                "Error creating remove objects request!", e);
                    }
                }
            }
            /*
             * Output warning in log that an unsupported action type was
             * received
             */
            else {
                statusHandler.warn("Unsupported action type received ["
                        + actionType + "]");
            }

        }
        statusHandler.info("Processing notification id ["
                + notification.getId() + "] completed in "
                + (TimeUtil.currentTimeMillis() - startTime) + " ms");
    }

    @Override
    @WebMethod(action = "SynchronousNotification")
    @WebResult(name = "RegistryResponse", targetNamespace = EbxmlNamespaces.RS_URI, partName = "partRegistryResponse")
    public RegistryResponseType synchronousNotification(
            @WebParam(name = "Notification", targetNamespace = EbxmlNamespaces.RIM_URI, partName = "Notification") NotificationType notification)
            throws MsgRegistryException {
        RegistryResponseType response = new RegistryResponseType();
        response.setRequestId(notification.getId());
        try {
            onNotification(notification);
            response.setStatus(RegistryResponseStatus.SUCCESS);
            return response;
        } catch (Throwable e) {
            throw EbxmlExceptionUtil.createMsgRegistryException(
                    "Error processing notification.", e);
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
            QueryManager queryManager = registrySoapClient
                    .getQueryServiceForHost(clientBaseURL);
            // Create a query to get the current state of the affected
            QueryRequest queryRequest = createGetCurrentStateQuery(
                    notificationId, objIds);
            // Query the remote server
            QueryResponse response = queryManager.executeQuery(queryRequest);
            List<RegistryObjectType> remoteObjects = response
                    .getRegistryObjectList().getRegistryObject();

            // Set the home server slot on the object denoting the home server
            // of the received object.
            for (RegistryObjectType object : remoteObjects) {
                SlotType homeSlot = object
                        .getSlotByName(EbxmlObjectUtil.HOME_SLOT_NAME);
                if (homeSlot == null) {
                    object.getSlot().add(
                            new SlotType(EbxmlObjectUtil.HOME_SLOT_NAME,
                                    new StringValueType(clientBaseURL)));
                } else {
                    ((StringValueType) homeSlot.getSlotValue())
                            .setStringValue(clientBaseURL);
                }
            }

            /*
             * Attach a slot on the submit objects request that will later be
             * attached to the generated auditable event. This slot is used so
             * notfications will not ping pong back and forth between servers if
             * they have identical subscriptions with one another
             */
            // Create the submit objects request object
            SubmitObjectsRequest request = new SubmitObjectsRequest();
            request.setId("Submit Objects for notification [" + notificationId
                    + "]");
            request.setComment("Notification object submission");
            request.setRegistryObjectList(new RegistryObjectListType(
                    remoteObjects));
            request.setCheckReferences(false);
            request.setMode(Mode.CREATE_OR_REPLACE);
            request.setUsername(RegistryUtil.registryUser);
            request.getSlot().add(
                    new SlotType(EbxmlObjectUtil.HOME_SLOT_NAME,
                            new StringValueType(clientBaseURL)));
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

        SlotType queryLanguageSlot = new SlotType(
                QueryConstants.QUERY_LANGUAGE, new StringValueType(
                        QueryLanguages.HQL));
        SlotType queryExpressionSlot = new SlotType(
                QueryConstants.QUERY_EXPRESSION, new StringValueType(
                        queryExpression.toString()));
        QueryType query = new QueryType();
        query.setQueryDefinition(CanonicalQueryTypes.ADHOC_QUERY);
        query.getSlot().add(queryLanguageSlot);
        query.getSlot().add(queryExpressionSlot);

        QueryRequest request = new QueryRequest();
        request.setResponseOption(new ResponseOptionType(
                QueryReturnTypes.REGISTRY_OBJECT, true));
        request.setId("Notification Update Query");
        request.setQuery(query);
        return request;
    }

    /**
     * This class organizes actions from the received list of auditable events.
     * The purpose of this class is to batch together consecutive events having
     * the same action type in order to minimize queries to the remote server
     * during notification processing. The actions must be processed in the
     * order in which the appear in the original auditable event list in order
     * to maintain the integrity of the registry across federation members.
     * 
     * @author bphillip
     * 
     */
    private class ProcessActionList {

        /** The list of actions contained in the received auditable events */
        private List<ProcessAction> actions = new ArrayList<NotificationListenerImpl.ProcessAction>();

        /**
         * Creates a new ProcessActionList from the provided list of events
         * 
         * @param events
         *            The events from which to generate the ProcessActionList
         */
        public ProcessActionList(List<AuditableEventType> events) {
            if (!CollectionUtil.isNullOrEmpty(events)) {
                for (AuditableEventType event : events) {
                    List<ActionType> actions = event.getAction();
                    for (ActionType action : actions) {
                        addAction(action.getEventType(),
                                getIdsFromAction(action));
                    }
                }
            }
        }

        /**
         * Adds an action to the list to be processed
         * 
         * @param actionType
         *            The type of action
         * @param ids
         *            The ids of the objects on which the action was executed
         */
        private void addAction(String actionType, List<String> ids) {
            if (actions.isEmpty()) {
                actions.add(new ProcessAction(actionType, ids));
            } else {
                ProcessAction lastAction = actions.get(actions.size() - 1);
                if (lastAction.getActionType().equals(actionType)) {
                    lastAction.addIds(ids);
                } else {
                    actions.add(new ProcessAction(actionType, ids));
                }
            }

        }

        /**
         * Extracts the affected object ids from the action
         * 
         * @param action
         *            The action to get the ids from
         * @return The list of ids from the action object
         */
        private List<String> getIdsFromAction(ActionType action) {
            List<String> objectIds = new ArrayList<String>();
            if (action.getAffectedObjectRefs() != null) {
                for (ObjectRefType ref : action.getAffectedObjectRefs()
                        .getObjectRef()) {
                    objectIds.add(ref.getId());
                }
            } else if (action.getAffectedObjects() != null) {
                for (RegistryObjectType regObj : action.getAffectedObjects()
                        .getRegistryObject()) {
                    objectIds.add(regObj.getId());
                }
            }
            return objectIds;
        }

        public List<ProcessAction> getActions() {
            return actions;
        }

    }

    /**
     * Private class encapsulating the list of object ids and the action that
     * was executed on them.
     * 
     * @author bphillip
     * 
     */
    private class ProcessAction {

        /** The type of action executed */
        private String actionType;

        /** The list of objects that the action was executed on */
        private List<String> idList;

        public ProcessAction(String actionType, List<String> ids) {
            this.actionType = actionType;
            addIds(ids);
        }

        public String getActionType() {
            return actionType;
        }

        public List<String> getIdList() {
            if (idList == null) {
                idList = new LinkedList<String>();
            }
            return idList;
        }

        public void addIds(List<String> ids) {
            getIdList().addAll(ids);
        }
    }

    public void setLcm(LifecycleManagerImpl lcm) {
        this.lcm = lcm;
    }

    public void setRegistryObjectDao(RegistryObjectDao registryObjectDao) {
        this.registryObjectDao = registryObjectDao;
    }

    public void setRegistryDao(RegistryDao registryDao) {
        this.registryDao = registryDao;
    }

    public void setRegistrySoapClient(RegistrySOAPServices registrySoapClient) {
        this.registrySoapClient = registrySoapClient;
    }
}
