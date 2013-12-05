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

import java.util.ArrayList;
import java.util.List;

import javax.annotation.Resource;
import javax.xml.ws.WebServiceContext;

import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.LifecycleManager;
import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.MsgRegistryException;
import oasis.names.tc.ebxml.regrep.xsd.lcm.v4.Mode;
import oasis.names.tc.ebxml.regrep.xsd.lcm.v4.RemoveObjectsRequest;
import oasis.names.tc.ebxml.regrep.xsd.lcm.v4.SubmitObjectsRequest;
import oasis.names.tc.ebxml.regrep.xsd.lcm.v4.UpdateActionType;
import oasis.names.tc.ebxml.regrep.xsd.lcm.v4.UpdateObjectsRequest;
import oasis.names.tc.ebxml.regrep.xsd.query.v4.QueryRequest;
import oasis.names.tc.ebxml.regrep.xsd.query.v4.QueryResponse;
import oasis.names.tc.ebxml.regrep.xsd.query.v4.ResponseOptionType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.AssociationType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.ClassificationNodeType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.ClassificationSchemeType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.ExtrinsicObjectType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.ObjectRefType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.QueryExpressionType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.QueryType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryObjectType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.StringQueryExpressionType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.TaxonomyElementType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.VersionInfoType;
import oasis.names.tc.ebxml.regrep.xsd.rs.v4.RegistryResponseStatus;
import oasis.names.tc.ebxml.regrep.xsd.rs.v4.RegistryResponseType;

import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import com.raytheon.uf.common.event.EventBus;
import com.raytheon.uf.common.registry.constants.ActionTypes;
import com.raytheon.uf.common.registry.constants.AssociationTypes;
import com.raytheon.uf.common.registry.constants.DeletionScope;
import com.raytheon.uf.common.registry.constants.QueryLanguages;
import com.raytheon.uf.common.registry.constants.QueryReturnTypes;
import com.raytheon.uf.common.registry.constants.RegistryObjectTypes;
import com.raytheon.uf.common.registry.constants.StatusTypes;
import com.raytheon.uf.common.registry.ebxml.RegistryUtil;
import com.raytheon.uf.common.registry.event.InsertRegistryEvent;
import com.raytheon.uf.common.registry.event.RegistryEvent.Action;
import com.raytheon.uf.common.registry.event.RegistryStatisticsEvent;
import com.raytheon.uf.common.registry.event.RemoveRegistryEvent;
import com.raytheon.uf.common.registry.event.UpdateRegistryEvent;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.common.util.CollectionUtil;
import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.registry.ebxml.dao.RegistryObjectDao;
import com.raytheon.uf.edex.registry.ebxml.exception.EbxmlRegistryException;
import com.raytheon.uf.edex.registry.ebxml.services.cataloger.CatalogerImpl;
import com.raytheon.uf.edex.registry.ebxml.services.query.QueryManagerImpl;
import com.raytheon.uf.edex.registry.ebxml.services.validator.ValidatorImpl;
import com.raytheon.uf.edex.registry.ebxml.util.EbxmlExceptionUtil;
import com.raytheon.uf.edex.registry.ebxml.util.EbxmlObjectUtil;
import com.raytheon.uf.edex.registry.ebxml.util.xpath.RegistryXPathProcessor;
import com.raytheon.uf.edex.registry.events.CreateAuditTrailEvent;

/**
 * The LifecycleManager interface allows a client to perform various lifecycle
 * management operations on RegistryObjects. These operations include submitting
 * RegistryObjects to the server, updating RegistryObjects in the server,
 * creating new versions of RegistryObjects in the server and removing
 * RegistryObjects from the server.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 18, 2012            bphillip     Initial creation
 * Sep 14, 2012 1169       djohnson     Throw exception when object exists during create only mode.
 * 3/18/2013    1802       bphillip    Modified to use transaction boundaries and spring injection
 * 4/9/2013     1802       bphillip    Changed how auditable events are handled
 * Apr 18, 2013 1693       djohnson    Changes to conform to Ebxml 4.0 SubmitObjects protocol.
 * Apr 24, 2013 1910       djohnson    Use validation framework to check references.
 * Jun 24, 2013 2106       djohnson    Requires a transaction to already be open.
 * 8/1/2013     1693       bphillip    Added check references and refactored submit objects to conform to EBXML 4.0 spec
 * 9/11/2013    2254       bphillip    Cleaned up creation of auditable events
 * 10/23/2013   1538       bphillip    Changed QueryRequest constructor call
 * Nov 08, 2013 2506       bgonzale    Added RegistryObjectType to RemoveRegistryEvent.
 *                                     Separate update from create notifications.
 * 12/2/2013    1829       bphillip    Auditable events are not genereted via messages on the event bus
 * 
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */
@Transactional(propagation = Propagation.MANDATORY)
public class LifecycleManagerImpl implements LifecycleManager {

    /** The logger */
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(LifecycleManagerImpl.class);

    public static final String REMOVE_OBJECTS_ERROR_MSG = "Error removing objects";

    public static final String SUBMIT_OBJECTS_ERROR_MSG = "Error submitting objects";

    public static final String UPDATE_OBJECTS_ERROR_MSG = "Error updating objects";

    @Resource
    private WebServiceContext wsContext;

    private ObjectReferenceResolver referenceResolver;

    /** The query manager */
    private QueryManagerImpl queryManager;

    /** The validator */
    @SuppressWarnings("unused")
    private ValidatorImpl validator;

    /** The cataloger */
    @SuppressWarnings("unused")
    private CatalogerImpl cataloger;

    /** The registry object data access object */
    private RegistryObjectDao registryObjectDao;

    private RegistryXPathProcessor xpathProcessor;

    /**
     * The Remove Objects protocol allows a client to remove or delete one or
     * more RegistryObject instances from the server.
     * 
     * A client initiates the RemoveObjects protocol by sending a
     * RemoveObjectsRequest message to the LifecycleManager endpoint.
     * 
     * The LifecycleManager sends a RegistryResponse back to the client as
     * response.
     */
    @Override
    public RegistryResponseType removeObjects(RemoveObjectsRequest request)
            throws MsgRegistryException {
        long startTime = TimeUtil.currentTimeMillis();
        statusHandler
                .info("LifecycleManager received removeObjectsRequest from ["
                        + EbxmlObjectUtil.getClientHost(wsContext) + "]");
        RegistryResponseType response = EbxmlObjectUtil.rsObjectFactory
                .createRegistryResponseType();
        response.setRequestId(request.getId());
        response.setStatus(RegistryResponseStatus.SUCCESS);

        boolean checkReferences = request.isCheckReferences();
        boolean deleteChildren = request.isDeleteChildren();
        String deletionScope = request.getDeletionScope();
        /*
         * Validate the deletion scope according to the spec: The value of the
         * deletionScope attribute MUST be a reference to a ClassificationNode
         * within the canonical DeletionScopeType ClassificationScheme as
         * described in ebRIM. A server MUST support the deletionScope types as
         * defined by the canonical DeletionScopeType ClassificationScheme. The
         * canonical DeletionScopeType ClassificationScheme may be extended by
         * adding additional ClassificationNodes to it.
         */
        RegistryObjectType deleteScopeObj = registryObjectDao
                .getById(deletionScope);
        if (deleteScopeObj == null) {
            throw EbxmlExceptionUtil.createQueryExceptionType(
                    REMOVE_OBJECTS_ERROR_MSG, "DeletionScope [" + deletionScope
                            + "] does not exist in registry!");
        }

        List<RegistryObjectType> objectsToRemove = new ArrayList<RegistryObjectType>();

        if (request.getObjectRefList() != null
                && !CollectionUtil.isNullOrEmpty(request.getObjectRefList()
                        .getObjectRef())) {
            for (ObjectRefType objectRef : request.getObjectRefList()
                    .getObjectRef()) {
                RegistryObjectType objReferenced = registryObjectDao
                        .getById(objectRef.getId());
                if (objReferenced == null) {
                    statusHandler.info("Object with id [" + objectRef.getId()
                            + "] does not exist in the registry.");
                } else {
                    objectsToRemove.add(objReferenced);
                }
            }
        }

        QueryType query = request.getQuery();
        if (query != null) {
            ResponseOptionType responseOption = new ResponseOptionType(
                    QueryReturnTypes.REGISTRY_OBJECT, true);
            QueryRequest queryRequest = new QueryRequest(
                    "Remove objects query for request id[" + request.getId()
                            + "]", query, responseOption);
            QueryResponse queryResponse = queryManager
                    .executeQuery(queryRequest);
            if (queryResponse.getStatus()
                    .equals(RegistryResponseStatus.SUCCESS)) {
                statusHandler.info("Remove objects query successful");
            } else {
                throw EbxmlExceptionUtil.createQueryExceptionType(
                        REMOVE_OBJECTS_ERROR_MSG,
                        "Remove objects query failed!");
            }
            if (CollectionUtil
                    .isNullOrEmpty(queryResponse.getRegistryObjects())) {
                statusHandler
                        .info("No results returned from remove objects query");
            } else {
                statusHandler.info("Remove objects query returned "
                        + queryResponse.getRegistryObjects() + " objects");
                objectsToRemove.addAll(queryResponse.getRegistryObjects());
            }
        }

        if (checkReferences) {
            statusHandler
                    .info("Client has selected to check object references before removing objects.");
            String message = referenceResolver
                    .allReferencesDoNotResolve(objectsToRemove);
            if (!message.isEmpty()) {
                throw EbxmlExceptionUtil.createReferencesExistExceptionType(
                        REMOVE_OBJECTS_ERROR_MSG, message);
            }
        }

        if (deletionScope.equals(DeletionScope.DELETE_ALL)) {
            for (RegistryObjectType objToRemove : objectsToRemove) {
                removeRepositoryItem(objToRemove);
                if (deleteChildren) {
                    registryObjectDao.deleteWithoutMerge(objToRemove);
                } else {
                    try {
                        registryObjectDao
                                .deleteObjectWithoutDeletingChildren(objToRemove);
                    } catch (DataAccessLayerException e) {
                        throw EbxmlExceptionUtil.createQueryExceptionType(
                                REMOVE_OBJECTS_ERROR_MSG,
                                "Error deleting object [" + objToRemove.getId()
                                        + "]" + e.getLocalizedMessage());
                    }
                }
            }
        } else if (deletionScope
                .equals(DeletionScope.DELETE_REPOSITORY_ITEM_ONLY)) {
            for (RegistryObjectType objToRemove : objectsToRemove) {
                removeRepositoryItem(objToRemove);
            }
        } else {
            throw EbxmlExceptionUtil.createUnsupportedCapabilityExceptionType(
                    REMOVE_OBJECTS_ERROR_MSG, "Unsupported DeletionScope ["
                            + deletionScope + "]");
        }

        long totalTime = System.currentTimeMillis() - startTime;

        long avTimePerRecord = 0;
        if (!objectsToRemove.isEmpty()) {
            avTimePerRecord = totalTime / objectsToRemove.size();
        }

        statusHandler
                .info("LifeCycleManager removeObjects operation completed in "
                        + totalTime + " ms");

        for (RegistryObjectType obj : objectsToRemove) {
            String objectType = obj.getObjectType();
            // Don't send notifications for Association types
            if (objectType != null
                    && !objectType.equals(RegistryObjectTypes.ASSOCIATION)) {
                RemoveRegistryEvent event = new RemoveRegistryEvent(
                        request.getUsername(), obj.getId(), obj);
                event.setAction(Action.DELETE);
                event.setLid(obj.getLid());
                event.setObjectType(objectType);
                EventBus.publish(event);
            }
            EventBus.publish(new RegistryStatisticsEvent(obj.getObjectType(),
                    obj.getStatus(), obj.getOwner(), avTimePerRecord));
        }

        EventBus.publish(new CreateAuditTrailEvent(request.getId(), request,
                ActionTypes.delete, objectsToRemove));

        return response;
    }

    /**
     * This method removes the repository item for the specified registry
     * object.
     * <p>
     * This method will have to be expanded to handle remove objects that are
     * linked to. Right now, the assumption is that the repository item is
     * contained in the repositoryItem field of the object
     * 
     * @param obj
     */
    private void removeRepositoryItem(RegistryObjectType obj) {
        if (obj instanceof ExtrinsicObjectType) {
            ExtrinsicObjectType extrinsicObject = (ExtrinsicObjectType) obj;
            extrinsicObject.setRepositoryItem(null);
            registryObjectDao.update(obj);
        }

    }

    /**
     * The SubmitObjects protocol allows a client to submit RegistryObjects to
     * the server. It also allows a client to completely replace existing
     * RegistryObjects in the server.
     * 
     * A client initiates the SubmitObjects protocol by sending a
     * SubmitObjectsRequest message to the LifecycleManager endpoint.
     * 
     * The LifecycleManager sends a RegistryResponse back to the client as
     * response.
     */
    @Override
    public RegistryResponseType submitObjects(SubmitObjectsRequest request)
            throws MsgRegistryException {

        statusHandler
                .info("LifecycleManager received submitObjectsRequest from ["
                        + EbxmlObjectUtil.getClientHost(wsContext) + "]");
        long startTime = TimeUtil.currentTimeMillis();

        RegistryResponseType response = new RegistryResponseType();
        response.setStatus(RegistryResponseStatus.SUCCESS);

        boolean checkReferences = request.isCheckReferences();

        Mode submitMode = request.getMode();
        List<RegistryObjectType> objs = request.getRegistryObjectList()
                .getRegistryObject();
        if (objs.isEmpty()) {
            statusHandler.info("No objects submitted to registry");
        } else if (objs.size() == 1) {
            statusHandler.info(objs.size() + " object submitted to registry");
        } else {
            statusHandler.info(objs.size() + " objects submitted to registry");
        }
        if (checkReferences) {
            statusHandler
                    .info("Client has selected to check object references before submitting objects.");
            String unresolvedReferencesMessage = referenceResolver
                    .allReferencesResolve(objs);
            if (!unresolvedReferencesMessage.isEmpty()) {
                throw EbxmlExceptionUtil
                        .createUnresolvedReferenceExceptionType(
                                SUBMIT_OBJECTS_ERROR_MSG,
                                unresolvedReferencesMessage);
            }

        }

        List<RegistryObjectType> objsCreated = new ArrayList<RegistryObjectType>();
        List<RegistryObjectType> objsUpdated = new ArrayList<RegistryObjectType>();
        for (RegistryObjectType obj : request.getRegistryObjectList()
                .getRegistryObject()) {
            String objectId = obj.getId();
            final String objectLid = obj.getLid();
            statusHandler.debug("Processing object [" + objectId + "]");
            if (objectLid == null) {
                throw EbxmlExceptionUtil.createInvalidRequestExceptionType(
                        SUBMIT_OBJECTS_ERROR_MSG,
                        "LID MUST be specified by client");
            }
            if (obj instanceof TaxonomyElementType) {
                generatePaths((TaxonomyElementType) obj, "");
            }

            RegistryObjectType existingObject = null;
            switch (submitMode) {
            case CREATE_OR_REPLACE:

                if (objectId == null) {
                    throw EbxmlExceptionUtil.createInvalidRequestExceptionType(
                            SUBMIT_OBJECTS_ERROR_MSG,
                            "ID MUST be specified by client");
                }
                existingObject = registryObjectDao.getById(objectId);
                if (existingObject == null) {
                    objsCreated.add(obj);
                    registryObjectDao.create(obj);
                    statusHandler.info("Object [" + objectId
                            + "] created in the registry.");
                } else {
                    /*
                     * A server MUST NOT perform update operations via
                     * SubmitObjects and UpdateObjects operations on a local
                     * replica of a remote object. (Except in the case of
                     * updating objects from notifications)
                     */
                    checkReplica(request, obj, existingObject);
                    objsUpdated.add(obj);
                    registryObjectDao.merge(obj, existingObject);
                    statusHandler.info("Object [" + objectId
                            + "] replaced in the registry.");
                }
                break;
            case CREATE_OR_VERSION:
                for (RegistryObjectType objectGenerated : versionObject(obj,
                        objsCreated)) {
                    registryObjectDao.create(objectGenerated);
                    objsCreated.add(objectGenerated);
                }
                break;
            case CREATE_ONLY:

                if (registryObjectDao.lidExists(objectLid)) {
                    throw EbxmlExceptionUtil.createObjectExistsExceptionType(
                            SUBMIT_OBJECTS_ERROR_MSG,
                            "Object already exists with lid: " + objectId
                                    + ". Cannot submit using CREATE_ONLY mode");
                }
                if (objectId == null) {
                    objectId = RegistryUtil.generateRegistryObjectId();
                    obj.setId(objectId);
                } else if (registryObjectDao.idExists(objectId)) {
                    throw EbxmlExceptionUtil.createObjectExistsExceptionType(
                            SUBMIT_OBJECTS_ERROR_MSG,
                            "Object already exists with id: " + objectId
                                    + ". Cannot submit using CREATE_ONLY mode");
                }
                objsCreated.add(obj);
                registryObjectDao.create(obj);
                statusHandler.info("Object [" + objectId
                        + "] created in the registry.");

                break;
            }
        }

        response.setRequestId(request.getId());
        response.setObjectRefList(EbxmlObjectUtil.createObjectRefList(objs));
        long totalTime = System.currentTimeMillis() - startTime;
        statusHandler
                .info("LifeCycleManager submitObjects operation completed in "
                        + totalTime + " ms");
        statusHandler.info("Creating auditable events....");
        // gives a close estimate to amount taken on each object
        // individually, this will be millis in most cases, hopefully
        long avTimePerRecord = objs.isEmpty() ? 0 : totalTime / objs.size();
        if (!objsCreated.isEmpty()) {
            for (RegistryObjectType obj : objsCreated) {
                EventBus.publish(new InsertRegistryEvent(obj.getId(), obj
                        .getLid(), obj.getObjectType()));
                EventBus.publish(new RegistryStatisticsEvent(obj
                        .getObjectType(), obj.getStatus(), obj.getOwner(),
                        avTimePerRecord));
            }
            EventBus.publish(new CreateAuditTrailEvent(request.getId(),
                    request, ActionTypes.create, objsCreated));
        }
        if (!objsUpdated.isEmpty()) {
            for (RegistryObjectType obj : objsUpdated) {
                EventBus.publish(new UpdateRegistryEvent(obj.getId(), obj
                        .getLid(), obj.getObjectType()));
                EventBus.publish(new RegistryStatisticsEvent(obj
                        .getObjectType(), obj.getStatus(), obj.getOwner(),
                        avTimePerRecord));
            }
            EventBus.publish(new CreateAuditTrailEvent(request.getId(),
                    request, ActionTypes.update, objsUpdated));
        }

        return response;
    }

    private List<RegistryObjectType> versionObject(RegistryObjectType obj,
            List<RegistryObjectType> objsCreated) throws MsgRegistryException {
        List<RegistryObjectType> objectsGenerated = new ArrayList<RegistryObjectType>(
                2);
        String objectId = obj.getId();
        String objectLid = obj.getLid();
        RegistryObjectType existingObject = null;

        if (objectId == null) {
            throw EbxmlExceptionUtil.createInvalidRequestExceptionType(
                    SUBMIT_OBJECTS_ERROR_MSG, "ID MUST be specified by client");
        }
        boolean idExists = registryObjectDao.idExists(objectId);
        boolean lidExists = registryObjectDao.lidExists(objectLid);

        /*
         * If id does not exist and lid does not exist, server MUST create new
         * object using the id (create)
         */
        if (!idExists && !lidExists) {
            objsCreated.add(obj);
            objectsGenerated.add(obj);
            statusHandler.info("Object [" + objectId
                    + "] created in the registry.");
        }
        /*
         * If id does not exist and lid exists server MUST throw
         * InvalidRequestException
         */
        else if (!idExists && lidExists) {
            throw EbxmlExceptionUtil
                    .createInvalidRequestExceptionType(
                            SUBMIT_OBJECTS_ERROR_MSG,
                            "Specified object ID does not exist yet lid exists, unable to version");
        }
        /*
         * If id exists, server MUST create a new version of existing object
         * matching the id
         */
        else if (idExists) {
            existingObject = registryObjectDao.getById(objectId);
            String nextVersion = registryObjectDao
                    .getNextVersion(existingObject);
            obj.setVersionInfo(new VersionInfoType(nextVersion));
            obj.setStatus(existingObject.getStatus());
            obj.setId(existingObject.getId() + "_"
                    + nextVersion.substring(nextVersion.lastIndexOf(".") + 1));
            AssociationType versionAssociation = EbxmlObjectUtil.rimObjectFactory
                    .createAssociationType();
            String idUUID = EbxmlObjectUtil.getUUID();
            versionAssociation.setId(idUUID);
            versionAssociation.setLid(idUUID);
            versionAssociation.setName(RegistryUtil
                    .getInternationalString("Version Association"));
            versionAssociation.setDescription(RegistryUtil
                    .getInternationalString(objectId + " Supersedes "
                            + existingObject.getId()));
            versionAssociation.setOwner(existingObject.getOwner());
            versionAssociation.setObjectType(RegistryObjectTypes.ASSOCIATION);
            versionAssociation.setSourceObject(objectId);
            versionAssociation.setTargetObject(existingObject.getId());
            versionAssociation.setStatus(StatusTypes.APPROVED);
            versionAssociation.setType(AssociationTypes.SUPERSEDES);
            versionAssociation.setVersionInfo(new VersionInfoType());

            objsCreated.add(obj);
            objsCreated.add(versionAssociation);
            objectsGenerated.add(versionAssociation);
            objectsGenerated.add(obj);
            statusHandler.info("Object [" + objectId
                    + "] versioned in the registry.");
        }
        return objectsGenerated;
    }

    /**
     * Verifies that the service adheres to the spec when dealing with replicas
     * 
     * @param request
     *            The request
     * @param object1
     *            The object being submitted
     * @param object2
     *            The current object in the registry
     * @throws MsgRegistryException
     *             If errors occur while checking for replicas
     */
    private void checkReplica(SubmitObjectsRequest request,
            RegistryObjectType object1, RegistryObjectType object2)
            throws MsgRegistryException {
        boolean fromNotification = request
                .getSlotValue(EbxmlObjectUtil.HOME_SLOT_NAME) != null;
        String object1Home = object1
                .getSlotValue(EbxmlObjectUtil.HOME_SLOT_NAME);
        String object2Home = object2
                .getSlotValue(EbxmlObjectUtil.HOME_SLOT_NAME);

        if (object1.getOwner() != null
                && (object1.getOwner().equals(object2.getOwner()))) {
            return;
        }
        if (fromNotification) {
            if (object1Home != null && object2Home == null) {
                throw EbxmlExceptionUtil.createObjectExistsExceptionType(
                        SUBMIT_OBJECTS_ERROR_MSG,
                        "Cannot overwrite local object with replica");
            } else if (object1Home != null && object2Home != null) {
                if (!object1Home.equals(object2Home)) {
                    throw EbxmlExceptionUtil
                            .createObjectExistsExceptionType(
                                    SUBMIT_OBJECTS_ERROR_MSG,
                                    "Cannot overwrite a remote replica from a different server");
                }
            }
        } else {
            if (object2Home != null) {
                throw EbxmlExceptionUtil.createInvalidRequestExceptionType(
                        SUBMIT_OBJECTS_ERROR_MSG, "Cannot update replicas");
            }
        }
    }

    private void generatePaths(TaxonomyElementType element, String pathPrefix) {
        if (element instanceof ClassificationSchemeType) {
            ClassificationSchemeType scheme = (ClassificationSchemeType) element;
            pathPrefix = "/" + scheme.getId();
        } else if (element instanceof ClassificationNodeType) {
            ClassificationNodeType node = (ClassificationNodeType) element;
            pathPrefix = pathPrefix + "/" + node.getCode();
            node.setPath(pathPrefix);
        }
        if (element.getClassificationNode() != null) {
            for (ClassificationNodeType node : element.getClassificationNode()) {
                generatePaths(node, pathPrefix);

            }
        }
    }

    /**
     * The UpdateObjectsRequest protocol allows a client to make partial updates
     * to one or more RegistryObjects that already exist in the server. This
     * protocol enables partial update of RegistryObjects rather than a complete
     * replacement. A client SHOULD use the SubmitObjects protocol for complete
     * replacement of RegistryObjects.
     */
    @Override
    public RegistryResponseType updateObjects(UpdateObjectsRequest request)
            throws MsgRegistryException {
        long startTime = TimeUtil.currentTimeMillis();
        statusHandler.info("LifecycleManager received updateObjects from ["
                + EbxmlObjectUtil.getClientHost(wsContext) + "]");
        RegistryResponseType response = new RegistryResponseType();
        response.setStatus(RegistryResponseStatus.SUCCESS);
        List<UpdateActionType> updateActions = request.getUpdateAction();
        boolean checkReferences = request.isCheckReferences();
        Mode mode = request.getMode();

        List<RegistryObjectType> objectsToUpdate = new ArrayList<RegistryObjectType>();
        if (request.getObjectRefList() != null
                && !CollectionUtil.isNullOrEmpty(request.getObjectRefList()
                        .getObjectRef())) {
            for (ObjectRefType objectRef : request.getObjectRefList()
                    .getObjectRef()) {
                RegistryObjectType objReferenced = registryObjectDao
                        .getById(objectRef.getId());
                if (objReferenced == null) {
                    throw EbxmlExceptionUtil.createObjectNotFoundExceptionType(
                            UPDATE_OBJECTS_ERROR_MSG,
                            "Unable to update object [" + objectRef.getId()
                                    + "]. Not present in registry");
                } else {
                    objectsToUpdate.add(objReferenced);
                }
            }
        }

        QueryType query = request.getQuery();
        if (query != null) {
            ResponseOptionType responseOption = new ResponseOptionType(
                    QueryReturnTypes.REGISTRY_OBJECT, true);
            QueryRequest queryRequest = new QueryRequest(
                    "Update objects query for request[" + request.getId() + "]",
                    query, responseOption);
            QueryResponse queryResponse = queryManager
                    .executeQuery(queryRequest);
            if (queryResponse.getStatus()
                    .equals(RegistryResponseStatus.SUCCESS)) {
                statusHandler.info("Update objects query successful");
            } else {
                throw EbxmlExceptionUtil.createQueryExceptionType(
                        UPDATE_OBJECTS_ERROR_MSG,
                        "Update objects query failed!");
            }
            if (CollectionUtil
                    .isNullOrEmpty(queryResponse.getRegistryObjects())) {
                statusHandler
                        .info("No results returned from update objects query");
            } else {
                statusHandler.info("Update objects query returned "
                        + queryResponse.getRegistryObjects() + " objects");
                objectsToUpdate.addAll(queryResponse.getRegistryObjects());
            }
        }

        if (checkReferences) {
            statusHandler
                    .info("Client has selected to check object references before submitting objects.");
            String unresolvedReferencesMessage = referenceResolver
                    .allReferencesResolve(objectsToUpdate);
            if (!unresolvedReferencesMessage.isEmpty()) {
                throw EbxmlExceptionUtil
                        .createUnresolvedReferenceExceptionType(
                                UPDATE_OBJECTS_ERROR_MSG,
                                unresolvedReferencesMessage);
            }

        }

        // Refresh the list with the versionedObjects instead of the originals
        // if CREATE_OR_VERSION mode is selected
        if (mode.equals(Mode.CREATE_OR_VERSION)) {
            List<RegistryObjectType> tempList = new ArrayList<RegistryObjectType>(
                    objectsToUpdate.size());
            for (RegistryObjectType objectToUpdate : objectsToUpdate) {
                List<RegistryObjectType> generatedObjects = versionObject(
                        objectToUpdate, new ArrayList<RegistryObjectType>());
                for (RegistryObjectType generatedObject : generatedObjects) {
                    if (generatedObject instanceof AssociationType) {
                        // save the association
                        registryObjectDao.create(generatedObject);
                    } else {
                        tempList.add(generatedObject);
                    }
                }
            }
            objectsToUpdate.clear();
            objectsToUpdate.addAll(tempList);
        }

        for (RegistryObjectType objToUpdate : objectsToUpdate) {
            statusHandler.info("Updating object: " + objToUpdate.getId()
                    + "...");
            RegistryObjectType updatedObject = applyUpdates(objToUpdate,
                    updateActions);
            registryObjectDao.merge(updatedObject, objToUpdate);
        }
        if (!objectsToUpdate.isEmpty()) {
            EventBus.publish(new CreateAuditTrailEvent(request.getId(),
                    request, ActionTypes.update, objectsToUpdate));
        }

        long totalTime = System.currentTimeMillis() - startTime;
        statusHandler
                .info("LifeCycleManager updateObjects operation completed in "
                        + totalTime + " ms");
        return response;
    }

    private RegistryObjectType applyUpdates(RegistryObjectType objectToUpdate,
            List<UpdateActionType> updateActions) throws MsgRegistryException {
        for (UpdateActionType updateAction : updateActions) {
            QueryExpressionType selector = updateAction.getSelector();
            String xpathExpression = null;
            if (!selector.getQueryLanguage().equals(QueryLanguages.XPATH)) {
                throw EbxmlExceptionUtil
                        .createUnsupportedCapabilityExceptionType(
                                UPDATE_OBJECTS_ERROR_MSG,
                                "This registry does not currently support "
                                        + selector.getQueryLanguage());

            }
            if (selector instanceof StringQueryExpressionType) {
                xpathExpression = ((StringQueryExpressionType) selector)
                        .getValue();
            } else {
                throw EbxmlExceptionUtil
                        .createUnsupportedCapabilityExceptionType(
                                UPDATE_OBJECTS_ERROR_MSG,
                                "This registry currently only supports XPath embedded in StringQueryExpressionType objects");
            }

            try {
                switch (updateAction.getUpdateMode()) {
                case Insert:
                    if (updateAction.getValueHolder() == null) {
                        throw EbxmlExceptionUtil
                                .createRegistryExceptionType(
                                        UPDATE_OBJECTS_ERROR_MSG,
                                        "Update action mode of Insert must specify a value!");
                    }
                    objectToUpdate = (RegistryObjectType) xpathProcessor
                            .insert(objectToUpdate, xpathExpression,
                                    updateAction.getValueHolder().getValue());
                    break;
                case Update:
                    if (updateAction.getValueHolder() == null) {
                        throw EbxmlExceptionUtil
                                .createRegistryExceptionType(
                                        UPDATE_OBJECTS_ERROR_MSG,
                                        "Update action mode of Update must specify a value!");
                    }
                    objectToUpdate = (RegistryObjectType) xpathProcessor
                            .update(objectToUpdate, xpathExpression,
                                    updateAction.getValueHolder().getValue());
                    break;
                case Delete:
                    if (updateAction.getValueHolder() != null) {
                        throw EbxmlExceptionUtil
                                .createRegistryExceptionType(
                                        UPDATE_OBJECTS_ERROR_MSG,
                                        "Update action mode of Delete must NOT specify a value!");
                    }
                    objectToUpdate = (RegistryObjectType) xpathProcessor
                            .delete(objectToUpdate, xpathExpression);
                    break;
                }
            } catch (EbxmlRegistryException e) {
                throw EbxmlExceptionUtil.createMsgRegistryException(
                        UPDATE_OBJECTS_ERROR_MSG, e);
            }
        }
        return objectToUpdate;
    }

    public QueryManagerImpl getQueryManager() {
        return queryManager;
    }

    public void setQueryManager(QueryManagerImpl queryManager) {
        this.queryManager = queryManager;
    }

    public void setValidator(ValidatorImpl validator) {
        this.validator = validator;
    }

    public void setCataloger(CatalogerImpl cataloger) {
        this.cataloger = cataloger;
    }

    public void setRegistryObjectDao(RegistryObjectDao registryObjectDao) {
        this.registryObjectDao = registryObjectDao;
    }

    public void setReferenceResolver(ObjectReferenceResolver referenceResolver) {
        this.referenceResolver = referenceResolver;
    }

    public void setXpathProcessor(RegistryXPathProcessor xpathProcessor) {
        this.xpathProcessor = xpathProcessor;
    }

}
