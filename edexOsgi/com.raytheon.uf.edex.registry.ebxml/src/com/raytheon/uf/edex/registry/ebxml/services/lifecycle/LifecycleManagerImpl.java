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
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.annotation.Resource;
import javax.xml.ws.WebServiceContext;

import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.LifecycleManager;
import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.MsgRegistryException;
import oasis.names.tc.ebxml.regrep.xsd.lcm.v4.Mode;
import oasis.names.tc.ebxml.regrep.xsd.lcm.v4.RemoveObjectsRequest;
import oasis.names.tc.ebxml.regrep.xsd.lcm.v4.SubmitObjectsRequest;
import oasis.names.tc.ebxml.regrep.xsd.lcm.v4.UpdateObjectsRequest;
import oasis.names.tc.ebxml.regrep.xsd.query.v4.QueryExceptionType;
import oasis.names.tc.ebxml.regrep.xsd.query.v4.QueryResponse;
import oasis.names.tc.ebxml.regrep.xsd.query.v4.ResponseOptionType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.AssociationType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.ClassificationNodeType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.ClassificationSchemeType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.ExtensibleObjectType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.ObjectRefType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.QueryType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryObjectType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.TaxonomyElementType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.VersionInfoType;
import oasis.names.tc.ebxml.regrep.xsd.rs.v4.InvalidRequestExceptionType;
import oasis.names.tc.ebxml.regrep.xsd.rs.v4.ObjectExistsExceptionType;
import oasis.names.tc.ebxml.regrep.xsd.rs.v4.RegistryExceptionType;
import oasis.names.tc.ebxml.regrep.xsd.rs.v4.RegistryResponseStatus;
import oasis.names.tc.ebxml.regrep.xsd.rs.v4.RegistryResponseType;
import oasis.names.tc.ebxml.regrep.xsd.rs.v4.UnresolvedReferenceExceptionType;
import oasis.names.tc.ebxml.regrep.xsd.rs.v4.UnsupportedCapabilityExceptionType;
import oasis.names.tc.ebxml.regrep.xsd.spi.v4.ValidateObjectsRequest;
import oasis.names.tc.ebxml.regrep.xsd.spi.v4.ValidateObjectsResponse;

import org.springframework.transaction.annotation.Transactional;

import com.raytheon.uf.common.event.EventBus;
import com.raytheon.uf.common.registry.constants.ActionTypes;
import com.raytheon.uf.common.registry.constants.AssociationTypes;
import com.raytheon.uf.common.registry.constants.DeletionScope;
import com.raytheon.uf.common.registry.constants.ErrorSeverity;
import com.raytheon.uf.common.registry.constants.QueryReturnTypes;
import com.raytheon.uf.common.registry.constants.RegistryObjectTypes;
import com.raytheon.uf.common.registry.constants.StatusTypes;
import com.raytheon.uf.common.registry.ebxml.RegistryUtil;
import com.raytheon.uf.common.registry.event.InsertRegistryEvent;
import com.raytheon.uf.common.registry.event.RegistryEvent.Action;
import com.raytheon.uf.common.registry.event.RegistryStatisticsEvent;
import com.raytheon.uf.common.registry.event.RemoveRegistryEvent;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.edex.registry.ebxml.dao.RegistryObjectDao;
import com.raytheon.uf.edex.registry.ebxml.dao.RegistryObjectTypeDao;
import com.raytheon.uf.edex.registry.ebxml.exception.EbxmlRegistryException;
import com.raytheon.uf.edex.registry.ebxml.services.AuditableEventService;
import com.raytheon.uf.edex.registry.ebxml.services.cataloger.CatalogerImpl;
import com.raytheon.uf.edex.registry.ebxml.services.query.QueryManagerImpl;
import com.raytheon.uf.edex.registry.ebxml.services.validator.ValidatorImpl;
import com.raytheon.uf.edex.registry.ebxml.util.EbxmlExceptionUtil;
import com.raytheon.uf.edex.registry.ebxml.util.EbxmlObjectUtil;

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
 * 
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */
@Transactional
public class LifecycleManagerImpl implements LifecycleManager {

    /** The logger */
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(LifecycleManagerImpl.class);

    @Resource
    private WebServiceContext wsContext;

    /** The query manager */
    private QueryManagerImpl queryManager;

    /** The validator */
    private ValidatorImpl validator;

    /** The cataloger */
    @SuppressWarnings("unused")
    private CatalogerImpl cataloger;

    /** The registry object data access object */
    private RegistryObjectDao registryObjectDao;

    private AuditableEventService auditableEventService;

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
        long startTime = System.currentTimeMillis();
        statusHandler
                .info("LifecycleManager received removeObjectsRequest from ["
                        + EbxmlObjectUtil.getClientHost(wsContext) + "]");
        RegistryResponseType response = EbxmlObjectUtil.rsObjectFactory
                .createRegistryResponseType();
        response.setRequestId(request.getId());
        response.setStatus(RegistryResponseStatus.SUCCESS);

        boolean checkReferences = request.isCheckReferences();
        if (checkReferences) {
            response.getException()
                    .add(EbxmlExceptionUtil
                            .createRegistryException(
                                    UnsupportedCapabilityExceptionType.class,
                                    "",
                                    "Checking references currently not supported",
                                    "This EBXML registry currently does not support checking references when removing objects",
                                    ErrorSeverity.WARNING, statusHandler));
            response.setStatus(RegistryResponseStatus.PARTIAL_SUCCESS);
        }

        boolean deleteChildren = request.isDeleteChildren();
        if (deleteChildren) {
            response.getException()
                    .add(EbxmlExceptionUtil
                            .createRegistryException(
                                    UnsupportedCapabilityExceptionType.class,
                                    "",
                                    "Deleting children currently not supported",
                                    "This EBXML registry currently does not support deleting children when removing objects",
                                    ErrorSeverity.WARNING, statusHandler));
            response.setStatus(RegistryResponseStatus.PARTIAL_SUCCESS);
        }

        String deletionScope = request.getDeletionScope();
        if (!deletionScope.equals(DeletionScope.DELETE_ALL)) {
            response.getException()
                    .add(EbxmlExceptionUtil
                            .createRegistryException(
                                    UnsupportedCapabilityExceptionType.class,
                                    "",
                                    "Deletion scope currently not supported",
                                    "This EBXML registry currently does not support specification of the deletion scope",
                                    ErrorSeverity.WARNING, statusHandler));
            response.setStatus(RegistryResponseStatus.PARTIAL_SUCCESS);
        }

        List<ObjectRefType> objRefs = new ArrayList<ObjectRefType>();
        List<RegistryObjectType> objRefTypes = new ArrayList<RegistryObjectType>();

        if (request.getObjectRefList() != null) {
            objRefs.addAll(request.getObjectRefList().getObjectRef());
            List<String> ids = new ArrayList<String>(objRefs.size());
            // First Query for the Objects that are to be deleted byReference
            // so that the proper notification message can be sent.
            for (ObjectRefType o : objRefs) {
                ids.add(o.getId());
            }
            try {
                objRefTypes.addAll(registryObjectDao.getById(ids));
            } catch (EbxmlRegistryException e) {
                throw EbxmlExceptionUtil
                        .createMsgRegistryException(
                                "Error deleting objects from the registry",
                                RegistryObjectTypeDao.class,
                                "",
                                "Error deleting objects from the registry",
                                "There was an unexpected error encountered while querying objects to delete using object refs",
                                ErrorSeverity.ERROR, e, statusHandler);
            }
        }

        // TODO: Handle querying for objects to delete
        QueryType query = request.getQuery();
        if (query != null) {
            ResponseOptionType responseOption = EbxmlObjectUtil.queryObjectFactory
                    .createResponseOptionType();
            responseOption.setReturnType(QueryReturnTypes.OBJECT_REF);
            QueryResponse queryResponse = queryManager.executeQuery(
                    responseOption, query);
            if (queryResponse.getStatus()
                    .equals(RegistryResponseStatus.SUCCESS)
                    || queryResponse.getStatus().equals(
                            RegistryResponseStatus.PARTIAL_SUCCESS)) {
                statusHandler.info("Remove objects query successful");
            }

            if (queryResponse.getObjectRefList() != null) {
                objRefs.addAll(queryResponse.getObjectRefList().getObjectRef());
            }
        }
        try {
            Map<String, List<ObjectRefType>> actionMap = new HashMap<String, List<ObjectRefType>>();
            actionMap.put(ActionTypes.delete, objRefs);
            auditableEventService.createAuditableEventsFromRefs(request,
                    actionMap, System.currentTimeMillis());
            registryObjectDao.deleteByRefs(objRefs);

        } catch (EbxmlRegistryException e) {
            throw EbxmlExceptionUtil
                    .createMsgRegistryException(
                            "Error deleting objects from the registry",
                            QueryExceptionType.class,
                            "",
                            "Error deleting objects from the registry",
                            "There was an unexpected error encountered while deleting objects using object refs",
                            ErrorSeverity.ERROR, e, statusHandler);
        }

        long totalTime = System.currentTimeMillis() - startTime;

        long avTimePerRecord = 0;
        if (!objRefTypes.isEmpty()) {
            avTimePerRecord = totalTime / objRefTypes.size();
        }

        statusHandler
                .info("LifeCycleManager removeObjects operation completed in "
                        + totalTime + " ms");

        for (RegistryObjectType obj : objRefTypes) {
            String objectType = obj.getObjectType();
            // Don't send notifications for Association types
            if (!objectType.equals(RegistryObjectTypes.ASSOCIATION)) {
                RemoveRegistryEvent event = new RemoveRegistryEvent(
                        request.getUsername(), obj.getId());
                event.setAction(Action.DELETE);
                event.setLid(obj.getLid());
                event.setObjectType(objectType);
                EventBus.publish(event);
            }

            EventBus.publish(new RegistryStatisticsEvent(obj.getObjectType(),
                    obj.getStatus(), obj.getOwner(), avTimePerRecord));
        }

        return response;
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
        long startTime = System.currentTimeMillis();

        RegistryResponseType response = EbxmlObjectUtil.rsObjectFactory
                .createRegistryResponseType();
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
            ValidateObjectsRequest validateObjectsRequest = new ValidateObjectsRequest();
            validateObjectsRequest.setOriginalObjects(request.getRegistryObjectList());

            // Uses the validation service directly, not going through the
            // web-service client interface
            final ValidateObjectsResponse validationResponse = validator
                    .serverValidateObjects(validateObjectsRequest,
                            EbxmlObjectUtil.spiObjectFactory
                                    .createValidateObjectsResponse());

            final List<RegistryExceptionType> validationExceptions = validationResponse.getException();
            final List<RegistryExceptionType> responseExceptions = response.getException();

            if (!validationExceptions.isEmpty()) {
                // Only care about unresolved references
                for (RegistryExceptionType exception : validationExceptions) {
                    if (exception instanceof UnresolvedReferenceExceptionType) {
                        responseExceptions.add(exception);
                    }
                }
            }

            if (!responseExceptions.isEmpty()) {
                throw EbxmlExceptionUtil
                        .createMsgRegistryException(
                                "Unresolved references occurred with the submitted registry objects",
                                responseExceptions.get(0), statusHandler);

            }
        }
        if (submitMode.equals(Mode.CREATE_OR_REPLACE)
                || submitMode.equals(Mode.CREATE_OR_VERSION)
                || submitMode.equals(Mode.CREATE_ONLY)) {
            processSubmit(request, response);
        } else {
            throw EbxmlExceptionUtil
                    .createMsgRegistryException(
                            "Error submitting",
                            UnsupportedCapabilityExceptionType.class,
                            "",
                            "Invalid submit mode: " + submitMode,
                            "Valid insert modes are: "
                                    + Arrays.toString(Mode.values()),
                            ErrorSeverity.ERROR, statusHandler);
        }

        response.setRequestId(request.getId());
        response.setObjectRefList(EbxmlObjectUtil.createObjectRefList(objs));
        long totalTime = System.currentTimeMillis() - startTime;
        statusHandler
                .info("LifeCycleManager submitObjects operation completed in "
                        + totalTime + " ms");

        // gives a close estimate to amount taken on each object
        // individually, this will be millis in most cases, hopefully
        long avTimePerRecord = 0;
        if (!objs.isEmpty()) {
            avTimePerRecord = totalTime / objs.size();
            for (RegistryObjectType obj : objs) {
                EventBus.publish(new InsertRegistryEvent(obj.getId(), obj
                        .getLid(), obj.getObjectType()));
                EventBus.publish(new RegistryStatisticsEvent(obj
                        .getObjectType(), obj.getStatus(), obj.getOwner(),
                        avTimePerRecord));
            }
        }

        return response;
    }

    /**
     * 
     * Submits objects to the registry
     * 
     * @param request
     *            The submit objects request
     * @param response
     *            The response object to update with any errors or warnings
     * @throws MsgRegistryException
     *             If the submission process encounters errors
     */
    private void processSubmit(SubmitObjectsRequest request,
            RegistryResponseType response) throws MsgRegistryException {

        Map<String, RegistryObjectType> storedObjects = new HashMap<String, RegistryObjectType>();
        List<RegistryObjectType> objsCreated = new ArrayList<RegistryObjectType>();
        List<RegistryObjectType> objsVersioned = new ArrayList<RegistryObjectType>();
        List<RegistryObjectType> objsUpdated = new ArrayList<RegistryObjectType>();
        for (RegistryObjectType obj : request.getRegistryObjectList()
                .getRegistryObject()) {
            String objectId = obj.getId();
            statusHandler.info("Processing object [" + objectId + "]");
            if (objectId == null) {
                if (request.getMode().equals(Mode.CREATE_ONLY)) {
                    statusHandler
                            .info("Generating id for object specified with CREATE_ONLY Mode");
                    objectId = EbxmlObjectUtil.getUUID();
                    obj.setId(objectId);
                } else {
                    final String message = "The id field MUST be specified by the client";
                    final String pleaseSpecifyId = "Please specify an id for all registry objects submitted";

                    final MsgRegistryException registryException = EbxmlExceptionUtil
                            .createMsgRegistryException(message,
                                    InvalidRequestExceptionType.class, "",
                                    message, pleaseSpecifyId,
                                    ErrorSeverity.ERROR, null, statusHandler);
                    response.getException().add(
                            registryException.getFaultInfo());
                    throw registryException;
                }
            }

            final String objectLid = obj.getLid();
            if (objectLid == null) {
                final String message = "The lid field MUST be specified by the client";
                final String pleaseSpecifyLid = "Please specify an lid for all registry objects submitted";

                final MsgRegistryException registryException = EbxmlExceptionUtil
                        .createMsgRegistryException(message,
                                InvalidRequestExceptionType.class, "", message,
                                pleaseSpecifyLid, ErrorSeverity.ERROR, null,
                                statusHandler);
                response.getException().add(registryException.getFaultInfo());
                throw registryException;
            }

            List<RegistryObjectType> dbObjects = registryObjectDao
                    .getByLid(objectLid);
            storedObjects.clear();
            for (RegistryObjectType regObj : dbObjects) {
                storedObjects.put(regObj.getId(), regObj);
            }

            if (obj instanceof TaxonomyElementType) {
                generatePaths((TaxonomyElementType) obj, "");
            }
            if (obj.getVersionInfo() == null) {
                VersionInfoType version = new VersionInfoType();
                obj.setVersionInfo(version);
            }

            switch (request.getMode()) {
            case CREATE_OR_REPLACE:
                if (storedObjects.containsKey(objectId)) {
                    VersionInfoType versionInfo = storedObjects.get(objectId)
                            .getVersionInfo();
                    obj.setVersionInfo(versionInfo);
                    obj.setStatus(storedObjects.get(objectId).getStatus());
                    /*
                     * A server MUST NOT perform update operations via
                     * SubmitObjects and UpdateObjects operations on a local
                     * replica of a remote object. (Except in the case of
                     * updating objects from notifications)
                     */
                    checkReplica(request, obj, storedObjects.get(objectId));
                    statusHandler.info("Object [" + objectId
                            + "] replaced in the registry.");
                    // registryObjectDao.delete(storedObjects.get(obj.getId()));
                    objsUpdated.add(obj);
                    registryObjectDao.merge(obj, storedObjects.get(objectId));

                } else {
                    obj.setStatus(StatusTypes.APPROVED);
                    obj.setVersionInfo(new VersionInfoType());
                    statusHandler.info("Object [" + objectId
                            + "] added to the registry.");
                    objsCreated.add(obj);
                    registryObjectDao.create(obj);
                }
                break;
            case CREATE_OR_VERSION:
                if (storedObjects.containsKey(objectId)) {
                    VersionInfoType versionInfo = dbObjects.get(0)
                            .getVersionInfo();
                    obj.setVersionInfo(EbxmlObjectUtil
                            .incrementVersion(versionInfo));
                    obj.setStatus(storedObjects.get(objectId).getStatus());
                    statusHandler.info("Object [" + objectId
                            + "] versioned in the registry.");
                    obj.setId(EbxmlObjectUtil.getUUID());
                    AssociationType versionAssociation = EbxmlObjectUtil.rimObjectFactory
                            .createAssociationType();
                    String idUUID = EbxmlObjectUtil.getUUID();
                    versionAssociation.setId(idUUID);
                    versionAssociation.setLid(idUUID);
                    versionAssociation.setName(RegistryUtil
                            .getInternationalString("Version Association"));
                    versionAssociation.setDescription(RegistryUtil
                            .getInternationalString(objectId + " Supersedes "
                                    + dbObjects.get(0).getId()));
                    versionAssociation.setOwner(dbObjects.get(0).getOwner());
                    versionAssociation
                            .setObjectType(RegistryObjectTypes.ASSOCIATION);
                    versionAssociation.setSourceObject(objectId);
                    versionAssociation
                            .setTargetObject(dbObjects.get(0).getId());
                    versionAssociation.setStatus(StatusTypes.APPROVED);
                    versionAssociation.setType(AssociationTypes.SUPERSEDES);
                    versionAssociation.setVersionInfo(new VersionInfoType());
                    registryObjectDao.create(versionAssociation);
                    objsVersioned.add(obj);
                    statusHandler
                            .info("Supersedes association for new version of ["
                                    + objectId + "] persisted to the registry");
                    registryObjectDao.create(obj);
                } else {
                    if (!dbObjects.isEmpty()) {
                        EbxmlExceptionUtil
                                .createRegistryException(
                                        InvalidRequestExceptionType.class,
                                        "",
                                        "Invalid submit request",
                                        "The submitted id does not exist, yet the lid does.  This is an invalid request",
                                        ErrorSeverity.ERROR, statusHandler);
                        continue;
                    }
                    obj.setStatus(StatusTypes.APPROVED);
                    obj.setVersionInfo(new VersionInfoType());
                    statusHandler.info("Object [" + objectId
                            + "] added to the registry.");
                    objsCreated.add(obj);
                    registryObjectDao.create(obj);
                }
                break;
            case CREATE_ONLY:
                if (storedObjects.containsKey(objectId)) {
                    final String message = "Object with id [" + objectId
                            + "] already exists";
                    final String onlyAcceptsNewObjects = "The "
                            + Mode.CREATE_ONLY
                            + " submit mode only accepts new objects";

                    final MsgRegistryException registryException = EbxmlExceptionUtil
                            .createMsgRegistryException(message,
                                    ObjectExistsExceptionType.class, "",
                                    message, onlyAcceptsNewObjects,
                                    ErrorSeverity.ERROR, null, statusHandler);
                    response.getException().add(
                            registryException.getFaultInfo());
                    throw registryException;

                } else if (!storedObjects.isEmpty()) {
                    final String message = "Object with lid [" + objectLid
                            + "] already exists";
                    final String onlyAcceptsNewObjects = "The "
                            + Mode.CREATE_ONLY
                            + " submit mode only accepts new objects";

                    final MsgRegistryException registryException = EbxmlExceptionUtil
                            .createMsgRegistryException(message,
                                    ObjectExistsExceptionType.class, "",
                                    message, onlyAcceptsNewObjects,
                                    ErrorSeverity.ERROR, null, statusHandler);
                    response.getException().add(
                            registryException.getFaultInfo());
                    throw registryException;

                } else {
                    obj.setVersionInfo(new VersionInfoType());
                    obj.setStatus(StatusTypes.APPROVED);
                    statusHandler.info("Object [" + objectId
                            + "] added to the registry.");
                    objsCreated.add(obj);
                    registryObjectDao.create(obj);
                }

                break;
            }

            // TODO: Implement proper cataloging of objects according to EbXML
            // spec
        }

        if (response.getException().isEmpty()) {
            statusHandler.info("Submit objects successful");
            statusHandler.info("Creating auditable events....");
            try {
                Map<String, List<RegistryObjectType>> actionMap = new HashMap<String, List<RegistryObjectType>>();
                if (!objsCreated.isEmpty()) {
                    actionMap.put(ActionTypes.create, objsCreated);
                }
                if (!objsVersioned.isEmpty()) {
                    actionMap.put(ActionTypes.version, objsVersioned);
                }
                if (!objsUpdated.isEmpty()) {
                    actionMap.put(ActionTypes.update, objsUpdated);
                }
                auditableEventService.createAuditableEventsFromObjects(request,
                        actionMap, System.currentTimeMillis());
            } catch (EbxmlRegistryException e) {
                response.getException()
                        .add(EbxmlExceptionUtil
                                .createRegistryException(
                                        RegistryExceptionType.class,
                                        "",
                                        "Error sending request to create auditable event",
                                        "Error sending request to create auditable event",
                                        ErrorSeverity.ERROR, e, statusHandler));
            }
        } else {
            statusHandler
                    .warn("Submit objects failed. Returning errors to client.");
        }

    }

    private void checkReplica(SubmitObjectsRequest request,
            ExtensibleObjectType object1, ExtensibleObjectType object2)
            throws MsgRegistryException {
        boolean fromNotification = request
                .getSlotValue(EbxmlObjectUtil.HOME_SLOT_NAME) != null;
        String object1Home = object1
                .getSlotValue(EbxmlObjectUtil.HOME_SLOT_NAME);
        String object2Home = object2
                .getSlotValue(EbxmlObjectUtil.HOME_SLOT_NAME);

        if (fromNotification) {
            if (object1Home != null && object2Home == null) {
                throw EbxmlExceptionUtil.createMsgRegistryException(
                        "Cannot overwrite local object with replica",
                        ObjectExistsExceptionType.class, "",
                        "Cannot overwrite local object with replica", "",
                        ErrorSeverity.ERROR, statusHandler);
            } else if (object1Home != null && object2Home != null) {
                if (!object1Home.equals(object2Home)) {
                    throw EbxmlExceptionUtil
                            .createMsgRegistryException(
                                    "Cannot overwrite a remote replica from a different server",
                                    ObjectExistsExceptionType.class,
                                    "",
                                    "Cannot overwrite a remote replica from a different server",
                                    "", ErrorSeverity.ERROR, statusHandler);
                }
            }
        } else {
            if (object2Home != null) {
                throw EbxmlExceptionUtil.createMsgRegistryException(
                        "Cannot update replicas",
                        InvalidRequestExceptionType.class, "",
                        "Cannot update replicas", "", ErrorSeverity.ERROR,
                        statusHandler);
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
        statusHandler.info("LifecycleManager received updateObjects from ["
                + EbxmlObjectUtil.getClientHost(wsContext) + "]");
        throw EbxmlExceptionUtil.createMsgRegistryException(
                "updateObjects not yet implemented",
                UnsupportedCapabilityExceptionType.class, "",
                "Unsupported Service", "Unsupported Service",
                ErrorSeverity.ERROR, statusHandler);
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

    public void setAuditableEventService(
            AuditableEventService auditableEventService) {
        this.auditableEventService = auditableEventService;
    }

    public void setCataloger(CatalogerImpl cataloger) {
        this.cataloger = cataloger;
    }

    public void setRegistryObjectDao(RegistryObjectDao registryObjectDao) {
        this.registryObjectDao = registryObjectDao;
    }

}
