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
import oasis.names.tc.ebxml.regrep.xsd.rs.v4.UnsupportedCapabilityExceptionType;

import org.springframework.transaction.annotation.Propagation;
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
 * Jun 24, 2013 2106       djohnson    Requires a transaction to already be open.
 * 8/1/2013     1693       bphillip    Added check references and refactored submit objects to conform to EBXML 4.0 spec
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

    @Resource
    private WebServiceContext wsContext;

    private ObjectReferenceResolver referenceResolver;

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
            auditableEventService.createAuditableEventFromRefs(request, null,
                    null, null, objRefs);
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
            referenceResolver.checkReferences(objs);

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

        List<RegistryObjectType> objsCreated = new ArrayList<RegistryObjectType>();
        List<RegistryObjectType> objsUpdated = new ArrayList<RegistryObjectType>();
        for (RegistryObjectType obj : request.getRegistryObjectList()
                .getRegistryObject()) {
            String objectId = obj.getId();
            final String objectLid = obj.getLid();
            statusHandler.debug("Processing object [" + objectId + "]");
            if (objectLid == null) {
                throw EbxmlExceptionUtil
                        .createInvalidRequestException("LID MUST be specified by client");
            }
            if (obj instanceof TaxonomyElementType) {
                generatePaths((TaxonomyElementType) obj, "");
            }

            RegistryObjectType existingObject = null;
            switch (request.getMode()) {
            case CREATE_OR_REPLACE:

                if (objectId == null) {
                    throw EbxmlExceptionUtil
                            .createInvalidRequestException("ID MUST be specified by client");
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
                if (objectId == null) {
                    throw EbxmlExceptionUtil
                            .createInvalidRequestException("ID MUST be specified by client");
                }
                boolean idExists = registryObjectDao.idExists(objectId);
                boolean lidExists = registryObjectDao.lidExists(objectLid);

                /*
                 * If id does not exist and lid does not exist, server MUST
                 * create new object using the id (create)
                 */
                if (!idExists && !lidExists) {
                    objsCreated.add(obj);
                    registryObjectDao.create(obj);
                    statusHandler.info("Object [" + objectId
                            + "] created in the registry.");
                }
                /*
                 * If id does not exist and lid exists server MUST throw
                 * InvalidRequestException
                 */
                else if (!idExists && lidExists) {
                    throw EbxmlExceptionUtil
                            .createInvalidRequestException("Specified object ID does not exist yet lid exists, unable to version");
                }
                /*
                 * If id exists, server MUST create a new version of existing
                 * object matching the id
                 */
                else if (idExists) {
                    existingObject = registryObjectDao.getById(objectId);
                    String nextVersion = registryObjectDao
                            .getNextVersion(existingObject);
                    obj.setVersionInfo(new VersionInfoType(nextVersion));
                    obj.setStatus(existingObject.getStatus());
                    obj.setId(existingObject.getId()
                            + "_"
                            + nextVersion.substring(nextVersion
                                    .lastIndexOf(".") + 1));
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
                    versionAssociation
                            .setObjectType(RegistryObjectTypes.ASSOCIATION);
                    versionAssociation.setSourceObject(objectId);
                    versionAssociation.setTargetObject(existingObject.getId());
                    versionAssociation.setStatus(StatusTypes.APPROVED);
                    versionAssociation.setType(AssociationTypes.SUPERSEDES);
                    versionAssociation.setVersionInfo(new VersionInfoType());

                    objsCreated.add(obj);
                    objsCreated.add(versionAssociation);
                    registryObjectDao.create(versionAssociation);
                    registryObjectDao.create(obj);
                    statusHandler.info("Object [" + objectId
                            + "] versioned in the registry.");
                }
                break;
            case CREATE_ONLY:

                if (registryObjectDao.lidExists(objectLid)) {
                    throw EbxmlExceptionUtil
                            .createObjectExistsException("Object already exists with lid: "
                                    + objectId
                                    + ". Cannot submit using CREATE_ONLY mode");
                }
                if (objectId == null) {
                    objectId = RegistryUtil.generateRegistryObjectId();
                } else {
                    if (registryObjectDao.idExists(objectId)) {
                        throw EbxmlExceptionUtil
                                .createObjectExistsException("Object already exists with id: "
                                        + objectId
                                        + ". Cannot submit using CREATE_ONLY mode");
                    } else {
                        objsCreated.add(obj);
                        registryObjectDao.create(obj);
                        statusHandler.info("Object [" + objectId
                                + "] created in the registry.");
                    }
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
                auditableEventService.createAuditableEventFromObjects(request,
                        objsCreated, objsUpdated, null, null);
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

    public void setReferenceResolver(ObjectReferenceResolver referenceResolver) {
        this.referenceResolver = referenceResolver;
    }

}
