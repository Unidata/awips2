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
import java.util.Set;

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
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.ClassificationType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.ExternalIdentifierType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.ExternalLinkType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.ObjectRefType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.QueryType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryObjectListType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryObjectType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.TaxonomyElementType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.VersionInfoType;
import oasis.names.tc.ebxml.regrep.xsd.rs.v4.InvalidRequestExceptionType;
import oasis.names.tc.ebxml.regrep.xsd.rs.v4.ObjectExistsExceptionType;
import oasis.names.tc.ebxml.regrep.xsd.rs.v4.RegistryExceptionType;
import oasis.names.tc.ebxml.regrep.xsd.rs.v4.RegistryResponseType;
import oasis.names.tc.ebxml.regrep.xsd.rs.v4.UnresolvedReferenceExceptionType;
import oasis.names.tc.ebxml.regrep.xsd.rs.v4.UnsupportedCapabilityExceptionType;
import oasis.names.tc.ebxml.regrep.xsd.spi.v4.ValidateObjectsRequest;
import oasis.names.tc.ebxml.regrep.xsd.spi.v4.ValidateObjectsResponse;

import com.raytheon.uf.common.registry.ebxml.RegistryUtil;
import com.raytheon.uf.common.registry.event.InsertRegistryEvent;
import com.raytheon.uf.common.registry.event.RegistryEvent.Action;
import com.raytheon.uf.common.registry.event.RegistryStatisticsEvent;
import com.raytheon.uf.common.registry.event.RemoveRegistryEvent;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.edex.registry.ebxml.audittrail.AuditTrailManager;
import com.raytheon.uf.edex.registry.ebxml.constants.ActionTypes;
import com.raytheon.uf.edex.registry.ebxml.constants.AssociationTypes;
import com.raytheon.uf.edex.registry.ebxml.constants.DeletionScope;
import com.raytheon.uf.edex.registry.ebxml.constants.ErrorSeverity;
import com.raytheon.uf.edex.registry.ebxml.constants.RegistryObjectTypes;
import com.raytheon.uf.edex.registry.ebxml.constants.RegistryResponseStatus;
import com.raytheon.uf.edex.registry.ebxml.constants.StatusTypes;
import com.raytheon.uf.edex.registry.ebxml.dao.RegistryObjectTypeDao;
import com.raytheon.uf.edex.registry.ebxml.exception.EbxmlRegistryException;
import com.raytheon.uf.edex.registry.ebxml.services.cataloger.CatalogerImpl;
import com.raytheon.uf.edex.registry.ebxml.services.query.QueryManagerImpl;
import com.raytheon.uf.edex.registry.ebxml.services.util.RegistrySessionManager;
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
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */

public class LifecycleManagerImpl implements LifecycleManager {

    /** The logger */
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(LifecycleManagerImpl.class);

    /** The query manager */
    private QueryManagerImpl queryManager;

    /** The validator */
    private ValidatorImpl validator;

    /** The cataloger */
    @SuppressWarnings("unused")
    private CatalogerImpl cataloger;

    /** The registry object data access object */
    private RegistryObjectTypeDao registryObjectDao;

    private AuditTrailManager auditTrailMgr;

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
        statusHandler.info("LifecycleManager received removeObjectsRequest");
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
            responseOption.setReturnType("ObjectRef");
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
            auditTrailMgr.createAuditTrailFromResponse(response,
                    ActionTypes.delete);
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
        long avTimePerRecord = totalTime / objRefTypes.size();

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
                RegistrySessionManager.postEvent(event);
            }

            // all registry removals are logged for statistics
            RegistrySessionManager.postEvent(new RegistryStatisticsEvent(obj
                    .getObjectType(), obj.getStatus(), obj.getOwner(),
                    avTimePerRecord));
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
        return submitObjects(request, true);
    }

    public RegistryResponseType submitObjectsInternal(
            SubmitObjectsRequest request) throws MsgRegistryException {
        if (request == null) {
            return null;
        }
        return submitObjects(request, false);
    }

    /**
     * Submits objects bypassing the validation step. This is used when
     * initializing the database
     */
    protected RegistryResponseType submitObjects(SubmitObjectsRequest request,
            boolean validate) throws MsgRegistryException {
        statusHandler.info("LifecycleManager received submitObjectsRequest");
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
            // check the references. A MsgRegistryException error will be thrown
            // if references fail to resolve
            for (RegistryObjectType obj : objs) {
                resolveReferences(obj, obj.getId());
            }
        }
        if (submitMode.equals(Mode.CREATE_OR_REPLACE)
                || submitMode.equals(Mode.CREATE_OR_VERSION)
                || submitMode.equals(Mode.CREATE_ONLY)) {

            if (validate) {
                statusHandler.info("Validating objects...");
                ValidateObjectsResponse validateResponse = validateObjects(request
                        .getRegistryObjectList());
                if (validateResponse.getException().isEmpty()) {
                    statusHandler
                            .info("Objects successfully validated! Submitting...");
                    processSubmit(submitMode, objs, response, request.getId());
                } else {
                    statusHandler
                            .info("Objects failed to validate! submitObjects aborted!");
                    response.getException().addAll(
                            validateResponse.getException());
                }
            } else {
                statusHandler.info("Object validation skipped");
                processSubmit(submitMode, objs, response, request.getId());
            }
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
        response.setObjectRefList(EbxmlObjectUtil
                .createObjectRefListFromObjects(objs));
        long totalTime = System.currentTimeMillis() - startTime;
        statusHandler
                .info("LifeCycleManager submitObjects operation completed in "
                        + totalTime + " ms");

        // gives a close estimate to amount taken on each object
        // individually, this will be millis in most cases, hopefully
        long avTimePerRecord = totalTime / objs.size();

        for (RegistryObjectType obj : objs) {
            RegistrySessionManager.postEvent(new InsertRegistryEvent(obj
                    .getId(), obj.getLid(), obj.getObjectType()));
            // also log a statistical event
            RegistrySessionManager.postEvent(new RegistryStatisticsEvent(obj
                    .getObjectType(), obj.getStatus(), obj.getOwner(),
                    avTimePerRecord));
        }

        return response;
    }

    /**
     * Checks the specified object to ensure that all references via references
     * attributes and slots to other RegistryObjects are resolvable
     * 
     * @param object
     *            The object to check
     * @param originalId
     *            A record of the original object's id as this id will not need
     *            to pass the check since it is the id of the object being
     *            submitted
     * @throws MsgRegistryException
     *             If errors occur while querying the registry, or there is an
     *             unresolvable property
     */
    private void resolveReferences(RegistryObjectType object, String originalId)
            throws MsgRegistryException {
        statusHandler.info("Checking references for object with id ["
                + object.getId() + "]...");
        Set<ClassificationType> classifications = object.getClassification();
        if (classifications != null) {
            for (ClassificationType classification : classifications) {
                resolveReferences(classification, originalId);
            }
        }
        Set<ExternalIdentifierType> externIdents = object
                .getExternalIdentifier();
        if (externIdents != null) {
            for (ExternalIdentifierType externIdent : externIdents) {
                resolveReferences(externIdent, originalId);
            }
        }
        Set<ExternalLinkType> externLinks = object.getExternalLink();
        if (externLinks != null) {
            for (ExternalLinkType externLink : externLinks) {
                resolveReferences(externLink, originalId);
            }
        }

        if (!object.getId().equals(originalId)) {
            RegistryObjectType classResult = null;
            try {
                classResult = registryObjectDao.getById(object.getId());
            } catch (EbxmlRegistryException e) {
                throw EbxmlExceptionUtil
                        .createMsgRegistryException(
                                "Query error encountered while checking references for submitObjects",
                                QueryExceptionType.class,
                                "",
                                "Query error encountered while checking references for submitObjects",
                                "An error was encountered while querying for an object with an id of ["
                                        + object.getId() + "]",
                                ErrorSeverity.ERROR, e, statusHandler);
            }
            if (classResult == null) {
                throw EbxmlExceptionUtil.createMsgRegistryException(
                        "Unresolved reference found",
                        UnresolvedReferenceExceptionType.class, "",
                        "Unresolved reference found",
                        "The registry does not contain a reference to type ["
                                + object.getClass().getCanonicalName()
                                + "] with id [" + object.getId() + "]",
                        ErrorSeverity.ERROR, statusHandler);
            }
        }
        statusHandler
                .info("References successfully resolve for object with id ["
                        + object.getId() + "]");
    }

    /**
     * Validates the given object list
     * 
     * @param objectList
     *            The list of objects to be validated
     * @return A response containing the status of the validation action
     * @throws MsgRegistryException
     *             If errors occur during validation
     */
    private ValidateObjectsResponse validateObjects(
            RegistryObjectListType objectList) throws MsgRegistryException {
        ValidateObjectsRequest validateRequest = EbxmlObjectUtil.spiObjectFactory
                .createValidateObjectsRequest();
        validateRequest.setOriginalObjects(objectList);
        validateRequest
                .setComment("Validation of objects before registry submission");
        return validator.validateObjects(validateRequest);
    }

    /**
     * 
     * Submits objects to the registry
     * 
     * @param submitMode
     *            The mode of submission
     * @param objs
     *            The objects to submit
     * @param response
     *            The response object to update with any errors or warnings
     * @throws MsgRegistryException
     *             If the submission process encounters errors
     * @return time taken for transaction
     * @throws MsgRegistryException
     */
    private void processSubmit(Mode submitMode, List<RegistryObjectType> objs,
            RegistryResponseType response, String requestId)
            throws MsgRegistryException {

        Map<String, RegistryObjectType> storedObjects = new HashMap<String, RegistryObjectType>();
        List<RegistryObjectType> objsCreated = new ArrayList<RegistryObjectType>();
        List<RegistryObjectType> objsVersioned = new ArrayList<RegistryObjectType>();
        List<RegistryObjectType> objsUpdated = new ArrayList<RegistryObjectType>();
        for (RegistryObjectType obj : objs) {
            statusHandler.info("Processing object [" + obj.getId() + "]");
            if (obj.getId() == null) {
                if (submitMode.equals(Mode.CREATE_ONLY)) {
                    statusHandler
                            .info("Generating id for object specified with CREATE_ONLY Mode");
                    String uuid = EbxmlObjectUtil.getUUID();
                    obj.setId(uuid);
                } else {
                    response.getException()
                            .add(EbxmlExceptionUtil
                                    .createRegistryException(
                                            InvalidRequestExceptionType.class,
                                            "",
                                            "The id field MUST be specified by the client",
                                            "Please specify an id for all registry objects submitted",
                                            ErrorSeverity.ERROR, statusHandler));
                    continue;
                }
            }
            if (obj.getLid() == null) {
                response.getException()
                        .add(EbxmlExceptionUtil
                                .createRegistryException(
                                        InvalidRequestExceptionType.class,
                                        "",
                                        "The lid field MUST be specified by the client",
                                        "Please specify an lid for all registry objects submitted",
                                        ErrorSeverity.ERROR, statusHandler));
                continue;

            }

            List<RegistryObjectType> dbObjects = new ArrayList<RegistryObjectType>();
            try {
                dbObjects = registryObjectDao.getByLid(obj.getLid());
            } catch (EbxmlRegistryException e1) {
                throw EbxmlExceptionUtil.createMsgRegistryException(
                        "Database submission error",
                        InvalidRequestExceptionType.class, "",
                        "Error querying for database objects",
                        "The query for objects having lid [" + obj.getLid()
                                + "] failed", ErrorSeverity.ERROR, e1,
                        statusHandler);
            }
            storedObjects.clear();
            for (RegistryObjectType regObj : dbObjects) {
                storedObjects.put(regObj.getId(), regObj);
            }

            if (obj instanceof TaxonomyElementType) {
                generatePaths((TaxonomyElementType) obj, "");
            }
            try {
                if (obj.getVersionInfo() == null) {
                    VersionInfoType version = EbxmlObjectUtil
                            .newVersionObject();
                    obj.setVersionInfo(version);
                }

                switch (submitMode) {
                case CREATE_OR_REPLACE:
                    if (storedObjects.containsKey(obj.getId())) {
                        VersionInfoType versionInfo = storedObjects.get(
                                obj.getId()).getVersionInfo();
                        obj.setVersionInfo(versionInfo);
                        obj.setStatus(storedObjects.get(obj.getId())
                                .getStatus());
                        statusHandler.info("Object [" + obj.getId()
                                + "] replaced in the registry.");
                        registryObjectDao
                                .delete(storedObjects.get(obj.getId()));
                        objsUpdated.add(obj);

                    } else {
                        obj.setStatus(StatusTypes.APPROVED);
                        obj.setVersionInfo(EbxmlObjectUtil.newVersionObject());
                        statusHandler.info("Object [" + obj.getId()
                                + "] added to the registry.");
                        objsCreated.add(obj);
                    }
                    break;
                case CREATE_OR_VERSION:
                    if (storedObjects.containsKey(obj.getId())) {
                        VersionInfoType versionInfo = dbObjects.get(0)
                                .getVersionInfo();
                        obj.setVersionInfo(EbxmlObjectUtil
                                .getNewVersion(versionInfo));
                        obj.setStatus(storedObjects.get(obj.getId())
                                .getStatus());
                        statusHandler.info("Object [" + obj.getId()
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
                                .getInternationalString(obj.getId()
                                        + " Supersedes "
                                        + dbObjects.get(0).getId()));
                        versionAssociation
                                .setOwner(dbObjects.get(0).getOwner());
                        versionAssociation
                                .setObjectType(RegistryObjectTypes.ASSOCIATION);
                        versionAssociation.setSourceObject(obj.getId());
                        versionAssociation.setTargetObject(dbObjects.get(0)
                                .getId());
                        versionAssociation.setStatus(StatusTypes.APPROVED);
                        versionAssociation.setType(AssociationTypes.SUPERSEDES);
                        versionAssociation.setVersionInfo(EbxmlObjectUtil
                                .newVersionObject());
                        registryObjectDao.save(versionAssociation);
                        objsVersioned.add(obj);
                        statusHandler
                                .info("Supersedes association for new version of ["
                                        + obj.getId()
                                        + "] persisted to the registry");
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
                        obj.setVersionInfo(EbxmlObjectUtil.newVersionObject());
                        statusHandler.info("Object [" + obj.getId()
                                + "] added to the registry.");
                        objsCreated.add(obj);
                    }
                    break;
                case CREATE_ONLY:
                    if (storedObjects.containsKey(obj.getId())) {
                        String message = "Object with id [" + obj.getId()
                                + "] already exists";
                        response.getException()
                                .add(EbxmlExceptionUtil
                                        .createRegistryException(
                                                ObjectExistsExceptionType.class,
                                                "",
                                                message,
                                                "The "
                                                        + Mode.CREATE_ONLY
                                                        + " submit mode only accepts new objects",
                                                ErrorSeverity.ERROR,
                                                statusHandler));

                        throw EbxmlExceptionUtil
                                .createMsgRegistryException(message,
                                        ObjectExistsExceptionType.class, "",
                                        message, "Error submitting object ["
                                                + obj.getId() + "]",
                                        ErrorSeverity.ERROR, null,
                                        statusHandler);

                    } else {
                        obj.setVersionInfo(EbxmlObjectUtil.newVersionObject());
                        obj.setStatus(StatusTypes.APPROVED);
                        statusHandler.info("Object [" + obj.getId()
                                + "] added to the registry.");
                        objsCreated.add(obj);
                    }

                    break;
                }
                registryObjectDao.save(obj);
            } catch (EbxmlRegistryException e) {
                throw EbxmlExceptionUtil.createMsgRegistryException(
                        "Database submission error", QueryExceptionType.class,
                        "", "Database submission error",
                        "Error submitting object [" + obj.getId() + "]",
                        ErrorSeverity.ERROR, e, statusHandler);
            }

            // TODO: Implement proper cataloging of objects accorind to EbXML
            // spec
        }

        if (response.getException().isEmpty()) {
            statusHandler.info("Submit objects successful");
            statusHandler.info("Creating auditable events....");
            try {
                if (!objsCreated.isEmpty()) {
                    auditTrailMgr.createAuditTrail(objsCreated,
                            ActionTypes.create, requestId);
                }
                if (!objsVersioned.isEmpty()) {
                    auditTrailMgr.createAuditTrail(objsCreated,
                            ActionTypes.version, requestId);
                }
                if (!objsUpdated.isEmpty()) {
                    auditTrailMgr.createAuditTrail(objsCreated,
                            ActionTypes.update, requestId);
                }
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
        statusHandler.info("LifecycleManager received updateObjects");
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

    public void setRegistryObjectDao(RegistryObjectTypeDao registryObjectDao) {
        this.registryObjectDao = registryObjectDao;
    }

    public void setAuditTrailMgr(AuditTrailManager auditTrailMgr) {
        this.auditTrailMgr = auditTrailMgr;
    }

    public void setCataloger(CatalogerImpl cataloger) {
        this.cataloger = cataloger;
    }

}
