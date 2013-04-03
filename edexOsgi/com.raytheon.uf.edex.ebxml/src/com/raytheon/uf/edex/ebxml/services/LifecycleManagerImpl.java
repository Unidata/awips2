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

package com.raytheon.uf.edex.ebxml.services;

import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.CopyOnWriteArrayList;

import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.LifecycleManager;
import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.MsgRegistryException;
import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.QueryManager;
import oasis.names.tc.ebxml.regrep.xsd.lcm.v4.Mode;
import oasis.names.tc.ebxml.regrep.xsd.lcm.v4.RemoveObjectsRequest;
import oasis.names.tc.ebxml.regrep.xsd.lcm.v4.SubmitObjectsRequest;
import oasis.names.tc.ebxml.regrep.xsd.lcm.v4.UpdateActionType;
import oasis.names.tc.ebxml.regrep.xsd.lcm.v4.UpdateObjectsRequest;
import oasis.names.tc.ebxml.regrep.xsd.query.v4.QueryRequest;
import oasis.names.tc.ebxml.regrep.xsd.query.v4.QueryResponse;
import oasis.names.tc.ebxml.regrep.xsd.query.v4.ResponseOptionType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.ObjectRefListType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.ObjectRefType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.QueryType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryObjectListType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryObjectType;
import oasis.names.tc.ebxml.regrep.xsd.rs.v4.InvalidRequestExceptionType;
import oasis.names.tc.ebxml.regrep.xsd.rs.v4.ObjectExistsExceptionType;
import oasis.names.tc.ebxml.regrep.xsd.rs.v4.RegistryExceptionType;
import oasis.names.tc.ebxml.regrep.xsd.rs.v4.RegistryResponseType;
import oasis.names.tc.ebxml.regrep.xsd.rs.v4.UnresolvedReferenceExceptionType;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.edex.ebxml.registry.IRegistry;
import com.raytheon.uf.edex.ebxml.registry.RegistryManager;

/**
 * Implementation of the {@link LifecycleManager}.
 * 
 * <br>
 * <br>
 * Note: Quoted comments come from regrep-rs.pdf
 * 
 * @author jsherida
 */
public class LifecycleManagerImpl implements LifecycleManager {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(LifecycleManagerImpl.class);

    /** The registry instance */
    protected final IRegistry registry;

    /** The query manager used for executing queries */
    protected QueryManager queryManager;

    /** The list of lifecycle event listeners */
    protected List<LifecycleListener> listeners = new CopyOnWriteArrayList<LifecycleListener>();

    /**
     * Constructs a new LifecycleManagerImpl
     */
    public LifecycleManagerImpl() {
        this.registry = RegistryManager.getRegistryInstance();
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
     * response
     * 
     * @param partSubmitObjectsRequest
     *            The partSubmitObjectsRequest is a container class containing
     *            details of the objects to be inserted or replaced in the
     *            registry. The protocol for handling such requests is describe
     *            herein. The registryObjectList element specifies a set of
     *            RegistryObject instances that are being submitted to the
     *            server. The RegistryObjects in the list may be new objects
     *            being submitted to the server or they may be current objects
     *            already existing in the server.
     *            <p>
     *            .The checkReferences attribute specifies the reference
     *            checking behavior expected of the server:
     *            <p>
     *            · <i>true</i> - Specifies that a server MUST check submitted
     *            objects and make sure that all references via reference
     *            attributes and slots to other RegistryObjects are resolvable.
     *            If a reference does not resolve then the server MUST return
     *            {@link UnresolvedReferenceException}
     *            <p>
     *            · <i>false (default)</i> – Specifies that a server MUST NOT
     *            check submitted objects to make sure that all references via
     *            reference attributes and slots to other RegistryObjects are
     *            resolvable. If a reference does not resolve then the server
     *            MUST NOT return {@link UnresolvedReferenceException}
     *            <p>
     *            The {@link Mode} attribute specifies the semantics for how the
     *            server should handle RegistryObjects being submitted when they
     *            already exist in the server:
     *            <p>
     * 
     *            · <i>CreateOrReplace (default)</i> - If an object does not
     *            exist, server MUST create it as a new object. If an object
     *            already exists, server MUST replace the existing object with
     *            the submitted object
     *            <p>
     *            · <i>CreateOrVersion</i> - If an object does not exist, server
     *            MUST create it as a new object. If an object already exists,
     *            server MUST not alter the existing object and instead it MUST
     *            create a new version of the existing object using the state of
     *            the submitted object
     *            <p>
     *            · <i>CreateOnly</i> - If an object does not exist, server MUST
     *            create it as a new object. If an object already exists, the
     *            server MUST return an {@link ObjectExistsException} fault
     *            message
     * 
     * @return {@link RegistryResponseType} - The response contains the status
     *         of the request. This is returned only upon success
     * @throws MsgRegistryException
     *             The MsgRegistryException is returned if the request failed
     *             and wraps the following exceptions for the following reasons:
     *             <p>
     *             {@link RegistryExceptionType} - If internal errors occur
     *             while creating or replacing objects in the registry.<br>
     *             {@link UnresolvedReferenceException} - If checkReferences is
     *             true and a reference does not resolve.<br>
     *             {@link ObjectExistsException} - If the mode is CreateOnly and
     *             an object specified in the regObjects list alread exists <br>
     *             {@link InvalidRequestException} - Returned in the following
     *             cases:
     *             <p>
     *             · The mode is CreateOrReplace and the id is not specified
     *             <i>or</i> the lid is not specified<br>
     *             · The mode is CreateOrVersion and the id is not specified
     *             <i>or</i> the id does not exist and the lid does <i>or</i>
     *             the lid is not specified <br>
     *             · The mode is CreateOnly and the specified id already exists
     *             <i>or</i> the lid is not specified <i>or</i> the specified
     *             lid already exists
     */
    public RegistryResponseType submitObjects(
            SubmitObjectsRequest partSubmitObjectsRequest)
            throws MsgRegistryException {
        RegistryObjectListType regObjList = partSubmitObjectsRequest
                .getRegistryObjectList();
        Mode mode = partSubmitObjectsRequest.getMode();

        boolean checkReferences = partSubmitObjectsRequest.isCheckReferences();
        if (checkReferences) {
            statusHandler
                    .info("Checking object references is currently unimplemented!");
            // TODO Implement. See section 4.1.1.2
        }

        List<RegistryObjectType> regObjects = regObjList.getRegistryObject();
        RegistryExceptionType e = internalSubmitObjects(regObjects, mode);
        if (e != null) {
            throw new MsgRegistryException("Request failed: " + e.getMessage(),
                    e);
        }

        statusHandler.info("Notifying listeners..");
        for (LifecycleListener listener : listeners) {
            listener.objectsSubmitted(regObjects);
        }

        oasis.names.tc.ebxml.regrep.xsd.rs.v4.ObjectFactory rsFactory = new oasis.names.tc.ebxml.regrep.xsd.rs.v4.ObjectFactory();

        RegistryResponseType response = rsFactory.createRegistryResponseType();
        response.setRequestId(partSubmitObjectsRequest.getId());
        response.setStatus("Success");
        statusHandler
                .info("submitObjects complete.  Returning success response.");
        return response;
    }

    /**
     * @See {@link LifecycleManagerImpl#submitObjects} for details
     */
    private RegistryExceptionType internalSubmitObjects(
            List<RegistryObjectType> regObjects, Mode mode) {
        statusHandler.info("Attempting to submit " + regObjects.size()
                + " objects.");
        oasis.names.tc.ebxml.regrep.xsd.rs.v4.ObjectFactory rsFactory = new oasis.names.tc.ebxml.regrep.xsd.rs.v4.ObjectFactory();

        List<RegistryObjectType> toCreate = new ArrayList<RegistryObjectType>();
        List<RegistryObjectType> toReplace = new ArrayList<RegistryObjectType>();
        List<RegistryObjectType> toVersion = new ArrayList<RegistryObjectType>();

        for (RegistryObjectType obj : regObjects) {
            String id = obj.getId();
            if (id == null && mode != Mode.CREATE_ONLY) {
                InvalidRequestExceptionType e = rsFactory
                        .createInvalidRequestExceptionType();
                e.setMessage("ID value is not specified");
                return e;
            }

            String lid = obj.getLid();
            if (lid == null) {
                InvalidRequestExceptionType e = rsFactory
                        .createInvalidRequestExceptionType();
                e.setMessage("LID value is not specified");
                return e;
            }

            switch (mode) {
            case CREATE_OR_REPLACE:
                try {
                    if (registry.containsId(id)) {
                        toReplace.add(obj);
                    } else {
                        toCreate.add(obj);
                    }
                } catch (IOException ioe) {
                    StringWriter writer = new StringWriter();
                    ioe.printStackTrace(new PrintWriter(writer));

                    RegistryExceptionType e = rsFactory
                            .createRegistryExceptionType();
                    e.setMessage("Error contacting registry.");
                    e.setDetail(writer.toString());
                    e.setCode(ioe.getClass().toString());
                    return e;
                }
                break;
            case CREATE_OR_VERSION:
                try {
                    if (!registry.containsId(id)) {
                        if (!registry.containsLid(lid)) {
                            toCreate.add(obj);
                        } else {
                            InvalidRequestExceptionType e = rsFactory
                                    .createInvalidRequestExceptionType();
                            e.setMessage("Object LID exists in registry: "
                                    + lid);
                            return e;
                        }
                    } else {
                        toVersion.add(obj);
                    }
                } catch (IOException ioe) {
                    RegistryExceptionType e = rsFactory
                            .createRegistryExceptionType();
                    e.setMessage("Error contacting registry.");
                    e.setDetail(ioe.toString());
                    e.setCode(ioe.getClass().toString());
                    return e;
                }
                break;
            case CREATE_ONLY:
                try {
                    if (registry.containsLid(lid)) {
                        ObjectExistsExceptionType e = rsFactory
                                .createObjectExistsExceptionType();
                        e.setMessage("Object LID exitsts in registry: " + lid);
                        return e;
                    }
                    if (id == null) {
                        // "If unspecified Server MUST generate UUID URN"
                        // TODO Generate UUID.
                    }
                    if (registry.containsId(id)) {
                        ObjectExistsExceptionType e = rsFactory
                                .createObjectExistsExceptionType();
                        e.setMessage("Object ID exitsts in registry: " + id);
                        return e;
                    } else {
                        toCreate.add(obj);
                    }
                } catch (IOException ioe) {
                    RegistryExceptionType e = rsFactory
                            .createRegistryExceptionType();
                    e.setMessage("Error contacting registry.");
                    e.setDetail(ioe.toString());
                    e.setCode(ioe.getClass().toString());
                    return e;
                }
                break;
            }
        }

        RegistryExceptionType e = null;
        if (toCreate.size() > 0) {
            for (RegistryObjectType obj : toCreate) {
                statusHandler.info("Creating entry (" + "id: " + obj.getId()
                        + " lid: " + obj.getLid());
            }
            e = registry.create(toCreate);
            if (e != null)
                return e;
        }
        if (toReplace.size() > 0) {
            for (RegistryObjectType obj : toReplace) {
                statusHandler.info("Replacing entry (" + "id: " + obj.getId()
                        + " lid: " + obj.getLid());
            }
            e = registry.replace(toReplace);
            if (e != null)
                return e;
        }
        if (toVersion.size() > 0) {
            for (RegistryObjectType obj : toVersion) {
                statusHandler.info("Versioning entry (" + "id: " + obj.getId()
                        + " lid: " + obj.getLid());
            }
            e = registry.version(toVersion);
            if (e != null)
                return e;
        }
        statusHandler.info("Entries successfully submitted to registry");
        return null;
    }

    /**
     * The Remove Objects protocol allows a client to remove or delete one or
     * more RegistryObject instances from the server.
     * <p>
     * A client initiates the RemoveObjects protocol by sending a
     * RemoveObjectsRequest message to the LifecycleManager endpoint.
     * <p>
     * The LifecycleManager sends a RegistryResponse back to the client as
     * response.
     * 
     * @param partRemoveObjectsRequest
     *            The partRemoveObjectsRequest is a container class containing
     *            details of the objects to be deleted from the registry. The
     *            protocol for handling such requests is describe herein. This
     *            object contains the following information:
     *            <p>
     *            checkReferences – Specifies the reference checking behavior
     *            expected of the server:
     * 
     *            <i>true</i> - Specifies that a server MUST check objects being
     *            removed and make sure that there are no references to them
     *            from other objects via reference attributes and slots. If a
     *            reference exists then the server MUST return
     *            {@link ReferencesExistsException} <br>
     *            <i>false (default)</i> – Specifies that a server MUST NOT
     *            check objects being removed to make sure that there are no
     *            references to them from other objects via reference attributes
     *            and slots. If a reference exists then the server MUST NOT
     *            return {@link ReferencesExistsException}
     *            <p>
     *            deleteChildren – This attribute specifies whether or not to
     *            delete children of the objects being deleted according to the
     *            following behavior:
     * 
     *            <i>false</i> – Specifies the server MUST NOT delete the
     *            children of objects that are specified to be deleted
     *            <p>
     *            <i>true</i> – Specifies the server MUST delete children of
     *            objects being deleted if and only if those children are not
     *            children of any other parent objects
     *            <p>
     *            deletionScope - This attribute specifies the scope of impact
     *            of the RemoveObjectsRequest. The value of the deletionScope
     *            attribute MUST be a reference to a ClassificationNode within
     *            the canonical DeletionScopeType ClassificationScheme as
     *            described in ebRIM. A server MUST support the deletionScope
     *            types as defined by the canonical DeletionScopeType
     *            ClassificationScheme. The canonical DeletionScopeType
     *            ClassificationScheme may be extended by adding additional
     *            ClassificationNodes to it
     *            <p>
     *            The following canonical ClassificationNodes are defined for
     *            the DeletionScopeType ClassificationScheme:
     *            <p>
     *            <i>DeleteRepositoryItemOnly</i> - Specifies that the server
     *            MUST delete the RepositoryItem for the specified
     *            ExtrinsicObjects but MUST NOT delete the specified
     *            ExtrinsicObjects
     *            <p>
     *            <i>DeleteAll (default)</i> - Specifies that the request MUST
     *            delete both the RegistryObject and the RepositoryItem (if any)
     *            for the specified objects
     *            <p>
     *            Query - Specifies a query to be invoked. A server MUST remove
     *            all objects that match the specified query in addition to any
     *            other objects identified by the ObjectRefList element.
     *            <p>
     *            ObjectRefList - Specifies a collection of references to
     *            existing RegistryObject instances in the server. A server MUST
     *            remove all objects that are referenced by this element in
     *            addition to any other objects identified by the Query element.
     * @returns {@link RegistryResponseType} - The response contains the status
     *          of the request. This is returned only upon success
     * @throws MsgRegistryException
     *             The MsgRegistryException is returned if the request failed
     *             and wraps the following exceptions for the following reasons:
     *             <p>
     *             {@link ReferencesExistsException} Returned in the following
     *             cases:
     *             <p>
     *             · If checkReferences is true and a reference exists to them
     *             <br>
     *             <p>
     *             {@link UnresolvedReferenceException} Returned in the
     *             following cases:
     *             <p>
     *             · If the requestor referenced an object within the request
     *             that was not resolved during the processing of the request.
     */
    @Override
    public RegistryResponseType removeObjects(
            RemoveObjectsRequest partRemoveObjectsRequest)
            throws MsgRegistryException {

        boolean checkReferences = partRemoveObjectsRequest.isCheckReferences();
        if (checkReferences) {
            statusHandler
                    .info("Checking object references is currently unimplemented!");
            // TODO Implement. See section 4.3.1.2
        }

        boolean deleteChildren = partRemoveObjectsRequest.isDeleteChildren();
        if (deleteChildren) {
            statusHandler
                    .warn("The deleteChildren option is currently unimplemented");
            // TODO: Implement deleteChildren behavior
        }

        String deletionScope = partRemoveObjectsRequest.getDeletionScope();
        if (deletionScope != null) {
            statusHandler
                    .warn("The deletionScope option is currently unimplemented");
            // TODO: Implement deletionScope behavior
        }

        // TODO Exceptions. See section 4.3.1.4

        ObjectRefListType objRefList = partRemoveObjectsRequest
                .getObjectRefList();
        List<ObjectRefType> objRefs = new ArrayList<ObjectRefType>();
        if (objRefList != null) {
            objRefs.addAll(objRefList.getObjectRef());
        }

        List<ObjectRefType> queriedRefs = new ArrayList<ObjectRefType>();
        try {
            QueryType query = partRemoveObjectsRequest.getQuery();
            if (query != null) {
                queriedRefs = performQuery(query);
            }
        } catch (IllegalStateException e) {
            oasis.names.tc.ebxml.regrep.xsd.rs.v4.ObjectFactory rsFactory = new oasis.names.tc.ebxml.regrep.xsd.rs.v4.ObjectFactory();
            StringWriter writer = new StringWriter();
            e.printStackTrace(new PrintWriter(writer));
            RegistryExceptionType re = rsFactory.createRegistryExceptionType();
            re.setMessage(e.getMessage());
            re.setDetail(writer.toString());
            re.setCode(e.getClass().toString());

            throw new MsgRegistryException("Request not implemented", re, e);
        }

        statusHandler.info("Object references specified: " + objRefs.size());
        statusHandler.info("Object references returned from query: "
                + queriedRefs.size());
        objRefs.addAll(queriedRefs);
        statusHandler.info("Attempting to remove " + objRefs.size()
                + " objects.");
        RegistryExceptionType e = internalRemoveObjects(objRefs);
        if (e != null) {
            throw new MsgRegistryException("Request failed: " + e.getMessage(),
                    e);
        }

        // TODO Implement Audit Trails. See section 4.1.2
        statusHandler.info("Notifying listeners..");
        for (LifecycleListener listener : listeners) {
            listener.objectsRemoved(objRefs);
        }

        oasis.names.tc.ebxml.regrep.xsd.rs.v4.ObjectFactory rsFactory = new oasis.names.tc.ebxml.regrep.xsd.rs.v4.ObjectFactory();

        RegistryResponseType response = rsFactory.createRegistryResponseType();
        response.setRequestId(partRemoveObjectsRequest.getId());
        response.setStatus("Success");
        statusHandler
                .info("removeObjects complete.  Returning success response.");
        return response;
    }

    /**
     * @See {@link LifecycleManagerImpl#removeObjects} for details
     */
    private RegistryExceptionType internalRemoveObjects(
            List<ObjectRefType> objectRefs) {
        oasis.names.tc.ebxml.regrep.xsd.rs.v4.ObjectFactory rsFactory = new oasis.names.tc.ebxml.regrep.xsd.rs.v4.ObjectFactory();

        try {
            for (ObjectRefType ref : objectRefs) {
                if (!registry.containsId(ref.getId())) {
                    UnresolvedReferenceExceptionType e = rsFactory
                            .createUnresolvedReferenceExceptionType();
                    e.setMessage("ID not found in registry: " + ref.getId());
                    return e;
                }
            }
        } catch (IOException ioe) {
            StringWriter writer = new StringWriter();
            ioe.printStackTrace(new PrintWriter(writer));

            RegistryExceptionType e = rsFactory.createRegistryExceptionType();
            e.setMessage("Error contacting registry.");
            e.setDetail(writer.toString());
            e.setCode(ioe.getClass().toString());
            return e;
        }

        return registry.remove(objectRefs);
    }

    /**
     * The UpdateObjectsRequest protocol allows a client to make partial updates
     * to one or more RegistryObjects that already exist in the server. This
     * protocol enables partial update of RegistryObjects rather than a complete
     * replacement. A client SHOULD use the SubmitObjects protocol for complete
     * replacement of RegistryObjects.
     * <p>
     * A server MUST return InvalidRequestException fault message if the client
     * attempts to update the id, lid or objectType attribute of a
     * RegistryObject.
     * <p>
     * Query - Specifies a query to be invoked. A server MUST use all objects
     * that match the specified query in addition to any other objects
     * identified by the ObjectRefList element as targets of the update action.
     * <p>
     * ObjectRefList - Specifies a collection of references to existing
     * RegistryObject instances in the server. A server MUST use all objects
     * that are referenced by this element in addition to any other objects
     * identified by the Query element as targets of the update action.
     * <p>
     * UpdateAction – Specifies the details of how to update the target objects
     * <p>
     * checkReferences – Specifies the reference checking behavior expected of
     * the server:
     * <p>
     * <i>true</i> - Specifies that a server MUST check updated objects and make
     * sure that all references via reference attributes and slots to other
     * RegistryObjects are resolvable. If a reference does not resolve then the
     * server MUST return UnresolvedReferenceException
     * <p>
     * <i>false (default)</i> – Specifies that a server MUST NOT check updated
     * objects to make sure that all references via reference attributes and
     * slots to other RegistryObjects are resolvable. If a reference does not
     * resolve then the server MUST NOT return UnresolvedReferenceException
     * <p>
     * mode – Specifies the semantics for how the server should handle
     * RegistryObjects being updated in the server:
     * <p>
     * <i>CreateOrReplace (default)</i> - If an object does not exist, server
     * MUST return ObjectNotFoundException. If an object already exists, server
     * MUST update the existing object without creating a new version
     * <p>
     * <i>CreateOrVersion</i> - If an object does not exist, server MUST return
     * ObjectNotFoundException. If an object already exists, server MUST create
     * a new version of the existing object before applying the requested update
     * action
     * <p>
     * <i>CreateOnly</i> – This mode does not apply to UpdateObjectsRequest. If
     * specified, server MUST return an InvalidRequestException
     * 
     * @param partUpdateObjectsRequest
     * @returns {@link RegistryResponseType}
     * @throws MsgRegistryException
     */
    public RegistryResponseType updateObjects(
            UpdateObjectsRequest partUpdateObjectsRequest)
            throws MsgRegistryException {
        boolean checkReferences = partUpdateObjectsRequest.isCheckReferences();
        if (checkReferences) {
            statusHandler
                    .info("Checking object references is currently unimplemented!");
            // TODO Implement. See section 4.3.1.2
        }
        Mode mode = partUpdateObjectsRequest.getMode();
        ObjectRefListType objRefList = partUpdateObjectsRequest
                .getObjectRefList();
        List<ObjectRefType> objRefs = new ArrayList<ObjectRefType>();
        if (objRefList != null) {
            objRefs.addAll(objRefList.getObjectRef());
        }

        List<ObjectRefType> queriedRefs = new ArrayList<ObjectRefType>();
        try {
            QueryType query = partUpdateObjectsRequest.getQuery();
            if (query != null) {
                queriedRefs = performQuery(query);
                objRefs.addAll(queriedRefs);
            }
        } catch (IllegalStateException e) {
            oasis.names.tc.ebxml.regrep.xsd.rs.v4.ObjectFactory rsFactory = new oasis.names.tc.ebxml.regrep.xsd.rs.v4.ObjectFactory();
            StringWriter writer = new StringWriter();
            e.printStackTrace(new PrintWriter(writer));
            RegistryExceptionType re = rsFactory.createRegistryExceptionType();
            re.setMessage(e.getMessage());
            re.setDetail(writer.toString());
            re.setCode(e.getClass().toString());
            throw new MsgRegistryException("Error executing query for update",
                    re, e);
        }
        List<UpdateActionType> updateActions = partUpdateObjectsRequest
                .getUpdateAction();
        RegistryExceptionType e = registry.update(objRefs, updateActions, mode);
        if (e != null) {
            throw new MsgRegistryException("Request failed: " + e.getMessage(),
                    e);
        }

        // TODO Implement Audit Trails. See section 4.1.2
        statusHandler.info("Notifying listeners..");
        for (LifecycleListener listener : listeners) {
            listener.objectsUpdated(objRefs);
        }

        oasis.names.tc.ebxml.regrep.xsd.rs.v4.ObjectFactory rsFactory = new oasis.names.tc.ebxml.regrep.xsd.rs.v4.ObjectFactory();

        RegistryResponseType response = rsFactory.createRegistryResponseType();
        response.setRequestId(partUpdateObjectsRequest.getId());
        response.setStatus("Success");
        statusHandler
                .info("updateObjects complete.  Returning success response.");
        return response;
    }

    /**
     * Perform the given query to retrieve a list of references.
     * 
     * @param query
     * @return
     * @throws MsgRegistryException
     *             if the query exceptioned.
     * @throws IllegalStateException
     *             if the QueryManager is not defined.
     */
    protected List<ObjectRefType> performQuery(QueryType query)
            throws MsgRegistryException, IllegalStateException {
        oasis.names.tc.ebxml.regrep.xsd.query.v4.ObjectFactory queryFactory = new oasis.names.tc.ebxml.regrep.xsd.query.v4.ObjectFactory();

        if (queryManager == null) {
            throw new IllegalStateException("QueryManager must not be null");
        }

        QueryRequest request = queryFactory.createQueryRequest();
        request.setQuery(query);
        request.setId(Long.toString(System.currentTimeMillis())); // TODO better
                                                                  // ID.

        ResponseOptionType responseOption = queryFactory
                .createResponseOptionType();
        responseOption.setReturnType("ObjectRef");
        request.setResponseOption(responseOption);

        List<ObjectRefType> refList = new ArrayList<ObjectRefType>();
        QueryResponse response = queryManager.executeQuery(request);
        if (response != null) {
            // TODO Replace the following with the commented out code below when
            // QueryManager works right.
            RegistryObjectListType objList = response.getRegistryObjectList();
            if (objList != null) {
                oasis.names.tc.ebxml.regrep.xsd.rim.v4.ObjectFactory rimFactory = new oasis.names.tc.ebxml.regrep.xsd.rim.v4.ObjectFactory();
                for (RegistryObjectType obj : objList.getRegistryObject()) {
                    ObjectRefType ref = rimFactory.createObjectRefType();
                    ref.setId(obj.getLid());
                    refList.add(ref);
                }
            }
            /*
             * TODO This should be the actual way to do it, but QueryManager
             * doesn't support references yet. ObjectRefListType objRefList =
             * response.getObjectRefList(); if (objRefList != null) {
             * refList.addAll(objRefList.getObjectRef()); }
             */
        }

        return refList;
    }

    /**
     * @return the queryManager
     */
    public QueryManager getQueryManager() {
        return queryManager;
    }

    /**
     * @param queryManager
     *            the queryManager to set
     */
    public void setQueryManager(QueryManager queryManager) {
        this.queryManager = queryManager;
    }

    /**
     * @param listener
     *            the {@link LifecycleListener} to add.
     */
    public void addListener(LifecycleListener listener) {
        listeners.add(listener);
    }

    /**
     * @param listener
     *            the {@link LifecycleListener} to remove.
     */
    public void removeListener(LifecycleListener listener) {
        listeners.remove(listener);
    }
}
