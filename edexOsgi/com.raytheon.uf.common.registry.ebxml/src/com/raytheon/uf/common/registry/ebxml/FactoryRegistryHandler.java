package com.raytheon.uf.common.registry.ebxml;

import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.Callable;

import javax.xml.bind.JAXBException;
import javax.xml.ws.WebServiceException;

import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.LifecycleManager;
import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.MsgRegistryException;
import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.QueryManager;
import oasis.names.tc.ebxml.regrep.xsd.lcm.v4.Mode;
import oasis.names.tc.ebxml.regrep.xsd.lcm.v4.RemoveObjectsRequest;
import oasis.names.tc.ebxml.regrep.xsd.lcm.v4.SubmitObjectsRequest;
import oasis.names.tc.ebxml.regrep.xsd.query.v4.QueryRequest;
import oasis.names.tc.ebxml.regrep.xsd.query.v4.QueryResponse;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.AssociationType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.IdentifiableType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.ObjectRefListType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.ObjectRefType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryObjectListType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryObjectType;
import oasis.names.tc.ebxml.regrep.xsd.rs.v4.RegistryExceptionType;
import oasis.names.tc.ebxml.regrep.xsd.rs.v4.RegistryResponseType;
import oasis.names.tc.ebxml.regrep.xsd.rs.v4.UnresolvedReferenceExceptionType;

import com.google.common.annotations.VisibleForTesting;
import com.raytheon.uf.common.comm.CommunicationException;
import com.raytheon.uf.common.registry.IMultipleResultFormatter;
import com.raytheon.uf.common.registry.IResultFormatter;
import com.raytheon.uf.common.registry.OperationStatus;
import com.raytheon.uf.common.registry.RegistryException;
import com.raytheon.uf.common.registry.RegistryHandler;
import com.raytheon.uf.common.registry.RegistryManager;
import com.raytheon.uf.common.registry.RegistryQuery;
import com.raytheon.uf.common.registry.RegistryQueryResponse;
import com.raytheon.uf.common.registry.RegistryResponse;
import com.raytheon.uf.common.registry.annotations.RegistryObject;
import com.raytheon.uf.common.serialization.JAXBManager;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.util.ITimer;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.common.util.ReflectionException;

/**
 * 
 * RegistryHandler which allows configurable registry operations.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 20, 2012 736        djohnson    Moved code from RegistryManager.
 * 8/3/2012     724        bphillip    Assigning owner to associations when storing objects
 * Aug 15, 2012 0743       djohnson    Type-safe result formatters, warn of time-consuming queries.
 * Aug 27, 2012 0743       djohnson    Add handling for AssociationQuery objects.
 * Sep 14, 2012 1169       djohnson    Add use of create only mode.
 * 
 * </pre>
 * 
 * @author djohnson
 */
public class FactoryRegistryHandler implements RegistryHandler {

    @VisibleForTesting
    static final long QUERY_DURATION_WARN_LEVEL = 12000L;

    private static final String QUERY_DURATION_WARN_MSG = "Query of type [%s] is exceeding warn threshold of [%d] ms.  "
            + "This may impact the user experience.";

    @VisibleForTesting
    static IUFStatusHandler statusHandler = UFStatus
            .getHandler(FactoryRegistryHandler.class);

    private static void print(Object object) {

        JAXBManager jaxb;
        try {
            jaxb = new JAXBManager(object.getClass());
            System.out.println(jaxb.marshalToXml(object));
        }

        catch (Throwable t) {
        }
    }

    private final int batchSize = 10;

    private LifecycleManagerFactory lifecycleManagerFactory;

    private QueryManagerFactory queryManagerFactory;

    private RegistryTxManager registryTxManager;

    /**
     * Private constructor to disallow instance creation.
     */
    protected FactoryRegistryHandler() {

    }

    /**
     * Get the maximum number of items to submit in a RegistryObjectList.
     * 
     * @return The batch size for Object submission.
     */
    public int getBatchSize() {
        return batchSize;
    }

    /**
     * Getter method for the LifecylceManagerFactory attribute. This attribute
     * will be set using Spring dependency injection based on where the
     * RegistryManager is called from.
     * 
     * @return The LifecycleManagerFactory to use for this instance of
     *         RegistryManager.
     */
    public LifecycleManagerFactory getLifecycleManagerFactory() {
        return lifecycleManagerFactory;
    }

    /**
     * Getter method for the QueryManagerFactory attribute. This attribute will
     * be set using Spring dependency injection based on where the
     * RegistryManager is called from.
     * 
     * @return The QueryManagerFactory to use for this instance of
     *         RegistryManager.
     */
    public QueryManagerFactory getQueryManagerFactory() {
        return queryManagerFactory;
    }

    /**
     * Getter method for the RegistryTxManager attribute. This attribute will be
     * set using Spring dependency injection based on where the RegistryManager
     * is called from.
     * 
     * @return The RegistryTxManager to use for this instance of
     *         RegistryManager.
     */
    public RegistryTxManager getRegistryTxManager() {
        return registryTxManager;
    }

    /**
     * Setter method for the LifecycleManagerFactory attribute. This attribute
     * will be set using Spring dependency injection based on where the
     * RegistryManager is called from.
     * 
     * @param lifecycleManagerFactory
     *            The LifecycleManagerFactory implementation to use for this
     *            instance of RegistryManager.
     */
    public void setLcmFactory(LifecycleManagerFactory lifecycleManagerFactory) {
        this.lifecycleManagerFactory = lifecycleManagerFactory;
    }

    /**
     * Setter method for the QueryManagerFactory attribute. This attribute will
     * be set using Spring dependency injection based on where the
     * RegistryManager is called from.
     * 
     * @param queryManagerFactory
     *            The QueryManagerFactory implementation to use for this
     *            instance of RegistryManager.
     */
    public void setQueryFactory(QueryManagerFactory queryManagerFactory) {
        this.queryManagerFactory = queryManagerFactory;
    }

    /**
     * Setter method for the RegistryTxManager attribute. This attribute will be
     * set using Spring dependency injection based on where the RegistryManager
     * is called from.
     * 
     * @param registryTxManager
     *            The RegistryTxManager implementation to use for this instance
     *            of RegistryManager.
     */
    public void setTxManager(RegistryTxManager registryTxManager) {
        this.registryTxManager = registryTxManager;
    }

    private List<RegistryObjectType> getAssociations(QueryManager qm,
            String sourceObjectId, String targetObjectId, String associationType)
            throws MsgRegistryException {

        AssociationQuery query = new AssociationQuery();
        query.setAssociationType(associationType);
        query.setReturnObjects(false);

        if (sourceObjectId != null)
            query.setSourceObjectId(sourceObjectId);
        if (targetObjectId != null)
            query.setTargetObjectId(targetObjectId);

        QueryRequest q = RegistryUtil.getQuery(query);
        QueryResponse r = qm.executeQuery(q);

        if (RegistryUtil.RESPONSE_SUCCESS.equals(r.getStatus())
                && r.getRegistryObjectList() != null) {
            return r.getRegistryObjectList().getRegistryObject();
        }

        return new ArrayList<RegistryObjectType>();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public <T> RegistryQueryResponse<T> getObjects(
            final RegistryQuery<T> registryQuery) {
        final RegistryQueryResponse<T> response = new RegistryQueryResponse<T>(
                registryQuery);
        final Callable<RegistryQueryResponse<T>> request = new Callable<RegistryQueryResponse<T>>() {
            @Override
            public RegistryQueryResponse<T> call() throws Exception {

                QueryManager qm = queryManagerFactory.getQueryManager();
                response.setRegistryObjects(getObjects(qm, registryQuery));

                return response;
            }
        };

        return processRequest(request, response);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public <T> RegistryResponse<T> removeObjects(
            final RegistryQuery<T> registryQuery) {
        return removeObjects(null, registryQuery);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public <T> RegistryResponse<T> removeObjects(final String username,
            final RegistryQuery<T> registryQuery) {
        final RegistryResponse<T> response = new RegistryResponse<T>();
        final Callable<RegistryResponse<T>> request = new Callable<RegistryResponse<T>>() {
            @Override
            public RegistryResponse<T> call() throws Exception {
                LifecycleManager lcm = lifecycleManagerFactory
                        .getLifeCycleManager();
                QueryManager qm = queryManagerFactory.getQueryManager();
                List<T> a = getObjects(qm, registryQuery);

                removeObjects(lcm, username, a);
                response.setRegistryObjects(a);

                return response;
            }
        };

        return processRequest(request, response);
    }

    /**
     * Processes the passed in {@link Callable} within the scope of a
     * transaction. Also handles any errors and sets the appropriate status.
     * 
     * @param request
     *            the callable request
     * @param response
     *            the response object to populate on error
     * @return the response
     */
    private <T extends RegistryResponse<U>, U> T processRequest(
            Callable<T> request,
            T response) {
        TxManager tx = registryTxManager.getTxManager();
        try {
            tx.startTransaction();

            T calledResponse = request.call();
            calledResponse.setStatus(OperationStatus.SUCCESS);
            return calledResponse;
        } catch (WebServiceException e) {
            return RegistryUtil.getFailedResponse(response,
                    new RegistryException(
                            RegistryUtil.UNABLE_TO_CONNECT_TO_REGISTRY, e));
        } catch (CommunicationException e) {
            return RegistryUtil.getFailedResponse(response, e);
        } catch (Throwable e) {
            return RegistryUtil.getFailedResponse(response, e);
        } finally {
            tx.closeTransaction();
        }
    }

    /**
     * A private implementation of the static method
     * getRegistryObjects(RegistryQuery), this method will use the configured
     * QueryManagerFactory and RegistryTxManager to retrieve registry objects
     * that satisfy the RegistryQuery.
     * 
     * @param registryQuery
     *            A RegistryQuery to search the registry for objects.
     * 
     * @return A RegistryQueryResponse containing the status of the request, any
     *         registry objects that satisfied the RegistryQuery and any
     *         Exceptions generated from processing the RegistryQuery.
     * @throws MsgRegistryException
     * 
     * @see AdhocRegistryQuery
     * @see IdQuery
     * @see RegistryManager.getRegistyObjects()
     */
    private <T> List<T> getObjects(QueryManager qm,
            RegistryQuery<T> registryQuery)
            throws MsgRegistryException {

        ITimer timer = TimeUtil.getTimer();
        timer.start();

        List<RegistryObjectType> objs = getRawObjects(qm, registryQuery);

        boolean debugLog = statusHandler.isPriorityEnabled(Priority.DEBUG);
        if (debugLog) {
            statusHandler.debug((timer.getElapsedTime())
                    + " ms for raw objects query ["
                    + registryQuery.getClass().getSimpleName() + "]: "
                    + objs.size() + " items returned");
        }

        List<T> registryObjects = FactoryRegistryHandler.filterResults(
                registryQuery, objs);

        timer.stop();
        long totalElapsedTime = timer.getElapsedTime();
        if (debugLog) {
            statusHandler.debug(totalElapsedTime
                    + " ms for entire query ["
                    + registryQuery.getClass().getSimpleName() + "]: "
                    + registryObjects.size() + " items returned");
        }

        if (totalElapsedTime > QUERY_DURATION_WARN_LEVEL) {
            statusHandler.warn(String.format(QUERY_DURATION_WARN_MSG,
                    registryQuery.getClass().getName(),
                    QUERY_DURATION_WARN_LEVEL));
        }

        return registryObjects;
    }

    /**
     * Filters the specified {@link RegistryObjectType}s to include either the
     * actual objects, or the results as specified by a {@link IResultFormatter}
     * or {@link IMultipleResultFormatter}.
     * 
     * @param <T>
     *            the type of objects the query returns
     * @param registryQuery
     *            the query
     * @param objs
     *            the {@link RegistryObjectType}s
     * @return the results
     */
    @VisibleForTesting
    static <T> List<T> filterResults(RegistryQuery<T> registryQuery,
            List<RegistryObjectType> objs) {
        final List<T> registryObjects = new ArrayList<T>(objs.size());

        if (registryQuery instanceof IResultFormatter) {
            FactoryRegistryHandler.filterResults(
                    (IResultFormatter<T>) registryQuery, objs, registryObjects);
        } else if (registryQuery instanceof IMultipleResultFormatter) {
            FactoryRegistryHandler.filterResults(
                    (IMultipleResultFormatter<T>) registryQuery, objs,
                    registryObjects);
        }
        // This handles Association queries that do NOT return @RegistryObject
        // annotated instances, they return the schema association type
        // representation. Can this be handled better?
        else if (registryQuery instanceof AssociationQuery
                && !((AssociationQuery) registryQuery).isReturnObjects()) {
            for (RegistryObjectType obj : objs) {
                registryObjects.add(registryQuery.getResultType().cast(obj));
            }
        } else {
            for (RegistryObjectType obj : objs) {
                try {
                    Object o = RegistryUtil.decodeObject(obj);
                    if (o != null) {
                        registryObjects.add(registryQuery.getResultType().cast(
                                o));
                    }
                } catch (SerializationException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            e.getLocalizedMessage(), e);
                }
            }
        }

        return registryObjects;
    }

    /**
     * Filters the results using the specified {@link IResultFormatter}.
     * 
     * @param <T>
     *            the result type
     * @param registryQuery
     *            the query
     * @param objs
     *            the {@link RegistryObjectType}s
     * @param results
     *            the collection to add results to
     */
    private static <T> void filterResults(IResultFormatter<T> registryQuery,
            List<RegistryObjectType> objs, Collection<T> results) {
        for (RegistryObjectType obj : objs) {
            try {
                T decodedObject = registryQuery.decodeObject(obj);
                if (decodedObject != null) {
                    results.add(decodedObject);
                }
            } catch (SerializationException e) {
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                        e);
            }
        }
    }

    /**
     * Filters the results using the specified {@link IMultipleResultFormatter}.
     * 
     * @param <T>
     *            the result type
     * @param registryQuery
     *            the query
     * @param objs
     *            the {@link RegistryObjectType}s
     * @param results
     *            the collection to add results to
     */
    private static <T> void filterResults(
            IMultipleResultFormatter<T> registryQuery,
            List<RegistryObjectType> objs, Collection<T> results) {
        for (RegistryObjectType obj : objs) {
            try {
                Collection<T> decodedObject = registryQuery.decodeObject(obj);
                if (decodedObject != null) {
                    results.addAll(decodedObject);
                }
            } catch (SerializationException e) {
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                        e);
            }
        }
    }

    /**
     * A private implementation of the static method
     * getRegistryObjects(RegistryQuery), this method will use the configured
     * QueryManagerFactory and RegistryTxManager to retrieve registry objects
     * that satisfy the RegistryQuery.
     * 
     * @param registryQuery
     *            A RegistryQuery to search the registry for objects.
     * 
     * @return A RegistryQueryResponse containing the status of the request, any
     *         registry objects that satisfied the RegistryQuery and any
     *         Exceptions generated from processing the RegistryQuery.
     * @throws MsgRegistryException
     * 
     * @see AdhocRegistryQuery
     * @see IdQuery
     * @see RegistryManager.getRegistyObjects()
     */
    private <T> List<RegistryObjectType> getRawObjects(QueryManager qm,
            RegistryQuery<T> registryQuery) throws MsgRegistryException {

        // Submit to the ebXML QueryManager
        QueryRequest query = RegistryUtil.getQuery(registryQuery);

        if (RegistryManager.DEBUG) {
            print(query);
        }

        QueryResponse r = qm.executeQuery(query);

        if (RegistryUtil.RESPONSE_SUCCESS.equals(r.getStatus())) {

            if (r.getRegistryObjectList() != null
                    && r.getRegistryObjectList().getRegistryObject() != null) {
                return r.getRegistryObjectList().getRegistryObject();
            }
        }

        return new ArrayList<RegistryObjectType>();
    }

    /**
     * This method will use the configured LifecycleManagerFactory and
     * RegistryTxManager to remove registry objects that have a registry object
     * id contained in the List<String> parameter.
     * 
     * @param ids
     *            A List<String> of registry object ids to remove from the
     *            registry.
     * 
     * @return A RegistryResponse containing the status of the request and any
     *         Exceptions generated from attempting to remove the specified
     *         registry objects.
     * 
     */
    private <T> RegistryResponse<T> remove(String username, List<String> ids) {
        RegistryResponse<T> response = new RegistryResponse<T>();
        List<ObjectRefType> objectRefs = new ArrayList<ObjectRefType>();

        try {
            if (ids != null && !ids.isEmpty()) {

                for (String id : ids) {
                    ObjectRefType t = new ObjectRefType();
                    t.setId(id);

                    objectRefs.add(t);
                    if (RegistryManager.DEBUG) {
                        System.out
                                .println("Attempting to remove RegistryObject with id ["
                                        + id + "]");
                    }
                }
            }

            RemoveObjectsRequest deleteRequest = new RemoveObjectsRequest();
            deleteRequest.setUsername(username);
            deleteRequest.setId(RegistryUtil.generateRegistryObjectId());

            // These options are not supported yet...

            // deleteRequest.setDeleteChildren(true);
            // deleteRequest.setDeletionScope(DELETE_REPOSITORY_ITEM_ONLY);

            ObjectRefListType i = new ObjectRefListType();
            i.getObjectRef().addAll(objectRefs);
            deleteRequest.setObjectRefList(i);

            if (RegistryManager.DEBUG) {
                print(deleteRequest);
            }

            // Submit to the ebXML LifecycleManager
            LifecycleManager a = lifecycleManagerFactory.getLifeCycleManager();
            RegistryResponseType r = a.removeObjects(deleteRequest);

            if (RegistryUtil.RESPONSE_SUCCESS.equals(r.getStatus())) {
                response.setStatus(OperationStatus.SUCCESS);
            }

        } catch (Throwable e) {
            response.setErrors(Arrays.asList(e));
            response.setStatus(OperationStatus.FAILED);
        }

        return response;
    }

    /**
     * This method will use RegistryUtil.getRegistryObjectKey(Object) to obtain
     * the registry id of each Object in the registryObjects List parameter.
     * These ids will be placed in a List and the remove(List<String>) method
     * will be used to remove the Objects from the registry.
     * 
     * @param registryObjects
     *            A List of Objects that are annotated with @RegistryObject.
     * 
     * @return A RegistryResponse containing the status of the request and any
     *         Exceptions generated from attempting to remove the specified
     *         registry objects.
     * @throws InvocationTargetException
     * @throws IllegalAccessException
     * @throws NoSuchMethodException
     * @throws IllegalArgumentException
     * @throws SecurityException
     * 
     * @see RegistryManager.remove(List<String>)
     * @see RegistryUtil.getRegistryObjectKey(Object)
     * @see RegistryObject
     */
    private <T> void removeObjects(LifecycleManager lcm, String userName,
            List<T> registryObjects) throws SecurityException,
            IllegalArgumentException, NoSuchMethodException,
            IllegalAccessException, InvocationTargetException {

        List<String> ids = new ArrayList<String>();

        if (registryObjects != null && !registryObjects.isEmpty()) {
            for (Object obj : registryObjects) {
                if (obj instanceof IdentifiableType) {
                    ids.add(((IdentifiableType) obj).getId());
                } else {
                    String id = RegistryUtil.getRegistryObjectKey(obj);
                    if (id != null) {
                        ids.add(id);
                    }
                }
            }
        }

        if (!ids.isEmpty()) {
            remove(userName, ids);
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public <T> RegistryResponse<T> removeObjects(final String username,
            final List<T> registryObjects) {
        final RegistryResponse<T> response = new RegistryResponse<T>();
        final Callable<RegistryResponse<T>> request = new Callable<RegistryResponse<T>>() {
            @Override
            public RegistryResponse<T> call() throws Exception {
                LifecycleManager lcm = lifecycleManagerFactory
                        .getLifeCycleManager();
                removeObjects(lcm, username, registryObjects);
                response.setRegistryObjects(registryObjects);

                return response;
            }
        };

        return processRequest(request, response);
    }

    /**
     * Store an Object to the registry.
     * 
     * @param object
     *            An Object whose Class is annotated with @RegistryObject. This
     *            Object will be stored in the registry.
     * 
     * @return A RegistryResponse containing the status of the request, and any
     *         Exceptions generated from attempting to store the Object into the
     *         registry.
     * 
     * @see RegistryObject
     * 
     */
    @Override
    public RegistryResponse<Object> storeObject(final Object object) {
        final RegistryResponse<Object> response = new RegistryResponse<Object>();
        final Callable<RegistryResponse<Object>> request = new Callable<RegistryResponse<Object>>() {
            @Override
            public RegistryResponse<Object> call() throws Exception {
                LifecycleManager lcm = lifecycleManagerFactory
                        .getLifeCycleManager();
                QueryManager qm = queryManagerFactory.getQueryManager();
                List<Object> a = store(lcm, qm, object, Mode.CREATE_ONLY);
                response.setRegistryObjects(a);

                return response;
            }
        };

        return processRequest(request, response);
    }

    /**
     * 
     * {@inheritDoc}
     */
    @Override
    public RegistryResponse<Object> storeOrReplaceObject(final Object object) {
        final RegistryResponse<Object> response = new RegistryResponse<Object>();
        final Callable<RegistryResponse<Object>> request = new Callable<RegistryResponse<Object>>() {
            @Override
            public RegistryResponse<Object> call() throws Exception {
                LifecycleManager lcm = lifecycleManagerFactory
                        .getLifeCycleManager();
                QueryManager qm = queryManagerFactory.getQueryManager();
                List<Object> a = store(lcm, qm, object, Mode.CREATE_OR_REPLACE);
                response.setRegistryObjects(a);

                return response;
            }
        };

        return processRequest(request, response);
    }

    /**
     * Store an Object whose Class hierarchy is annotated with
     * <code>RegistryObject</code> to the registry. The Class hierarchy of the
     * object parameter will be searched, top down, for the
     * <code>RegistryObject</code> annotation. When found, the registry object
     * type will be set to the fully qualified Class name of the Class. If the
     * object is composed of other RegistryObjects, annotated with the
     * <code>RegistryObjectAssociation</code> annotation, association will be
     * made in the registry between the object being stored and those objects.
     * 
     * @param object
     *            An Object This Object will be stored in the registry.
     * @param object2
     * @param qm
     * 
     * @return A RegistryResponse containing the status of the request, and any
     *         Exceptions generated from attempting to store the Object into the
     *         registry.
     * @throws MsgRegistryException
     * @throws JAXBException
     * @throws IllegalArgumentException
     * @throws ReflectionException
     * @throws SerializationException
     * 
     * @see RegistryObject
     * 
     */
    @SuppressWarnings("unchecked")
    private <T> List<T> store(LifecycleManager lcm, QueryManager qm, T object,
            Mode mode)
            throws MsgRegistryException, JAXBException,
            IllegalArgumentException, ReflectionException,
            SerializationException {
        List<T> storedObjects = new ArrayList<T>();

        // Storing one Object to the registry results in many "RegistryObjects"
        // being created depending on the composition of the Object being
        // stored. We will accumulate the RegistryObjectType Objects that need
        // to be stored for this Object in a list so that appropriate
        // associations can be created between the RegistryObjects created (the
        // associations themselves are RegistryObjects).
        RegistryObjectType registryObject = RegistryUtil
                .newRegistryObject(object);

        // Need to find all associations for the Objects referenced by the 'new'
        // RegistryObject so that the appropriate 'parent' associations can also
        // be made.
        // Map<String, RegistryObjectType> dependentObjects =
        // RegistryUtil.getAssociatedObjects(object);
        Map<String, AssociationInfo> dependentObjects = RegistryUtil
                .getAssociations(object);

        // First, find out if the object already exists..
        Class<?> clazz = object.getClass();
        @SuppressWarnings({ "rawtypes" })
        IdQuery<?> idQuery = new IdQuery(clazz);
        idQuery.setID(registryObject.getId());
        List<RegistryObjectType> objects = getRawObjects(qm, idQuery);

        RegistryResponseType rt;

        // If the object does not currently exist this block will succeed,
        // otherwise an exception will be thrown
        if (mode == Mode.CREATE_ONLY || objects.isEmpty()) {

            Set<String> dependentObjectIds = new HashSet<String>(
                    dependentObjects.keySet());

            // Are there dependent references to resolve?
            if (!dependentObjectIds.isEmpty()) {

                List<String> unresolvedReferences = new ArrayList<String>();

                // Now Query to see if all the dependent RegistryObjects
                // actually exist...
                idQuery.setIDs(new ArrayList<String>(dependentObjectIds));
                objects = getRawObjects(qm, idQuery);

                // Check to make sure all the dependent object ids were found
                // with the query. If not, resolve the differences...
                if (objects.size() != dependentObjectIds.size()) {

                    // Start with all the associations that were declared in the
                    // RegistryObject passed in...
                    unresolvedReferences.addAll(dependentObjectIds);

                    // If any were found, remove them from the Unresolved list
                    for (RegistryObjectType ro : objects) {
                        unresolvedReferences.remove(ro.getId());
                    }
                }

                if (!unresolvedReferences.isEmpty()) {
                    String message = "The RegistryObject["
                            + registryObject.getId()
                            + "] has specified a reference to a non-existant RegistryObject";
                    UnresolvedReferenceExceptionType faultInfo = new UnresolvedReferenceExceptionType();
                    UnresolvedReferenceException cause = new UnresolvedReferenceException(
                            message, unresolvedReferences);
                    throw new MsgRegistryException(message, faultInfo, cause);
                }
            }

            // Make sure the objectType classificationNode exists before
            // storing the registryObject otherwise AssociationQueries
            // for target/source objectTypes will not work...
            RegistryObjectType classificationNode = RegistryUtil
                    .getObjectTypeNode(registryObject.getObjectType());
            idQuery.setID(classificationNode.getId());
            objects = getRawObjects(qm, idQuery);
            if (objects.isEmpty()) {
                rt = submitObjects(lcm, Arrays.asList(classificationNode));
                if (!RegistryUtil.RESPONSE_SUCCESS.equals(rt.getStatus())) {
                    throwUnsuccessfulResponseException(rt);
                }
            }

            rt = submitObjects(lcm, Arrays.asList(registryObject));
            if (!RegistryUtil.RESPONSE_SUCCESS.equals(rt.getStatus())) {
                throwUnsuccessfulResponseException(rt);
            }

            // Since the Object successfully stored, add it to the list.
            storedObjects.add(object);

            // Now store the associations
            List<RegistryObjectType> associations = RegistryUtil
                    .makeAssociations(registryObject.getId(), dependentObjects);
            for (RegistryObjectType association : associations) {
                association.setOwner(registryObject.getOwner());
            }
            if (!associations.isEmpty()) {
                rt = submitObjects(lcm, associations, mode);
            }

            if (!RegistryUtil.RESPONSE_SUCCESS.equals(rt.getStatus())) {
                throwUnsuccessfulResponseException(rt);
            }
        }
        // The object exists, determine what associations must be maintained or
        // removed
        else if (mode == Mode.CREATE_OR_REPLACE) {
            // OK, something may have changed with this Object.
            // Need to check dependent objects to make sure the appropriate
            // associations are stored. First, get the current associations.
            List<RegistryObjectType> existingAssociations = getAssociations(qm,
                    registryObject.getId(), null,
                    RegistryUtil.PATH_ASSOCIATION_CONTAINS);

            // Get the targetObjectIds from the existing associations and
            // resolve that Set against the new list of target associations
            Set<String> existingDependentIds = new HashSet<String>();
            Map<String, String> targetToObjectId = new HashMap<String, String>();

            for (RegistryObjectType t : existingAssociations) {
                AssociationType association = (AssociationType) t;
                // Map the TargetObjectId with the ObjectId of the
                // AssociationType in case we need to remove some of the
                // associations later
                // (which is done by RegistryObjectId)...
                if (existingDependentIds.add(association.getTargetObject())) {
                    targetToObjectId.put(association.getTargetObject(),
                            association.getId());
                }
            }

            Set<String> dependentIdSet = new HashSet<String>(
                    dependentObjects.keySet());

            // If the Sets are the same size and one contains all the members of
            // the other they are equal and no update of the associations is
            // necessary, however if that is not the case we need to resolve
            // which associations need to be kept, which to remove and which to
            // add...
            if (!(existingDependentIds.size() == dependentIdSet.size() && existingDependentIds
                    .containsAll(dependentIdSet))) {

                // First find the keepers...
                Set<String> commonIds = new HashSet<String>(
                        existingDependentIds);
                commonIds.retainAll(dependentIdSet);

                // Now find the ones to remove
                existingDependentIds.removeAll(commonIds);
                if (!existingDependentIds.isEmpty()) {

                    List<String> associationsToRemove = new ArrayList<String>();
                    for (String id : existingDependentIds) {
                        associationsToRemove.add(targetToObjectId.get(id));
                    }

                    remove(null, associationsToRemove);
                }

                // Now the ones to add..
                for (String id : commonIds) {
                    dependentObjects.remove(id);
                }
                if (!dependentObjects.isEmpty()) {

                    List<RegistryObjectType> associationsToAdd = RegistryUtil
                            .makeAssociations(registryObject.getId(),
                                    dependentObjects);

                    if (!associationsToAdd.isEmpty()) {
                        rt = submitObjects(lcm, associationsToAdd, mode);
                    }
                }
            }

            // Now that the association are worked out, try again with replace
            // mode set..
            rt = submitObjects(lcm, Arrays.asList(registryObject), mode);
        } else {
            throw new IllegalArgumentException("Mode [" + mode
                    + "] is not currently supported!");
        }

        return Arrays.<T> asList(object);
    }

    /**
     * Throws a {@link MsgRegistryException} based on the exceptions provided on
     * the {@link RegistryResponseType}.
     * 
     * @param response
     *            the response
     * @throws MsgRegistryException
     *             everytime
     */
    private static void throwUnsuccessfulResponseException(
            final RegistryResponseType response)
            throws MsgRegistryException {
        List<RegistryExceptionType> exceptions = response.getException();
        if (exceptions.isEmpty()) {
            throw new MsgRegistryException(
                    "Unsuccessful store, but no exception was provided!", null);
        } else {
            RegistryExceptionType exception = exceptions.iterator().next();
            throw new MsgRegistryException(exception.getMessage(), exception);
        }
    }

    /**
     * method used to submit a <code>List</code> of
     * <code>RegistryObjectType</code> Objects to the LifecycleManager for
     * insertion into the Registry. The List will be submitted to the
     * LifecycleManager with a <code>Mode</code> of "CreateOnly".
     * 
     * @param lifecycleManager
     *            A LifecycleManager instance to use to attempt to store the
     *            SubmitObjectsRequest.
     * 
     * @param registryObjects
     *            A <code>List</code> of <code>RegistryObjectType</code> Objects
     *            to insert into the registry.
     * 
     * @return The <code>RegistryResponseType</code> returned from the
     *         LifecycleManager.
     */
    private RegistryResponseType submitObjects(
            LifecycleManager lifecycleManager,
            List<RegistryObjectType> registryObjects)
            throws MsgRegistryException, JAXBException {
        return submitObjects(lifecycleManager, registryObjects,
                Mode.CREATE_ONLY);
    }

    /**
     * method used to submit a <code>List</code> of
     * <code>RegistryObjectType</code> Objects to the LifecycleManager for
     * insertion into the Registry.
     * 
     * @param lifecycleManager
     *            A LifecycleManager instance to use to attempt to store the
     *            SubmitObjectsRequest.
     * 
     * @param registryObjects
     *            A <code>List</code> of <code>RegistryObjectType</code> Objects
     *            to insert into the registry.
     * 
     * @param mode
     *            The mode for the insert.
     * 
     * @return The <code>RegistryResponseType</code> returned from the
     *         LifecycleManager.
     */
    private RegistryResponseType submitObjects(
            LifecycleManager lifecycleManager,
            List<RegistryObjectType> registryObjects, Mode mode)
            throws MsgRegistryException, JAXBException {

        RegistryResponseType response = new RegistryResponseType();
        response.setObjectRefList(new ObjectRefListType());
        response.setRegistryObjectList(new RegistryObjectListType());
        response.setException(new ArrayList<RegistryExceptionType>());
        response.setStatus(RegistryUtil.RESPONSE_SUCCESS);

        if (registryObjects != null && registryObjects.size() > 0) {

            // Store the RegistryObjects in batches to minimize processing on
            // the server.
            int count = 0;
            int lastIndex = registryObjects.size();
            while (count < registryObjects.size()) {
                List<RegistryObjectType> batch = registryObjects.subList(count,
                        (count = Math.min(count + batchSize, lastIndex)));

                SubmitObjectsRequest a = RegistryUtil.newSubmitObjects(batch,
                        mode);
                if (RegistryManager.DEBUG) {
                    JAXBManager jaxb = new JAXBManager(
                            SubmitObjectsRequest.class);
                    System.out.println("submitObjects( ["
                            + jaxb.marshalToXml(a) + "])");
                }

                RegistryResponseType rt = lifecycleManager.submitObjects(a);

                if (RegistryUtil.RESPONSE_SUCCESS.equals(rt.getStatus())) {
                    // Accumulate the ObjectRefList and RegistryObjectList
                    // returns
                    // from each sub-submit to aggregate back into a full
                    // response..
                    if (rt.getObjectRefList() != null) {
                        if (rt.getObjectRefList().getObjectRef() != null) {
                            response.getObjectRefList()
                                    .getObjectRef()
                                    .addAll(rt.getObjectRefList()
                                            .getObjectRef());
                        }
                    }

                    if (rt.getRegistryObjectList() != null) {
                        if (rt.getRegistryObjectList().getRegistryObject() != null) {
                            response.getRegistryObjectList()
                                    .getRegistryObject()
                                    .addAll(rt.getRegistryObjectList()
                                            .getRegistryObject());
                        }
                    }

                    // Successful inserts may return some MsgRegistryExceptions
                    // keep those as well..
                    if (rt.getException() != null) {
                        response.getException().addAll(rt.getException());
                    }
                } else {
                    return rt;
                }
            }
        }

        return response;
    }

}
