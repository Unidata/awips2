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
package com.raytheon.uf.edex.registry.ebxml.services.validator;

import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import javax.annotation.Resource;
import javax.xml.ws.WebServiceContext;

import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.MsgRegistryException;
import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.QueryManager;
import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.Validator;
import oasis.names.tc.ebxml.regrep.xsd.query.v4.QueryRequest;
import oasis.names.tc.ebxml.regrep.xsd.query.v4.QueryResponse;
import oasis.names.tc.ebxml.regrep.xsd.query.v4.ResponseOptionType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.ObjectRefListType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.ObjectRefType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.QueryType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryObjectListType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryObjectType;
import oasis.names.tc.ebxml.regrep.xsd.rs.v4.InvalidRequestExceptionType;
import oasis.names.tc.ebxml.regrep.xsd.rs.v4.RegistryResponseStatus;
import oasis.names.tc.ebxml.regrep.xsd.spi.v4.ValidateObjectsRequest;
import oasis.names.tc.ebxml.regrep.xsd.spi.v4.ValidateObjectsResponse;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.google.common.collect.ArrayListMultimap;
import com.google.common.collect.Lists;
import com.google.common.collect.Multimap;
import com.raytheon.uf.common.registry.constants.ErrorSeverity;
import com.raytheon.uf.common.registry.constants.QueryReturnTypes;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.util.registry.GenericRegistry;
import com.raytheon.uf.edex.registry.ebxml.dao.RegistryObjectDao;
import com.raytheon.uf.edex.registry.ebxml.util.EbxmlExceptionUtil;
import com.raytheon.uf.edex.registry.ebxml.util.EbxmlObjectUtil;

/**
 * Implementation of the validator web service interface. This service is used
 * to validate registry objects.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 2/13/12      184        bphillip     Initial creation
 * Apr 24, 2013 1910       djohnson     Start to fill in the validation logic.
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */
@Service
@Transactional
public class ValidatorImpl implements Validator {

    /** The logger */
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(ValidatorImpl.class);

    /** The query manager **/
    private QueryManager queryManager;

    /** The registry object data access object */
    private RegistryObjectDao registryObjectDao;

    private Validator registryObjectTypeValidator;

    @Resource
    private WebServiceContext wsContext;

    /** Holds the registry of plugin validators **/
    private final GenericRegistry<String, Validator> validatorPlugins = new GenericRegistry<String, Validator>() {
    };


    /**
     * Constructor.
     */
    public ValidatorImpl() {
    }

    /*
     * (non-Javadoc)
     * 
     * @see oasis.names.tc.ebxml_regrep.wsdl.registry.services._4.Validator#
     * validateObjects
     * (oasis.names.tc.ebxml_regrep.xsd.spi._4.ValidateObjectsRequest)
     */
    @Override
    public ValidateObjectsResponse validateObjects(
            ValidateObjectsRequest request) throws MsgRegistryException {
        // TODO: Implement the validator implementation using Schematron
        statusHandler
                .info("Validator service received validateObjects request from ["
                        + EbxmlObjectUtil.getClientHost(wsContext) + "]");

        final ValidateObjectsResponse response = EbxmlObjectUtil.spiObjectFactory
                .createValidateObjectsResponse();

        // Resolve the object references passed in, and validate the objects
        List<RegistryObjectType> registryObjects = Lists.newArrayList();
        if (request.getObjectRefList() != null) {
            final List<ObjectRefType> objectRefs = request.getObjectRefs();
            registryObjects = Lists
                    .newArrayListWithExpectedSize(objectRefs.size());

            for (ObjectRefType objectRef : objectRefs) {
                final String referenceId = objectRef.getId();
                final RegistryObjectType registryObject = registryObjectDao
                        .getById(referenceId);
                if (registryObject == null) {
                    response.getException().add(
                            EbxmlExceptionUtil
                                    .createUnresolvedReferenceException(null,
                                            referenceId, statusHandler));
                    continue;
                }

                registryObjects.add(registryObject);
            }
        }

        // Resolve any objects requested by a client query
        final QueryType query = request.getQuery();
        if (query != null) {

            final ResponseOptionType responseOption = new ResponseOptionType();
            responseOption.setReturnType(QueryReturnTypes.REGISTRY_OBJECT);

            QueryRequest queryRequest = new QueryRequest();
            queryRequest.setResponseOption(responseOption);
            queryRequest.setQuery(query);

            final QueryResponse queryResponse = queryManager
                    .executeQuery(queryRequest);
            final RegistryObjectListType registryObjectList = queryResponse
                    .getRegistryObjectList();
            if (registryObjectList != null) {
                registryObjects.addAll(registryObjectList.getRegistryObject());
            }
        }

        request.setObjectRefList(new ObjectRefListType());
        request.setOriginalObjects(new RegistryObjectListType(
                    registryObjects));

        return serverValidateObjects(request, response);
    }

    /**
     * Performs a server-side only validation using the registry objects on the
     * request. Will be used by the server itself to verify registry object
     * integrity, or on a client request when the server resolves the registry
     * object references and queries to find the objects for validation.
     * 
     * @param request
     *            the request
     * @param response
     *            the response to use
     * @return the response
     * @throws MsgRegistryException
     *             on errors encountered during validation
     */
    public ValidateObjectsResponse serverValidateObjects(
            ValidateObjectsRequest request, ValidateObjectsResponse response)
            throws MsgRegistryException {

        final RegistryObjectListType originalObjects = request
                .getOriginalObjects();

        if (originalObjects == null) {
            final String message = "The OriginalObjects element MUST specify the target objects to be verified!";
            throw EbxmlExceptionUtil.createMsgRegistryException(message,
                    InvalidRequestExceptionType.class, "", message, message,
                    ErrorSeverity.ERROR, null, statusHandler);
        }

        // Place all of the objects into a map keyed by their object type
        Multimap<String, RegistryObjectType> objectTypeToObjects = ArrayListMultimap
                .create();
        final List<RegistryObjectType> objects = originalObjects
                .getRegistryObject();
        for (RegistryObjectType object : objects) {
            objectTypeToObjects.put(object.getObjectType(), object);
        }

        // Validate each set of objects with the appropriate validator plugin
        final Map<String, Collection<RegistryObjectType>> entries = objectTypeToObjects
                .asMap();
        for (Entry<String, Collection<RegistryObjectType>> entry : entries
                .entrySet()) {
            final String objectType = entry.getKey();
            if (statusHandler.isPriorityEnabled(Priority.DEBUG)) {
                statusHandler.debug("Validating registry objects with type ["
                        + objectType + "]");
            }

            // Create a validation request
            final RegistryObjectListType registryObjectListType = new RegistryObjectListType();
            registryObjectListType.getRegistryObject().addAll(entry.getValue());
            ValidateObjectsRequest validateRegistryObjects = new ValidateObjectsRequest();
            validateRegistryObjects.setOriginalObjects(registryObjectListType);

            // TODO: Make sure to include any registry objects passed in,
            // e.g. an AssociationType can create an association between two
            // objects that were passed in with it

            // Find any specific validator for this type
            Validator validator = validatorPlugins
                    .getRegisteredObject(objectType);
            if (validator == null) {
                if (statusHandler.isPriorityEnabled(Priority.DEBUG)) {
                    statusHandler
                            .debug("There is no plugin validator for registry objects with type ["
                                    + objectType
                                    + "].  Only the generic registry object validator will be used...");
                }
            } else {
                final ValidateObjectsResponse validateRegistryObjectsResponse = validator
                        .validateObjects(request);
                response.getException().addAll(
                        validateRegistryObjectsResponse.getException());
            }

            // Perform general registry object validation
            final ValidateObjectsResponse generalValidationResponse = registryObjectTypeValidator
                    .validateObjects(validateRegistryObjects);
            response.getException().addAll(
                    generalValidationResponse.getException());
        }

        RegistryResponseStatus status = (response.getException().isEmpty()) ? RegistryResponseStatus.SUCCESS
                : RegistryResponseStatus.PARTIAL_SUCCESS;
        response.setStatus(status);

        return response;
    }

    /**
     * Retrieve the plugin validator registry.
     * 
     * @return the validatorplugins
     */
    public GenericRegistry<String, Validator> getPluginValidatorRegistry() {
        return validatorPlugins;
    }

    /**
     * @param registryObjectDao
     *            the registryObjectDao to set
     */
    public void setRegistryObjectDao(RegistryObjectDao registryObjectDao) {
        this.registryObjectDao = registryObjectDao;
    }

    /**
     * @param registryObjectTypeValidator
     *            the registryObjectTypeValidator to set
     */
    public void setRegistryObjectTypeValidator(
            Validator registryObjectTypeValidator) {
        this.registryObjectTypeValidator = registryObjectTypeValidator;
    }

    /**
     * @param queryManager
     *            the queryManager to set
     */
    public void setQueryManager(QueryManager queryManager) {
        this.queryManager = queryManager;
    }

}
