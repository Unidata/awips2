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
package com.raytheon.uf.edex.registry.ebxml.services.validator.plugins;

import java.util.List;

import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.MsgRegistryException;
import oasis.names.tc.ebxml.regrep.wsdl.registry.services.v4.Validator;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryObjectListType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryObjectType;
import oasis.names.tc.ebxml.regrep.xsd.rs.v4.InvalidRequestExceptionType;
import oasis.names.tc.ebxml.regrep.xsd.rs.v4.RegistryExceptionType;
import oasis.names.tc.ebxml.regrep.xsd.rs.v4.RegistryResponseStatus;
import oasis.names.tc.ebxml.regrep.xsd.spi.v4.ValidateObjectsRequest;
import oasis.names.tc.ebxml.regrep.xsd.spi.v4.ValidateObjectsResponse;

import com.raytheon.uf.common.registry.constants.ErrorSeverity;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.util.CollectionUtil;
import com.raytheon.uf.edex.registry.ebxml.services.validator.IRegistryObjectReferenceValidator;
import com.raytheon.uf.edex.registry.ebxml.util.EbxmlExceptionUtil;

/**
 * Base class for {@link Validator} plugin implementations.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 23, 2013 1910       djohnson     Initial creation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public abstract class ValidatorPlugin<T extends RegistryObjectType> implements
        Validator {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(ValidatorPlugin.class);

    protected IRegistryObjectReferenceValidator registryObjectReferenceValidator;

    /**
     * Constructor.
     * 
     * @param validatorService
     *            the overall validator service
     */
    protected ValidatorPlugin(
            IRegistryObjectReferenceValidator registryObjectReferenceValidator) {
        this.registryObjectReferenceValidator = registryObjectReferenceValidator;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public ValidateObjectsResponse validateObjects(
            ValidateObjectsRequest validateObjectsRequest)
            throws MsgRegistryException {
        final RegistryObjectListType originalObjects = validateObjectsRequest
                .getOriginalObjects();
        if (originalObjects == null) {
            final String message = "The Validator plugin invocation MUST specify the target objects for that set using the OriginalObjects element";
            throw EbxmlExceptionUtil.createMsgRegistryException(message,
                    InvalidRequestExceptionType.class, "", message, message,
                    ErrorSeverity.ERROR, null, statusHandler);
        }

        ValidateObjectsResponse response = new ValidateObjectsResponse();
        final List<RegistryExceptionType> allExceptions = response
                .getException();

        for (RegistryObjectType registryObject : originalObjects
                .getRegistryObject()) {
            final T expectedType = castToExpectedType(registryObject);
            final List<RegistryExceptionType> exceptions = validate(expectedType);
            if (!CollectionUtil.isNullOrEmpty(exceptions)) {
                allExceptions.addAll(exceptions);
            }
        }

        RegistryResponseStatus status = (allExceptions.isEmpty()) ? RegistryResponseStatus.SUCCESS
                : RegistryResponseStatus.PARTIAL_SUCCESS;
        response.setStatus(status);

        return response;
    }

    /**
     * Verify the {@link RegistryObjectType} is a type supported by this
     * Validator plugin.
     * 
     * @param registryObject
     *            the registry object to check
     * @throws MsgRegistryException
     *             on an incorrect registry object type
     */
    private T castToExpectedType(RegistryObjectType registryObject)
            throws MsgRegistryException {
        final Class<T> registryObjectTypeClass = getRegistryObjectTypeClass();
        if (!registryObjectTypeClass
                .isAssignableFrom(registryObject.getClass())) {
            final String message = "This Validator plugin should only be passed registry objects of type ["
                    + registryObjectTypeClass + "]!";
            throw EbxmlExceptionUtil.createMsgRegistryException(message,
                    InvalidRequestExceptionType.class, "", message, message,
                    ErrorSeverity.ERROR, null, statusHandler);
        }
        return registryObjectTypeClass.cast(registryObject);
    }

    /**
     * Return the required registry object type class. May be a super-class of
     * multiple types.
     * 
     * @return the registry object type class
     */
    protected abstract Class<T> getRegistryObjectTypeClass();

    /**
     * Validate the object, returning any exceptions that should be added to the
     * response.
     * 
     * @param registryObject
     *            the object to validate
     */
    protected abstract List<RegistryExceptionType> validate(T registryObject);
}
