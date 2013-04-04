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

import java.util.HashMap;
import java.util.Map;

import oasis.names.tc.ebxml.regrep.xsd.rim.v4.ClassificationType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.ExtensibleObjectType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.IdentifiableType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryObjectType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.VersionInfoType;
import oasis.names.tc.ebxml.regrep.xsd.spi.v4.ValidateObjectsResponse;

import com.raytheon.uf.edex.registry.ebxml.services.validator.types.ClassificationTypeValidator;
import com.raytheon.uf.edex.registry.ebxml.services.validator.types.ExtensibleObjectTypeValidator;
import com.raytheon.uf.edex.registry.ebxml.services.validator.types.IdentifiableTypeValidator;
import com.raytheon.uf.edex.registry.ebxml.services.validator.types.RegistryObjectTypeValidator;
import com.raytheon.uf.edex.registry.ebxml.services.validator.types.VersionInfoTypeValidator;

/**
 * This class is responsible for managing validator implementations.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 2/13/12      184        bphillip     Initial creation
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */
public class ValidatorTypeManager {

    // TODO: Make discovery of validator implementations dynamic
    /** The validator implementation map */
    private static final Map<Class<?>, IRegistryValidator> validatorMap;
    static {
        validatorMap = new HashMap<Class<?>, IRegistryValidator>();
        validatorMap.put(ExtensibleObjectType.class,
                new ExtensibleObjectTypeValidator());
        validatorMap.put(IdentifiableType.class,
                new IdentifiableTypeValidator());
        validatorMap.put(RegistryObjectType.class,
                new RegistryObjectTypeValidator());
        validatorMap.put(ClassificationType.class,
                new ClassificationTypeValidator());
        validatorMap.put(VersionInfoType.class, new VersionInfoTypeValidator());
    }

    /** The singleton instance */
    private static ValidatorTypeManager instance;

    /**
     * Gets the singleton instance of the ValidatorTypeManager
     * 
     * @return The singleton instance of the ValidatorTypeManager
     */
    public static ValidatorTypeManager getInstance() {
        if (instance == null) {
            instance = new ValidatorTypeManager();
        }
        return instance;
    }

    /**
     * Private constructor
     */
    private ValidatorTypeManager() {

    }

    /**
     * Validates the given object. This method updates the provided response
     * object appropriately. If a validator does not exists for the provided
     * object type, validation automatically passes.
     * 
     * @param objectToValidate
     *            The object to be validated
     * @param response
     *            The response object to update
     */
    public void validateObject(Object objectToValidate,
            ValidateObjectsResponse response) {
        IRegistryValidator validator = getValidator(objectToValidate.getClass());
        if (validator != null) {
            validator.validate(objectToValidate, response);
        }
    }

    /**
     * Gets a validator implementation for the given object type
     * 
     * @param objectClass
     *            The object type to get the validator for
     * @return The validator implementation. This method returns null if a
     *         validator does not exist for a given object typ
     */
    public IRegistryValidator getValidator(Class<?> objectClass) {

        for (Class<?> clazz : validatorMap.keySet()) {
            if (objectClass.equals(clazz)) {
                return validatorMap.get(clazz);
            }
        }
        return null;

    }

}
