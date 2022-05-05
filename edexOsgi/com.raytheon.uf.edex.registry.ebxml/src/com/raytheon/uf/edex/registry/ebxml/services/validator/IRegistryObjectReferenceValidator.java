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

import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryObjectType;

/**
 * Interface to validate a registry object reference.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 23, 2013 1910       djohnson     Initial creation
 * May 02, 2013 1910       djohnson     Add ability to validate registry object type.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
public interface IRegistryObjectReferenceValidator {

    /**
     * Return values for validating the type of a reference.
     */
    enum ValidateObjectTypeResponse {
        VALID, DOESNT_EXIST, WRONG_TYPE;
    }

    /**
     * Check a reference for validity.
     * 
     * @param reference
     *            the reference to check
     * @return true if a valid reference
     */
    boolean isValidReference(final String reference);

    /**
     * Check a reference to be of the correct type of registry object.
     * 
     * @param reference
     *            the reference
     * @param expectedType
     *            the expected type
     * @return true if the registry object is of the expected type
     */
    ValidateObjectTypeResponse isValidObjectType(String reference,
            Class<? extends RegistryObjectType> expectedType);

}
