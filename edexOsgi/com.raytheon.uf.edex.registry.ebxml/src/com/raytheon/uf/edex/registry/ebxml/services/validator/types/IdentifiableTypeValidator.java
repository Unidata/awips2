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
package com.raytheon.uf.edex.registry.ebxml.services.validator.types;

/**
 * Validator implementation for validation of IdentifiableType objects
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
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.ExtensibleObjectType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.IdentifiableType;
import oasis.names.tc.ebxml.regrep.xsd.spi.v4.ValidateObjectsResponse;
import oasis.names.tc.ebxml.regrep.xsd.spi.v4.ValidationExceptionType;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.edex.registry.ebxml.constants.ErrorSeverity;
import com.raytheon.uf.edex.registry.ebxml.services.validator.IRegistryValidator;
import com.raytheon.uf.edex.registry.ebxml.services.validator.ValidatorTypeManager;
import com.raytheon.uf.edex.registry.ebxml.util.EbxmlExceptionUtil;

public class IdentifiableTypeValidator implements IRegistryValidator {

    protected static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(IdentifiableTypeValidator.class);

    @Override
    public void validate(Object object, ValidateObjectsResponse response) {
        ValidatorTypeManager.getInstance()
                .getValidator(ExtensibleObjectType.class)
                .validate(object, response);
        IdentifiableType obj = (IdentifiableType) object;
        if (obj.getId() == null) {
            response.getException()
                    .add(EbxmlExceptionUtil
                            .createRegistryException(
                                    ValidationExceptionType.class,
                                    "",
                                    "Validation Failed: Error verifying IdentifiableType",
                                    "IdentifiableType objects must specify an id",
                                    ErrorSeverity.ERROR, statusHandler));
        }
    }

}
