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

import java.util.Collection;

import oasis.names.tc.ebxml.regrep.xsd.rim.v4.AnyValueType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.ExtensibleObjectType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.SlotType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.ValueType;
import oasis.names.tc.ebxml.regrep.xsd.spi.v4.ValidateObjectsResponse;

import com.raytheon.uf.edex.registry.ebxml.services.validator.IRegistryValidator;
import com.raytheon.uf.edex.registry.ebxml.services.validator.ValidatorTypeManager;

/**
 * Validator implementation for validation of ExtensibleObjectType objects
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
public class ExtensibleObjectTypeValidator implements IRegistryValidator {

    @Override
    public void validate(Object object, ValidateObjectsResponse response) {
        ExtensibleObjectType obj = (ExtensibleObjectType) object;
        Collection<SlotType> slots = obj.getSlot();
        for (SlotType slot : slots) {
            ValueType slotValue = slot.getSlotValue();
            if (slotValue instanceof AnyValueType) {
                ValidatorTypeManager.getInstance().validateObject(
                        ((AnyValueType) slotValue).getAny(), response);
            }
        }
    }
}
