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
package com.raytheon.uf.common.registry.ebxml.slots;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.List;

import oasis.names.tc.ebxml.regrep.xsd.rim.v4.IntegerValueType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.SlotType;

/**
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 2013                                 Initial Creation
 * 12/2/2013    1829       bphillip     Removed generic setValue method on ValueType
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1
 **/
public class IntegerSlotConverter implements SlotConverter {

    public static final SlotConverter INSTANCE = new IntegerSlotConverter();

    @Override
    public List<SlotType> getSlots(String slotName, Object slotValue)
            throws IllegalArgumentException {
        List<SlotType> slots = new ArrayList<SlotType>();

        // Integer slot will accommodate integer, long and BigInteger types...
        if (slotValue instanceof Integer || slotValue instanceof BigInteger
                || slotValue instanceof Long) {

            SlotType slot = new SlotType();
            IntegerValueType sv = new IntegerValueType();
            slot.setName(slotName);
            sv.setIntegerValue(new BigInteger(slotValue.toString().trim()));
            slot.setSlotValue(sv);
            slots.add(slot);
        } else {
            throw new IllegalArgumentException("Object of type "
                    + slotValue.getClass().getName()
                    + " cannot be converted by "
                    + IntegerSlotConverter.class.getName());
        }
        return slots;
    }

}
