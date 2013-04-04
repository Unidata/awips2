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

import java.util.List;

import oasis.names.tc.ebxml.regrep.xsd.rim.v4.SlotType;

/**
 * {@link SlotConverter} for enumerations.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 22, 2012 0743       djohnson     Initial creation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
public class EnumSlotConverter implements SlotConverter {

    public static final SlotConverter INSTANCE = new EnumSlotConverter();

    @Override
    public List<SlotType> getSlots(String slotName, Object slotValue)
            throws IllegalArgumentException {
        if (Enum.class.isAssignableFrom(slotValue.getClass())) {
            return StringSlotConverter.INSTANCE.getSlots(slotName,
                    ((Enum<?>) slotValue).toString());
        } else {
            throw new IllegalArgumentException("Object of type "
                    + slotValue.getClass().getName()
                    + " cannot be converted by "
                    + EnumSlotConverter.class.getName());
        }
    }
}
