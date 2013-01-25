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

import java.util.Date;
import java.util.List;

import oasis.names.tc.ebxml.regrep.xsd.rim.v4.SlotType;

/**
 * Converts {@link Date} objects for storage in a slot.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 6, 2012  955        djohnson     Initial creation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
public class DateSlotConverter implements SlotConverter {

    public static final SlotConverter INSTANCE = new DateSlotConverter();

    /**
     * {@inheritDoc}
     */
    @Override
    public List<SlotType> getSlots(String slotName, Object slotValue)
            throws IllegalArgumentException {
        if (!(slotValue instanceof Date)) {
            throw new IllegalArgumentException(
                    "slotValue must be a Date object!");
        }

        return IntegerSlotConverter.INSTANCE.getSlots(slotName,
                ((Date) slotValue).getTime());
    }

}
