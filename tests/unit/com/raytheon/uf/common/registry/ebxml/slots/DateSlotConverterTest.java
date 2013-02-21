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

import static org.junit.Assert.assertEquals;

import java.math.BigInteger;
import java.util.Date;
import java.util.List;

import oasis.names.tc.ebxml.regrep.xsd.rim.v4.SlotType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.ValueType;

import org.junit.Test;

/**
 * Test {@link DateSlotConverter}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 6, 2012  955       djohnson     Initial creation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public class DateSlotConverterTest {
    private static final Date DATE = new Date();

    @Test
    public void testConvertingDateToSlotReturnsCorrectNumberOfSlots() {
        List<SlotType> slots = DateSlotConverter.INSTANCE.getSlots("someName",
                DATE);

        assertEquals(1, slots.size());
    }

    @Test
    public void testConvertingDateToSlotReturnsTimeInMillis() {
        List<SlotType> slots = DateSlotConverter.INSTANCE.getSlots("someName",
                DATE);

        ValueType valueType = slots.iterator().next().getSlotValue();
        Object object = valueType.getValue();
        assertEquals("Expected the slot to have the time in millis!",
                DATE.getTime(), ((BigInteger) object).longValue());
    }

    @Test(expected = IllegalArgumentException.class)
    public void testConvertingDateToSlotThrowsIllegalArgumentExceptionIfNotDate() {
        DateSlotConverter.INSTANCE.getSlots("someName", "notADate");
    }
}
