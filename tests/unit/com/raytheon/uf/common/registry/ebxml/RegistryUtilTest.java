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
package com.raytheon.uf.common.registry.ebxml;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryObjectType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.SlotType;

import org.junit.Test;

import com.raytheon.uf.common.datadelivery.registry.DataType;
import com.raytheon.uf.common.registry.MockRegistryObject;
import com.raytheon.uf.common.registry.ebxml.encoder.RegistryEncoders;
import com.raytheon.uf.common.registry.ebxml.encoder.RegistryEncoders.Type;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.util.ReflectionException;

/**
 * Test {@link RegistryUtil}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 22, 2012 0743       djohnson     Initial creation
 * Sep 07, 2012 1102       djohnson     Setup the registry encoder.
 * Jun 24, 2013 2106       djohnson     RegistryUtil no longer holds the encoder instance.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public class RegistryUtilTest {

    @Test
    public void newRegistryObjectCanConvertEnumValues()
            throws ReflectionException, SerializationException {
        MockRegistryObject registryObject = new MockRegistryObject();
        registryObject.setDataType(DataType.GRID);
        
        RegistryObjectType type = RegistryUtil
                .newRegistryObject(registryObject,
                        RegistryEncoders.ofType(Type.JAXB));

        SlotType slotToCheck = null;
        for (SlotType slot : type.getSlot()) {
            if (slot.getName().equals("dataType")) {
                slotToCheck = slot;
                break;
            }
        }

        assertNotNull("Did not find the slot with the enumeration value!",
                slotToCheck);
        assertEquals(DataType.GRID.toString(), slotToCheck.getSlotValue()
                .getValue());
    }

}
