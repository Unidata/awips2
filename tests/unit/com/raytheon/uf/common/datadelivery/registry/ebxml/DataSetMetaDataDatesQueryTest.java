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
package com.raytheon.uf.common.datadelivery.registry.ebxml;

import static org.junit.Assert.assertEquals;

import java.util.List;

import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryObjectType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.SlotType;

import org.junit.Test;

import com.raytheon.uf.common.datadelivery.registry.DataSetMetaData;
import com.raytheon.uf.common.registry.ebxml.encoder.RegistryEncoders;
import com.raytheon.uf.common.registry.ebxml.encoder.RegistryEncoders.Type;
import com.raytheon.uf.common.registry.ebxml.slots.DateSlotConverter;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.common.time.util.ImmutableDate;

/**
 * Test {@link DataSetMetaDataDatesQuery}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 15, 2012 0743       djohnson     Initial creation
 * Jun 24, 2013 2106       djohnson     IResultFormatter implementations now require an encoder passed to them.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public class DataSetMetaDataDatesQueryTest {

    @Test
    public void testReturnsDateSlotValueAsImmutableDate()
            throws SerializationException {
        ImmutableDate date = new ImmutableDate();
        List<SlotType> slots = new DateSlotConverter().getSlots(
                DataSetMetaData.DATE_SLOT, date);

        RegistryObjectType registryObject = new RegistryObjectType();
        registryObject.setSlot(slots);

        DataSetMetaDataDatesQuery query = new DataSetMetaDataDatesQuery();
        ImmutableDate result = query.decodeObject(registryObject,
                RegistryEncoders.ofType(Type.JAXB));

        assertEquals(
                "The result from the query should have matched the initial date!",
                date, result);
    }

    @Test
    public void testCanBeDynamicSerialized() throws SerializationException {
        DataSetMetaDataDatesQuery query = new DataSetMetaDataDatesQuery();
        query.setProviderName("someProvider");
        query.setDataSetName("someName");

        byte[] serialized = SerializationUtil.transformToThrift(query);
        SerializationUtil.transformFromThrift(serialized);
    }
}
