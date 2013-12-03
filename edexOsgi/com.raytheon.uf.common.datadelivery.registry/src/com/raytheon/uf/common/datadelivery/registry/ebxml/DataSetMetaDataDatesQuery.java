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

import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

import oasis.names.tc.ebxml.regrep.xsd.rim.v4.IntegerValueType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryObjectType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.SlotType;

import com.raytheon.uf.common.datadelivery.registry.DataSetMetaData;
import com.raytheon.uf.common.registry.IResultFormatter;
import com.raytheon.uf.common.registry.ebxml.encoder.IRegistryEncoder;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.time.util.ImmutableDate;

/**
 * Returns the {@link ImmutableDate} objects corresponding to filtered
 * {@link DataSetMetaData} objects.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 15, 2012 0743       djohnson     Initial creation
 * Jun 24, 2013 2106       djohnson     Pass encoder to result formatters.
 * 12/2/2013    1829       bphillip     Changed slot field in ExtensibleObjectType to be List instead of Set
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
@DynamicSerialize
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
public class DataSetMetaDataDatesQuery extends
        DataSetMetaDataFilterableQuery<ImmutableDate> implements
        IResultFormatter<ImmutableDate> {

    /**
     * {@inheritDoc}
     */
    @Override
    public ImmutableDate decodeObject(RegistryObjectType registryObjectType,
            IRegistryEncoder encoder) throws SerializationException {
        List<SlotType> returnedSlots = registryObjectType.getSlot();

        for (SlotType s : returnedSlots) {
            if (DataSetMetaData.DATE_SLOT.equals(s.getName())) {
                IntegerValueType value = (IntegerValueType) s.getSlotValue();
                return new ImmutableDate(value.getIntegerValue().longValue());
            }
        }

        return null;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Class<ImmutableDate> getResultType() {
        return ImmutableDate.class;
    }
}
