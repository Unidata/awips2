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

import java.util.Set;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;

import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryObjectType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.SlotType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.StringValueType;

import com.raytheon.uf.common.datadelivery.registry.Subscription;
import com.raytheon.uf.common.registry.IResultFormatter;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;

/**
 * Return the name of the data set the subscription is subscribed to.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 24, 2012 0743       djohnson     Initial creation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class SubscriptionDataSetNameQuery extends
        SubscriptionFilterableQuery<String> implements
        IResultFormatter<String> {

    /**
     * {@inheritDoc}
     */
    @Override
    public Class<Subscription> getObjectType() {
        return Subscription.class;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Class<String> getResultType() {
        return String.class;
    }
    /**
     * {@inheritDoc}
     */
    @Override
    public String decodeObject(RegistryObjectType registryObjectType)
            throws SerializationException {
        Set<SlotType> returnedSlots = registryObjectType.getSlot();

        // Cherry pick the values to return...
        for (SlotType s : returnedSlots) {
            if (Subscription.DATA_SET_SLOT.equals(s.getName())) {
                StringValueType sv = (StringValueType) s.getSlotValue();
                return sv.getStringValue();
            }
        }

        return null;
    }

}
