package com.raytheon.uf.common.datadelivery.registry.ebxml;

import java.util.Set;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;

import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryObjectType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.SlotType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.StringValueType;

import com.raytheon.uf.common.registry.IResultFormatter;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;

/**
 * Query to retrieve the names of {@link Parameter}s.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 21, 2012 736        djohnson    Initial creation
 * Aug 02, 2012 955        djohnson    Add generics and results retrieval to registry queries.
 * Aug 15, 2012 0743       djohnson    Type-safe result formatters.
 * 
 * </pre>
 * 
 * @author djohnson
 */
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class ParameterNameQuery extends ParameterFilterableQuery<String>
        implements IResultFormatter<String> {

    @Override
    public String decodeObject(RegistryObjectType registryObjectType) {

        String object = null;

        Set<SlotType> returnedSlots = registryObjectType.getSlot();

        // Cherry pick the values to return...
        for (SlotType s : returnedSlots) {
            if ("name".equals(s.getName())) {
                StringValueType sv = (StringValueType) s.getSlotValue();
                object = sv.getStringValue();
                break;
            }
        }

        return object;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Class<String> getResultType() {
        return String.class;
    }
}
