package com.raytheon.uf.common.datadelivery.registry.ebxml;

import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;

import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryObjectType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.SlotType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.StringValueType;

import com.raytheon.uf.common.datadelivery.registry.GroupDefinition;
import com.raytheon.uf.common.registry.IResultFormatter;
import com.raytheon.uf.common.registry.ebxml.encoder.IRegistryEncoder;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;

/**
 * Query to retrieve the names of {@link GroupDefinition}s.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 02, 2013 1441       djohnson    Initial creation
 * Jun 24, 2013 2106       djohnson    Pass encoder to result formatters.
 * 12/2/2013    1829       bphillip    Changed slot field in ExtensibleObjectType to be List instead of Set
 * 
 * </pre>
 * 
 * @author djohnson
 */
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class GroupNameQuery extends GroupDefinitionFilterableQuery<String>
        implements IResultFormatter<String> {

    @Override
    public String decodeObject(RegistryObjectType registryObjectType,
            IRegistryEncoder encoder) {

        List<SlotType> returnedSlots = registryObjectType.getSlot();

        // Cherry pick the values to return...
        for (SlotType s : returnedSlots) {
            if (GroupDefinition.GROUP_NAME_SLOT.equals(s.getName())) {
                StringValueType sv = (StringValueType) s.getSlotValue();
                return sv.getStringValue();
            }
        }

        return null;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Class<String> getResultType() {
        return String.class;
    }
}
