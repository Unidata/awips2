package com.raytheon.uf.common.datadelivery.registry.ebxml;

import java.util.Set;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;

import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryObjectType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.SlotType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.StringValueType;

import com.raytheon.uf.common.datadelivery.registry.DataSetName;
import com.raytheon.uf.common.registry.IResultFormatter;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;

/**
 * Extension of the AdhocQuery registry query. This implementation searches the
 * registry for DataSetMetaData Objects that satisfy the values added with the
 * various set methods.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 28, 2012 356        jspinks     Initial creation
 * May 15, 2012 455        jspinks     Updated with parameters slot.
 * Jun 21, 2012 736        djohnson    Add thrift serialization annotations.
 * Aug 02, 2012 955        djohnson    Add generics and results retrieval to registry queries.
 * Aug 15, 2012 0743       djohnson    Type-safe result formatters.
 * 
 * </pre>
 * 
 * @author jspinks
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class DataSetNameQuery extends
        DataSetFilterableQuery<String, DataSetName> implements
        IResultFormatter<String> {

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
    public Class<DataSetName> getObjectType() {
        return DataSetName.class;
    }

    @Override
    public String decodeObject(RegistryObjectType registryObjectType) {

        String object = null;

        Set<SlotType> returnedSlots = registryObjectType.getSlot();

        // Cherry pick the values to return...
        for (SlotType s : returnedSlots) {
            if ("dataSetName".equals(s.getName())) {
                StringValueType sv = (StringValueType) s.getSlotValue();
                object = sv.getStringValue();
                break;
            }
        }

        return object;
    }
}