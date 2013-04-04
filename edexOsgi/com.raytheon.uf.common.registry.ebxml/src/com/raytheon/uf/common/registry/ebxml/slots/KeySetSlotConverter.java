package com.raytheon.uf.common.registry.ebxml.slots;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import oasis.names.tc.ebxml.regrep.xsd.rim.v4.CollectionValueType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.SlotType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.StringValueType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.ValueType;

/**
 * A <code>SlotConverter</code> implementation for converting the
 * keys from a Map into a RegistryObject slot type. A collection
 * slot type will be created and the key values converted to elements
 * with slot values of a appropriate type for the key. The collection
 * type for the slot will be set to Set.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 15, 2012 455        jspinks     Initial creation
 *
 * </pre>
 *
 * @author jspinks
 * @version 1.0
 */
public class KeySetSlotConverter implements SlotConverter {

    /**
     * Extract a List of SlotType Objects from the given slotValue Object.
     * 
     * @param slotName
     *            The name to use for each SlotType created.
     * 
     * @param slotValue
     *            The Object that is to be interrogated for slot values.
     * 
     * @return A List of SlotType Objects extracted from the slotValue Object
     *         provided.
     * 
     * @throws IllegalArgumentException
     *             If the slotValue Object provided is not an instance of Map, or
     *             the class of the keys of the Map cannot be converted.
     * 
     * @see java.util.Map
     */
    @Override
    public List<SlotType> getSlots(String slotName, Object slotValue) throws IllegalArgumentException {
        List<SlotType> slots = new ArrayList<SlotType>();
        
        
        SlotType slot = new SlotType();
        slot.setName(slotName);
        List<ValueType> collectionValues = new ArrayList<ValueType>();
        CollectionValueType cvt = new CollectionValueType();
        
        
        if (slotValue instanceof Map) {
            
            for (Object o : ((Map<?,?>)slotValue).keySet()) {
                ValueType vt = newEntrySlot(o);
                if (vt != null) {
                   collectionValues.add(vt);
                } else {
                    throw new IllegalArgumentException("Map with key type " + o.getClass().getName() +
                            " cannot be converted by " + KeySetSlotConverter.class.getName());
                }
            }

                
        }
        else {
           throw new IllegalArgumentException("Object of type " + slotValue.getClass().getName() +
                   " cannot be converted by " + KeySetSlotConverter.class.getName());
        }
        
        cvt.setCollectionValue(collectionValues);
        cvt.setCollectionType("urn:oasis:names:tc:ebxml-regrep:CollectionType:Set");
        slot.setSlotValue(cvt);
        slots.add(slot);
        return slots;
    }
    
    /**
     * Create a new value for inclusion in the collection slot values.
     * 
     * @param value
     *        The Object to extract the slot value from. 
     *        
     * @return A ValueType to add to the collection slot.
     */
    private ValueType newEntrySlot(Object value) {
        
        // TODO: Add more types as necessary.
        if (value instanceof String) {
            StringValueType entry = new StringValueType();
            entry.setStringValue((String)value);
            return entry;
        }
        
        return null;
    }
}
