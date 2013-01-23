package com.raytheon.uf.common.registry.ebxml.slots;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;

import oasis.names.tc.ebxml.regrep.xsd.rim.v4.CollectionValueType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.SlotType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.StringValueType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.ValueType;

/**
 * A <code>SlotConverter</code> implementation for converting the
 * members of a Set into a RegistryObject slot type. A collection
 * slot type will be created and the Set members converted to elements
 * with slot values of a appropriate type for the member. The collection
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
public abstract class SetSlotConverter implements SlotConverter {

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
     *             If the slotValue Object provided is not an instance of Set, or
     *             the Set member cannot be converted to a value.
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
        
        
        if (slotValue instanceof Set) {
            
            for (Object o : (Set<?>)slotValue) {
                ValueType vt = newEntrySlot(o);
                if (vt != null) {
                   collectionValues.add(newEntrySlot(o));
                } else {
                    throw new IllegalArgumentException("Set with parameterized type " + o.getClass().getName() +
                            " cannot be converted by " + SetSlotConverter.class.getName());
                }
            }

                
        }
        else {
           throw new IllegalArgumentException("Object of type " + slotValue.getClass().getName() +
                   " cannot be converted by " + SetSlotConverter.class.getName());
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
