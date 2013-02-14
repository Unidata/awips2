package com.raytheon.uf.common.datadelivery.registry;

import java.util.ArrayList;
import java.util.List;

import com.raytheon.uf.common.registry.ebxml.slots.SlotConverter;

import oasis.names.tc.ebxml.regrep.xsd.rim.v4.CollectionValueType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.SlotType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.StringValueType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.ValueType;

/**
 * A <code>SlotConverter</code> implementation for converting a
 * Levels Object into a RegistryObject slot type. A collection
 * slot type will be created and it's elements will contain the
 * RegistryObject id of the ParameterLevel Objects that would be
 * created from the Levels Object. The  type for the
 * collectionSlot will be set to 'Set'.
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
 * 
 * @see Levels
 * @see DataSetMetaData
 */
public class ParameterLevelsConverter implements SlotConverter {

    /**
     * Extract a List of ParamterLevel Objects from the given Levels.
     * 
     * @param slotName
     *            The name to use for each SlotType created.
     * 
     * @param slotValue
     *            The Object that is to be interrogated for slot values. Must
     *            be a Levels Object or an IllegalArgumentException will be 
     *            thrown.
     * 
     * @return A List of SlotType Objects extracted from the slotValue Object
     *         provided. The List will contain one collectionValue slot with
     *         the keys of all the extracted ParameterLevel Objects from the
     *         Levels Object.  
     * 
     * @throws IllegalArgumentException
     *             If the slotValue Object provided cannot be converted to
     *             SlotValue Objects.
     * 
     */
    @Override
    public List<SlotType> getSlots(String slotName, Object slotValue) throws IllegalArgumentException {
        List<SlotType> slots = new ArrayList<SlotType>();
        
        
        SlotType slot = new SlotType();
        slot.setName(slotName);
        List<ValueType> collectionValues = new ArrayList<ValueType>();
        CollectionValueType cvt = new CollectionValueType();
        
        
        if (slotValue instanceof Levels) {
            Levels levels = (Levels)slotValue;
            for (Double levelValue : levels.getLevel()) {
                ValueType vt = newEntrySlot(levels.getLevelType(), levelValue);
                collectionValues.add(vt);
            }
        }
        else {
           throw new IllegalArgumentException("Object of type " + slotValue.getClass().getName() +
                   " cannot be converted by " + ParameterLevelsConverter.class.getName());
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
    private ValueType newEntrySlot(int levelType, double levelValue) {

        StringValueType entry = new StringValueType();
        entry.setStringValue(Integer.toString(levelType) + "-" + Double.toString(levelValue));
        return entry;
    }
}
