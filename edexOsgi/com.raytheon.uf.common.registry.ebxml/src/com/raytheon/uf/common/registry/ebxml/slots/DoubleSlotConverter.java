package com.raytheon.uf.common.registry.ebxml.slots;

import java.util.ArrayList;
import java.util.List;

import oasis.names.tc.ebxml.regrep.xsd.rim.v4.FloatValueType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.SlotType;

/**
 * A <code>SlotConverter</code> implementation for converting a 
 * double or float attribute into a RegistryObject slot type.
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 15, 2012 355        jspinks     Initial creation
 *
 * </pre>
 *
 * @author jspinks
 * @version 1.0
 */
public class DoubleSlotConverter implements SlotConverter {

    public static final SlotConverter INSTANCE = new DoubleSlotConverter();

    /**
     * Extract a List of SlotType Objects from the given slotValue Object.
     * 
     * @param slotName
     *            The name to use for each SlotType created.
     * 
     * @param slotValue
     *            The Object that is to be interrogated for slot values. Must
     *            be a double or float.
     * 
     * @return A FloatValueType slot for the provided slotValue parameter.
     * 
     * @throws IllegalArgumentException
     *             If the slotValue Object provided cannot be converted to
     *             SlotValue Objects.
     * 
     */
    @Override
    public List<SlotType> getSlots(String slotName, Object slotValue) throws IllegalArgumentException {
        List<SlotType> slots = new ArrayList<SlotType>();
        
        if (slotValue instanceof Double || 
            slotValue instanceof Float) {

            SlotType slot = new SlotType();
            FloatValueType sv = new FloatValueType();
            slot.setName(slotName);
            sv.setValue(((Double)slotValue).floatValue());
            slot.setSlotValue(sv);
            slots.add(slot);
        }
        else {
           throw new IllegalArgumentException("Object of type " + slotValue.getClass().getName() +
                   " cannot be converted by " + DoubleSlotConverter.class.getName());
        }
        return slots;
    }
    

}
