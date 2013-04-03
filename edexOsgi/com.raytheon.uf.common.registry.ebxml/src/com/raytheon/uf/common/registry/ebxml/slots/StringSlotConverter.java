package com.raytheon.uf.common.registry.ebxml.slots;

import java.util.ArrayList;
import java.util.List;

import oasis.names.tc.ebxml.regrep.xsd.rim.v4.SlotType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.StringValueType;

public class StringSlotConverter implements SlotConverter {

    public static final SlotConverter INSTANCE = new StringSlotConverter();

    @Override
    public List<SlotType> getSlots(String slotName, Object slotValue) throws IllegalArgumentException {
        List<SlotType> slots = new ArrayList<SlotType>();
        if (slotValue instanceof String) {
            SlotType slot = new SlotType();
            StringValueType sv = new StringValueType();
            slot.setName(slotName);
            sv.setStringValue((String)slotValue);
            slot.setSlotValue(sv);
            slots.add(slot);
        }
        else {
           throw new IllegalArgumentException("Object of type " + slotValue.getClass().getName() +
                   " cannot be converted by " + StringSlotConverter.class.getName());
        }
        return slots;
    }
    

}
