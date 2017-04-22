package com.raytheon.uf.common.registry.ebxml.slots;

import java.util.ArrayList;
import java.util.List;

import oasis.names.tc.ebxml.regrep.xsd.rim.v4.BooleanValueType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.SlotType;

public class BooleanSlotConverter implements SlotConverter {

    public static final SlotConverter INSTANCE = new BooleanSlotConverter();

    @Override
    public List<SlotType> getSlots(String slotName, Object slotValue) throws IllegalArgumentException {
        List<SlotType> slots = new ArrayList<SlotType>();
        if (slotValue instanceof Boolean) {
            SlotType slot = new SlotType();
            BooleanValueType sv = new BooleanValueType();
            slot.setName(slotName);
            sv.setBooleanValue((Boolean)slotValue);
            slot.setSlotValue(sv);
            slots.add(slot);
        }
        else {
           throw new IllegalArgumentException("Object of type " + slotValue.getClass().getName() +
                   " cannot be converted by " + BooleanSlotConverter.class.getName());
        }
        return slots;
    }
    

}
